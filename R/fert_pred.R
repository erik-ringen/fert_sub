require(future)
require(tidyverse)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

fert_pred <- function(
    post,
    pop_id = NA,
    pid = NA,
    age = 60,
    n_samps = 500,
    n_sims = 1000,
    resp = "mean",
    sub = NA,
    MI = NA,
    ind_pred = F,
    mono_pred = F,
    pred_value = 0) {
  
  samp_seq <- 1:n_samps
  
  # Mean parameters
  a_k <- post$a_k[samp_seq]
  a_b <- post$a_b[samp_seq]
  a_alpha <- post$a_alpha[samp_seq]
  a_oi <- post$a_oi[samp_seq]
  
  # Individual-level predictors
  if (mono_pred == T) b_pred <- post$b_ord[samp_seq,]
  if (ind_pred == T & mono_pred == F) b_pred <- post$b_pred[samp_seq,]
  
  # Individual random effects
  pid_v <- matrix(0, nrow=n_samps, ncol=2)
  if (!(is.na(pid))) pid_v <- post$pid_v[samp_seq,pid,]
  
  # Population level random effects
  pop_v <- matrix(0, nrow=n_samps, ncol=50)
  if (!(is.na(pop_id))) pop_v <- post$pop_v[samp_seq,pop_id,]
  
  # Subsistence mode random effects
  sub_v <- matrix(0, nrow=n_samps, ncol=2)
  if (!(is.na(sub))) sub_v <- post$sub_v[samp_seq,sub,]
  
  # Effect of market integration level
  if (is.na(MI)) {
    b_MI <- matrix(0, nrow=n_samps, ncol = 2)
    S_MI <- array(0, dim=c(n_samps, 2, 2))
  }
  
  else {
    b_MI <- post$b_MI[samp_seq,]
    S_MI <- post$S_MI[samp_seq,,]
  }
  
  # Simplex for monotonic effects
  if (mono_pred == T) {
    a_scale <- post$a_scale[samp_seq,,]
    
    K = dim(post$pop_scale)[3]
    pop_scale = array(0, dim=c(n_samps, K, 2))
    
    for (j in 1:2) {
      c <- matrix(NA, nrow=n_samps, ncol=K)
      
      if (K > 2) {
        for (k in 1:(K-1)) c[,k] = a_scale[,k,j] + pop_v[,(6 + (j-1)*(K-1) + k)]
      }
      
      else if (K == 2) c[,1] = a_scale[,j] + pop_v[,(6 + (j-1)*(K-1) + 1)]
      
      c[,K] = 0
      pop_scale[,,j] = c
    }
  }
  
  k <- rep(NA, n_samps)
  b <- rep(NA, n_samps)
  alpha <- rep(NA, n_samps)
  
  for (i in samp_seq) {
    
    # only pop-level parameters
    if (ind_pred == F) {
      k[i] = exp(a_k[i] + pop_v[i,1] + pid_v[i,1] + sub_v[i,1] + b_MI[i,1]*monotonic(S_MI[i,1,],MI))
      b[i] = exp(a_b[i] + pop_v[i,2])
      alpha[i] = exp(a_alpha[i] + pop_v[i,3] + pid_v[i,2] + sub_v[i,2] + b_MI[i,2]*monotonic(S_MI[i,2,],MI))
    }
    
    # individual-level preds, and monotonic
    if (ind_pred == T & mono_pred == T) {
      k[i] = exp(a_k[i] + pop_v[i,1] + pid_v[i,1] + sub_v[i,1] + b_MI[i,1]*monotonic(S_MI[i,1,],MI) + (b_pred[i,1] + pop_v[i,5])*monotonic(softmax(pop_scale[i,,1]), pred_value))
      
      b[i] = exp(a_b[i] + pop_v[i,2]);
      
      alpha[i] = exp(a_alpha[i] + pop_v[i,3] + pid_v[i,2] + sub_v[i,2] + b_MI[i,2]*monotonic(S_MI[i,2,],MI) + (b_pred[i,2] + pop_v[i,6])*monotonic(softmax(pop_scale[i,,2]), pred_value))
    }
    
    # individual-level preds, NOT monotonic
    if (ind_pred == T & mono_pred == F) {
      k[i] = exp(a_k[i] + pop_v[i,1] + pid_v[i,1] + sub_v[i,1] + b_MI[i,1] * monotonic(S_MI[i,1,],MI) + (b_pred[i,1] + pop_v[i,5]) * pred_value)
      
      b[i] = exp(a_b[i] + pop_v[i,2] + pid_v[i,2]);
      
      alpha[i] = exp(a_alpha[i] + pop_v[i,3] + pid_v[i,2] + sub_v[i,2] + b_MI[i,2] * monotonic(S_MI[i,2,],MI) + (b_pred[i,2] + pop_v[i,6])*pred_value)
    }
    
  }
  
  p_oi <- inv_logit(a_oi + pop_v[,4])
  
  fert_cu <- (1 - exp(-k*(age/80)))^b * alpha
  
  if (resp == "mean") {
    # incorporate zero-inflation
    mean_fert <- fert_cu * (1 - p_oi) + p_oi
    return(mean_fert)
  }
  
  if (resp == "samps") {
      sims <- future_pmap(data.frame(p_oi = p_oi, lambda = fert_cu, n_sims = n_sims), poisson_oi_rng) %>% as_vector() %>% sample(size = n_samps, replace = T)
    return(sims)
  }
  
}
