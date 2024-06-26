functions{
  /* compute monotonic effects, function lifted from brms package
  * Args:
    *   scale: a simplex parameter
  *   i: index to sum over the simplex
  * Returns:
    *   a scalar between 0 and 1
  */
    real mo(vector scale, int i) {
      if (i > 0) return sum(scale[1:i]);
      else return 0;
    }
}

data{
    int N_obs;
    int N_id;
    int N_pop;
    int N_MI;
    array[N_id] int pop_id;
    array[N_obs] int pid;
    vector[N_obs] age;
    array[N_obs] int live_births;
    vector[N_id] birthyear_s;
    array[N_id] int MI;
}

parameters {
    // Global mean parameters //
    real a_alpha; // avg alpha
    real a_k; // avg k
    real a_b; // avg b
    real a_oi;
    
    real b_BY; // avg effect of birth year
    
    vector[2] b_MI; // total market integration
    // monotonic effects
    array[2] simplex[N_MI] S_MI;
    
    vector[N_pop] pop_BY_z;
    real<lower=0> sigma_pop_BY;
    
    // Random effects //
    matrix[N_id,2] pid_z; // individual-level random effects, unscaled and uncorrelated
    vector<lower=0>[2] sigma_pid; // scale par for pid

    matrix[N_pop,4] pop_z; // population-level random effects
    vector<lower=0>[4] sigma_pop; // 
}

transformed parameters{
  matrix[N_id,2] pid_v;
  matrix[N_pop,4] pop_v;
  vector[N_pop] pop_BY_v;
  
  vector[N_id] k;
  vector[N_id] b;
  vector[N_id] alpha;
  vector[N_id] oi;

  // Scale random effects //
  for (n in 1:N_id)
    for (j in 1:2 ) {
  pid_v[n,j] = pid_z[n,j] * sigma_pid[j];
    }
  
  for (j in 1:4) pop_v[,j] = pop_z[,j] * sigma_pop[j];

  pop_BY_v = pop_BY_z * sigma_pop_BY;

  // Compose parameters
  for (n in 1:N_id) {
    k[n] = exp(a_k + pop_v[pop_id[n],1] + pid_v[n,1] + b_MI[1]*mo(S_MI[1, ], MI[n]));
    b[n] = exp(a_b + pop_v[pop_id[n],2]);
    alpha[n] = exp(a_alpha + pop_v[pop_id[n],3] + pid_v[n,2] + (b_BY + pop_BY_v[pop_id[n]])*birthyear_s[n] + b_MI[2]*mo(S_MI[2, ], MI[n]));
    oi[n] = inv_logit(a_oi + pop_v[pop_id[n],4]);
  }
} // end transformed parameters block

model{
  //// Priors ////
  // Overall intercepts
  a_k ~ normal(0,2); 
  a_b ~ normal(0,2);
  a_alpha ~ normal(0,2);
  a_oi ~ normal(0,2);

  // Overall effect of birth year
  b_BY ~ std_normal(); // std_normal = Normal(0,1)
  
  // Total effect of MI
  b_MI ~ std_normal();
  
  for (j in 1:2) {
    S_MI[j,] ~ dirichlet(rep_vector(2, N_MI));
  }

  // varying effects, unscaled
  pop_BY_z ~ std_normal();
  to_vector(pid_z) ~ std_normal();
  to_vector(pop_z) ~ std_normal();

  // variance components (half normal)
  sigma_pid ~ std_normal();
  sigma_pop ~ std_normal();
  sigma_pop_BY ~ std_normal();

  // likelihood
        for (i in 1:N_obs) { 
        real fert_cu;

        fert_cu = pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]]) * alpha[pid[i]];
        
        if (live_births[i] == 1) {
            target += log_sum_exp(
            log(oi[pid[i]]),
            log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu)
          );
        }
        
        else {
          target += log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu);
        }
    }
}

generated quantities {
   vector[N_obs] log_lik;
   vector[N_obs] y_hat;

   for (i in 1:N_obs) {
    real fert_cu;
    int oi_hat;

    fert_cu = pow(1 - exp(-k[pid[i]]*age[i]), b[pid[i]]) * alpha[pid[i]];
       if (live_births[i] == 1) {
            log_lik[i] = log_sum_exp(
            log(oi[pid[i]]),
            log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu)
          );
        }
        
        else {
          log_lik[i] = log1m(oi[pid[i]]) + poisson_lpmf(live_births[i] | fert_cu);
        }
    
    oi_hat = bernoulli_rng(oi[pid[i]]);
    if (oi_hat == 1) y_hat[i] = 1;
    else y_hat[i] = poisson_rng(fert_cu);
   }
}
