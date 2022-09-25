require(rethinking)
require(ggridges)
require(HDInterval)

PPD_check <- function(fit, data){
  
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  y_rep <- post$y_hat
  y <- data$live_births
  
  y_rep_df <- as.data.frame(y_rep) %>% 
    mutate(rep = 1:n_samps) %>% 
    filter(rep < 201) %>% 
    pivot_longer(-rep) %>% 
    group_by(rep, value) %>% 
    tally() %>% 
    group_by(rep) %>% 
    mutate(prop = n / sum(n))
  
  y_rep_summary <- y_rep_df %>% 
    group_by(value) %>% 
    summarise(
      lower = HDInterval::hdi(prop, credMass = 0.96)[1],
      upper = HDInterval::hdi(prop, credMass = 0.96)[2],
      med = median(prop)
      ) %>% 
    mutate(grp = "PPD")
  
  d_y <- data %>% 
    group_by(live_births) %>% 
    tally() %>% 
    mutate(prop = n / sum(n), grp = "data")
    
  p <- ggplot(y_rep_summary, aes(x = as.integer(value), y = med, color = grp)) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, lwd = 1.6) +
    geom_point(data = d_y, aes(y = prop, x = as.integer(live_births)), size = 2, alpha = 0.5) +
    scale_color_manual(values = c("black", "skyblue")) +
    scale_x_continuous(limits = c(0, max(data$live_births))) +
    theme_bw(base_size = 16) +
    theme(legend.title = element_blank()) +
    ylab("Proportion of Observations") +
    xlab("Num. Live Births")
  
  return(p)
}
