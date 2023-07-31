require(rethinking)
require(ggridges)
require(HDInterval)

PPD_check <- function(fit, data){
  
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  y_rep <- post$y_hat
  y <- data$live_births
  
  data$obs <- as.character(1:nrow(data))
  
  y_rep_df <- as.data.frame(y_rep) %>% 
    mutate(rep = 1:n_samps) %>% 
    filter(rep < 51) %>% 
    pivot_longer(-rep) %>% 
    rename(obs = name) %>% 
    mutate(obs = substr(obs, 2, nchar(obs))) %>% 
    left_join(select(data, c(obs, population)))
  
  p <- ggplot(y_rep_df, aes(x = value, group = rep)) + 
    facet_wrap(~population, scales = "free") +
    geom_density(alpha = 0.1, linewidth = 0.1, color = "cornflowerblue") +
    geom_density(data = data, aes(x = live_births, group = NA), color = "black") +
      theme_minimal(base_size = 12) +
      theme(legend.title = element_blank(),
            strip.background = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA)) +
      ylab("density") +
      xlab("Num. Live Births")
  
  ggsave("posterior_predictive_check.pdf", plot = p, height = 8.5, width = 11)
  
  return(p)
}
