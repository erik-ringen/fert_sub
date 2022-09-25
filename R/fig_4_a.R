require(HDInterval)
require(future)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

fig_4_a <- function(fit, data) {
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  
  n_ages <- 30
  age_seq <- seq(from = 0, to = 60, length.out = n_ages)

    # # Pop-level variance ##
  # d_pred <- expand.grid(age = age_seq, pop_id = 1:data$data_list$N_pop)
  # 
  # m <- future_pmap(d_pred, post = post, resp = "samps", fert_pred) %>% 
  #   as_tibble(.name_repair = "minimal")
  # 
  # pop_pred_summary <- data.frame(
  #   lower = apply(m, 2, HPDI, prob = 0.9)[1,],
  #   upper = apply(m, 2, HPDI, prob = 0.9)[2,],
  #   age = d_pred$age,
  #   pop_id = d_pred$pop_id
  # )
  
  # Individuals within populations ##
  d_pred <- expand.grid(age = age_seq, pid = 1:data$data_list$N_id)
  d_pred <- left_join(d_pred, select(data$df, pid, pop_id))
  
  m <- future_pmap(select(d_pred, -c(population)), post = post, resp = "mean", fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  
  pred_median <- apply(m, 2, median)
  
  pid_pred <- data.frame(
    med = pred_median,
    age = d_pred$age,
    pop_id = d_pred$pop_id,
    pid = d_pred$pid
  )
  
  pid_pred <- left_join(pid_pred, select(data$df, pid, pop_id, population, subsist, MI))
  
  pid_pred$population <- fct_reorder(pid_pred$population, as.numeric(as.factor(pid_pred$subsist)))

  pid_pred$MI <- factor(pid_pred$MI, labels = c("Low MI", "Medium MI", "High MI"))
  
  lh_curves <- ggplot(pid_pred, aes(x = age, y = med, color = subsist)) + 
    facet_wrap(MI ~ population, labeller = label_wrap_gen(width = 6)) +
    geom_line(aes(group = pid), lwd = 0.2, alpha = 0.1 ) +
    theme_minimal(base_size = 12) + 
    xlab("") +
    ylab("") + 
    col_scale +
    scale_x_continuous(breaks = c(0,16,32,48)) +
    theme(
      strip.background = element_blank(),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      legend.position = c(0.7,0.05),
      legend.direction = 'horizontal',
      legend.title = element_blank()) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, lwd = 2)))
  
  ggsave("fig_4_a.pdf", plot = lh_curves, height = 8, width = 9)
  
  return(lh_curves)
}
