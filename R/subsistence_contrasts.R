require(HDInterval)
require(future)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

subsistence_contrasts <- function(fit, data) {
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  
  age_pred <- 60

  d_pred <- expand.grid(age = age_pred, sub = 1:data$data_list$N_sub)

  m <- future_pmap(d_pred, post = post, resp = "mean", n_samps = n_samps, fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  names(m) <- c("HG", "Ag", "hort", "fish", "past")
  
  HG_Ag <- m$HG - m$Ag
  HG_Hort <- m$HG - m$hort
  
  pop_pred <- data.frame(
    med = apply(m, 2, median),
    lower = apply(m, 2, HPDI, prob = 0.9)[1,],
    upper = apply(m, 2, HPDI, prob = 0.9)[2,],
    age = d_pred$age,
    sub_id = d_pred$sub
  )
  
  data$df$sub_id
  d_pop <- data$df %>%
    group_by(sub_id) %>% 
    summarise(subsist = unique(subsist))
  
  pop_pred <- left_join(pop_pred, d_pop)
  
  fert_age_60 <- ggplot(pop_pred, aes(x = med, y = fct_reorder(subsist, med), color = subsist)) + 
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = fct_reorder(subsist, med)), height = 0, lwd = 1.5) +
    geom_point(size = 3) +
    col_scale +
    theme_minimal(base_size = 18) +
    ylab("") +
    xlab("Cumulative Fertility | Age = 60") + 
    theme(legend.title = element_blank())
  
  ggsave("submode_CF.pdf", plot = fert_age_60, height = 8, width = 9)
  
  pop_pred_export <- pop_pred %>% 
    mutate(
      med = round(med, 2),
      lower = round(lower, 2),
      upper = round(upper, 2)) %>% 
    select(-c(sub_id))
  
  write_csv(pop_pred_export, file = "submode_estimates.csv")
  
  return(fert_age_60)
}
