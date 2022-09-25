require(HDInterval)
require(future)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

MI_contrasts <- function(fit, data) {
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  
  age_pred <- 60

  d_pred <- expand.grid(age = age_pred, MI = 0:2)

  m <- future_pmap(d_pred, post = post, resp = "mean", n_samps = n_samps, fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  names(m) <- c("low", "med", "high")
  
  diff_high_low <- m$high - m$low
  
  pop_pred <- data.frame(
    med = apply(m, 2, median),
    lower = apply(m, 2, HPDI, prob = 0.9)[1,],
    upper = apply(m, 2, HPDI, prob = 0.9)[2,],
    level = c("Low MI", "Med MI", "High MI")
  )

  fert_age_60 <- ggplot(pop_pred, aes(x = med, y = fct_reorder(level, med), color = level)) + 
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = fct_reorder(level, med)), height = 0, lwd = 1.5) +
    scale_color_manual(values=c("slateblue4", "slateblue1", "slateblue3")) +
    geom_point(size = 3) +
    theme_minimal(base_size = 18) +
    ylab("") +
    xlab("Cumulative Fertility | Age = 60") + 
    theme(legend.title = element_blank(), legend.position = "none")
  
  ggsave("MI_CF.pdf", plot = fert_age_60, height = 5, width = 5)
  
  pop_pred$PP <- NA
  
  pop_pred[nrow(pop_pred) + 1, ] <- c(
    median(diff_high_low), HPDI(diff_high_low, prob = 0.9)[1], HPDI(diff_high_low, prob = 0.9)[2], "diff_high_low", mean(diff_high_low < 0)
  )
  
  pop_pred_export <- pop_pred %>% 
    mutate(
      med = round(as.numeric(med), 2),
      lower = round(as.numeric(lower), 2),
      upper = round(as.numeric(upper), 2),
      PP = round(as.numeric(PP), 3))
  
  write_csv(pop_pred_export, file = "MI_estimates.csv")
  
  return(fert_age_60)
}
