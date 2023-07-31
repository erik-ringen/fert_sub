require(HDInterval)
require(future)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

# Establish color schemes
subsistence_cols <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "indianred", "black")
names(subsistence_cols) <- c("Ag", "fish", "HG", "hort", "labour", "past", "Average")

col_scale <- scale_color_manual(name = "subtype", values = subsistence_cols)
fill_scale <- scale_fill_manual(name = "subtype", values = subsistence_cols) 

fig_4_b <- function(fit, d) {
  data <- stan_data(d)
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  
  age_pred <- 60

  d_pred <- expand.grid(age = age_pred, pop_id = 1:data$data_list$N_pop)

  m <- future_pmap(d_pred, post = post, resp = "mean", fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  
  pop_pred <- data.frame(
    med = apply(m, 2, median),
    lower = apply(m, 2, HPDI, prob = 0.9)[1,],
    upper = apply(m, 2, HPDI, prob = 0.9)[2,],
    age = d_pred$age,
    pop_id = d_pred$pop_id
  )
  
  d_pop <- data$df %>%
    group_by(pop_name) %>% 
    summarise(pop_id = unique(pop_id), subsist = unique(subsist), MI = unique(MI))
  
  pop_pred <- left_join(pop_pred, d_pop)
  
  pop_pred$pop_name <- fct_reorder(pop_pred$pop_name, as.numeric(as.factor(pop_pred$subsist)))

  pop_pred$MI <- factor(pop_pred$MI, labels = c("Low MI", "Medium MI", "High MI"))
  
  fert_age_60 <- ggplot(pop_pred, aes(x = med, y = fct_reorder(pop_name, med), color = subsist)) + 
    facet_grid(fct_rev(MI)~., scales = "free_y") +
    geom_errorbarh(aes(xmin = lower, xmax = upper, y = fct_reorder(pop_name, med)), height = 0, lwd = 1.5) +
    geom_point(size = 3) +
    col_scale +
    theme_minimal(base_size = 18) +
    ylab("") +
    xlab("Cumulative Fertility | Age = 60") + 
    theme(legend.title = element_blank())
  
  ggsave("figure_4_b.pdf", plot = fert_age_60, height = 8, width = 9)
  
  pop_pred_export <- pop_pred %>% 
    mutate(
      med = round(med, 2),
      lower = round(lower, 2),
      upper = round(upper, 2)) %>% 
    select(-c(pop_id))
  
  write_csv(pop_pred_export, file = "fig_4_b_estimates.csv")
  
  return(fert_age_60)
}
