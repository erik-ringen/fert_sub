require(HDInterval)
require(future)
require(furrr)
plan(multisession, workers = availableCores())
options(future.rng.onMisuse = "ignore")

urban_contrasts <- function(fit, var_name, d) {
  data <- stan_data(d, variable = var_name)
  post <- extract.samples(fit)
  n_samps <- length(post$lp__)
  
  age_pred <- 60
  
  ## First, calculate overall contrast
  pred <- data$df %>% ungroup() %>% summarise(min_pred = min(pred), max_pred = max(pred))

  d_pred <- expand.grid(age = age_pred, pred_value = (pred[1,]))

  m <- future_pmap(d_pred, post = post, resp = "mean", n_samps = n_samps, ind_pred = T, mono_pred = T, fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  names(m) <- c("min_pred", "max_pred")
  
  overall_contrast <- m$max_pred - m$min_pred
  
  # Now do for each pop, taking relative sds
  pop_df <- data$df %>% 
    group_by(pop_id) %>% 
    summarise(population = unique(pop_name), min_pred = min(pred), max_pred = max(pred),
              subsistence = unique(subsist),
              MI = unique(MI))
  
  d_pred <- data.frame(age = age_pred, pred_value = (pop_df$min_pred), pop_id = pop_df$pop_id)
  pred_min <- future_pmap(d_pred, post = post, resp = "mean", n_samps = n_samps, ind_pred = T, mono_pred = T, fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  names(pred_min) <- pop_df$population
  
  d_pred <- data.frame(age = age_pred, pred_value = (pop_df$max_pred), pop_id = pop_df$pop_id)
  pred_max <- future_pmap(d_pred, post = post, resp = "mean", n_samps = n_samps, ind_pred = T, mono_pred = T, fert_pred) %>% 
    as_tibble(.name_repair = "minimal")
  names(pred_max) <- pop_df$population
  
  PP <- apply(pred_max - pred_min, 2, function(x){mean(x > 0)})
  PP <- ifelse(PP < 0.5, 1 - PP, PP)
  
  pop_pred_contrast <- data.frame(
    med = apply(pred_max - pred_min, 2, median),
    lower = apply(pred_max - pred_min, 2, HPDI, prob = 0.9)[1,],
    upper = apply(pred_max - pred_min, 2, HPDI, prob = 0.9)[2,],
    pop = pop_df$population,
    subsist = pop_df$subsistence,
    MI = pop_df$MI,
    PP = PP
  )
  
  PP_overall <- mean(overall_contrast > 0)
  PP_overall <- ifelse(PP_overall < 0.5, 1 - PP_overall, PP_overall)
  
  pop_pred_contrast <- bind_rows(pop_pred_contrast, data.frame(med = median(overall_contrast), lower = HPDI(overall_contrast, prob = 0.9)[1], upper = HPDI(overall_contrast, prob = 0.9)[2], pop = "Average", subsist = "Average", MI = 99, PP = PP_overall))
  
  pop_pred_contrast$subsist <- ifelse(pop_pred_contrast$pop == "Average", "Average", pop_pred_contrast$subsist)
  pop_pred_contrast$MI <- factor(pop_pred_contrast$MI, levels = rev(c("99", "0", "1", "2")), labels=rev(c(" ", "Low MI", "Medium MI", "High MI")))
  
  pop_pred_contrast$pop <- fct_relevel(pop_pred_contrast$pop, "Average", after=0)
  
      fert_age_60 <- ggplot(pop_pred_contrast, aes(x = med, y = pop, color = subsist)) + 
        facet_grid(MI~., scales="free_y") +
        geom_errorbarh(aes(xmin = lower, xmax = upper, y = pop), height = 0, lwd = 1.5) +
        geom_point(size = 3) +
        col_scale +
        theme_minimal(base_size = 18) +
        geom_vline(aes(xintercept = 0), linetype = "dashed") +
        ylab("") +
        xlab(as.expression(bquote(Delta~"CF, Urban - Rural"))) + 
        theme(legend.title = element_blank())
  
  ggsave(paste0("CF_forest_", as.character(var_name), ".pdf"), plot = fert_age_60, height = 8, width = 9)
  
  pred_export <- pop_pred_contrast %>% 
    mutate(
      med = round(med, 2),
      lower = round(lower, 2),
      upper = round(upper, 2),
      PP = round(PP, 3))
  
  write_csv(pred_export, file = paste0(as.character(var_name), "_contrasts", ".csv"))
  
  return(fert_age_60)
}
