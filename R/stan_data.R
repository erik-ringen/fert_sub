require(tidyverse)

stan_data <- function(data, var = "base"){

d <- data

# Order market levels
d$market <- factor(droplevels(d$market),
 levels = c("low", "medium", "high"))
d$MI <- as.numeric(d$market) - 1

d_pid <- d %>%
  group_by(pid) %>%
  summarise(pop_id = unique(pop_id),
            birthyear_s = unique(birthyear_s),
            sub_id = unique(sub_id),
            MI = unique(MI))

d_pop <- d %>%
  group_by(pop_id) %>%
  summarise(pop_name = unique(population))

data_list <- list(
  N_obs = nrow(d),
  N_id = max(d$pid),
  N_pop = max(d$pop_id),
  pop_id = d_pid$pop_id,
  pid = d$pid,
  age = d$age/80,
  live_births = d$livebirths,
  birthyear_s = d_pid$birthyear_s
)

if (var == "MI") {
  data_list$MI <- d_pid$MI
  data_list$N_MI <- max(d_pid$MI)
}

return(data_list)
}