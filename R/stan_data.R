require(tidyverse)

stan_data <- function(data, var){

d <- data

d_pid <- d %>%
  group_by(pid) %>%
  summarise(pop_id = unique(pop_id),
            birthyear_s = unique(birthyear_s))

d_pop <- d %>%
  group_by(pop_id) %>%
  summarise(pop_name = unique(population))

if (var == "base") {
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
}

return(data_list)
}