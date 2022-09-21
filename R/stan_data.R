require(tidyverse)

stan_data <- function(data, variable = "base"){

dat <- data

#### Contingencies depending on the model being fit ##
if (variable == "test") dat <- dat[sample(1:nrow(dat), size = 2000, replace = T),]

if (variable == "submode") {
  dat <- dplyr::filter(dat, subsist != "labour") # excludes Mestizo Altiplano
  dat$sub_id <- match(dat$sub_id, unique(dat$sub_id)) # reset indices
}

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket")) {
  d_pop <- dat %>% 
    group_by(population) %>% 
    summarise( prop_missing = mean(is.na(get(variable))),
               variable_var = var(get(variable), na.rm = T)) %>% 
    filter(prop_missing < 1 & variable_var != 0)
  
  dat <- dat %>% 
    filter(population %in% d_pop$population) %>% 
    filter( !is.na(get(variable)) ) # removing individuals with missing predictor for now
}
######################################################

# Make stan friendly index
dat$pid <- match(dat$pid, unique(dat$pid))
dat$pop_id <- match(dat$pop_id, unique(dat$pop_id))

# Order market levels
dat$market <- factor(droplevels(dat$market),
 levels = c("low", "medium", "high"))
dat$MI <- as.numeric(dat$market) - 1

d_pid <- dat %>%
  group_by(pid) %>%
  summarise(pop_id = unique(pop_id),
            birthyear_s = unique(birthyear_s),
            sub_id = unique(sub_id),
            MI = unique(MI))

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket")) {
  d_pid2 <- dat %>%
    group_by(pid) %>%
    summarise(pred = mean(get(variable)))
  
  d_pid <- left_join(d_pid, d_pid2)
}

d_pop <- dat %>%
  group_by(pop_id) %>%
  summarise(pop_name = unique(population))

# Data that goes into every model
data_list <- list(
  N_obs = nrow(dat),
  N_id = max(dat$pid),
  N_pop = max(dat$pop_id),
  pop_id = d_pid$pop_id,
  pid = dat$pid,
  age = dat$age/80,
  live_births = dat$live_births,
  birthyear_s = d_pid$birthyear_s
)

# Data for specific models only
if (variable == "MI") {
  data_list$MI <- d_pid$MI
  data_list$N_MI <- max(d_pid$MI)
}

if (variable == "submode") {
  data_list$sub_id <- d_pid$sub_id
  data_list$N_sub <- max(d_pid$sub_id)
}

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket")) {
  data_list$pred <- d_pid$pred
}

return(data_list)
}
