require(tidyverse)

stan_data <- function(dat, variable = "base") {

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket", "occ", "land", "livestock", "income", "urban", "edu")) {
dat <- data %>% 
  mutate(pred = get(variable))
}

#### Contingencies depending on the model being fit ##
if (variable == "test") dat <- dat[sample(1:nrow(dat), size = 2000, replace = T),]

if (variable == "submode") {
  dat <- dplyr::filter(dat, subsist != "labour") # excludes Mestizo Altiplano
  dat$sub_id <- match(dat$sub_id, unique(dat$sub_id)) # reset indices
}

if (variable == "urban") {
  dat <- dat %>% 
    mutate( 
      pred = case_when(
        pred == "rural" ~ 0,
        pred == "transition" ~ 1,
        pred == "urban" ~ 2
      ))
}

if (variable == "edu") {
  dat <- dat %>% 
    filter(population != "Himba") %>% # measure not comparable
    # Collapse more than 12 years of edu
    mutate(variable = ifelse(variable > 12, 13, variable))
}

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket", "occ", "land", "livestock", "income")) {
  d_pop <- dat %>% 
    group_by(population) %>% 
    summarise( prop_missing = mean(is.na(pred)),
               variable_var = var(pred, na.rm = T)) %>% 
    filter(prop_missing < 1 & variable_var != 0)
  
  dat <- dat %>% 
    filter(population %in% d_pop$population) %>% 
    filter( !is.na(pred) ) # removing individuals with missing predictor for now

  if (variable %in% c("land", "livestock", "income")) {
    ## Standardize within pop #####
    dat <- dat %>% group_by(population) %>% 
      mutate(pred = as.numeric(scale( log(1 + pred)))) %>% 
      filter( pred > -10 ) # dropping extreme low outliers for now
  }
}

if (variable %in% c("urban", "edu")) {
  d_pop <- dat %>% 
    group_by(population) %>% 
    summarise( prop_missing = mean(is.na(pred)),
               variable_var = var(pred, na.rm = T)) %>% 
    filter(variable_var > 0)
  
  dat <- dat %>% 
    filter(population %in% d_pop$population) %>% 
    filter( !is.na(pred) ) # removing individuals with missing predictor for now
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

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket", "occ", "land", "livestock", "income")) {
  d_pid2 <- dat %>%
    group_by(pid) %>%
    summarise(pred = mean(pred))
  
  d_pid <- left_join(d_pid, d_pid2)
}

if (variable %in% c("urban", "edu")) {
  d_pid2 <- dat %>%
    group_by(pid) %>%
    summarise(pred = unique(pred))
  
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

if (variable %in% c("propHG", "propcult", "proplab", "dietpropHG", "dietpropfarm", "dietpropmarket", "occ", "land", "livestock", "income")) {
  data_list$pred <- d_pid$pred
}

if (variable %in% c("urban", "edu")) {
  data_list$ord_pred = ifelse( is.na(d_pid$pred), -99, round(d_pid$pred) + 1) # +1 for monotonic effect calculation
  data_list$K = max(d_pid$pred, na.rm = T)
}

return(data_list)
}
