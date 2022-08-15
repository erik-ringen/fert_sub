require(tidyverse)

clean_data <- function(file_data){

d_raw <- read_csv(file_data)

vars_keep <- c(
    "population",
     "ID",
     "livebirths",
     "AFB",
     "ALB",
     "age",
     "subsist",
     "year",
     "market",
     "wealth",
     "income",
     "occ1",
     "dietpropmarket",
     "dietpropfarm",
     "edu",
     "contra",
     "proplab",
     "propHG",
     "propag",
     "propcult",
     "residencesub",
     "livestock",
     "land",
     "urban")

d_raw$subsist <- ifelse(d_raw$subsist == "agropastoral", "past", d_raw$subsist)

d_raw <- d_raw[, vars_keep]

# Create stan-friendly indices
d_raw <- d_raw %>%
  filter(!(is.na(livebirths))) %>%
  mutate(
    pop_id = match(population, unique(population)),
    pid = match(paste(population, ID), unique(paste(population, ID))),
    sub_id = match(subsist, unique(subsist))
  )

# Clean up age variables
for (i in 1:nrow(d_raw)) {
  if (d_raw$livebirths[i] == 0) {
    d_raw$AFB[i] <- NA
    d_raw$ALB[i] <- NA
  }
  if (d_raw$livebirths[i] == 1) {
    d_raw$ALB[i] <- NA
  }

  if (!(is.na(d_raw$ALB[i]))) {
  if (d_raw$age[i] <= d_raw$ALB[i]) {
    d_raw$ALB[i] <- NA
  }
  }
}

d_raw$birth_year <- d_raw$year - d_raw$age

d_raw <- d_raw %>%
  group_by(population) %>%
  mutate(
    # standardize to 20 year-units
    birthyear_s = (birth_year - median(birth_year)) / 20)

# Pivot long with respect to births...
# we want a row for first birth and number of current births
d_long <- d_raw %>%
  pivot_longer(
    cols = c("AFB", "ALB", "age"),
    values_to = "age"
  ) %>%
  filter(!(is.na(age))) %>%
  mutate(live_births = ifelse(name == "AFB", 1, livebirths))

# Order market levels
d_long$market <- ifelse(d_long$market == "med", "medium", d_long$market)
d_long$market <- factor(d_long$market,
 levels = c("none", "low", "medium", "high"))

d <- dplyr::select(d_long, -c(ID, live_births, name))
rm(d_raw, d_long, vars_keep)
return(d)
}
