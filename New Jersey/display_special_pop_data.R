library(dplyr)
require(stringr)
require(tidyr)
require(readr)

state_loc <- "C:/Users/jillk/Dropbox/Dashboards/States/NJ/Data/Output/"

specialpop_school <- read_csv(paste0(state_loc, "enroll_school.csv"))
specialpop_district <- read_csv(paste0(state_loc, "enroll_district.csv"))
specialpop_county <- read_csv(paste0(state_loc, "enroll_county.csv"))
specialpop_state <- read_csv(paste0(state_loc, "enroll_state.csv"))

## Calculate percent ELL, FRL, Special Education for school, distrinct, county, and state by year

current_year <- 2017

special_pop_cols <- c("category", 
                       "state_name",
                       "county_name",
                       "district_name",
                       "school_name",
                       "year", "total", "frl", "ell", "spec_ed") 

calc_specialpop_percents <- function(dataset){
  dataset %>%
  filter(year == current_year) %>%
  select(one_of(special_pop_cols)) %>%
  gather(variable, value, -(category:total)) %>%
  mutate(perc = round(as.numeric(value) / as.numeric(total), 2))
}

per_special_pop_state <- calc_specialpop_percents(specialpop_state)
per_special_pop_county <- calc_specialpop_percents(specialpop_county)
per_special_pop_district <- calc_specialpop_percents(specialpop_district)
per_special_pop_school <- calc_specialpop_percents(specialpop_school)


## Prepare state, county, and district files

per_special_pop_state <- per_special_pop_school %>%
  select(state_name:year, variable) %>%
  right_join(per_special_pop_state)

per_special_pop_county <- per_special_pop_school %>%
  select(state_name:year, variable) %>%
  right_join(per_special_pop_county)

per_special_pop_district <- per_special_pop_school %>%
  select(state_name:year, variable) %>%
  right_join(per_special_pop_district)

per_special_pop_all <- bind_rows(per_special_pop_state,
                                 per_special_pop_county,
                                 per_special_pop_district,
                                 per_special_pop_school)

per_special_pop_all <- per_special_pop_all %>%
  mutate(name = ifelse(category == "state", state_name,
                ifelse(category == "county", county_name,
                ifelse(category == "district", district_name,
                ifelse(category == "school", school_name, NA)))))

write.csv(per_special_pop_all, paste0(state_loc, "specialpop_all.csv"), row.names = F, na = "")




