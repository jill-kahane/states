library(dplyr)
require(stringr)
require(tidyr)
require(readr)

raw_loc <- "C:/Users/jillk/Dropbox/Dashboards/States/NJ/Data/Output/"

# Load all race and ethnicity data

race_school <- read_csv(paste0(raw_loc, "raceeth_school.csv"))
race_district <- read_csv(paste0(raw_loc, "raceeth_district.csv"))
race_county <- read_csv(paste0(raw_loc, "raceeth_county.csv"))
race_state <- read_csv(paste0(raw_loc, "raceeth_state.csv"))

# Use function to make each dataset long

long_race_eth <- function(dataset, x) {
  dataset %>%
    select(state_name:other) %>%
    gather(variable, value, -(state_name:school_name)) %>%
    mutate(value = as.numeric(value),
           level = x)
}

race_state_long <- long_race_eth(race_state, "State")
race_county_long <- long_race_eth(race_county, "County")
race_district_long <- long_race_eth(race_district, "District") 
race_school_long <- long_race_eth(race_school, "School") 

# Bind data

race_eth <- bind_rows(race_state_long,
                      race_county_long,
                      race_district_long,
                      race_school_long)

# Create name variable that will display proper label, based on the level field

race_eth <- race_eth %>%
  mutate(name = ifelse(level == "State", state_name,
                ifelse(level == "County", county_name,
                ifelse(level == "District", district_name,
                ifelse(level == "School", school_name, NA)))))

write.csv(race_eth, paste0(raw_loc, "raceeth_all.csv"), row.names = F, na = "")




  


