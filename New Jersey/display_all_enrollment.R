library(dplyr)
require(stringr)
require(tidyr)
require(readr)

state_loc <- "C:/Users/jillk/Dropbox/Dashboards/States/NJ/Data/Output/"

# Load all enrollment data

enroll_school <- read_csv(paste0(state_loc, "enroll_school.csv"))

# Select columns specific to grade level enrollment and make the data long

enroll_long <- enroll_school %>%
  select(state_name:school_name,
         county_code:year,
         total,
         gr_pk:gr_ug) %>%
  gather(grade, students, -(state_name:year)) %>%
  mutate(students = round(students, 0)) %>%
  filter(!(students == 0 | is.na(students)))

write.csv(enroll_long, paste0(state_loc, "enroll_all.csv"), row.names = F, na = "")
