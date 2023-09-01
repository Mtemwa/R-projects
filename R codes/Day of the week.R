# Load the lubridate package
library(lubridate)
library(dplyr)
df <- Claims_2023

# Convert the ASSESSMENT DATE column to a date format
df$ASSESSEDDATE <- as.Date(df$ASSESSEDDATE)

# Extract the day of the week from the TREATMENT DATE column
day_names <- weekdays(Claims$TREATMENTDATE)
Claims$DAYOFWEEK <- weekdays(Claims$TREATMENTDATE)

# GROUP
Days <- Claims %>%
  group_by(DAYOFWEEK,BENEFITTYPE) %>%
  summarize(Claim_amount = sum(SETTLEDAMOUNT),
            visits = n_distinct(ASSESSMENTID))

write.xlsx(Days, file = "Days.xlsx")
## MONTHS
Claims$month <- months(Claims$TREATMENTDATE)

# Count the number of visits for each day of the week
library(dplyr)
Month <- Claims %>%
  group_by(month,BENEFITTYPE) %>%
  summarize(Claim_amount = sum(SETTLEDAMOUNT),
            visits = n_distinct(ASSESSMENTID))

write.xlsx(Month, file = "Month.xlsx")  
# define the new day names vector
new_day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# use match() function to map the old day names to new day names
day_names_mapped <- match(day_names, new_day_names)

# use the new day names vector to rename the days of the week
renamed_days <- new_day_names[day_names_mapped]

