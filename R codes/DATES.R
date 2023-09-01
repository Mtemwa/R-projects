library(readxl)
library(lubridate)
library(dplyr)

# CLAIMS
Claims_2018 <- read_excel("Claims/Claims Paid 2018.xlsx")
Claims_2019 <- read_excel(file.choose())
Claims_2020 <- read_excel(file.choose())
Claims_2021 <- read_excel(file.choose())
Claims_2022 <- read_excel(file.choose())
Claims_2023 <- read_excel(file.choose())

# SKIP THIS NEXT CODE
# Define a function to convert date columns to Date class
convert_to_dates <- function(df) {
  # select all date columns
  date_cols <- c("EFFECTIVEDATE", "TREATMENTDATE", "POLD_ORIG_JOIN_DATE", "ENDDATE", "DOB", "PAYMENTDATE")
  
  # loop through all date columns and convert to date format
  for (col in date_cols) {
    df[[col]] <- as.Date(df[[col]])
  }
  
  # convert POSIXct columns to Date class
  df$EFFECTIVEDATE <- as.Date(df$EFFECTIVEDATE)
  df$TREATMENTDATE <- as.Date(df$TREATMENTDATE)
  df$POLD_ORIG_JOIN_DATE <- as.Date(df$POLD_ORIG_JOIN_DATE)
  df$ENDDATE <- as.Date(df$ENDDATE)
  df$DOB <- as.Date(df$DOB)
  df$PAYMENTDATE <- as.Date(df$PAYMENTDATE)
  
  return(df)
}
# END OF SKIPPED CODE

# Removing the followind DATES column because they are not uniform and making the rbind not work
Claims_2018 <-  subset(Claims_2018, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))
Claims_2019 <- subset(Claims_2019, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))
Claims_2020 <- subset(Claims_2020, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))
Claims_2021 <- subset(Claims_2021, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))
Claims_2022 <- subset(Claims_2022, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))
Claims_2023 <- subset(Claims_2023, select = -c(CAPTUREDATE, ASSESSEDDATE, RECEIVEDDATE))

# DONT RUN THIS CODE
# read in the Excel files and convert date columns to Date class
Claims_2019 <- convert_to_dates(Claims_2019)
Claims_2020 <- convert_to_dates(Claims_2020)
Claims_2018 <- convert_to_dates(Claims_2018)
Claims_2021 <- convert_to_dates(Claims_2021)
Claims_2022 <- convert_to_dates(Claims_2022)
Claims_2023 <- convert_to_dates(Claims_2023)
# END OF THIS WARNING

Claims_2018_2023 <- rbind(Claims_2018,Claims_2019,Claims_2020,Claims_2021,Claims_2022,Claims_2023)
remove(Claims_2018,Claims_2019,Claims_2020,Claims_2021,Claims_2022,Claims_2023)
View(Claims_2018_2023)

# PREMIUMS
premiums_2018 <- read_excel("Premiums/2018.xlsx")
premiums_2019  <- read_excel("Premiums/2019.xlsx")
premiums_2020  <- read_excel("Premiums/2020.xlsx")
premiums_2021  <- read_excel("Premiums/2021.xlsx")
premiums_2022 <- read_excel("Premiums/2022.xlsx")
premiums_2023  <- read_excel("Premiums/2023.xlsx")

premiums_2018_2023 <- rbind(premiums_2018,premiums_2019,premiums_2020,premiums_2021,premiums_2022,premiums_2023)
remove(premiums_2018,premiums_2019,premiums_2020,premiums_2021,premiums_2022,premiums_2023)
View(premiums_2018_2023)

# ------ CLAIMS DATASET --------#

# Convert the date into date format
Claims_2018_2023$EFFECTIVEDATE <- as.Date(Claims_2018_2023$EFFECTIVEDATE)
Claims_2018_2023$TREATMENTDATE <- as.Date(Claims_2018_2023$TREATMENTDATE)
Claims_2018_2023$DOB <- as.Date(Claims_2018_2023$DOB)
Claims_2018_2023$ENDDATE <- as.Date(Claims_2018_2023$ENDDATE)
Claims_2018_2023$PAYMENTDATE <- as.Date(Claims_2018_2023$PAYMENTDATE)
Claims_2018_2023$POLD_ORIG_JOIN_DATE <- as.Date(Claims_2018_2023$POLD_ORIG_JOIN_DATE)

Claims$LENGTHSTAY <- as.numeric(difftime(Claims$DISCHARGEDATE, Claims$ADMISSIONDATE, units = "days"))

# Creating columns in the dataset
Claims_2018_2023$VALUATIONDATE <- as.Date("2023-03-31")
Claims_2018_2023$UWYEAR <- year(Claims_2018_2023$EFFECTIVEDATE)
Claims_2018_2023$LOSSYEAR <- year(Claims_2018_2023$TREATMENTDATE)
Claims_2018_2023$AGE <- floor(as.numeric(difftime(as.Date(Claims_2018_2023$VALUATIONDATE, format="%Y/%m/%d"), as.Date(Claims_2018_2023$DOB, format="%Y/%m/%d"), units = "days"))/365.23)
Claims_2018_2023$AGE <- floor(difftime(Sys.Date(), ymd(Claims_2018_2023$DOB))/365.25)
# Remove the "years" text from the Age column
Claims_2018_2023$AGE <- paste0(Claims_2018_2023$AGE)
Claims_2018_2023$AGE <- as.numeric(Claims_2018_2023$AGE)
# AGEBANDS
Claims_2018_2023 <- Claims_2018_2023 %>% mutate(AGEBAND = cut(AGE, breaks = c(-1, 20, 40, 54, 65, 70, 75, 80, Inf),
                                                                  labels = c("0-20", "21-40", "41-54", "55-65", "66-70", "71-75", "76-80", "80+")))


# ------- PREMIUM DATASET --------- #

# Convert the date into date formart
premiums_2018_2023$EFFECTIVEDATE <- as.Date(premiums_2018_2023$EFFECTIVEDATE)
premiums_2018_2023$DOB <- as.Date(premiums_2018_2023$DOB)
premiums_2018_2023$ENDDATE <- as.Date(premiums_2018_2023$ENDDATE)
premiums_2018_2023$JOINDATE <- as.Date(premiums_2018_2023$JOINDATE)
premiums_2018_2023$UWYEAR <- year(premiums_2018_2023$EFFECTIVEDATE)
premiums_2018_2023$AGE <- as.numeric(difftime(as.Date(premiums_2018_2023$VALUATIONDATE, format="%Y/%m/%d"), as.Date(premiums_2018_2023$DOB, format="%Y/%m/%d"), units = "days"))/365.25
premiums_2018_2023$AGENEW <- floor(premiums_2018_2023$AGE)
# AGEBANDS
premiums_2018_2023 <- premiums_2018_2023 %>% mutate(AGEBAND = cut(AGENEW, breaks = c(-1, 20, 40, 54, 65, 70, 75, 80, Inf),
                                                                    labels = c("0-20", "21-40", "41-54", "55-65", "66-70", "71-75", "76-80", "80+")))
# UPR
premiums_2018_2023$VALUATIONDATE <- as.Date("2023-03-31")
premiums_2018_2023$REM_DAYS <- ifelse(premiums_2018_2023$ENDDATE <= premiums_2018_2023$VALUATIONDATE, 0, 
                                 ifelse(premiums_2018_2023$EFFECTIVEDATE > premiums_2018_2023$VALUATIONDATE, 0, 
                                        as.numeric(premiums_2018_2023$ENDDATE - premiums_2018_2023$VALUATIONDATE) / 
                                          as.numeric(premiums_2018_2023$ENDDATE - premiums_2018_2023$EFFECTIVEDATE + 1)))
premiums_2018_2023$UPR <- ifelse(premiums_2018_2023$ENDDATE <= premiums_2018_2023$VALUATIONDATE, 0, 
                            ifelse(premiums_2018_2023$EFFECTIVEDATE > premiums_2018_2023$VALUATIONDATE, 0, 
                              as.numeric(premiums_2018_2023$ENDDATE - premiums_2018_2023$VALUATIONDATE) / 
                                as.numeric(premiums_2018_2023$ENDDATE - premiums_2018_2023$EFFECTIVEDATE + 1))) * premiums_2018_2023$BENEFITPRICE

premiums_2018_2023$EARNEDPREM <- premiums_2018_2023$BENEFITPRICE - premiums_2018_2023$UPR

# GET THE WAITING PERIOD Claims amount and numbers
library(dplyr)
ClaimsOS <- ClaimsOS %>% filter(UWYEAR %in% 2018:2023)

Claims <- CLAIMS
Claims$LENGTHOFSTAY <- as.numeric(Claims$LENGTHOFSTAY)
df <- CLAIMS %>%
  mutate(band = cut(LENGTHOFSTAY, breaks = seq(-1, 421, by = 30), right = FALSE, include.lowest = TRUE, labels = c("0-30", "31-60", "61-90", "91-120", "121-150", "151-180", "181-210", "211-240", "241-270", "271-300", "301-330", "331-360", "361-390", "391+"))) %>%#,"391-420", "421-450", "451-480", "481-510", "511-540", "541-570", "571-600", "601-630", "631-660", "661-690", "691-720", "721-750", "751-780", "781-810", "811-840", "841-870", "871-900", "901-930", "931-960", "961-990", "991+"))) %>%
  group_by(band, BENEFITTYPE,UWYEAR) %>% 
  summarise(amount_claims = sum(SETTLEDAMOUNT), num_claims = n_distinct(ASSESSMENTID))


Claims <- Claims %>% ifelse(LENGTHOFSTAY > 30,
                            
  mutate(band = cut(LENGTHOFSTAY, breaks = seq(10, 31, by = 5), right = FALSE, include.lowest = TRUE, 
                    labels = c("0-30", "31-60", "61-90", "91-120", "121-150", "151-180", "181-210", 
                               "211-240", "241-270", "271-300", "301-330", "331-360", "361-390", "391+")))
