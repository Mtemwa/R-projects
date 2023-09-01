premiums_2016 <- read_excel("Premiums/2016.xlsx")
premiums_2017 <- read_excel("Premiums/2017.xlsx")

premiums_2016_2017 <- rbind(premiums_2016,premiums_2017)

remove(premiums_2016)
remove(premiums_2017)

CLAIMS <- Claims %>% filter(M =="Unknown")

premiums_2016_2017$VALUATIONDATE <- as.Date("2023-03-31")
premiums_2016_2017$AGE <- as.numeric(difftime(as.Date(premiums_2016_2017$VALUATIONDATE, format="%Y/%m/%d"), as.Date(premiums_2016_2017$DOB, format="%Y/%m/%d"), units = "days"))/365.25

df_grouped_2016_2017 <- premiums_2016_2017 %>%
  group_by(PARENTID) %>%
  mutate(
    NEWRELATIONSHIP = paste(RELATIONSHIP, AGE, sep = "-"),
    COMPANY = case_when(
      RELATIONSHIP == "Company Employee" ~ "M",
      RELATIONSHIP == "Spouse" ~ "S",
      grepl("Unmarried Child", RELATIONSHIP) ~ "C"
    )
  ) %>%
  distinct(PARENTID, NEWRELATIONSHIP, COMPANY) %>%
  group_by(PARENTID) %>%
  summarise(
    NEWRELATIONSHIP = toString(sort(unique(NEWRELATIONSHIP))),
    COMPANY = paste0(
      ifelse("M" %in% COMPANY, "M", ""),
      ifelse("M" %in% COMPANY & "S" %in% COMPANY, "+S", ifelse("S" %in% COMPANY, "S", "")),
      #ifelse("S" %in% COMPANY, "+S", ""),
      ifelse("C" %in% COMPANY, paste0("+C", sum(COMPANY == "C")), "")
    )
  )

unique_benefit_family_2016 <- data.frame(unique(df_grouped_2016_2017$COMPANY))
unique_benefit_family_2016 <- unique_benefit_family_2016 %>%
  rename(COMPANY = unique.df_grouped_2016_2017.COMPANY.)
unique(unique_benefit_family_2016$COMPANY)

df_filtered_2016 <- df_grouped_2016_2017 %>% filter(!(COMPANY %in% c("M+CNA", "M+S+CNA", "", "S+CNA","+CNA")))
df_filtered_2016 <- df_grouped_2016_2017 %>% filter(COMPANY %in% c("M+CNA", "M+S+CNA", "", "S+CNA","+CNA"))
write.xlsx(df_filtered_2016, file = "Lookup_2016_2017.xlsx")
# Replace each value in INVOICEBENFIT with a BENEFITTYE before Vlookup
unique_benefit_family_2016$M <- NA
for (benefit in unique_benefit_family_2016$COMPANY) {
  new_benefit_type <- readline(prompt = paste("Replace", benefit, "with: "))
  unique_benefit_family_2016$M[unique_benefit_family_2016$COMPANY == benefit] <- new_benefit_type
}

# Merge M column from unique_benefit_family into df_grouped
df <- lookup(datacols = df_grouped_2016_2017,
             lookupframe = unique_benefit_family_2016,
             colmatch = list("COMPANY", "COMPANY"),
             colsget = "M",
             replace = TRUE,
             na_if_not_found = TRUE)
df<- df_grouped_2016_2017
df[,2] <- NULL
# Removing columns so that I remain with two
df[,2:3]<- NULL

# DO A VLOOKUP INTO THE PREMIUM DATASET
claims_filtered <- lookup(datacols = CLAIMS,
                   lookupframe = df,
                   colmatch = list("PARENTID", "PARENTID"),
                   colsget = "M",
                   replace = TRUE,
                   na_if_not_found = TRUE)

# Match parent IDs and fill in COMPANY column in claims dataset
Claims <- Claims %>%
  left_join(df %>% select(PARENTID, M), by = "PARENTID") %>%
  mutate(M = ifelse(is.na(M), "Unknown", M))

# PREMIUM PER PARENT ID
premiums_sum <- premiums %>%
  group_by(PARENTID) %>%
  summarize(total_premium = sum(BENEFITPRICE))

# VLOOKUP INTO DF_GROUPED
df_grouped <- lookup(datacols = df_grouped,
                     lookupframe = premiums_sum,
                     colmatch = list("PARENTID", "PARENTID"),
                     colsget = "total_premium",
                     replace = TRUE,
                     na_if_not_found = TRUE)
premiums_2017$UWYEAR <- year(premiums_2017$EFFECTIVEDATE)
premiums_2017$AGE <- as.numeric(difftime(as.Date(premiums_2017$VALUATIONDATE, format="%Y/%m/%d"), as.Date(premiums_2017$DOB, format="%Y/%m/%d"), units = "days"))/365.25
premiums_2017$AGENEW <- floor(premiums_2017$AGE)
# UPR
premiums_2017$VALUATIONDATE <- as.Date("2023-03-31")
premiums_2017$REM_DAYS <- ifelse(premiums_2017$ENDDATE <= premiums_2017$VALUATIONDATE, 0, 
                                      ifelse(premiums_2017$EFFECTIVEDATE > premiums_2017$VALUATIONDATE, 0, 
                                             as.numeric(premiums_2017$ENDDATE - premiums_2017$VALUATIONDATE) / 
                                               as.numeric(premiums_2017$ENDDATE - premiums_2017$EFFECTIVEDATE + 1)))
premiums_2017$UPR <- ifelse(premiums_2017$ENDDATE <= premiums_2017$VALUATIONDATE, 0, 
                                 ifelse(premiums_2017$EFFECTIVEDATE > premiums_2017$VALUATIONDATE, 0, 
                                        as.numeric(premiums_2017$ENDDATE - premiums_2017$VALUATIONDATE) / 
                                          as.numeric(premiums_2017$ENDDATE - premiums_2017$EFFECTIVEDATE + 1))) * premiums_2017$BENEFITPRICE

premiums_2017$EARNEDPREM <- premiums_2017$BENEFITPRICE - premiums_2017$UPR

premiums_2017 <- premiums_2017[, c(1:25,27:32,26)]# move column 26 to the last

premiums<- rbind(premiums,premiums_2017)
