library(dplyr)
library(tidyr)
library(stringr)

# Group the data by PARENTID
# FAMILY(MAIN,SPOUSE,CHILD)
# Group by PARENTID and merge unmarried children
df_grouped <- premiums %>%
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

unique_benefit_family <- data.frame(unique(df_grouped$COMPANY))
unique_benefit_family <- unique_benefit_family %>%
  rename(COMPANY = unique.df_grouped.COMPANY.)
unique(unique_benefit_family$COMPANY)

# Replace each value in INVOICEBENFIT with a BENEFITTYE before Vlookup
unique_benefit_family$M <- NA
for (benefit in unique_benefit_family$COMPANY) {
  new_benefit_type <- readline(prompt = paste("Replace", benefit, "with: "))
  unique_benefit_family$M[unique_benefit_family$COMPANY == benefit] <- new_benefit_type
}

# Merge M column from unique_benefit_family into df_grouped
df <- lookup(datacols = df_grouped,
                   lookupframe = unique_benefit_family,
                   colmatch = list("COMPANY", "COMPANY"),
                   colsget = "M",
                   replace = TRUE,
                   na_if_not_found = TRUE)


# Removing columns so that I remain with two
df[,2:3]<- NULL

# DO A VLOOKUP INTO THE PREMIUM DATASET
premiums <- lookup(datacols = premiums,
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

df_filtered <- df_grouped %>% filter(!(COMPANY %in% c("M+CNA", "M+S+CNA", "")))
CLAIM <- mydata_filtered %>% filter(M %in% "Unknown")