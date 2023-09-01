# Load the two datasets into data frames
premium_2018 <- premiums %>% filter(UWYEAR %in% 2020)
premium_2019 <- premiums %>% filter(UWYEAR %in% 2021)
df_2021 <- premium_2018
df_2022 <- premium_2019
remove(premium_2018)
remove(premium_2019)

# Select the columns to keep
df_2022 <- df_2022 %>%
  select(POLICYNUMBER)
df_2021 <- df_2021 %>%
  select(POLICYNUMBER)


# Find the unique combinations of client name and policy number in both datasets
unique_clients_2022 <- unique(df_2022[, c("POLICYNUMBER")])
unique_clients_2021 <- unique(df_2021[, c("POLICYNUMBER")])

# Find the clients that renewed their policy
renewed_clients <- intersect(unique_clients_2021, unique_clients_2022)

# Fill the unique_clients_2021 with either Renewed or Not Renewed
unique_clients1_2021 <- unique_clients_2021 %>%
  mutate(Renewed = ifelse((POLICYNUMBER %in% renewed_clients$POLICYNUMBER) & (POLICYNUMBER %in% renewed_clients$POLICYNUMBER), "Renewed", "Not Renewed"))

# Find the unique total number of clients
total_clients1 <- unique(rbind(unique_clients_2021, unique_clients_2022))

total_clients1 <- total_clients1 %>%
  mutate(Renewed = ifelse((POLICYNUMBER %in% renewed_clients$POLICYNUMBER) & (POLICYNUMBER %in% renewed_clients$POLICYNUMBER), "Renewed", "Not Renewed"))


# Combine the unique policy numbers with their original SI_BAND values
unique_policy_numbers <- unique(rbind(unique_clients_2021, unique_clients_2022))

# Subset the rows in df_2021 and df_2022 based on the unique policy numbers
df_2021_matched <- df_2021[df_2021$POLICYNUMBER %in% unique_policy_numbers$POLICYNUMBER, ]
df_2022_matched <- df_2022[df_2022$POLICYNUMBER %in% unique_policy_numbers$POLICYNUMBER, ]


# Merge the two dataframes for 2022 based on POLICY_NO
df_merged <- left_join(unique_clients_2022, df_2022_matched[, c("POLICYNUMBER")], by = "POLICYNUMBER") %>%
  distinct(POLICYNUMBER, .keep_all = TRUE)

total_clients_2022 <- df_merged %>%
  mutate(Renewed = ifelse((POLICYNUMBER %in% renewed_clients$POLICYNUMBER), "Renewed", "New Business"))

# Merge the two dataframes for 2022 based on POLICY_NO
df_merged <- left_join(unique_clients_2021, df_2021_matched[, c("POLICYNUMBER")], by = "POLICYNUMBER") %>%
  distinct(POLICYNUMBER, .keep_all = TRUE)

total_clients_2021 <- df_merged %>%
  mutate(Renewed = ifelse((POLICYNUMBER %in% renewed_clients$POLICYNUMBER), "Renewed", "Not Renewed"))
 

# NEW BUSINESS and RENEWED

# VLOOKUP ONTO PREMIUM DATA
premiums_2018 <- premiums %>% filter(UWYEAR %in% 2021)
premiums_2018 <- lookup(datacols = premiums_2018,
                        lookupframe = total_clients_2022,
                        colmatch = list("POLICYNUMBER", "POLICYNUMBER"),
                        colsget = "Renewed",
                        replace = TRUE,
                        na_if_not_found = TRUE)

# PREMIUMS_2018
PREM_2018 <- premiums_2018 %>%
  group_by(Renewed,BENEFITTYPE,POLICYHOLDERNAME,UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(POLICYNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(PREM_2018, file = "PREM_RENEWED1_2019.xlsx")

remove(premiums_2018)

# CLAIMS
claims_2018 <- Claims %>% filter(UWYEAR %in% 2021)
claims_2018 <- lookup(datacols = claims_2018,
                      lookupframe = total_clients_2022,
                      colmatch = list("POLICYNUMBER", "POLICYNUMBER"),
                      colsget = "Renewed",
                      replace = TRUE)

CLAIMS_REN <- claims_2018 %>%
  group_by(Renewed, BENEFITTYPE, POLICYHOLDERNAME,UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(POLICYNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(CLAIMS_REN, file = "CLAIMS_RENEWED_2023.xlsx")

remove(claims_2018)
