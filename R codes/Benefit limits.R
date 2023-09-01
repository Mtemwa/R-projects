library(dplyr)
library(tidyr)

lookup <- function(datacols, lookupframe, colmatch, colsget, na_if_not_found = FALSE) {
  # check if columns to match exist in both dataframes
  datacols_match <- intersect(names(datacols), colmatch[[1]])
  lookupframe_match <- intersect(names(lookupframe), colmatch[[2]])
  if (length(datacols_match) == 0 | length(lookupframe_match) == 0) {
    print("No feasible matching lookup column references")
    return(datacols)
  }
  if (length(datacols_match) != length(lookupframe_match)) {
    print("Feasible fields to match are not of equal number")
    print("Fields used are as follows:")
    print("for datacols", datacols_match)
    print("for lookupframe", lookupframe_match)
  }
  # perform the lookup operation
  joined <- left_join(datacols, lookupframe, by = setNames(lookupframe_match, datacols_match))
  # group by the match columns and summarize the colsget values
  summarized <- joined %>%
    group_by(across(all_of(colmatch[[1]]))) %>%
    summarize_at(vars(colsget), ~ifelse(all(is.na(.)), NaN, max(., na.rm = TRUE))) %>%
    ungroup()
  # handle missing values
  if (na_if_not_found) {
    summarized[is.na(summarized)] <- NaN
  }
  # add the colsget column to the original dataframe
  datacols <- left_join(datacols, summarized, by = colmatch[[1]])
  datacols <- select(datacols, -one_of(colmatch[[2]]))
  return(datacols)
}


# Combine the parentid with benefitdescription
df_combined_new <- premiums %>%
  unite(col = "Member_Benefit_Description", c("PARENTID", "BENEFITTYPE"), sep = "-")

df_claims_new <- Claims %>%
  unite(col = "Claim_Benefit_Description", c("PARENTID", "BENEFITTYPE"), sep = "-")


# Perform the VLOOKUP:
# CLAIMS

df_claims_new$BENEFITTYPE <- Claims$BENEFITTYPE
df_claims_selected <- select(df_claims_new, Claim_Benefit_Description, UWYEAR, DOB,SETTLEDAMOUNT, ASSESSMENTID, Claims_Incurred, BENEFITTYPE)
df_benefit_selected <- select(df_combined_new, Member_Benefit_Description, BENEFITLIMIT)
remove(df_claims_new)
remove(df_combined_new)

result <- lookup(datacols = df_claims_selected,
                      lookupframe = df_benefit_selected,
                      colmatch = list("Claim_Benefit_Description", "Member_Benefit_Description"),
                      colsget = "BENEFITLIMIT",
                      na_if_not_found = TRUE)

df_filtered <- result %>% filter(BENEFITLIMIT %in% "NaN")

# We work on the filtered data
# we will load  2016 and 2017 premiums
premiums_2017 <- read_excel("Premiums/2017.xlsx")
premiums_2016 <- read_excel("Premiums/2016.xlsx")
premiums_2016_2017 <- rbind(premiums_2016,premiums_2017)
df_filtered[,8]<- NULL

# peform vlookup and have the benefit type column

# Combine the parentid with benefitdescription
df_combined_new <- premiums_2016_2017 %>%
  unite(col = "Member_Benefit_Description", c("PARENTID", "BENEFITTYPE"), sep = "-")

# Then final part
df_filtered <- lookup(datacols = df_filtered,
                 lookupframe = df_combined_new,
                 colmatch = list("Claim_Benefit_Description", "Member_Benefit_Description"),
                 colsget = "BENEFITLIMIT",
                 na_if_not_found = TRUE)

# Subset with NaN
result <- result[result$BENEFITLIMIT != "NaN", ]
result <- rbind(result,df_filtered)

# Outpatient
result <- result %>% filter(UWYEAR %in% 2018:2023)

Outpatient <- result %>%
  filter(BENEFITTYPE == "Outpatient")
max(outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "100K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>500000  ~ "500K+",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Outpatient_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Outpatient")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "100K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>500000  ~ "500K+",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Outpatient_Limits_Prem.xlsx")

# INPATIENT
# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Inpatient")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "<300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>500000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=1000000 ~ "500K-1M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>1000000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=5000000 ~ "1M-5M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>5000000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=10000000 ~ "5M-10M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>10000000  ~ "10M+",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Inpatient_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Inpatient")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "<300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>500000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=1000000 ~ "500K-1M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>1000000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=5000000 ~ "1M-5M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>5000000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=10000000 ~ "5M-10M",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>10000000  ~ "10M+",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Inpatient_Limits_Prem.xlsx")

# DENTAL
# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Dental")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=150000 ~ "100K-150K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>150000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "150K-200K",
  
  TRUE~"NaN"
))



sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Dental_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Dental")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=150000 ~ "100K-150K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>150000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "150K-200K",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Dental_Limits_Prem.xlsx")

# Optical
# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Optical")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  
  TRUE~"NaN"
))



sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Optical_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Optical")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  
  TRUE~"NaN"
))

sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Optical_Limits_Prem.xlsx")


# Dental & Optical

# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Dental & Optical")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  
  TRUE~"NaN"
))



sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Dental&Optical_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Dental & Optical")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Dental&Optical_Limits_Prem.xlsx")

# Maternity(Standalone)

# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Maternity(Standalone)")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>200000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "200K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  
  TRUE~"NaN"
))




sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Maternity(Standalone)_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Maternity(Standalone)")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>200000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "200K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Maternity(Standalone)_Limits_Prem.xlsx")

# Maternity

# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Maternity")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>200000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "200K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  
  TRUE~"NaN"
))




sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Maternity_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Maternity")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "100K-200K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>200000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=300000 ~ "200K-300K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>300000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=500000 ~ "300K-500K",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Maternity_Limits_Prem.xlsx")

# Last Expense

# CLAIMS
Outpatient <- result %>%
  filter(BENEFITTYPE == "Last Expense")
max(Outpatient$BENEFITLIMIT)
# BANDS
# Claims
library(tidyverse)
df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=150000 ~ "100K-150K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>150000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "150K-200K",
  
  TRUE~"NaN"
))




sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Last Expense_Limits_Claims.xlsx")

# Premiums
Outpatient <- premiums %>%
  filter(BENEFITTYPE == "Last Expense")

df_result<-Outpatient%>%add_column(BENEFITLIMIT_BANDS=case_when(
  
  abs(as.numeric(Outpatient$BENEFITLIMIT))>=0 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=50000 ~ "0-50K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>50000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=100000 ~ "50K-100K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>100000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=150000 ~ "100K-150K",
  abs(as.numeric(Outpatient$BENEFITLIMIT))>150000 & abs(as.numeric(Outpatient$BENEFITLIMIT))<=200000 ~ "150K-200K",
  
  TRUE~"NaN"
))


sum(df_result$BENEFITLIMIT == "NaN")

OutpatientLimits <- df_result %>%
  group_by(BENEFITLIMIT_BANDS, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits, file = "Last Expense_Limits_Prem.xlsx")



