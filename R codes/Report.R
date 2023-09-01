library(dplyr)
library(openxlsx)
library(tidyr)

# ---------- CLAIMS ----------#
Claims <- Claims_2018_2023 %>% filter (PRODUCT %in% "Corporate")

# subset
Claim_subset <- Claims[, c("ASSESSMENTID","PAYEE", "INVOICEBENEFIT", "AFFECTEDSYSTEMDESC", "POLICYTYPE", "PARENTID","SETTLEDAMOUNT", "UWYEAR", "LOSSYEAR", "AGEBAND", "BENEFITTYPE", "MEDICALSERVICES"  )]
Claim_subset <- Claims[, c("ASSESSMENTID", "POLICYTYPE", "SETTLEDAMOUNT", "UWYEAR")]
# POLICYLR
PolicyLR <- Claims %>%
  group_by(POLICYTYPE, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(PolicyLR, file = "Policy.xlsx")
# POLICYLR_incurred
PolicyLR <- Claims %>%
  group_by(POLICYTYPE, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(PolicyLR, file = "Policy_Incurred.xlsx")
# BENEFITTYPELR
BenefittypeLR <- Claims %>%
  group_by(BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(BenefittypeLR, file = "BenefittypeClaims.xlsx")

# BENEFITLR
INSIDE_CLAIM_1 <- ClaimsOS %>%
  group_by(BENEFITTYPE, INVOICEBENEFIT,UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(INSIDE_CLAIM_1, file = "INSIDECLAIM_1.xlsx")

# AGEBAND
AGEBAND <- Claims %>%
  group_by(AGEBAND,BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(AGEBAND, file = "AGEBANDClaims.xlsx")

# MEDICAL SERVICES
MEDSERVICES <- Claims %>%
  group_by(MEDICALSERVICES,BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(MEDSERVICES, file = "MedservicesClaims.xlsx")

# LENGTH OF STAY
LENGTHSTAY <- Claims %>%
  group_by(BENEFITTYPE, band, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

LENGTHSTAY <- Claims %>%
  group_by(BENEFITTYPE,PAYEE) %>%
  summarize(Claim_amount = sum(COPAYAMOUNT),
            visits = n_distinct(ASSESSMENTID))

write.xlsx(MEDSERVICES, file = "MedservicesClaims.xlsx")

# DISEASES
DISEASES <- Claims_2018_2023 %>%
  group_by(INVOICELINESUBBENEFIT,BENEFITTYPE, LOSSYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = LOSSYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(DISEASES, file = "DiseasesClaims1.xlsx")

# PANEL
PANEL <- Claims %>%
  group_by(PAYEE,BENEFITTYPE, LOSSYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = LOSSYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

claims <- Claims %>% filter(LOSSYEAR %in% 2023)
PANEL <- claims %>%
  group_by(PAYEE,BENEFITTYPE) %>%
  summarize(Claim_amount = sum(SETTLEDAMOUNT),
            Visits = n_distinct(ASSESSMENTID))

write.xlsx(PANEL, file = "PANELClaims.xlsx")

# People
People <- Claims %>%
  group_by(PARENTID,) %>%
  summarize(Claim_amount = sum(SETTLEDAMOUNT),
            visits = n_distinct(ASSESSMENTID))

People <- Claims %>%
  group_by(PARENTID, BENEFITTYPE, LOSSYEAR) %>%
  summarize(distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = LOSSYEAR,
              values_from = (distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
# SCHEMES
library(dplyr)
library(tidyr)

# Calculate claims by POLICYHOLDERNAME, BENEFITTYPE, and UWYEAR
Claims_Sum <- Claims %>%
  group_by(POLICYHOLDERNAME, BENEFITTYPE, UWYEAR) %>%
  summarize(Claims_Sum = sum(SETTLEDAMOUNT, na.rm = TRUE),
            distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(Claims_Sum, distint_count)) %>%
  mutate_at(vars(-c(POLICYHOLDERNAME, BENEFITTYPE)), funs(gsub(",", "", .)))

# Calculate premiums by POLICYHOLDERNAME, BENEFITTYPE, and UWYEAR
Premiums_Sum <- premiums %>%
  group_by(POLICYHOLDERNAME, BENEFITTYPE, UWYEAR) %>%
  summarize(Premiums_Sum = sum(EARNEDPREM, na.rm = TRUE),
            distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(Premiums_Sum, distint_count)) %>%
  mutate_at(vars(-c(POLICYHOLDERNAME, BENEFITTYPE)), funs(gsub(",", "", .)))

# Join the two data frames by POLICYHOLDERNAME
SCHEME <- left_join(Claims_Sum, Premiums_Sum, by = c("POLICYHOLDERNAME"))

SCHEME <- SCHEME %>%
  mutate_at(vars(matches("_Sum|_count")), as.integer)


write.xlsx(SCHEME, file = "SchemeClaims1.xlsx")

SCHEME <- CLAIMS %>%
  group_by(POLICYHOLDERNAME, BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

common_schemes <- inner_join(SCHEME, SumPivot, by = c("POLICYHOLDERNAME"))

only_in_claims <- anti_join(SCHEME, SumPivot, by = c("POLICYHOLDERNAME"))
only_in_premiums <- anti_join(SumPivot, SCHEME, by = c("POLICYHOLDERNAME"))

write.xlsx(only_in_claims, file = "Scheme_Only in Claims.xlsx")
write.xlsx(only_in_premiums, file = "Scheme_Only in Premiums.xlsx")
# INSIDE THE BENEFITTYPES
INSIDE <- Claims %>%
  group_by(BENEFITTYPE,INVOICEBENEFIT, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))


write.xlsx(INSIDE, file = "INSIDEClaims.xlsx")

# ---------- PREMIUM ---------#
# Get sum pivot by schemes benefitprice
# POLICYTYPE
POLICY <- premiums %>%
  group_by(POLICYTYPE,UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(POLICY, file = "PolicyPrem.xlsx")

# BENEFITTYPE-GWP
BENEFITTYPE <- premiums %>%
  group_by(BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(BENEFITPRICE, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(BENEFITTYPE, file = "BenefittypeGWP.xlsx")

# BENEFITTYPE-EARNED
BENEFITTYPE <- premiums %>%
  group_by(BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(BENEFITTYPE, file = "BenefittypeEarnedPREM.xlsx")

# AGEBAND
AGEBAND_PREM <- premiums %>%
  group_by(AGEBAND, BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(AGEBAND_PREM, file = "AGEBANDPREM.xlsx")

# Get sum pivot by schemes companies
SumPivot <- PREMIUMS %>%
  group_by(POLICYHOLDERNAME, BENEFITTYPE, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))


# Save the data frame to an Excel file in .xlsx format
write.xlsx(SumPivot, file = "PremiumNew_2018_2022.xlsx")

# INSIDE
INSIDE_PREM <- premiums %>%
  group_by(BENEFITTYPE, BENEFITDESCRIPTION,UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))
write.xlsx(INSIDE_PREM, file = "INSIDEPREM.xlsx")

# BENEFITLIMIT
# Claims
OutpatientLimits_2 <- ClaimsOS %>%
  group_by(BENEFITTYPE, BENEFITLIMIT, UWYEAR) %>%
  summarize(sum = sum(SETTLEDAMOUNT, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits_2, file = "Limits_ClaimsOS.xlsx")

# Premiums
OutpatientLimits_1 <- premiums %>%
  group_by(BENEFITTYPE, BENEFITLIMIT, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(OutpatientLimits_1, file = "Limits_Prem.xlsx")
