library(openxlsx)
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)

# Load Data
# Load data per year .i.e. 2018,2019 etc

#Claims_2018 <- read_excel("2018.xlsx")
premiums <- read_excel(file.choose())
premiums <- X2022

final_result <- premiums %>%
  mutate(
    "POLICY_TYPE" = PROD_DESC,
    "GROUP_POLICY_NUMBER" = POL_POLICY_NO,
    "CVT_DESC" = CVT_DESC,
    "COVER_START_DATE" = ENDR_COVER_FROM_DATE,
    "COVER_END_DATE" = ENDR_COVER_TO_DATE,
    "GROUP POLICY STATUS AS AT 31 DECEMBER (ACTIVE)" = "Active",
    "PRINCIPAL_MEMBER_NUMBER" = MEM_NO,
    "LIFE NUMBER (WHERE COVERED LIFE IS A DEPENDANT)" = MEM_NO,
    "SEX" = MEM_SEX,
    "CRITICAL ILLNESS RIDER INDICATOR" = case_when(
      CVT_DESC == "Critical Illness" & PCM_PREMIUM >= 0 ~ 1,
      TRUE ~ 0
    ),
    "LAST EXPENSE/FUNERAL COVER RIDER INDICATOR" = case_when(
      CVT_DESC %in% c("Last Expense", "Group Last Expense") & PCM_PREMIUM >= 0 ~ 1,
      TRUE ~ 0
    ),
    "DISABILITY RIDER INDICATOR" = case_when(
      CVT_DESC %in% c("Permanent And Total Disability", "Temporary and Total Disability") & PCM_PREMIUM >= 0 ~ 1,
      TRUE ~ 0
    ),
    "INDIVIDUAL LIFE STATUS AS AT 31 DECEMBER " = "Active",
    "SUMS ASSURED-MAIN BENEFIT" = PCM_SA,
    "SUMS ASSURED-CRITICAL ILLNESS COVER RIDER BENEFIT" = case_when(
      CVT_DESC == "Critical Illness" ~ PCM_SA,
      TRUE ~ 0
    ),
    "SUMS ASSURED-LAST EXPENSE/FUNERAL COVER RIDER BENEFIT" = case_when(
      CVT_DESC %in% c("Last Expense", "Group Last Expense") ~ PCM_SA,
      TRUE ~ 0
    ),
    "SUM ASSURED DISABILITY RIDER" = case_when(
      CVT_DESC %in% c("Permanent And Total Disability", "Temporary and Total Disability") ~ PCM_SA,
      TRUE ~ 0
    ),
    "ANNUAL - MAIN BENEFIT PREMIUM" = PCM_PREMIUM,
    "EXTRA ANNUAL PREMIUM - MAIN BENEFIT (FOR IMPAIRED LIVES)" = 0,  
    "ANNUAL PREMIUM - CRITICAL ILLNESS COVER RIDER BENEFIT" = case_when(
      CVT_DESC == "Critical Illness" & PCM_PREMIUM >= 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    "ANNUAL PREMIUM - LAST EXPENSE/FUNERAL COVER RIDER" = case_when(
      CVT_DESC %in% c("Last Expense", "Group Last Expense") & PCM_PREMIUM >= 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    "ANNUAL PREMIUM - DISABILITY RIDER" = case_when(
      CVT_DESC %in% c("Permanent And Total Disability", "Temporary and Total Disability") & PCM_PREMIUM >= 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    "DATE_OF_BIRTH" = MEM_DOB,
    "LOAN START DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)" = case_when(
      CVT_DESC == "Credit Life" ~ PCM_LOAN_ISSUE_DATE,
      TRUE ~ "") ,# credit life
    "LOAN END DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)" = "",# credit life  
    "EXIT DATE OF THE LIFE ASSURED (DDMMYYYY, WHERE APPLICABLE)" = "", # credit life
    "REASON_FOR_EXIT" = ""
  ) %>%
  select(
    POLICY_TYPE,
    GROUP_POLICY_NUMBER,
    CVT_DESC,
    COVER_START_DATE,
    COVER_END_DATE,
    "GROUP POLICY STATUS AS AT 31 DECEMBER (ACTIVE)",
    PRINCIPAL_MEMBER_NUMBER,
    "LIFE NUMBER (WHERE COVERED LIFE IS A DEPENDANT)",
    SEX,
    "CRITICAL ILLNESS RIDER INDICATOR",
    "LAST EXPENSE/FUNERAL COVER RIDER INDICATOR",
    "DISABILITY RIDER INDICATOR",
    "INDIVIDUAL LIFE STATUS AS AT 31 DECEMBER ",
    "SUMS ASSURED-MAIN BENEFIT",
    "SUMS ASSURED-CRITICAL ILLNESS COVER RIDER BENEFIT",
    "SUMS ASSURED-LAST EXPENSE/FUNERAL COVER RIDER BENEFIT",
    "SUM ASSURED DISABILITY RIDER",
    "ANNUAL - MAIN BENEFIT PREMIUM",
    "EXTRA ANNUAL PREMIUM - MAIN BENEFIT (FOR IMPAIRED LIVES)",
    "ANNUAL PREMIUM - CRITICAL ILLNESS COVER RIDER BENEFIT",
    "ANNUAL PREMIUM - LAST EXPENSE/FUNERAL COVER RIDER",
    "ANNUAL PREMIUM - DISABILITY RIDER",
    DATE_OF_BIRTH,
    "LOAN START DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)",
    "LOAN END DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)",
    "EXIT DATE OF THE LIFE ASSURED (DDMMYYYY, WHERE APPLICABLE)",
    REASON_FOR_EXIT
  )


library(openxlsx)
# Save as an excel file
write.xlsx(final_result, file = "2022_Result.xlsx")
