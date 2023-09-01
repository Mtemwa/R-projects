library(dplyr)
library(tidyr)
library(stringr)

premiums <- X2018
# Step 1: Data Transformation
df_filtered <- premiums %>% 
  #filter(format(PCM_LOAN_ISSUE_DATE, "%Y") %in% c("2018", "2019", "2020", "2021", "2022") &
           #CVT_DESC %in% c("Group Life", "Last Expense")) %>%#
  group_by(MEM_NO) %>%
  summarise(
    POLICY_TYPE = unique(PROD_DESC),
    COVER_START_DATE = min(format(ENDR_COVER_FROM_DATE, "%d%m%Y")),
    COVER_END_DATE = max(format(ENDR_COVER_TO_DATE, "%d%m%Y")),
    PRINCIPAL_MEMBER_NUMBER = unique(POL_POLICY_NO),
    SEX = unique(MEM_SEX),
    SUMS_ASSURED = sum(as.numeric(gsub(",", "", PCM_SA))),
    ANNUAL_PREMIUM = sum(as.numeric(gsub(",", "", PCM_PREMIUM))),
    DATE_OF_BIRTH = format(unique(MEM_DOB), "%d%m%Y")
  )

df_filtered <- premiums %>%
  group_by(MEM_NO) %>%
  summarise(
    POLICY_TYPE = PROD_DESC,
    COVER_START_DATE = as.Date(ENDR_COVER_FROM_DATE),
    COVER_END_DATE = as.Date(ENDR_COVER_TO_DATE),
    PRINCIPAL_MEMBER_NUMBER = MEM_NO,
    SEX = MEM_SEX,
    SUMS_ASSURED = sum(as.numeric(gsub(",", "", PCM_SA))),
    ANNUAL_PREMIUM = sum(as.numeric(gsub(",", "", PCM_PREMIUM))),
    DATE_OF_BIRTH = format(first(MEM_DOB), "%d%m%Y")
  )

# Step 2: Format Transformation
df_transformed <- df_filtered %>%
  mutate(
    "GROUP POLICY STATUS AS AT 31 DECEMBER 2022" = "Inforce",  # Example, populate as needed
    "CRITICAL ILLNESS RIDER INDICATOR" = 0,  # Example, populate as needed
    "LAST EXPENSE/FUNERAL COVER RIDER INDICATOR" = 0,  # Example, populate as needed
    "DISABILITY RIDER INDICATOR" = 0,  # Example, populate as needed
    "INDIVIDUAL LIFE STATUS AS AT 31 DECEMBER 2022" = "Active",  # Example, populate as needed
    "ANNUAL - MAIN BENEFIT PREMIUM" = ANNUAL_PREMIUM,  # Example, populate as needed
    # ...
    "LOAN START DATE" = "",  # Example, populate as needed
    "LOAN END DATE" = "",  # Example, populate as needed
    "EXIT DATE OF THE LIFE ASSURED" = "",  # Example, populate as needed
    REASON_FOR_EXIT = ""  # Example, populate as needed
  )

# Step 3: Result Formatting
final_result <- df_transformed %>%
  select(
    INDUSTRY_SECTOR,
    POLICY_TYPE,
    PRINCIPAL_MEMBER_NUMBER,
    COVER_START_DATE,
    COVER_END_DATE,
    "GROUP POLICY STATUS AS AT 31 DECEMBER 2022",
    MEM_NO,
    SEX,
    "CRITICAL ILLNESS RIDER INDICATOR",
    "LAST EXPENSE/FUNERAL COVER RIDER INDICATOR",
    "DISABILITY RIDER INDICATOR",
    "INDIVIDUAL LIFE STATUS AS AT 31 DECEMBER 2022",
    SUMS_ASSURED,
    "SUMS ASSURED-CRITICAL ILLNESS COVER RIDER BENEFIT" = 0,  # Example, populate as needed
    "SUMS ASSURED-LAST EXPENSE/FUNERAL COVER RIDER BENEFIT" = 0,  # Example, populate as needed
    "SUM ASSURED DISABILITY RIDER" = 0,  # Example, populate as needed
    "ANNUAL - MAIN BENEFIT PREMIUM",
    "ANNUAL PREMIUM - CRITICAL ILLNESS COVER RIDER BENEFIT" = 0,  # Example, populate as needed
    "ANNUAL PREMIUM - LAST EXPENSE/FUNERAL COVER RIDER" = 0,  # Example, populate as needed
    "ANNUAL PREMIUM - DISABILITY RIDER" = 0,  # Example, populate as needed
    DATE_OF_BIRTH,
    "LOAN START DATE",
    "LOAN END DATE",
    "EXIT DATE OF THE LIFE ASSURED",
    REASON_FOR_EXIT
  )

final_result <- premiums %>%
  group_by(MEM_NO) %>%
  summarise(
    POLICY_TYPE = PROD_DESC,
    GROUP_POLICY_NUMBER = POL_POLICY_NO,
    COVER_START_DATE = as.Date(ENDR_COVER_FROM_DATE, "%d%m%Y"),
    COVER_END_DATE = as.Date(ENDR_COVER_TO_DATE, "%d%m%Y"),
    "GROUP POLICY STATUS AS AT 31 DECEMBER 2018 (ACTIVE)" = "Active",
    PRINCIPAL_MEMBER_NUMBER = MEM_NO,
    "LIFE NUMBER (WHERE COVERED LIFE IS A DEPENDANT)" = MEM_NO,
    SEX = MEM_SEX,
    "CRITICAL ILLNESS RIDER INDICATOR" = case_when(
      CVT_DESC == "Critical Illness" & PCM_PREMIUM > 0 ~ 1,
      TRUE ~ 0
    ),
    "LAST EXPENSE/FUNERAL COVER RIDER INDICATOR" = case_when(
      CVT_DESC == c("Last Expense","Group Last Expense") & PCM_PREMIUM > 0 ~ 1,
      TRUE ~ 0
    ),
    "DISABILITY RIDER INDICATOR" = case_when(
      CVT_DESC == c("Permanent And Total Disability","Temporary and Total Disability") & PCM_PREMIUM > 0 ~ 1,
      TRUE ~ 0
    ),
    "INDIVIDUAL LIFE STATUS AS AT 31 DECEMBER " = "Active",
    "SUMS ASSURED-MAIN BENEFIT" = PCM_SA,
    "SUMS ASSURED-CRITICAL ILLNESS COVER RIDER BENEFIT" = case_when(
      CVT_DESC == "Critical Illness" ~ PCM_SA,
      TRUE ~ 0
    ),
    "SUMS ASSURED-LAST EXPENSE/FUNERAL COVER RIDER BENEFIT" = case_when(
      CVT_DESC == c("Last Expense","Group Last Expense") ~ PCM_SA,
      TRUE ~ 0
    ),
    "SUM ASSURED DISABILITY RIDER" = case_when(
      CVT_DESC == c("Permanent And Total Disability","Temporary and Total Disability") ~ PCM_SA,
      TRUE ~ 0
    ),
    "ANNUAL - MAIN BENEFIT PREMIUM" = PCM_PREMIUM,
    "EXTRA ANNUAL PREMIUM - MAIN BENEFIT (FOR IMPAIRED LIVES)" = 0,  # Placeholder
    "ANNUAL PREMIUM - CRITICAL ILLNESS COVER RIDER BENEFIT" = case_when(
      CVT_DESC == "Credit Life" & PCM_PREMIUM > 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    "ANNUAL PREMIUM - LAST EXPENSE/FUNERAL COVER RIDER" = case_when(
      CVT_DESC == c("Last Expense","Group Last Expense") & PCM_PREMIUM > 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    "ANNUAL PREMIUM - DISABILITY RIDER" = case_when(
      CVT_DESC == c("Permanent And Total Disability","Temporary and Total Disability") & PCM_PREMIUM > 0 ~ PCM_PREMIUM,
      TRUE ~ 0
    ),
    DATE_OF_BIRTH = as.Date(MEM_DOB, "%d%m%Y"),
    "LOAN START DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)" = as.Date(PCM_LOAN_ISSUE_DATE),
    "LOAN END DATE (DDMMYYYY, FOR GROUP CREDIT POLICIES ONLY)" = "",  
    "EXIT DATE OF THE LIFE ASSURED (DDMMYYYY, WHERE APPLICABLE)" = "", 
    REASON_FOR_EXIT = ""  
  )

# Display the final result
print(final_result)
