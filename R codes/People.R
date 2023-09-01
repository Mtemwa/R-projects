library(dplyr)
library(openxlsx)
Claims_2021 <- Claims %>% filter(UWYEAR %in% 2023)
premiums_2021 <- premiums %>% filter(UWYEAR %in% 2023)
# Get the sum of settled amounts for each unique parent ID in claims
Total_claims <- Claims_2021 %>%
  group_by(PARENTID,INVOICEBENEFIT) %>%
  summarize(Claim_Paid = sum(SETTLEDAMOUNT),
            Visits = n_distinct(ASSESSMENTID),
            severity = Claim_Paid/Visits,
            unique <- unique(PARENTID))

Total_premiums <- premiums_2021 %>%
  group_by(PARENTID,BENEFITDESCRIPTION) %>%
  summarize(premium_amount = sum(EARNEDPREM),
            premium_members = n_distinct(MEMBERNUMBER),
            unique <- unique(PARENTID))

Total_premiums <- Total_premiums %>%
  rename(INVOICEBENEFIT = BENEFITDESCRIPTION)

# merge claims and premium datasets by parentid
merged_data <- Total_claims %>% 
  left_join(Total_premiums, by = c("PARENTID","INVOICEBENEFIT"))

# summarize results for parent IDs present in claims dataset
result <- merged_data %>% 
  group_by(PARENTID, INVOICEBENEFIT,Claim_Paid, Visits, severity, premium_amount, premium_members) %>% 
  summarize(loss_ratio = round((Claim_Paid/premium_amount),2),
            incidence_rate = round((Visits/premium_members),2))

write.xlsx(result, file = "People_2023.xlsx")
