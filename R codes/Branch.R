## MODIFIED ##
library(dplyr)
library(tidyr)
lookup <- function(datacols, lookupframe, colmatch, colsget, replace = FALSE, na_if_not_found = FALSE, return_pos = FALSE) {
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
  # add the specified columns to the original dataframe
  colsget_names <- intersect(names(lookupframe), colsget)
  if (length(colsget_names) > 0) {
    cols_to_add <- joined[,colsget_names, drop = FALSE]
    if (replace) {
      datacols[,colsget_names] <- cols_to_add
    } else {
      datacols <- bind_cols(datacols, cols_to_add)
    }
  } else {
    print("Column to return does not exist in lookupframe")
  }
  # handle missing values
  if (na_if_not_found) {
    colsget_names <- intersect(names(lookupframe), colsget)
    datacols[colsget_names][is.na(datacols[colsget_names])] <- NaN
  }
  if (return_pos) {
    return(list(datacols = datacols, joined = joined))
  }
  return(datacols)
}


# Combine the parentid with benefitdescription
df_combined_new <- premiums %>%
  unite(col = "Member_Benefit_Description", c("PARENTID", "BENEFITTYPE"), sep = "-")

df_claims_new <- Claims %>%
  unite(col = "Claim_Benefit_Description", c("PARENTID", "BENEFITTYPE"), sep = "-")


# Perform the VLOOKUP:

df_claims_new$BENEFITTYPE <- Claims$BENEFITTYPE
df_claims_selected <- select(df_claims_new, Claim_Benefit_Description, UWYEAR, ASSESSMENTID, Claims_Incurred, BENEFITTYPE)
df_benefit_selected <- select(df_combined_new, Member_Benefit_Description, BRANCHNAME)
remove(df_claims_new)
remove(df_combined_new)

df_claims_selected <- select(Claims, PARENTID, UWYEAR, ASSESSMENTID, Claims_Incurred, BENEFITTYPE)
df_benefit_selected <- select(premiums, PARENTID, BRANCHNAME)
df_benefit_selected <- select(premiums, PARENTID, BRANCHNAME) %>% 
  distinct()

result <- lookup(datacols = df_claims_selected,
                 lookupframe = df_benefit_selected,
                 colmatch = list("PARENTID", "PARENTID"),
                 colsget = "BRANCHNAME",
                 na_if_not_found = TRUE)


result <- lookup(datacols = df_claims_selected,
                 lookupframe = df_benefit_selected,
                 colmatch = list("Claim_Benefit_Description", "Member_Benefit_Description"),
                 colsget = "BRANCHNAME",
                 na_if_not_found = TRUE)


# Join the two dataframes by PARENTID column
df_joined <- left_join(df_claims_selected, df_benefit_selected, by = "PARENTID")
names(df_joined)
df_joined <- select(df_joined, PARENTID, UWYEAR, ASSESSMENTID, Claims_Incurred, BENEFITTYPE, BRANCHNAME) %>% 
  distinct()

# Group for claims
df_joined <- df_joined %>% filter(UWYEAR %in% 2018:2023)
BRANCH <- df_joined %>%
  group_by(BENEFITTYPE, BRANCHNAME, UWYEAR) %>%
  summarize(sum = sum(Claims_Incurred, na.rm = TRUE), distint_count = n_distinct(ASSESSMENTID, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(BRANCH, file = "BRANCHClaims.xlsx")
# for premiums
BRANCH_PREM <- premiums %>%
  group_by(BENEFITTYPE, BRANCHNAME, UWYEAR) %>%
  summarize(sum = sum(EARNEDPREM, na.rm = TRUE), distint_count = n_distinct(MEMBERNUMBER, na.rm = TRUE)) %>%
  pivot_wider(names_from = UWYEAR,
              values_from = c(sum, distint_count)) %>%
  mutate_at(vars(), funs(gsub(",","",.)))

write.xlsx(BRANCH_PREM, file = "BRANCHPREM.xlsx")
