## MODIFIED ##
library(dplyr)
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

save.image("DATA.RData")
# RUNNING THE FUNCTION
#------- CLAIMS --------#
# BENEFITTYPE column
Claims <- CLAIMS

# Combine the  columns with a dash separator and create a new column
#Claims <- Claims %>% mutate(BENEFITDESCRIPTION = paste(INVOICEBENEFIT, INVOICELINESUBBENEFIT, sep = "-"))

unique_benefit <- data.frame(unique(Claims$INVOICEBENEFIT))
unique_benefit <- unique_benefit %>%
  rename(INVOICEBENEFIT = unique.Claims.INVOICEBENEFIT.)
unique(unique_benefit$INVOICEBENEFIT)

# Replace each value in INVOICEBENFIT with a BENEFITTYE before Vlookup
unique_benefit$BENEFITLIMIT <- NA
for (benefit in unique_benefit$INVOICEBENEFIT) {
  new_benefit_type <- readline(prompt = paste("Replace", benefit, "with: "))
  unique_benefit$BENEFITLIMIT[unique_benefit$INVOICEBENEFIT == benefit] <- new_benefit_type
}

unique_benefit[2, 2] <- "Outpatient"
unique_benefit[7, 2] <- "Outpatient"
unique_benefit[16, 2] <- "Inpatient" #XL (12 too)

# Merge BENEFITTYPE column from unique_benefit into Claims dataframe
Claims <- lookup(datacols = Claims,
                 lookupframe = unique_benefit,
                 colmatch = list("INVOICEBENEFIT", "INVOICEBENEFIT"),
                 colsget = "BENEFITLIMIT",
                 replace = TRUE,
                 na_if_not_found = TRUE)

# MEDICAL SERVICES
unique_benefit_med <- data.frame(unique(Claims$INVOICELINEDETAILS))
unique_benefit_med <- unique_benefit_med %>%
  rename(INVOICELINEDETAILS = unique.Claims.INVOICELINEDETAILS.)
unique(unique_benefit_med$INVOICELINEDETAILS)


# Replace each value in INVOICEBENFIT with a MEDICALSERVICES before Vlookup
unique_benefit_med$MEDICALSEVICES <- NA
for (benefit in unique_benefit_med$INVOICELINEDETAILS) {
  new_medicalservices <- readline(prompt = paste("Replace", benefit, "with: "))
  unique_benefit_med$MEDICALSERVICES[unique_benefit_med$INVOICELINEDETAILS == benefit] <- new_medicalservices
}
unique_benefit_med[, 2] <- NULL # delete second column 
unique_benefit_med[67, 2] <- "Medication"

# Merge MEDICALSERVICES column from unique_benefit into Claims dataframe
Claims <- lookup(datacols = Claims,
                 lookupframe = unique_benefit_med,
                 colmatch = list("INVOICELINEDETAILS", "INVOICELINEDETAILS"),
                 colsget = "MEDICALSERVICES",
                 replace = TRUE,
                 na_if_not_found = TRUE)


#------ PREMIUM ---------#
# BENEFITTYPE COLUMN IN THE DATA 
premiums <- premiums_2018_2023
unique_benefit_prem <- data.frame(unique(premiums$BENEFITDESCRIPTION))
unique_benefit_prem <- unique_benefit_prem %>%
  rename(BENEFITDESCRIPTION = unique.premiums.BENEFITDESCRIPTION.)
unique(unique_benefit_prem$BENEFITDESCRIPTION)

# Replace each value in INVOICEBENFIT with a BENEFITTYE before Vlookup
unique_benefit_prem$BENEFITTYPE <- NA
for (benefit in unique_benefit_prem$BENEFITDESCRIPTION) {
  new_benefit_type <- readline(prompt = paste("Replace", benefit, "with: "))
  unique_benefit_prem$BENEFITTYPE[unique_benefit_prem$BENEFITDESCRIPTION == benefit] <- new_benefit_type
}

# Merge BENEFITTYPE column from unique_benefit into Claims dataframe
premiums <- lookup(datacols = premiums,
                 lookupframe = unique_benefit_prem,
                 colmatch = list("BENEFITDESCRIPTION", "BENEFITDESCRIPTION"),
                 colsget = "BENEFITTYPE",
                 replace = TRUE,
                 na_if_not_found = TRUE)

unique_benefit_prem[22, 2] <- "Inpatient"

# INTERMEDIARY
unique <- premiums %>% select (POLICYNUMBER,INTERESTEDPARTYTYPE)
Claims <- lookup(datacols = Claims,
                   lookupframe = unique,
                   colmatch = list("POLICYNUMBER", "POLICYNUMBER"),
                   colsget = "INTERESTEDPARTYTYPE",
                   replace = TRUE,
                   na_if_not_found = TRUE)
unique <- unique(unique)
df_merged <- left_join(Claims, unique, by = "POLICYNUMBER") 
