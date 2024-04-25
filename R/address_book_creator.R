library(stringr)
library(countrycode)
path <- 'C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/'
address_book_name <- 'FedExTest.csv'
# Read in CSV file
address_book <- read.csv(paste0(path,address_book_name), header = TRUE, stringsAsFactors = FALSE)

address_book$tenantId <- 605
address_book$addressType <- "'any'"

# Update columns to all uppercase
for (i in 1:nrow(address_book)) {
  for (j in 1:ncol(address_book)) {
    if (is.character(address_book[i, j])) {
      address_book[i, j] <- toupper(address_book[i, j])
    }
  }
}

# Clean Zip codes

clean_zip <- function(zip, country) {
    tryCatch({
      zip <- as.character(zip)
      zip <- gsub("-", "", zip)
      zip <- gsub(" ", "", zip)
      
      if (country == "US") {
        zip <- gsub("-", "", zip)
        zip <- gsub(" ", "", zip)
        zip <- gsub("[^[:digit:]]", "", zip)
        zip[nchar(zip) == 3] <- stringr::str_pad(zip[nchar(zip) < 5], 5, pad = "00")
        zip[nchar(zip) == 4] <- stringr::str_pad(zip[nchar(zip) < 5], 5, pad = "0")
        zip[nchar(zip) > 5] <- stringr::str_trunc(zip[nchar(zip) > 5], 5, ellipsis = "")
        invalid_zips <- zip[nchar(zip) != 5 & nchar(zip) != 9]
      } else if (country == "CN") {
        zip <- gsub(" ", "", zip)
        zip[nchar(zip) < 6] <- stringr::str_pad(zip[nchar(zip) < 6], 6, pad = "0")
        invalid_zips <- zip[nchar(zip) != 6]
      } else if (country == "CA") {
        zip <- gsub(" ", "", zip)
        invalid_zips <- zip[nchar(zip) != 6]
        
      } else {
        invalid_zips <- NULL
      }
      if (length(invalid_zips) > 0) {
        warning(paste("Invalid zip codes detected:", paste(invalid_zips, collapse = ", ")))
      }
      
      return(zip)
    }, error = function(e) {
      message("Error occurred while cleaning zip codes: ", conditionMessage(e))
      return(NULL)
    })
  }
  


# Clean Zip Codes
address_book$Zip <- mapply(clean_zip, address_book$Zip, address_book$CountryCode)

# Update columns ship To and ship From
tryCatch({
  # If no value, default to ship From
  if (is.null(address_book$AddressTypeCode)) {
    address_book$shipTo <- 1
    address_book$shipFrom <- 0
  } 
  
  # add columns ship To and ship From
  address_book$shipTo <- ifelse(address_book$AddressTypeCode %in% c("S", "X"), 1, 0)
  address_book$shipFrom <- ifelse(address_book$AddressTypeCode %in% c("B", "X"), 1, 0)
}, error = function(e) {
  cat("Error: Failed to update columns shipTo and shipFrom.\n")
  message(e)
})

# clean_phone function
clean_phone <- function(phone_number) {
  # Remove anything that isn't a digit
  clean_number <- gsub("[^[:digit:]]", "", phone_number)
  
  # If the phone number is all zeros or empty, return NULL
  if (all(strsplit(clean_number, "")[[1]] == "0") | nchar(clean_number) < 7) {
    return("NULL")
  }
  
  # Remove any leading 0's
  clean_number <- gsub("^0+", "", clean_number)
  
  # If the phone number is 10 digits long, add parentheses
  if (nchar(clean_number) == 10) {
    cleaned_number <- paste0(substr(clean_number, 1, 3), substr(clean_number, 4, 6), substr(clean_number, 7, 10))
  } else {
    cleaned_number <- clean_number
  }
  
  return(cleaned_number)
}

# Apply the clean_phone function to columns of the data frame

address_book$PhoneNumber <- sapply(address_book$PhoneNumber, clean_phone)


# Add Country Name 

address_book$CountryName <- countrycode(address_book$CountryCode, "iso2c", "country.name")


# Map state names for US addresses
get_state_name <- function(address_book) {
  ifelse(address_book$CountryCode == "US", 
         address_book$StateName <- state.name[match(address_book$State, state.abb)], 
         {
           # Get the ISO code for the country
           iso_code <- countrycode(sourcevar = address_book$CountryCode, origin = "iso3c", destination = "iso2c")
           # Get a data frame of state codes and names for the country
           state_codes <- data.frame(countrycode(sourcevar = iso_code, origin = "iso2c", destination = "iso3n"), 
                                     state.name = countrycode(sourcevar = iso_code, origin = "iso2c", destination = "country.name"))
           # Map state names for each row
           address_book$StateName <- sapply(address_book$State, function(x) {
             state_name <- state_codes$state.name[state_codes$countrycode == x]
             ifelse(length(state_name) == 0, NA, state_name)
           })
         })
  
  return(address_book)
}


address_book <- get_state_name(address_book)

# Handle cases where state name is not found
address_book$StateName[is.na(address_book$StateName)] <- "Unknown"


# Generate SQL insert statements for address_book table
address_book_inserts <- apply(address_book, 1, function(x) {
  paste0("INSERT INTO address_book (",
         paste0(c("tenantId", "addressType", "aliasName", "name", "company", "phone", "address1", "address2",
                  "city", "stateName", "stateCode", "zip", "countryName", "countryCode", "email",
                  "userId", "roleId", "isResidential", "isDC", "shipFrom", "shipTo", "taxid",
                  "is_email_opt_in"), collapse = ", "),
         ") VALUES (",
         ifelse(is.na(x[["tenantId"]]) | x[["tenantId"]] == "", "NULL", x[["tenantId"]]), ", ",
         ifelse(is.na(x[["addressType"]]) | x[["addressType"]] == "", "NULL", paste0("'", x[["addressType"]], "'")), ", ",
         ifelse(is.na(x[["Nickname"]]) | x[["Nickname"]] == "", "NULL", paste0("'", x[["Nickname"]], "'")), ", ",
         ifelse(is.na(x[["FullName"]]) | x[["FullName"]] == "", "NULL", paste0("'", x[["FullName"]], "'")), ", ",
         ifelse(is.na(x[["Company"]]) | x[["Company"]] == "", "NULL", paste0("'", x[["Company"]], "'")), ", ",
         ifelse(is.na(x[["PhoneNumber"]]) | x[["PhoneNumber"]] == "", "NULL", paste0("'", x[["PhoneNumber"]], "'")), ", ",
         ifelse(is.na(x[["AddressOne"]]) | x[["AddressOne"]] == "", "NULL", paste0("'", x[["AddressOne"]], "'")), ", ",
         ifelse(is.na(x[["AddressTwo"]]) | x[["AddressTwo"]] == "", "NULL", paste0("'", x[["AddressTwo"]], "'")), ", ",
         ifelse(is.na(x[["City"]]) | x[["City"]] == "", "NULL", paste0("'", x[["City"]], "'")), ", ",
         ifelse(is.na(x[["stateName"]]) | x[["stateName"]] == "", "NULL", paste0("'", x[["stateName"]], "'")), ", ",
         ifelse(is.na(x[["State"]]) | x[["State"]] == "", "NULL", paste0("'", x[["State"]], "'")), ", ",
         ifelse(is.na(x[["Zip"]]) | x[["Zip"]] == "", "NULL", paste0("'", x[["Zip"]], "'")), ", ",
         ifelse(is.na(x[["CountryName"]]) | x[["CountryName"]] == "", "NULL", paste0("'", x[["CountryName"]], "'")), ", ",
         ifelse(is.na(x[["CountryCode"]]) | x[["CountryCode"]] == "", "NULL", paste0("'", x[["CountryCode"]], "'")), ", ",
         ifelse(is.na(x[["EmailAddress"]]) | x[["EmailAddress"]] == "", "NULL", paste0("'", x[["EmailAddress"]], "'")), ", ",
         "NULL,", # userId
         "NULL,", # roleId
         ifelse(is.na(x[["ResidentialFlag"]]) | x[["ResidentialFlag"]] == "", "NULL", x[["ResidentialFlag"]]), ", ",
         "0,", # isDC
         ifelse(is.na(address_book$shipFrom) | address_book$shipFrom == "", "NULL", address_book$shipFrom), ",",
         ifelse(is.na(address_book$shipTo) | address_book$shipTo == "", "NULL", address_book$shipTo), ",",
         ifelse(is.na(address_book$CustomsID) | address_book$CustomsID == "", "NULL", paste0("'", address_book$CustomsID, "'")), ")"
         )}
)
  



#inserts <- address_book_inserts(address_book)


# Check that string fields are within max length allowed in database table
max_lengths <- c(11, 45, 120, 120, 120, 50, 120, 120, 100, 30, 2, 16, 100, 2, 50)
for (i in nrow(address_book)) {
  for (j in ncol(address_book[i, ])) {
    if (is.character(address_book[i, j]) && nchar(address_book[i, j]) > max_lengths[j]) {
      cat(sprintf("Warning: value in row %d, column %d exceeds maximum length for column in database table\n", i, j))
    }
  }
}

# Check that zip codes are valid
for (i in seq_along(address_book$Zip)) {
  if (nchar(address_book$Zip[i]) != 5 && nchar(address_book$Zip[i]) != 6 && !is.na(address_book$Zip[i])) {
    cat(sprintf("Warning: invalid zip code in row %d\n", i))
  }
}

for (i in seq_along(address_book$Zip)) {
  if (nchar(address_book$Zip[i]) != 5 && nchar(address_book$Zip[i]) != 6 && !is.na(address_book$Zip[i])) {
    if (substr(address_book$Zip[i], 1, 2) == "AB" || substr(address_book$Zip[i], 1, 2) == "BC" || 
        substr(address_book$Zip[i], 1, 2) == "MB" || substr(address_book$Zip[i], 1, 2) == "NB" ||
        substr(address_book$Zip[i], 1, 2) == "NL" || substr(address_book$Zip[i], 1, 2) == "NS" ||
        substr(address_book$Zip[i], 1, 2) == "NT" || substr(address_book$Zip[i], 1, 2) == "NU" ||
        substr(address_book$Zip[i], 1, 2) == "ON" || substr(address_book$Zip[i], 1, 2) == "PE" ||
        substr(address_book$Zip[i], 1, 2) == "QC" || substr(address_book$Zip[i], 1, 2) == "SK" ||
        substr(address_book$Zip[i], 1, 2) == "YT") {
      # Canadian zip code
      next
    } else if (grepl("^[0-9]+$", address_book$Zip[i]) && nchar(address_book$Zip[i]) == 6) {
      # Chinese zip code
      next
    } else {
      cat(sprintf("Warning: invalid zip code in row %d\n", i))
    }
  }
}

# Write SQL insert statements for address_book table to text file
writeLines(address_book_inserts, paste0(path,'Address_Book_SQL_Inserts.txt'))


# Read in CSV file
bill_to_address <- read.csv("path/to/fedex/address_book_export.csv", header = TRUE, stringsAsFactors = FALSE)

# Generate SQL insert statements for bill_to_address table
bill_to_address_inserts <- sprintf("INSERT INTO bill_to_address (tenantid, ship_from_address_id, ship_to_address_id, bill_to_type, acc_no, bill_to_country, acc_no_duties_taxes, bill_to_type_duties_taxes, bill_to_country_duties_taxes, acc_zipcode_duties_taxes, acc_zipcode, address_default, ref1, ref2, ref3, ref4, acc_desc, es_flag, is_deleted, modified_datetime, created_datetime, carrierid) VALUES (%d, NULL, NULL, '%s', '%s', '%s', '%s', '%s', '%s', NULL, '%s', %d, '%s', '%s', '%s', '%s', '%s', NULL, %d, %d, NULL, NULL, NULL)",
                                   bill_to_address$tenantid, bill_to_address$BillCode, bill_to_address$BillAccountNumber, bill_to_address$CountryCode, bill_to_address$DutyBillAccountNumber, bill_to_address$DutyBillCode, bill_to_address$CountryCode, bill_to_address$DutyBillCode, bill_to_address$CountryCode, bill_to_address$Zip, bill_to_address$acc_zipcode, bill_to_address$address_default, bill_to_address$ref1, bill_to_address$ref2, bill_to_address$ref3, bill_to_address$ref4, bill_to_address$acc_desc, NULL, bill_to_address$is_deleted, NULL, NULL, NULL)

# Write SQL insert statements for
