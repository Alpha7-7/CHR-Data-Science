library(pdftools)
library(stringr)
library(openxlsx)
library(stringdist)
library(tidyverse)
library(dplyr)
library(tidyr)
library(countrycode)



########### UPS Parse ##############

path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/R/Data/"
file_name <- 'input.pdf'
source <- "WW Express"

#raw_text <- pdf_text(paste0(path,file_name))
raw_text <- pdf_ocr_text(pdf = paste0(path,file_name))


#### Helper Functions 

# Function to parse a block
parse_block <- function(block, source) {
  
  tracking_no <- str_extract(block, "(?<=UPS No: )[\\w\\d]+")
  tracking_no <- trimws(x = substr(x = tracking_no,start = 1,stop = 18),which = "both")    
  
  pickup_date <- str_extract(block, "(?<=Pickup Date )[\\d\\/]+")
  
  total_packages <- 1
  
  pay_type <- str_extract(block, "(?<=Payer )[\\w]+")
  pay_type <- trimws(x = substr(x = pay_type,start = 1,stop = 12),which = "both")
  package_type <- "CUSTOMER PACKAGING"
  
  service_type <- service_type <- str_extract(block, "(?<=Service Level )([^\\s]+\\s[^\\s]+)")
  service_type <- trimws(x = substr(x = service_type,start = 1,stop = 24),which = "both")
  
  actual_wt <- as.numeric(str_extract(block, "(?<=Weight )[\\d]+"))
  billable_wt <- as.numeric(str_extract(block, "(?<=Weight )[\\d]+"))
  
  sender_address <- ifelse(!is.na(sender_name),
                           str_extract(block, paste0("(?<=\\n", sender_name, ")[^\\n]+")),
                           NA)
  receiver_address <- ifelse(!is.na(receiver_name),
                             str_extract(block, paste0("(?<=\\n", receiver_name, ")[^\\n]+")),
                             NA)
  
  
  #tracking_no <- str_extract(block, "(?<=Tracking No.:)(.*)(?=\\n)")
  
  
  #service_type <- str_extract(block, "(?<=Service Type:)(.*)(?=\\n)")
  
  #total_packages <- as.integer(str_extract(block, "(?<=Total Packages:)(.*)(?=\\n)"))
  
  
  #pay_type <- str_extract(block, "(?<=Transportation:     )(.*)(?=\\n)")
  #pay_type <- trimws(x = substr(x = pay_type,start = 1,stop = 12),which = "both")
  
  
  #package_type <- str_extract(block, "(?<=Package Type:)(.*)(?=\\n)")
  #package_type <- trimws(x = substr(x = package_type,start = 1,stop = 15),which = "both")
  
  
  
  #actual_wt <- as.numeric(str_extract(block, "(?<=Actual Wt:)(.*)(?= lb)"))
  #billable_wt <- as.numeric(str_extract(block, "(?<=Billable Wt:)(.*)(?= lb)"))
  
  
  
  
  return(data.frame(tracking_no, pickup_date, sender_address,
                    recipient_address,service_type, total_packages,
                    package_type, actual_wt,billable_wt, pay_type,
                    stringsAsFactors = FALSE))
}


#Surcharges

#freight <- as.numeric(str_extract(block, "(?<=Freight )[\\d\\.]+"))
#freight <- ifelse(is.na(as.numeric(freight)), 0, as.numeric(freight))

#additional_handling <- as.numeric(str_extract(block, "(?<=Additional Handling )[\\d\\.]+"))
#additional_handling <- ifelse(is.na(as.numeric(additional_handling)), 0, as.numeric(additional_handling))

#fuel_surcharge <- as.numeric(str_extract(block, "(?<=Fuel Surcharge )[\\d\\.]+"))
#fuel_surcharge <- ifelse(is.na(as.numeric(fuel_surcharge)), 0, as.numeric(fuel_surcharge))

#delivery_area_surcharge <- as.numeric(str_extract(block, "(?<=Delivery Area Surcharge )[\\d\\.]+"))
#delivery_area_surcharge <- ifelse(is.na(as.numeric(delivery_area_surcharge)), 0, as.numeric(delivery_area_surcharge))
# Define function to parse charges
parse_charges <- function(block) {
  lines <- strsplit(block, "\n")[[1]]
  #tracking_no <- str_extract(block, "(?<=Tracking No.\\:\\s{7})\\w+")
  tracking_no <- str_extract(block, "UPS No: ")
  #tracking_no <- gsub(".*Tracking No.: *(.*)", "\\1", lines[6])
  lines <- trimws(substr(x = lines,start = 75,stop = 130))
  
  # Create data frame
  df <- data.frame(
    tracking_no = rep(x = tracking_no, length(lines)),
    lines = unlist(lines),
    stringsAsFactors = FALSE
  )
  
  return(df)
}



# Split the text into blocks, each block represents a shipment
#blocks <- str_split(raw_text, "\n\nShip To:") %>% unlist()
#blocks <- str_split(raw_text, "\n\nUPS No:") %>% unlist()
blocks <- str_split(raw_text, "(?=1Z)|(?=\n\nUPS No:)") %>% unlist()

# Adding back "1Z" to each block if it was removed
blocks <- ifelse(str_starts(blocks, "1Z"), blocks, paste0("1Z", blocks))

# Apply the function to each block
df <- lapply(blocks, parse_block) %>% do.call(rbind, .)
df <- df[!is.na(df$tracking_no),]
df <- df[!is.na(df$total_packages),]

# Remove the last three numbers
#df$pay_type <- gsub("\\d{3}$", "", df$pay_type)
#df$pay_type <- sub("Receiver \\w+", "Receiver", df$pay_type)

#df$recipient.address <- sub(" Pack$", "", df$recipient.address)
#df$recipient.country <- sub(" Pack$", "", df$recipient.country)

# Extract and split the address into city, state, and zip
df <- df %>%
  mutate(address = str_extract(recipient.address, "\\b[A-Za-z ]+ [A-Z]{2} \\d{5}\\b"),
         address_split = str_split(recipient.address, " ", simplify = TRUE)) %>%
  mutate(city = ifelse(length(address_split[1,]) >= 3,
                       paste(address_split[,1], address_split[,2]),
                       address_split[,1]),
         state = ifelse(length(address_split[1,]) >= 3, 
                        address_split[,3], 
                        address_split[,2]),
         zip = ifelse(length(address_split[1,]) >= 3, 
                      address_split[,4], 
                      address_split[,3])) %>%
  select(-address_split)
df <- df %>%
  mutate(address = str_extract(recipient.address, "\\b[A-Za-z ]+ [A-Z]{2} \\d{5}\\b"),
         address_split = str_split(recipient.address, " ", simplify = TRUE)) %>%
  mutate(city = ifelse(is.na(address_split[,3]), 
                       address_split[,1], 
                       paste(address_split[,1], address_split[,2])),
         state = ifelse(is.na(address_split[,3]), 
                        address_split[,2], 
                        address_split[,3]),
         zip = ifelse(is.na(address_split[,3]), 
                      address_split[,3], 
                      address_split[,4])) %>%
  select(-address_split)


df <- df %>%
  mutate(
    split_address = str_split(address, " "),
    city = if_else(zip == "", map_chr(split_address, ~.x[1]), city),
    state = if_else(zip == "", map_chr(split_address, ~.x[2]), state),
    zip_code = if_else(zip == "", map_chr(split_address, ~.x[3]), zip)
  ) %>%
  select(-split_address) # Remove the split_address column

df <- df %>%
  select(-zip)
df$zip_code <- substr(df$zip_code,start = 1,stop = 5)

# Fix address splitting
df <- df %>%
  mutate(
    city = str_extract(address, "(.*)(?= [A-Z]{2} \\d{5})"),
    state = str_extract(address, "(?<= )([A-Z]{2})(?= \\d{5})"),
    zip_code = str_extract(address, "(?<= )[0-9]{5}$")
  )



# Apply the function to each block
df_charges <- lapply(blocks, parse_charges) %>% do.call(rbind, .)
df_charges <- df_charges[!is.na(df_charges$tracking_no),]
df_charges <- df_charges[df_charges$lines != "",]

# Filter the data frame
df_charges <- df_charges[grepl("surcharge|charge|cost|Additional|weight", df_charges$lines, ignore.case = TRUE), ]

df_charges$lines <- sub("^\\d{1}\\s+", "", df_charges$lines)
df_charges$lines <- sub("^\\d{2}\\s+", "", df_charges$lines)
df_charges$lines <- sub("^\\d{4}\\s+", "", df_charges$lines)

# Remove rows containing the specified string pattern
df_charges$lines <- toupper(df_charges$lines)

pattern <- "Billing\\s+Option\\s+Shpts\\s+Pkgs\\s+Pub\\s+Charges"
df_charges <- df_charges[!grepl(pattern, df_charges$lines), ]
pattern <- "BILLING OPTION      SHPTS   PKGS   PUB CHARGE"
df_charges <- df_charges[!grepl(pattern, df_charges$lines), ]
pattern <- "CHARGES:                               RATE CH" 
df_charges <- df_charges[!grepl(pattern, df_charges$lines), ]
pattern <- ":"
removed <- df_charges[!grepl(pattern, df_charges$lines), ]
df_charges$lines <- gsub(pattern <- "3UPS TOTAL CHARGE",replacement =  "UPS TOTAL CHARGE", x = df_charges$lines )
pattern <- "BILLING OPTION      SHPTS      PKGS   PUB CHARGE"  
df_charges <- df_charges[!grepl(pattern, df_charges$lines), ]
df_charges$lines <- gsub(pattern = "IPMENT SERVICE CHARGE:|MENT SERVICE CHARGE:|HSHIPMENT SERVICE CHARGE:|HSHSHIPMENT SERVICE CHARGE:|SHSHSHIPMENT SERVICE CHARGE:|SHSHIPSHIPMENT SERVICE CHARGE:|HSHIPSHIPMENT SERVICE CHARGE:|SHIPSHIPMENT SERVICE CHARGE:|SHSHSHIPMENT SERVICE CHARGE:|SHSHIPMENT SERVICE CHARGE:", 
                         replacement = "SHIPMENT SERVICE CHARGE: ", 
                         x = df_charges$lines)

df_charges$lines <- str_replace(df_charges$lines, 
                                "-2023 UPS TOTAL CHARGE:|3/10/23   UPS TOTAL CHARGE|3UPS TOTAL CHARGE(?!:)", 
                                "UPS TOTAL CHARGE:")

df_charges$lines <- gsub(pattern = "TOTAL CHARGES  ", 
                         replacement = "TOTAL CHARGES:", 
                         x = df_charges$lines)

df_charges$lines <- gsub(pattern = "IPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "MENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "HSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "HSHSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "HSHSHIPMENT", replacement = "SHIPMENT: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "SHSHIPMENT", replacement = "SHIPMENT: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "SHSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "SHSHIPSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "HSHIPSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "SHIPSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "SHSHSHIPMENT SERVICE CHARGE:", replacement = "SHIPMENT SERVICE CHARGE: ", x = df_charges$lines,fixed = TRUE)
df_charges$lines <- gsub(pattern = "-2023 UPS TOTAL CHARGE:", replacement = "UPS TOTAL CHARGE:", x = df_charges$lines)
df_charges$lines <- gsub(pattern = "3/10/23   UPS TOTAL CHARGE", replacement = "UPS TOTAL CHARGE:", x = df_charges$lines)
df_charges$lines <- gsub(pattern = "3UPS TOTAL CHARGE:", replacement = "UPS TOTAL CHARGE:", x = df_charges$lines)

df_charges$lines <- gsub(pattern = "SHSHIPMENT SERVICE CHARGE:", 
                         replacement = "SHIPMENT SERVICE CHARGE:", 
                         x = df_charges$lines,
                         fixed = TRUE)

df_charges$lines <- gsub(pattern = "SHIPMENT SERVICE CHARGE: :",
                         replacement = "SHIPMENT SERVICE CHARGE: ",
                         x = df_charges$lines,
                         fixed = TRUE)
df_charges$lines <- gsub(pattern = "HSHIPMENT SERVICE CHARGE",
                         replacement = "SHIPMENT SERVICE CHARGE",
                         x = df_charges$lines,
                         fixed = TRUE)

df_charges$lines <- gsub(pattern = "ADDITIONAL HANDLING - WEIGHT", 
                         replacement = "ADDITIONAL HANDLING WEIGHT:", 
                         x = df_charges$lines)

df_charges$lines <- gsub(pattern = "ADDITIONAL HANDLING", 
                         replacement = "ADDITIONAL HANDLING:", 
                         x = df_charges$lines, 
                         fixed = TRUE)


df_charges$lines <- gsub(pattern = "HSHIPMENT SERVICE CHARGE",
                         replacement = "SHIPMENT SERVICE CHARGE",
                         x = df_charges$lines,
                         fixed = TRUE)
df_charges <- df_charges %>%
  mutate(lines = str_trim(lines)) %>%
  separate(lines, into = c("charge", "cost"), sep = ":", remove = TRUE, extra = "merge", fill = "right") %>%
  mutate(cost = parse_number(cost))

df_charges <- df_charges[!is.na(df_charges$cost),]

# Now we do a validation
is.numeric(df_charges$cost)
unique(df_charges$charge)


df_charges <- df_charges[!df_charges$charge %in% c("UPS TOTAL CHARGE","TOTAL CHARGES"), ]

#Specify the charges to check
#charges_to_check <- c("SHIPMENT SERVICE CHARGE",
#                      "DELIVERY AREA SURCHARGE - EXTENDED",
#                      "FUEL SURCHARGE",
#                      "DELIVERY AREA SURCHARGE",
#                      "RESIDENTIAL SURCHARGE",
#                      "PEAK/DEMAND SURCHARGE-ADDL.HANDLING",
#                      "DELIVERY AREA SURCHARGE - EXTENDED")

#df_charges <- df_charges[df_charges$charge %in% charges_to_check, ]


# Calculate the sum of charges in check_sum
check_sum <- aggregate(cost ~ tracking_no, data = df_charges[df_charges$charge %in% unique(df_charges$charge), ], FUN = sum)

# Calculate the sum of "UPS TOTAL CHARGE" in check_sum_totals
#check_sum_totals <- aggregate(cost ~ tracking_no, data = df_charges[df_charges$charge %in% c("UPS TOTAL CHARGE","TOTAL CHARGES"), ], FUN = sum)

# Calculate the sum of "UPS TOTAL CHARGE" in check_sum_totals
#check_sum_totals <- aggregate(cost ~ tracking_no, data = df_charges[df_charges$charge %in% c("TOTAL CHARGES"), ], FUN = sum)

# Find the matching tracking numbers
#matched_tracking_no <- intersect(check_sum$tracking_no, check_sum_totals$tracking_no)

# Filter the rows with matching tracking numbers and costs
#matched_rows <- merge(check_sum, check_sum_totals, by = "tracking_no")
#matched_rows <- matched_rows[matched_rows$cost.x == matched_rows$cost.y, ]

# Print the matched tracking numbers
#length(matched_rows$tracking_no)

# Compare the totals
#matched_tracking_no <- check_sum$tracking_no[check_sum$cost == check_sum_totals$cost]

#check_sum_totals <- aggregate(cost ~ tracking_no, 
#                              data = df_charges[df_charges$charge == "TOTAL CHARGES", ], 
#                              FUN = sum)

#matched_tracking_no <- check_sum$tracking_no[check_sum$cost == check_sum_totals$cost]

#matched_tracking_no <- check_sum$tracking_no[check_sum$tracking_no %in% check_sum_totals$tracking_no &
#                                               check_sum$cost == check_sum_totals$cost]



# Calculate the sum of the charges
#sum_charges <- rowSums(df[, charges_to_check], na.rm = TRUE)

# Check if the sum of charges equals the UPS total charges
#is_equal <- sum_charges == df$UPS_TOTAL_CHARGE | sum_charges == df$TOTAL_CHARGES

# Filter the rows where the sum of charges does not equal UPS total charges or TOTAL CHARGES
#mismatched_rows <- df[!is_equal, ]

# Print the mismatched rows
#print(mismatched_rows)
#Combine it all togethers
# Create a unique id for each charge within a tracking_no
df_charges <- df_charges %>%
  group_by(tracking_no) %>%
  mutate(charge_id = row_number()) %>%
  ungroup()


# Separate charges into separate rows

df_charges_long_charge <- df_charges %>%
  pivot_longer(
    cols = charge,
    names_to = "charge_type",
    values_to = "charge_value",
    names_prefix = "charge"
  ) %>%
  mutate(charge_id = paste(charge_id, charge_type, sep = "_")) %>%
  select(-charge_type)
#Separate costs into separate rows

df_charges_long_cost <- df_charges %>%
  pivot_longer(
    cols = cost,
    names_to = "cost_type",
    values_to = "cost_value",
    names_prefix = "cost"
  ) %>%
  mutate(charge_id = paste(charge_id, cost_type, sep = "_")) %>%
  select(-cost_type)

# Join charges and costs
df_charges_long <- left_join(df_charges_long_charge, df_charges_long_cost, by = c("tracking_no", "charge_id"))
df_charges_long <- df_charges_long %>%
  select(-c("cost","charge"))
# Pivot wider to separate each charge and cost into their own columns
df_charges_wide <- df_charges_long %>%
  pivot_wider(names_from = charge_id, values_from = c(charge_value, cost_value), names_glue = "{charge_id}_{.value}")

# Join df and df_charges_wide
df_merged <- left_join(df, df_charges_wide, by = "tracking_no")

#Add Totals
df_merged <- merge(df_merged, check_sum, by = "tracking_no", all.x = TRUE)

#Keep only single packages
df_merged <- df_merged[df_merged$total_packages == 1,]


# Keep pay type as shipper
df_merged <- df_merged[df_merged$pay_type == "Shipper",]

if (!"CountryCode" %in% colnames(df_merged)) {
  df_merged$CountryCode <- "US"
}



# Clean Zip Codes
df_merged$zip_code <- mapply(clean_zip, df_merged$zip_code, df_merged$CountryCode)

df_merged <- df_merged[!df_merged$zip_code == "",]
df_merged <- df_merged[!is.na(df_merged$zip_code),]

### Add ship from zones

sender_info <- read.csv(file = paste0(path,"shipfrom_addresses.csv"),
                        header = TRUE,
                        sep = ",",
                        na.strings = NA)

locations <-  unique(sender_info$Ship.From.Site)

locations <- data.frame(rbind(c(site = "DMN:DMS Salisbury" ,zip_code = "28147"),
                              c(site = "DMP:DMS York" ,zip_code =  "17406"),
                              c(site = "DMT:DMS Fort Worth TX" ,zip_code =  "76117"),
                              c(site = "DMI:DMS Woodstock" ,zip_code =  "60098")))

colnames(sender_info)[colnames(sender_info) == "Pro.."] <- "tracking_no"
colnames(sender_info)[colnames(sender_info) == "POSTAL_CODE"] <- "zip_code"
# Match Ship.From.Site values with zip codes
sender_info$zip_code <- locations[match(table = locations$site,x = sender_info$Ship.From.Site),]$zip_code


if (!"CountryCode" %in% colnames(sender_info)) {
  sender_info$CountryCode <- "US"
}

#Clean Zip Codes
sender_info$zip_code <- mapply(clean_zip, sender_info$zip_code, sender_info$CountryCode)


sender_info <- sender_info[!sender_info$tracking_no == "",]
sender_info <- sender_info[!sender_info$zip_code == "",]
sender_info <- sender_info[!is.na(sender_info$zip_code),]


# Keep Revelant Columns
sender_info <- sender_info %>%
  select(c("tracking_no","zip_code"))

df_merged <- merge(df_merged, sender_info, by = "tracking_no")


colnames(df_merged)[colnames(df_merged) == "zip_code.y"] <- "sender_zip_code"
colnames(df_merged)[colnames(df_merged) == "zip_code.x"] <- "zip_code"

df_merged$zone <- mapply(FUN = find.zone,origin = df_merged$sender_zip_code, destination = df_merged$zip_code)

# Find Das and Extended Das 
df_merged$das <- df_merged$zip_code %in% das_zips$das
df_merged$extended_das <-  df_merged$zip_code %in% das_zips$extended_das

for (i in 1:nrow(df_merged)) {
  
  df_merged$estimated_discounted_rate[i] <- find_rate(weight = df_merged$billable_wt[i], 
                                                      zone = df_merged$zone[i])
}

df_merged$estimated_das <- ifelse(test = df_merged$das == TRUE,yes = das_cost, no = 0 )

df_merged$estimated_extended_das <- ifelse(test = df_merged$extended_das == TRUE,
                                           yes = extended_das_cost, no = 0 )


for (i in 14:25) {
  # Replace NA values in the i-th column with a specific value, e.g., ""
  df_merged[, i] <- replace(df_merged[, i], is.na(df_merged[, i]), "")
}

df_merged$estimated_additional_handling <- 0  # Initialize column with 0

for (i in 14:25) {
  df_merged$estimated_additional_handling[df_merged[, i] == "ADDITIONAL HANDLING"] <- 
    additional_handling[as.character(df_merged$zone[df_merged[, i] == "ADDITIONAL HANDLING"])]
}

df_merged$estimated_additional_handling <- as.numeric(df_merged$estimated_additional_handling)

df_merged$estimated_peak_additional_handling <- 0  # Initialize column with 0

for (i in 14:25) {
  df_merged$estimated_peak_additional_handling[df_merged[, i] == "PEAK/DEMAND SURCHARGE-ADDL.HANDLING"] <-  peak_additional_handling
}

df_merged$estimated_residential_surcharge <- 0  

for (i in 14:25) {
  df_merged$estimated_residential_surcharge[df_merged[, i] == "RESIDENTIAL SURCHARGE" ] <-  ground_residential
}

#Sum it all up
df_merged$estimated_fuel <- rowSums(
  df_merged[, c("estimated_discounted_rate", "estimated_das", "estimated_extended_das", "estimated_additional_handling", "estimated_peak_additional_handling", "estimated_residential_surcharge")], na.rm = TRUE
) * ground_fuel

df_merged$estimated_total_cost <- rowSums(
  df_merged[, c("estimated_discounted_rate", "estimated_das", "estimated_extended_das", "estimated_additional_handling", "estimated_peak_additional_handling", "estimated_residential_surcharge","estimated_fuel")], na.rm = TRUE
) 

df_merged$estimated_cost_difference <- df_merged$cost - df_merged$estimated_total_cost


}
# Save the workbook

# Create a new workbook
wb <- createWorkbook(paste0(path,"analysis.xlsx"))
addWorksheet(wb, "Analysis")

writeData(wb = wb, borders = "columns",
          sheet = "Analysis",
          x =  df_merged, 
          rowNames = FALSE,startCol = 1,
          startRow = 1)


saveWorkbook(wb, paste0(path,"analysis.xlsx"))

########### Random Tracking Number Parse 

library(tesseract)

ocr_data <- pdf_ocr_data(paste0(path,file_name))

blocks <- str_split(ocr_data[[2]]$word, "Tracking") %>% unlist()

tracking_number <- c()

j <- 1

for (k in 1:17) {
  blocks <- str_split(ocr_data[[k]]$word, "Tracking") %>% unlist()
  
  for (i in 1:length(str_length(blocks))) {
    if (blocks[i] == "#:") {
      tracking_number[j] <-  (blocks[i + 1])
      j <- j + 1
    }
  }
}
str_extract(blocks, "Tracking")

tracking_number <- as.data.frame(tracking_number)

#Specify the file path and name for the Excel workbook

file_path <- paste0(path,"cleansed.csv")

write.csv(x = tracking_number,file = file_path)