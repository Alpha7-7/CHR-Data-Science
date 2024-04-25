

library(pdftools)
library(stringr)

# Load the PDF text
pdf_path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/R/Data/input.pdf"
pdf_text <- pdf_text(pdf_path)

system(paste0('start "', pdf_path, '"'))
# Define regular expressions for data extraction
tracking_number_pattern <- "(1Z[\\w-]+)"
pickup_date_pattern <- "Pickup Date\\s+(\\d{2}/\\d{2}/\\d{4})"
service_level_pattern <- "Service Level ([A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?)"
weight_pattern <- "Weight\\s+(\\d+)\\s+lbs"
zone_pattern <- "Zone\\s+(\\d+)"
payer_pattern <- "Payer\\s+([A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_name_pattern <- "\\sPickup Date\\s+(\\d{2}/\\d{2}/\\d{4})\\s+([A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_address_pattern <- "\\d+\\s+[A-Za-z].*,\\s+[A-Za-z]+\\s+\\d+"
receiver_address_pattern <- "\\d+\\s+.*[A-Za-z]{2}\\d{5}"

# Initialize variables to store extracted data
tracking_numbers <- str_match(pdf_text, tracking_number_pattern)[, 2]
pickup_dates <- str_match(pdf_text, pickup_date_pattern)[, 2]
pickup_dates <- as.Date(pickup_dates, format = "%m/%d/%Y")  # Convert to Date
service_levels <- str_match(pdf_text, service_level_pattern)[, 2]
weights <-as.integer(str_match(pdf_text, weight_pattern)[, 2])
zones <- as.integer(str_match(pdf_text, zone_pattern)[, 2])
payers <- str_match(pdf_text, payer_pattern)[, 2]
shipper_names <- str_match(pdf_text, shipper_name_pattern)[,3] 
shipper_addresses <- str_match(pdf_text, shipper_address_pattern)
receiver_addresses <- str_match(pdf_text, receiver_address_pattern)

# Create a data frame
parsed_data <- data.frame(
  TrackingNumber = tracking_numbers,
  PickupDate = pickup_dates,
  ServiceLevel = service_levels,
  Weight = weights,
  Zone = zones,
  Payer = payers,
  ShipperName = shipper_names,
  ShipperAddress = shipper_addresses,
  ReceiverAddress = receiver_addresses
)

# Print or further process the parsed data
print(parsed_data)


print(tracking_numbers)

###############################################################################
library(pdftools)
library(stringr)

# Load the PDF text
pdf_path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/R/Data/input.pdf"
pdf_text <- pdf_text(pdf_path)

# Define regular expressions for data extraction
tracking_number_pattern <- "UPS No: (1Z[\\w-]+)"
pickup_date_pattern <- "Pickup Date\\s+(\\d{2}/\\d{2}/\\d{4})"
service_level_pattern <- "Service Level ([A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?)"
weight_pattern <- "Weight\\s+(\\d+)\\s+lbs"
zone_pattern <- "Zone\\s+(\\d+)"
payer_pattern <- "Payer\\s+([A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_name_pattern <- "\\sPickup Date\\s+(\\d{2}/\\d{2}/\\d{4})\\s+([A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_address_pattern <- "\\d+\\s+[A-Za-z].*,\\s+[A-Za-z]+\\s+\\d+"
receiver_address_pattern <- "\\d+\\s+.*[A-Za-z]{2}\\d{5}"

# Initialize lists to store data from each page
all_tracking_numbers <- list()
all_pickup_dates <- list()
all_service_levels <- list()
all_weights <- list()
all_zones <- list()
all_payers <- list()
all_shipper_names <- list()
all_shipper_addresses <- list()
all_receiver_addresses <- list()

# Iterate through each page's text
for (page_text in pdf_text) {
  page_tracking_numbers <- str_match_all(page_text, tracking_number_pattern)[[1]][, 2]
  all_tracking_numbers <- c(all_tracking_numbers, page_tracking_numbers)
  
  try_pickup_dates <- str_match(page_text, pickup_date_pattern)[[1]][,2]
  all_pickup_dates <- c(all_pickup_dates, try_pickup_dates)

  
  page_service_levels <- str_match(page_text, service_level_pattern)[[1]][, 2]
  all_service_levels <- c(all_service_levels, page_service_levels)
  
  page_weights <- as.integer(str_match(page_text, weight_pattern)[[1]][, 2])
  all_weights <- c(all_weights, page_weights)
  
  page_zones <- as.integer(str_match(page_text, zone_pattern)[[1]][, 2])
  all_zones <- c(all_zones, page_zones)
  
  page_payers <- str_match(page_text, payer_pattern)[[1]][, 2]
  all_payers <- c(all_payers, page_payers)
  
  page_shipper_names <- str_match(page_text, shipper_name_pattern)[[1]][, 3]
  all_shipper_names <- c(all_shipper_names, page_shipper_names)
  
  page_shipper_addresses <- str_extract(page_text, shipper_address_pattern)[[1]]
  all_shipper_addresses <- c(all_shipper_addresses, page_shipper_addresses)
  
  page_receiver_addresses <- str_match(page_text, receiver_address_pattern)[[1]][, 1]
  all_receiver_addresses <- c(all_receiver_addresses, page_receiver_addresses)
}

# Convert lists to character vectors
all_tracking_numbers <- unlist(all_tracking_numbers)
all_pickup_dates <- as.Date(all_pickup_dates, format = "%m/%d/%Y")
all_service_levels <- unlist(all_service_levels)
all_weights <- unlist(all_weights)
all_zones <- unlist(all_zones)
all_payers <- unlist(all_payers)
all_shipper_names <- unlist(all_shipper_names)
all_shipper_addresses <- unlist(all_shipper_addresses)
all_receiver_addresses <- unlist(all_receiver_addresses)

# Create a data frame with extracted data
parsed_data <- data.frame(
  TrackingNumber = all_tracking_numbers,
  PickupDate = all_pickup_dates,
  ServiceLevel = all_service_levels,
  Weight = all_weights,
  Zone = all_zones,
  Payer = all_payers,
  ShipperName = all_shipper_names,
  ShipperAddress = all_shipper_addresses,
  ReceiverAddress = all_receiver_addresses
)

# Print or further process the parsed data
print(all_service_levels)



##########################
library(pdftools)
library(stringr)

# Load the PDF text
pdf_path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/R/Data/input.pdf"
pdf_text <- pdf_text(pdf_path)
print(pdf_text)
# Define regular expressions for data extraction
tracking_number_pattern <- "UPS No: (1Z[\\w-]+)"
pickup_date_pattern <- "Pickup Date\\s+(\\d{2}/\\d{2}/\\d{4})"
service_level_pattern <- "Service Level\\s+([A-Za-z]+\\s+[A-Za-z]+(\\s+[A-Za-z]+)?)"
weight_pattern <- "Weight\\s+(\\d+)\\s+lbs"
zone_pattern <- "Zone\\s+(\\d+)"
payer_pattern <- "Payer\\s+([A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_name_pattern <- "\\sPickup Date\\s+(\\d{2}/\\d{2}/\\d{4})\\s+([A-Za-z]+\\s[A-Za-z]+(\\s[A-Za-z]+)?)"
shipper_address_pattern <- "\\d+\\s+[A-Za-z].*,\\s+[A-Za-z]+\\s+\\d+"
receiver_address_pattern <- "Receiver Address: (.+)"


# Initialize lists to store data from each page
all_tracking_numbers <- list()
all_pickup_dates <- list()
all_service_levels <- list()
all_weights <- list()
all_zones <- list()
all_payers <- list()
all_shipper_names <- list()
all_shipper_addresses <- list()
all_receiver_addresses <- list()

# Iterate through each page's text
for (page_text in pdf_text) {
  page_tracking_numbers <- str_match_all(page_text, tracking_number_pattern)[[1]][,1]
  all_tracking_numbers <- c(all_tracking_numbers, page_tracking_numbers)
  
  page_pickup_dates <- str_match(page_text, pickup_date_pattern)[,2]
  all_pickup_dates <- c(all_pickup_dates, page_pickup_dates)
  
  page_service_levels <- str_match(page_text, service_level_pattern)[, 1]
  all_service_levels <- c(all_service_levels, page_service_levels)
  
  page_weights <- str_match(page_text, weight_pattern)[, 1]
  all_weights <- c(all_weights, page_weights)
  
  page_zones <- str_match(page_text, zone_pattern)[, 1]
  all_zones <- c(all_zones, page_zones)
  
  page_payers <- str_match(page_text, payer_pattern)[, 1]
  all_payers <- c(all_payers, page_payers)
  
  page_shipper_names <- str_match(page_text, shipper_name_pattern)[, 2]
  all_shipper_names <- c(all_shipper_names, page_shipper_names)
  
  page_shipper_addresses <- str_extract(page_text, shipper_address_pattern)
  all_shipper_addresses <- c(all_shipper_addresses, page_shipper_addresses)
  
  page_receiver_addresses <- str_match(page_text, receiver_address_pattern)[, 1]
  all_receiver_addresses <- c(all_receiver_addresses, page_receiver_addresses)
}

# Convert lists to character vectors
all_tracking_numbers <- unlist(all_tracking_numbers)
all_pickup_dates <- as.Date(unlist(all_pickup_dates), format = "%m/%d/%Y")
all_service_levels <- unlist(all_service_levels)
all_weights <- unlist(all_weights)
all_zones <- unlist(all_zones)
all_payers <- unlist(all_payers)
all_shipper_names <- unlist(all_shipper_names)
all_shipper_addresses <- unlist(all_shipper_addresses)
all_receiver_addresses <- unlist(all_receiver_addresses)

# Create a data frame with extracted data
parsed_data <- data.frame(
  TrackingNumber = all_tracking_numbers,
  PickupDate = all_pickup_dates,
  ServiceLevel = all_service_levels,
  Weight = all_weights,
  Zone = all_zones,
  Payer = all_payers,
  ShipperName = all_shipper_names,
  ShipperAddress = all_shipper_addresses,
  ReceiverAddress = all_receiver_addresses
)

# Print or further process the parsed data
print(all_tracking_numbers)
print(all_shipper_addresses)
