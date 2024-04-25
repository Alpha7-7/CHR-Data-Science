library(pdftools)
library(dplyr)

pdf_path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/R/Data/input.pdf"
pdf_text <- pdf_text(pdf_path)

# Split the text into sections based on "UPS No"
sections <- strsplit(pdf_text, "\\s*UPS No:\\s+")[[1]]

# Define regular expression patterns for data extraction
section_pattern <- "\\s*UPS No:\\s+([1-9A-Z-]+)\\s+Shipper\\s+([A-Za-z\\s]+)\\s+Receiver\\s+([A-Za-z\\s]+)\\s+Freight\\s+([0-9.]+)\\s+([0-9.]+)\\s+Pickup Date\\s+(\\d{2}/\\d{2}/\\d{4})\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+Fuel Surcharge\\s+([0-9.]+)\\s+([0-9.]+)\\s+Service Level ([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+Weight\\s+(\\d+) lbs\\s+([A-Za-z\\s0-9,-]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+([0-9.]+)\\s+([0-9.]+)\\s+Zone\\s+(\\d+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z\\s]+)\\s+Payer\\s+([A-Za-z\\s]+)\\s+([A-Za-z0-9\\s]+)\\s+([A-Za-z\\s]+)\\s+([A-Za-z0-9\\s]+)\\s+Bill Reference:\\s+([A-Za-z0-9\\s]+)\\s+-\\s+([A-Za-z0-9\\s]+)\\s+([A-Za-z0-9\\s]+)\\s+Total\\s+([0-9.]+)\\s+([0-9.]+)"

# Initialize an empty data frame to store the parsed data
parsed_df <- data.frame(
  UPS_No = character(),
  Pickup_Date = character(),
  Service_Level = character(),
  Weight = character(),
  Zone = character(),
  Payer = character(),
  Shipper_Name = character(),
  Shipper_Address = character(),
  Receiver_Name = character(),
  Receiver_Address = character(),
  Billing = character(),
  List_Price = character(),
  Discount_Price = character(),
  stringsAsFactors = FALSE
)

# Loop through each section and extract required information
for (section_text in sections) {
  if (grepl(section_pattern, section_text, perl = TRUE)) {
    matches <- regmatches(section_text, gregexpr(section_pattern, section_text, perl = TRUE))
    
    for (match in matches[[1]]) {
      parsed_df <- parsed_df %>%
        add_row(
          UPS_No = match[1],
          Pickup_Date = match[7],
          Service_Level = match[11],
          Weight = match[15],
          Zone = match[21],
          Payer = match[27],
          Shipper_Name = match[2],
          Shipper_Address = paste(match[8], match[9]),
          Receiver_Name = match[3],
          Receiver_Address = paste(match[10], match[26]),
          Billing = match[12],
          List_Price = match[13],
          Discount_Price = match[14]
        )
    }
  }
}

# Print the parsed data frame
print(parsed_df)
