#-------------------------------------------------------------------------------
# FEDEX / UPS services comparison
#
#-------------------------------------------------------------------------------

# (0) Organize -----------------------------------------------------------------
library(httr)
library(XML)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)

path <- 'C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop'
data_file <- paste0(path,'/SBDintltoca.csv')
service_match_file <- paste0('C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop','/fedex_ups_service_match.xlsx')

source(paste0(path,'/function_2.R'))

fedex <- 0
# (1) Read and clean data ------------------------------------------------------

if (fedex == 1) {
  all.data.fedex <- read.csv(file = data_file,
                             header = TRUE,
                             # nrows = 2000,
                             colClasses = "character", 
                             stringsAsFactors = FALSE)  
  
  relevant.columns <- c("Shipment.Tracking.Number",
                        "Shipment.Date..mm.dd.yyyy.",
                        "Invoice.date..mm.dd.yyyy.",
                        "Invoice.number",
                        "Service.Type",
                        "Service.Description",
                        "Shipper.Country.Territory",
                        "Shipper.Postal.Code",
                        "Residential..Commericial",
                        "Recipient.Country.Territory",
                        "Recipient.State.Province",
                        "Recipient.Postal.Code",
                        "Net.Charge.Amount.USD",
                        "Actual.Weight",
                        "Dimmed.height",
                        "Dimmed.width",
                        "Dimmed.length",
                        "Pieces.in.Shipment")
  
  # Set alternative names to be used in the further code
  short.column.names <- c("Shipment.Tracking.Number",
                          "Shipment.Date",
                          "Invoice.Date",
                          "Invoice.Number",
                          "Service.Type",
                          "Service.Description",
                          "Shipper.Country",
                          "Shipper.Postal.Code",
                          "Recipient.Residential",
                          "Recipient.Country",
                          "Recipient.State.Province",
                          "Recipient.Postal.Code",
                          "Charge",
                          "Weight",
                          "Height",
                          "Width",
                          "Length",
                          "pieces.in.shipment")
  
  data.fedex <- all.data.fedex[, relevant.columns]
  colnames(data.fedex) <- short.column.names
} else{
  all.data.fedex <- read.csv(file = data_file,
                             header = TRUE,
                             
                             colClasses = "character", 
                             stringsAsFactors = FALSE)  
  
  colnames(all.data.fedex)
  # Set alternative names to be used in the further code
  relevant.columns <- c("Shipment.Tracking.Number",
                        "Shipment.Date",
                        "Invoice.Date",
                        "Invoice.Number",
                        "UPS.Service.Type",
                        "UPS.Service.Description",
                        "Shipper.Country",
                        "Shipper.Postal.Code",
                        "Recipient.Residential",
                        "Recipient.Country",
                        "Recipient.State.Province",
                        "Recipient.Postal.Code",
                        "Charge",
                        "Weight",
                        "Height",
                        "Width",
                        "Length",
                        "pieces.in.shipment")
  
  data.fedex <- all.data.fedex[, relevant.columns]
  
  
}

data.fedex$row.number <- 1:nrow(data.fedex)

data.fedex$Weight <- as.numeric(data.fedex$Weight)  * as.numeric(data.fedex$pieces.in.shipment)
# Add UPS analog Service

# ---------- Fedex Service ----------|---------- UPS Service ----------|
# ES | FedEx 2Day                    | 02 | 2nd Day Air                |
# SO | FedEx Standard Overnight      | 13 | Next Day Air Saver         |
# XS | FedEx Economy                 | 12 | 3 Day Select               |
# PO | FedEx Priority Overnight      | 01 | Next Day Air               |
# FO | FedEx First Overnight         | 14 | UPS Next Day Air Early     |
# TA | FedEx 2Day AM                 | 59 | 2nd Day Air A.M.           |
# QH | Home Delivery                 | 03*| Ground + (*Residential)    |
# SG | Ground                        | 03*| Ground + (*Commercial)     |
# IE | FedEx International Economy   | 08 | Worldwide Expedited        |
# IP | FedEx International Priority  | 65 | Saver                      |
# SG | International Ground          | 11 | Standard                   |


if (fedex == 1) {
  
  service.match.table <- read_excel(path = service_match_file,
                                    col_names = TRUE,
                                    col_types = "text",sheet = 1)
  
  
  data.fedex <- left_join(data.fedex, service.match.table)
}

# Validate Data ----------------------------------------------------------------

# Validate "Residential" indicator: convert all 'Y' to 'R', then convert to 'C'
#   all which are not 'R' 
data.fedex$Recipient.Residential[data.fedex$Recipient.Residential %in% c("Y")] <- "R"
data.fedex$Recipient.Residential[data.fedex$Recipient.Residential != "R"] <- "C"

# Change Country Code for Saint Martin (MF) to Saint Maarten (SX).
#   UPS considers the both parts of the island equally as SX.
data.fedex$Recipient.Country[data.fedex$Recipient.Country == "MF"] <- "SX"
data.fedex$Shipper.Country[data.fedex$Shipper.Country == "MF"] <- "SX"

# Replace State.Province = PQ with QC
#   PQ is unofficially used, short for Province du Québec. Official is QC
data.fedex$Recipient.State.Province[data.fedex$Recipient.Country == "CA" &
                                      data.fedex$Recipient.State.Province == "PQ"] <- "QC"

# Find all US shipments with 0 weight and change it to 0.1
data.fedex$Weight[data.fedex$Weight == 0 & data.fedex$Shipper.Country == "US"] <- "0.1"

# Correct Postal Codes
# aa <-
#   data.fedex[
#     (nchar(data.fedex$Recipient.Postal.Code) < 5 & data.fedex$Recipient.Country == "US")
#     | (nchar(data.fedex$Recipient.Postal.Code) > 9 & data.fedex$Recipient.Country == "US")
#     | (nchar(data.fedex$Recipient.Postal.Code) > 5 & nchar(data.fedex$Recipient.Postal.Code) < 9 &
#          data.fedex$Recipient.Country == "US")
#     | (nchar(data.fedex$Shipper.Postal.Code) < 5 & data.fedex$Shipper.Country == "US")
#     | (nchar(data.fedex$Shipper.Postal.Code) > 9 & data.fedex$Shipper.Country == "US")
#     | (nchar(data.fedex$Shipper.Postal.Code) > 5 & nchar(data.fedex$Shipper.Postal.Code) < 9 &
#          data.fedex$Shipper.Country == "US")
#     , ]   # 19295 codes to be corrected
# rm(aa)
# Recipient: Add leading zeros to short US postal codes ( <5 digits)
data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) < 5 &
                                   data.fedex$Recipient.Country == "US"] <- 
  str_pad(data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) < 5 &
                                             data.fedex$Recipient.Country == "US"],
          5, pad = "0")

# Recipient: Trim too long US postal codes ( >9 digits to 9)
data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) > 9 &
                                   data.fedex$Recipient.Country == "US"] <- 
  str_trunc(data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) > 9 &
                                               data.fedex$Recipient.Country == "US"],
            9, ellipsis = "")


# Recipient: Add leading zeros to long US postal codes (6-8 digits to 9)
data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) > 5 &
                                   nchar(data.fedex$Recipient.Postal.Code) < 9 &
                                   data.fedex$Recipient.Country == "US"] <- 
  str_pad(data.fedex$Recipient.Postal.Code[nchar(data.fedex$Recipient.Postal.Code) > 5 &
                                             nchar(data.fedex$Recipient.Postal.Code) < 9 &
                                             data.fedex$Recipient.Country == "US"],
          9, pad = "0")

# Shipper: Add leading zeros to short US postal codes ( <5 digits)
data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) < 5 &
                                 data.fedex$Shipper.Country == "US"] <- 
  str_pad(data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) < 5 &
                                           data.fedex$Shipper.Country == "US"],
          5, pad = "0")

# Shipper: Trim too long US postal codes ( >9 digits to 9)
data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) > 9 &
                                 data.fedex$Shipper.Country == "US"] <- 
  str_trunc(data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) > 9 &
                                             data.fedex$Shipper.Country == "US"],
            9, ellipsis = "")

# Shipper: Add leading zeros to long US postal codes (6-8 digits to 9)
data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) > 5 &
                                 nchar(data.fedex$Shipper.Postal.Code) < 9 &
                                 data.fedex$Shipper.Country == "US"] <- 
  str_pad(data.fedex$Shipper.Postal.Code[nchar(data.fedex$Shipper.Postal.Code) > 5 &
                                           nchar(data.fedex$Shipper.Postal.Code) < 9 &
                                           data.fedex$Shipper.Country == "US"],
          9, pad = "0")





# (2) Group similar shippings --------------------------------------------------
data.fedex$group_id <- data.fedex %>%
  group_indices(Shipper.Country, 
                Shipper.Postal.Code,
                Recipient.Country,
                Recipient.State.Province,
                Recipient.Postal.Code,
                Recipient.Residential,
                Weight,
                Height,
                Width,
                Length,
                UPS.Service.Type,
                UPS.Service.Description,
                pieces.in.shipment)

data.fedex.grouped <- data.fedex %>%
  group_by(group_id) %>%
  slice(1) %>%
  select(Shipper.Country, 
         Shipper.Postal.Code,
         Recipient.Country,
         Recipient.State.Province,
         Recipient.Postal.Code,
         Recipient.Residential,
         Weight,
         Height,
         Width,
         Length,
         UPS.Service.Type,
         UPS.Service.Description,
         pieces.in.shipment,
         group_id)


# (3) Set up API settings ------------------------------------------------------
api.base.url <- "https://onlinetools.ups.com/ups.app/xml/Rate"  # For production
# api.base.url <- "https://wwwcie.ups.com/ups.app/xml/Rate"       # For testing
api.credentials <- list(UserID = "utopiac05317",
                        Password = "Sbd=0311",
                        AccessLicenseNumber = "ED9CDF90FE4D5E1D",
                        Account = "V423F5")


# (4) Request rating in a loop -------------------------------------------------
data.rated <- rate_shippings(data = data.fedex.grouped,
                             api.credentials)

# Merge results with the initial data by 'group_id'
final <- merge(data.fedex, 
               data.rated %>% 
                 select(group_id,
                        UPS.Charge,
                        UPS.Charge.Currency,
                        UPS.RateType,
                        Error),
               all.x = T)

# Save results
write.csv(final,
          file = paste0(path ,"/UPS.rated.csv"),
          row.names = F)
# # Read results
# final <- read.csv("UPS.rated.csv",
#                   colClasses = "character",
#                   stringsAsFactors = F)


# # Making Corrections -----------------------------------------------------------
# length(which(!is.na(final$Error)))   # 1192
# errors.tab <- as.data.frame(table(final$Error), stringsAsFactors = F)
# 
# # Select rows to make corrections and then to re-send requests
# rows_to_correct <-
#   final[ (final$Weight == 0 & final$Shipper.Country == "US") |
#           (final$Recipient.Country == "CA" & final$Recipient.State.Province == "PQ")
#         , ]
# 
# # Re-send requests
# rows_to_correct.rated <- rate_shippings(data = rows_to_correct,
#                                         api.credentials)
# 
# final <- rbind(final %>% filter(!group_id %in% rows_to_correct.rated$group_id) ,
#                rows_to_correct.rated)
# 
# # Count the number of errors
# length(which(!is.na(final$Error)))
# 
# # Save results
# write.csv(final,
#           file = "UPS.rated.csv",
#           row.names = F)


# A brief comparison of charges ------------------------------------------------
library(ggplot2)

final <- read.csv("UPS.rated.csv",
                  colClasses = "character",
                  stringsAsFactors = F)


data.plot <- final %>%
  filter(is.na(Error),
         UPS.Charge.Currency == "USD") %>%
  select(Shipper.Country, Recipient.Residential, Recipient.Country, Weight,
         UPS.Service.Description, Fedex.Service.Description, Fedex.Charge, UPS.Charge) %>%
  mutate(Recipient.Residential = as.factor(Recipient.Residential),
         UPS.Service.Description = as.factor(UPS.Service.Description),
         Fedex.Service.Description = as.factor(Fedex.Service.Description),
         Shipper.Country = as.factor(Shipper.Country),
         Recipient.Country = as.factor(Recipient.Country),
         Fedex.Charge = as.numeric(Fedex.Charge),
         UPS.Charge = as.numeric(UPS.Charge),
         Weight = as.numeric(Weight)) %>%
  mutate(Charge.Ratio = UPS.Charge / Fedex.Charge)


# Wait 20 sec before the plot is rendered
ggplot(data.plot, aes(x=Fedex.Charge, y=UPS.Charge, color=Fedex.Service.Description)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
# Points above the line depict the services which are more expensive
#  at UPS than as compared to Fedex.


ggplot(data.plot, aes(Fedex.Service.Description, Charge.Ratio, fill=Fedex.Service.Description)) +
  geom_boxplot() +
  ylim(0, 5) +
  ylab("UPS_Charge / Fedex_Charge")
# Ratios are mainly less than 1, which means that UPS services are cheaper.

