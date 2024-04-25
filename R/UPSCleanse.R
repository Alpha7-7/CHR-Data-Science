suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(openxlsx))

rezone <- FALSE
path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/Sigma Thermal/"


if(rezone == TRUE){
  zones <- read.csv(file = paste0(path,"zones.csv"),
                    header = TRUE,
                    sep = ",",
                    na.strings = NA,
                    colClasses = rep(1000,x = "character"),
                    as.is = TRUE,
                    check.names = FALSE)
  
}
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("No folder path provided. Please provide a folder path as a command-line argument.", call. = FALSE)
}

path <- args[1]


classes <- c(
  "double","character","character","character", "character", "character", "character", "character","character",
  "character","double","character","character","character", "character","character","character","character",
  "integer","integer","character","character", "character", "character","character","character","double","character",
  "double","character","character","character","character", "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","integer","character",
  "double","character","character","double","double","character", "double","double","character","double","double","double",
  "double","double","character","character","character","character",
  "character","character","character","character","character",  "character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character",
  "character", "character","character","character","character",
  "character","character","character","character",  "character","character", "character","character",
  "character","character","character", "character", "character",
  "character", "character", "character", "character", "character", "character",
  "character", "character", "character", "character", "character", "character", "character", "character",
  "character", "character", "double","character", "character", "character", "double",
  "character", "character", "character", "character", "integer", "character", "double",
  "double", "character", "character", "character",  "character", "character", "character", "character",  "character",  "character",
  "character",  "character",  "character", "character",  "character", "double", "character","character",
  "character","character", "character", "character","double", "double","double", "double", "double", "double","character",  "character", "double","double","double", "character",
  "character","character","character","character","character",  "character","character","character","character",
  "character","double","double","double","double","double", "double","double","character","character","character",
  "character","integer","character","character","double", "character","double","character","character","character",
  "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character",
  "character","character","character","character","character", "character","character","character","character","character"
)

print("Loading Files...")
filenames_list <- list.files(path = path, full.names = TRUE)

load_data <- function() {
  shipment.detail <- tryCatch(
    ldply(filenames_list, read.csv, skip = 0, colClasses = rep(x = "character", 250),
          header = FALSE, sep = ",", stringsAsFactors = FALSE),
    error = function(e) {
      shipment.detail <- ldply(filenames_list, read.csv, skip = 0, colClasses = rep(x = "character", 244),
                               header = FALSE, sep = ",", stringsAsFactors = FALSE)
      shipment.detail$V245 <- ""
      shipment.detail$V246 <- ""
      shipment.detail$V247 <- ""
      shipment.detail$V248 <- ""
      shipment.detail$V249 <- ""
      shipment.detail$V250 <- ""
      return(shipment.detail)
    }
  )
  return(shipment.detail)
}

shipment.detail <- load_data()

find.zone <- function(origin,destination){
  
  origin <- substr(x = origin,start = 1,stop = 3)
  destination <- substr(x = destination,start = 1,stop = 3)
  
  return(zones[origin,destination])
  
}


#Fix Dimention issue

shipment.detail$V33  <- ifelse(test = shipment.detail$V33 == "", shipment.detail$V226,shipment.detail$V33)

#substr(x = shipment.detail$V226,start = 1,4) #Length
#substr(x = shipment.detail$V226,start = 7,10) #Width
#substr(x = shipment.detail$V226,start = 12,16) #Height


#shipment.detail$V34 <- ifelse(test = shipment.detail$V34 == "",
#               yes = paste0("00",
#                            mapply(FUN = find.zone,origin = 640302886,
#                                        destination = shipment.detail$V81)),
#               no = shipment.detail$V34
#              )


#Remove All Commas
shipment.detail <- data.frame(lapply(shipment.detail,function(x) gsub(",","",x)))

# Create Report of SCC Audit fees, Late Payment Fees and Third Party Billing Fees
print("Creating Supplemental Reports...")
print("Creating Supplemental Reports...Late Payment Fee Report")
#LPF
late_payment_fees <- shipment.detail[shipment.detail$V36 == "FEES" & shipment.detail$V46 == 'Late Payment Fee',]

late_payment_fees <- unique(late_payment_fees[, colnames(late_payment_fees)
                                              %in% c("V3","V5","V6","V46","V53"),])

late_payment_fees$V53 <- as.double(late_payment_fees$V53)

colnames(late_payment_fees) <- c('Account Number','Invoice Date','Invoice Number','Description',
                                 'Net Amount')

wb <- createWorkbook()
addWorksheet(wb = wb,sheetName = "LatePaymentFees")
writeDataTable(wb = wb,sheet = "LatePaymentFees",x = late_payment_fees,startCol = 1,startRow = 1,
               colNames = TRUE,rowNames = FALSE)
setColWidths(wb = wb, sheet = "LatePaymentFees", cols = 1:5, widths = 20)

print("Creating Supplemental Reports...Shipping Charge Correction Audit Fees")
#SCC
scc_audit_fees <- shipment.detail[shipment.detail$V175 == "SHIPPING CHARGE CORRECTION AUDIT FEE",]

scc_audit_fees <- unique(scc_audit_fees[, colnames(scc_audit_fees)
                                        %in% c("V3","V5","V6","V46","V53","V175","V176","V177"),])

scc_audit_fees$V53 <- as.double(scc_audit_fees$V53)

colnames(scc_audit_fees) <- c('Account Number','Invoice Date','Invoice Number','Description',
                              'Net Amount','Miscellaneous Line 1','Miscellaneous Line 2',
                              'Miscellaneous Line 3')


addWorksheet(wb = wb,sheetName = "SCCAuditFees")
writeDataTable(wb = wb,sheet = "SCCAuditFees",x = scc_audit_fees,startCol = 1,startRow = 1,
               colNames = TRUE,rowNames = FALSE)
setColWidths(wb = wb, sheet = "SCCAuditFees", cols = 1:8, widths = "auto")

print("Creating Supplemental Reports...Third Party Fees")

#Third Party Fees
third_party_fees <- shipment.detail[shipment.detail$V45 == "FTP",]

third_party_fees <- unique(third_party_fees[, colnames(third_party_fees)
                                            %in% c("V3","V5","V6","V46","V53"),])

third_party_fees$V53 <- as.double(third_party_fees$V53)

colnames(third_party_fees) <- c('Account Number','Invoice Date','Invoice Number','Description',
                                'Net Amount')

addWorksheet(wb = wb,sheetName = "ThirdPartyFees")
writeDataTable(wb = wb,sheet = "ThirdPartyFees",x = third_party_fees,startCol = 1,startRow = 1,
               colNames = TRUE,rowNames = FALSE)
setColWidths(wb = wb, sheet = "ThirdPartyFees", cols = 1:5, widths = 20)


#Save Workbook
saveWorkbook(wb, paste0(path,"/","Report.XLSX"), overwrite = TRUE)

print("Supplemental Report has been created")  

print("Removing Adjustments")  

#Remove adjustments
shipment.detail <- shipment.detail[!shipment.detail$V35 == "ADJ",]

#Output File 
# Set chunk size and number of chunks
chunk_size <- 20000
total_rows <- nrow(shipment.detail)
num_chunks <- ceiling(total_rows / chunk_size)
print("Creating Cleansed Files")  
# Loop through chunks and save them as separate files
for (i in 1:num_chunks) {
  start_index <- ((i - 1) * chunk_size) + 1
  end_index <- min(i * chunk_size, total_rows)
  
  # Create chunk
  chunk <- shipment.detail[start_index:end_index, ]
  
  # Save chunk to file
  write.table(x = chunk,
              file = paste0(path, "/chunk", i, ".csv"),
              row.names = FALSE,
              na = "",
              col.names = FALSE,
              sep = ",",
              quote = FALSE)
}

print("Success!") 

Sys.sleep(.5)

