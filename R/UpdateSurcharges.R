
file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.data <- read.csv(file = paste0(file.path, "",'UpdateUPSFedEXSurchargesValidates.csv'),header = TRUE,sep = ",")
Update.data <- function(tenantid,normalid , description,carrierid,service_group,zonecd,year,valid_from,valid_to,surcharge_group,rate) {
  result <- paste0("Update surcharges set valid_to = ", "'",valid_to,"'"," Where tenantid = ",tenantid, " and description = ","'",description,"'",
                   " and carrierid = ",carrierid," and service_group = ",service_group," and zonecd = ",zonecd," and year = ",year,
                   " and valid_from = ","'",valid_from,"'"," and surcharge_group = ",surcharge_group," and rate = ",rate,";")
  return(result)
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.data)){
  output[i] <-  Update.data(
    tenantid = input.data$tenantid[j],
    description = input.data$description[j],
    carrierid = input.data$carrierid[j],
    service_group = input.data$service_group[j],
    zonecd = input.data$zonecd[j],
    year = input.data$year[j],
    valid_from = input.data$valid_from[j],
    valid_to = input.data$valid_to[j],
    surcharge_group = input.data$surcharge_group[j],
    rate = input.data$rate[j] )
  i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"UpdateUPSFedEXSurchargesValidates.sql"), append = TRUE)
