file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.data <- read.csv(file = paste0(file.path, "",'NormalizePeak.csv'),header = TRUE,sep = ",")
Update.data <- function( tenantid, year, carrierid, carrier, wtunit, normalized_svc_desc, export_import, container_type, zone, Minimum) {
  result <- paste0("Update published_rates set Minimum = ", Minimum,
                   " Where tenantid = ",tenantid," and year = ",year," and carrierid = ",carrierid," and carrier = ","'",carrier,"'", " and wtunit = ","'",wtunit,"'",
                   " and normalized_svc_desc = ","'",normalized_svc_desc,"'", " and export_import = ","'", export_import,"'", 
                   " and container_type = ","'", container_type,"'", 
                   " and zone = ", "'",zone,"'",";")
  return(result)
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.data)){
  output[i] <-  Update.data(
    Minimum = input.data$Minimum[j],
    tenantid = input.data$tenantid[j],
    year = input.data$year[j],
    carrierid = input.data$carrierid[j],
    carrier = input.data$carrier[j],
    wtunit = input.data$wtunit[j],
    normalized_svc_desc = input.data$normalized_svc_desc[j],
    export_import= input.data$export_import[j],
    container_type = input.data$container_type[j],
    zone = input.data$zone[j])
  i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"Update Internatioanl Minimum.txt"), append = TRUE)
