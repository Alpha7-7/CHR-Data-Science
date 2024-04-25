# Last Updated 2/3/2023

file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.data <- read.csv(file = paste0(file.path, "",'ShipmentEvents_ToNormalize(updated1-23-2023).csv'),header = TRUE,sep = ",")
Update.data <- function( id, normal_statuscd,normal_delexid, normal_delex_catid, 
                         non_controllable) {
  result <- paste0("Update shipment_events set normal_statuscd = ", normal_statuscd,
                   ", normal_delexid = ", normal_delexid, ", normal_delex_catid = ", normal_delex_catid, ", non_controllable = ", non_controllable,
                   " Where id = ",id,";")
  return(result)
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.data)){
  output[i] <-  Update.data(
    normal_statuscd = input.data$normal_statuscd[j],
    normal_delexid= input.data$normal_delexid[j],
    normal_delex_catid = input.data$normal_delex_catid[j],
    non_controllable = input.data$non_controllable[j],
    id = input.data$id[j])
  i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"ShipmentEvents.txt"), append = TRUE)



