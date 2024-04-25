# Last Updated 2/3/2023

file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.data <- read.csv(file = paste0(file.path, "",'NormalizePeak.csv'),header = TRUE,sep = ",")
Update.data <- function( id, normalid, surcharge_group_id, chargecode, description) {
  result <- paste0("Update charge_codes set normalid = ", normalid,
                   ", surcharge_group_id = ", surcharge_group_id," Where id = ",id," and chargecode = ","'",chargecode,"'"
                   ," and description = ","'",description,"'",";")
  return(result)
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.data)){
  output[i] <-  Update.data(
    normalid = input.data$Proposed_normalid[j],
    surcharge_group_id= input.data$Proposed_surcharge_group_id[j],
    chargecode = input.data$chargecode[j],
    description = input.data$description[j],
    id = input.data$id[j])
  i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"Update_ChargeCodes_Statement.sql"), append = TRUE)
