# Last Updated 11/28/2022

### Libraries

file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/Fedex 2023 Rates/"

input.rates <- read.csv(file = paste0(file.path,'2023Surcharges.csv'),header = TRUE,sep = ",")

Update.Rates <- function( id, tenantid, description,carrierid,service_group,
                         zonecd,year,valid_from,valid_to, surcharge_group, rate,
                         percent_rate, minimum, fromvalue, tovalue, maximum) {
  result <- paste0("Update surcharges set id = ",id,
                   " Where tenantid = ",tenantid,
                   " and description = '",description, "'", 
                   " and carrierid = ",carrierid,
                   " and service_group = '",service_group, "'",
                   " and zonecd = '",zonecd, "'", 
                   " and yaer = '",year, "'",
                   " and valid_from = '", valid_from, "'",
                   " and valid_to = '", valid_to, "'",
                   " and rate = '", rate,"'",
                   " and percent_rate = '", percent_rate,"'",
                   " and minimum = '", minimum,"'",
                   " and fromvalue = '", fromvalue,"'",
                   " and tovalue = '", tovalue,"'",
                   " and maximum = '", maximum,"';")
  return(result)
  
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.rates)){
    output[i] <-  Update.Rates(
                               id = input.rates$id[j],
                               tenantid = 0,
                               description = input.rates$description[j],
                               carrierid = 1,
                               service_group = input.rates$service_group[j],
                               zonecd = input.rates$zone[j],
                               year = 2022,
                               valid_from = input.rates$valid_from[j],
                               valid_to = input.rates$valid_to[j],
                               rate = input.rates$rate[j],
                               percent_rate = input.rates$percent_rate[j],
                               minimum = input.rates$minimum[j],
                               fromvalue = input.rates$fromvalue[j],
                               tovalue = input.rates$tovalue[j],
                               maximum = input.rates$maximum[j])
    i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"2023Surcharges.txt"), append = TRUE)



