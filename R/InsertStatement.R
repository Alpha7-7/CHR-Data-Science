file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.rates <- read.csv(file = paste0(file.path,'InsertUPSFedEXSurcharges2024.csv'),header = TRUE,sep = ",")

Insert.Rates <- function(tenantid, description, carrierid, service_group, zonecd, year, valid_from, valid_to, surcharge_group, rate, percent_rate,
                         minimum, fromvalue, tovalue, maximum) {
  result <- paste("INSERT INTO surcharges (tenantid, description, carrierid, service_group, zonecd, year, valid_from, valid_to, surcharge_group, rate, percent_rate, minimum, fromvalue, tovalue, maximum)",
                  "VALUES (", tenantid, ",", "'", description, "'", ",", carrierid, ",", service_group, ",", zonecd, ",", year, ",",
                  "'", valid_from, "'", ",", "'", valid_to, "'", ",", surcharge_group, ",", rate, ",", percent_rate, ",", minimum, ",",
                  fromvalue, ",", tovalue, ",", maximum, ");", sep = " ")
  return(result)
}


output <- list()
i <- 1

for (j in 1:nrow(input.rates)) {
  output[[i]] <- Insert.Rates(
    tenantid = input.rates$tenantid[j],
    description = input.rates$description[j],  # Updated to match your data
    carrierid =  input.rates$carrierid[j],
    service_group = input.rates$service_group[j],  # You need to define this based on your data
    zonecd = input.rates$zone[j],
    year = input.rates$year[j],
    valid_from = input.rates$valid_from[j],  # Updated to match your data
    valid_to = input.rates$valid_to[j],      # Updated to match your data
    surcharge_group = input.rates$surcharge_group[j],                   # You need to define this based on your data
    rate = input.rates$rate[j],
    percent_rate = input.rates$percent_rate[j],
    minimum = input.rates$minimum[j],
    fromvalue = input.rates$fromvalue[j],                          # You need to define this based on your data
    tovalue = input.rates$tovalue[j],                            # You need to define this based on your data
    maximum = input.rates$maximum[j]
  )
  i <- i + 1
}

output

lapply(output,  write, paste0(file.path,"InsertUPSFedEXSurcharges2024.sql"), append = TRUE)
