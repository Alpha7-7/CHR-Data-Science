file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"

input.rates <- read.csv(file = paste0(file.path,'UPS published LTR rate.csv'),header = TRUE,sep = ",")

Insert.Rates <- function( tenantid, contractid, year, carrierid, carrier, weight,wtunit, zone,
                          normalized_svc_desc, rate, standard_daily, export_import, container_type, fromweight, toweight,
                          normalid, Minimum, tier, divisor, rateid, startdate, enddate) {
  result <- paste0("Insert into published_rates (tenantid, contractid, year, carrierid, carrier, weight,wtunit, zone
                   normalized_svc_desc, rate, standard_daily, export_import, container_type, fromweight, toweight,
                   normalid, Minimum, tier, divisor, rateid, startdate, enddate)",
                   " Values (",tenantid,contractid, ",",year,",", carrierid, ",","'",carrier,"'",",", weight,",","'",wtunit,"'",",", "'",zone,"'",",",
                   "'",normalized_svc_desc,"'",",", rate,",", standard_daily,",", "'",export_import,"'",",", "'",container_type,"'",",", fromweight,",", toweight,",",
                   normalid,",", Minimum,",", tier,",", divisor,",", rateid,",", "'",startdate,"'",",", "'",enddate,"');")
  return(result)
  
}

output <- list()
i <- 1
j <- 1
for(j in 1:nrow(input.rates)){
  output[i] <-  Insert.Rates(
    tenantid = 0,
    contractid = NULL,
    year = 2023,
    carrierid = 2,
    carrier = "UPS",
    weight = 1,
    wtunit = "LB",
    zone = input.rates$zone[j],
    normalized_svc_desc = input.rates$normalized_svc_desc[j],
    rate = input.rates$rate[j],
    standard_daily = input.rates$standard_daily[j],
    export_import = input.rates$export_import[j],
    container_type = input.rates$container_type[j],
    fromweight = input.rates$fromweight[j],
    toweight = input.rates$toweight[j],
    normalid = input.rates$normalid[j],
    Minimum = input.rates$Minimum[j],
    tier = input.rates$tier[j],
    divisor = input.rates$divisor[j],
    rateid = input.rates$rateid[j],
    startdate = input.rates$startdate[j],
    enddate = input.rates$enddate[j]
    )
  i <- i + 1
}
output

lapply(output,  write, paste0(file.path,"UPS_LTR_1lb_Insert_Statement.txt"), append = TRUE)
