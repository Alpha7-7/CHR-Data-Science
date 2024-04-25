file.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/"
input.data <- read.csv(file = paste0(file.path, "Update_SBD_accounts.csv",''),header = TRUE, sep = ",")
Update.data <- function(tenantid, acc_groupid, carrierid, acc_no, acc_desc, acc_addr, acc_city, acc_state, acc_zipcode) {
  # Pad acc_no with leading zeros to ensure it's nine digits
  #formatted_acc_no <- sprintf("%09s", acc_no)
  
  result <- paste0("UPDATE accounts",
                   " SET acc_groupid = ", acc_groupid,
                   " Where tenantid = ", tenantid, 
                   " AND carrierid = ", carrierid,
                   " AND acc_no = \"", acc_no, "\"",
                   " AND acc_desc = \"", acc_desc, "\"",
                   " AND acc_addr = \"", acc_addr, "\"",
                   " AND acc_city = \"", acc_city, "\"",
                   " AND acc_state = \"", acc_state, "\"",
                   " AND acc_zipcode = \"", acc_zipcode, "\";")
  return(result)
}

output <- list()

for (i in 1:nrow(input.data)) {
  update_statement <- Update.data(
    acc_groupid = input.data$acc_groupid[i],
    tenantid = 399,
    carrierid = input.data$carrierid[i],
    acc_no = input.data$acc_no[i],
    acc_desc = input.data$acc_desc[i],
    acc_addr = input.data$acc_addr[i],
    acc_city = input.data$acc_city[i],
    acc_state = input.data$acc_state[i],
    acc_zipcode = input.data$acc_zipcode[i]
  )
  output[[i]] <- update_statement
}

# Write the SQL update statements to a file
lapply(output, write, paste0(file.path, "Update_SBD_AcctGroupId.sql"), append = TRUE)


