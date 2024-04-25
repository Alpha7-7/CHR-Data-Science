library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(openxlsx)

############ Load Files ##############
data.path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/" # Data Path 
# PI Output 
filenames_list <- list.files(path = paste0(data.path,"PIOutput"), full.names = TRUE)
Shortened_filenames_list <- list.files(path = paste0(data.path,"PIOutput"), full.names = FALSE)
customer_Name_Position <- c()
number_records <- as.integer()

for(i in 1:length(Shortened_filenames_list)){
  customer_Name_Position[i] <- gregexpr(pattern = "_",text = Shortened_filenames_list)[[i]][1]
  number_records[i] <- as.integer(nrow(read.xlsx(xlsxFile = filenames_list[i])))
}
customer_names <- substr(x = Shortened_filenames_list,start = 1,stop =customer_Name_Position -1)
records <- data.frame(cbind(customer_names,as.integer(number_records)))

records$V2 <- as.integer(records$V2)

PI_Output <- ldply(filenames_list, read_xlsx,na = "NA") 


customer_name <- c()

k <- 1
for(i in 1:nrow(records)){
  for(j in 1:records[i,2]){
    customer_name[k] <- records[i,1]
    k <- k + 1
  }
}

PI_Output$Customer.Name <- customer_name

unique(PI_Output$Customer.Name)
customer.tiers <- rbind(
      c("Babcock Power","GP"),
      c("Bearcat", "Tier 2"),
      c("Beta Fueling Systems", "GP"),
      c("BMC", "GP"),
      c("ED Etnyre Co", "Tier 2"),
      c("Handgards", "GP"),
      c("Lavazza", "Tier 2"),
      c("Nicholson Mfg", "GP"),
      c("Northern Star Industries Inc", "Tier 2"),
      c("Reyco Grannning", "Tier 2"))


PI_Output$Customer.tier <-  customer.tiers[match(x = PI_Output$Customer.Name ,table = customer.tiers[,1],
                      nomatch = "NA"),2]

# Actual Output
filenames_list <- list.files(path = paste0(data.path,"File Customer Daily Reports/2023"), full.names = TRUE)
Actual_today <- row.names(file.info(filenames_list)%>% filter(grepl(Sys.Date(), mtime))) # Today's 
#file_info <- lapply(filenames_list,file.info)
#mod <- sapply(file_info, function(x) as.Date(x$mtime, origin = "2023-02-10"))
#Actual_today <-  filenames_list[as.numeric(Sys.Date()-mod, units="days")<=5]# Week's 
#Actual_today <-  "C:/Users/CHENAL/OneDrive - C.H. Robinson/File Customer Daily Reports/2023/Bearcat_daily_rating_report.xlsx" # Specific customer 
#Actual_today <- grep("C:/Users/CHENAL/OneDrive - C.H. Robinson/File Customer Daily Reports/2023/Northern Star Industries Daily Estimated Rates - [0-9]+-[0-9]+-[0-9]+\\.xlsx", 
#                     row.names(file.info(filenames_list)), value=TRUE) # Specific customer all entries 
Actual_Output <- ldply(Actual_today, read_xlsx,na = "NA")


columns <- c("Customer.Name" ,"Customer.tier" ,"Ship date","Tracking ID","Service type","Shipper postal","Recipient postal",
             "Number of Pieces","Pkg Wt","Billable Wt","Rated Zone",
             "Length","Width","Height","Full Tariff Rate","Customer Rate")
PI_Output <- PI_Output[,columns]

colnames(PI_Output)
names(PI_Output) <- c("Customer.Name" ,"Customer.tier","Ship date","Tracking_ID",
                      "PI_Service type","PI_Shipper postal","PI_Recipient postal",
                      "PI_Pieces","PI_Pkg Wt","PI_Billable Wt","PI_Zone",
                      "PI_Length","PI_Width","PI_Height","PI_Full Tariff",
                      "PI_Customer Rate")
columns <- c("Ship date","Tracking ID","Service type","Shipper postal","Recipient postal",
             "Number of Pieces","Pkg Wt","Billable Wt","Rated Zone",
             "L","W","H","Full Tariff Rate","Customer Rate")
Actual_Output <- Actual_Output[,columns]
names(Actual_Output) <- c("Ship date","Tracking_ID","A_Service type","A_Shipper postal","A_Recipient postal",
                          "A_Number of Pieces","A_Pkg Wt","A_Billable Wt","A_Rated Zone",
                          "A_Length","A_Width","A_Height","A_Full Tariff","A_Customer Rate")

complete.records <- full_join(PI_Output,Actual_Output, by ="Tracking_ID")
complete.records$rate.difference <- complete.records$`A_Customer Rate` - complete.records$`PI_Customer Rate`
complete.records$accuracy <- abs(complete.records$rate.difference)<0.3 ## mark differences that are smaller than 30 cents 
#complete.records <- complete.records[!is.na(complete.records$rate.difference),]
# PI Missing Data 
PI_Missing <- complete.records[is.na(complete.records$'PI_Customer Rate'),]
# Actual Missing Data 
Actual_Missing <- complete.records[is.na(complete.records$'A_Customer Rate'),]

############ Export CSV   ##############
mylist <- list("Comparison"= complete.records, 'PI_Missing'=PI_Missing, 'Actual_Missing'=Actual_Missing)
names(mylist)
wb <- createWorkbook()
lapply(seq_along(mylist), function(i){
  addWorksheet(wb=wb, sheetName = names(mylist[i]))
  writeData(wb, sheet = i, mylist[[i]])
})
saveWorkbook(wb, "DailyRaterComparison_03_01_2023.xlsx", overwrite = TRUE)
 
############ Visualization ##############
# Histogram 
hist(complete.records$rate.difference ,main="Histogram of Rate Differences", xlab = "Rate Differences in $",
     ylab = "Counts", col = "Black",
     col.axis = "blue",
     col.lab = "blue")

#Divide the screen in 2 columns and 2 lines
data = data.frame("x"=complete.records$Tracking_ID, "y"=complete.records$rate.difference, "z"=complete.records$accuracy)
data
#Add a plot in each sub-screen !
par(mfrow=c(2,2))
plot(data$y , pch=20)
hist(data$y, breaks=30 , border=F , col=rgb(0.1,0.8,0.3,0.5) , xlab="Distribution of Rate Differences" , main="")
boxplot(data$y , col="grey" , xlab="a")
dotchart(data$y)
############ Analysis   ##############
                      
summary(complete.records)


