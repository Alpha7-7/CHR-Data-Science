
library(lubridate)

library(plyr)

library(dplyr)

library(stringr)

path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Desktop/Hutchinson/"

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

filenames_list <- list.files(path = path, full.names = TRUE)

shipment.detial <- ldply(filenames_list, read.csv,skip = 0,colClasses = rep(x = "character",250),
                         
                         header = FALSE,sep = ",",
                         
                         stringsAsFactors = FALSE)

#Remove All Commas
shipment.detial <- data.frame(lapply(shipment.detial,function(x) gsub(",","",x)))

shipment.detial$V245 <- ''
shipment.detial$V246 <- ''
shipment.detial$V247 <- ''
shipment.detial$V248 <- ''
shipment.detial$V249 <- ''
shipment.detial$V250 <- ''
nrow(shipment.detial)

#Remove adjustments
shipment.detial <- shipment.detial[!shipment.detial$V35 == "ADJ",]

# Chunk Data
chunk1 <- shipment.detial[1:nrow(shipment.detial),]

chunk2 <- shipment.detial[50001:100000,]

chunk3 <- shipment.detial[100001:124890,]

chunk4 <- shipment.detial[150001:200000,]

chunk5 <- shipment.detial[200001:250000,]

chunk6 <- shipment.detial[250001:300000,]

chunk7 <- shipment.detial[300001:350000,]

chunk8 <- shipment.detial[350001:400000,]

chunk9 <- shipment.detial[400001:450000,]

chunk10 <- shipment.detial[450001:500000,]

chunk9 <- shipment.detial[500001:550000,]

chunk10 <- shipment.detial[550001:600000,]

chunk9 <- shipment.detial[600001:650000,]

chunk10 <- shipment.detial[650001:700000,]

chunk9 <- shipment.detial[700001:750000,]

chunk10 <- shipment.detial[750001:800000,]

chunk9 <- shipment.detial[800001:850000,]

chunk10 <- shipment.detial[850001:907927,]

write.table(x = chunk1,file = paste0(path,"chunk1.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk2,file = paste0(path,"chunk2.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk3,file = paste0(path,"chunk3.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk4,file = paste0(path,"chunk4.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk5,file = paste0(path,"chunk5.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk6,file = paste0(path,"chunk6.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk7,file = paste0(path,"chunk7.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk8,file = paste0(path,"chunk8.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk9,file = paste0(path,"chunk9.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)

write.table(x = chunk10,file = paste0(path,"chunk10.csv"),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)