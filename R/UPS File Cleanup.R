path <- "C:/Users/CHENAL/OneDrive - C.H. Robinson/Trial Customers/Automan/"

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


filename <- "UPSTX.csv"
shipment.detial <- read.csv(file = paste0(path,filename),skip = 0,colClasses = rep(x = "character",250),
                             header = FALSE,sep = ",",
                            stringsAsFactors = FALSE)


shipment.detial$V16 <- gsub(",","",shipment.detial$V16)
shipment.detial$V17 <- gsub(",","",shipment.detial$V17)
shipment.detial$V18 <- gsub(",","",shipment.detial$V18)
shipment.detial$V19 <- gsub(",","",shipment.detial$V19)
shipment.detial$V20 <- gsub(",","",shipment.detial$V20)
shipment.detial$V21 <- gsub(",","",shipment.detial$V21)
shipment.detial$V22 <- gsub(",","",shipment.detial$V22)
shipment.detial$V23 <- gsub(",","",shipment.detial$V23)
shipment.detial$V24 <- gsub(",","",shipment.detial$V24)
shipment.detial$V25 <- gsub(",","",shipment.detial$V25)
shipment.detial$V26 <- gsub(",","",shipment.detial$V26)
shipment.detial$V67 <- gsub(",","",shipment.detial$V67)
shipment.detial$V68 <- gsub(",","",shipment.detial$V68)
shipment.detial$V69 <- gsub(",","",shipment.detial$V69)
shipment.detial$V70 <- gsub(",","",shipment.detial$V70)
shipment.detial$V71 <- gsub(",","",shipment.detial$V71)
shipment.detial$V75 <- gsub(",","",shipment.detial$V75)
shipment.detial$V76 <- gsub(",","",shipment.detial$V76)
shipment.detial$V77 <- gsub(",","",shipment.detial$V77)
shipment.detial$V78 <- gsub(",","",shipment.detial$V78)
shipment.detial$V79 <- gsub(",","",shipment.detial$V79)
shipment.detial$V84 <- gsub(",","",shipment.detial$V84)
shipment.detial$V79 <- gsub(",","",shipment.detial$V79)
shipment.detial$V91 <- gsub(",","",shipment.detial$V91)
shipment.detial$V92 <- gsub(",","",shipment.detial$V92)
shipment.detial$V93 <- gsub(",","",shipment.detial$V93)
shipment.detial$V94 <- gsub(",","",shipment.detial$V94)
shipment.detial$V95 <- gsub(",","",shipment.detial$V95)
shipment.detial$V131 <- gsub(",","",shipment.detial$V131)
shipment.detial$V132 <- gsub(",","",shipment.detial$V132)
shipment.detial$V133 <- gsub(",","",shipment.detial$V133)
shipment.detial$V134 <- gsub(",","",shipment.detial$V134)
shipment.detial$V140 <- gsub(",","",shipment.detial$V140)
shipment.detial$V175 <- gsub(",","",shipment.detial$V175)
shipment.detial$V177 <- gsub(",","",shipment.detial$V177)


write.table(x = shipment.detial,file = paste0(path,"cleaned",filename),row.names = FALSE
            ,na = "",col.names = FALSE,sep = ",",quote = FALSE)


chunk1 <- shipment.detial[1:50000,]
chunk2 <- shipment.detial[50001:100000,]

chunk3 <- shipment.detial[100001:150000,]
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



