path = "U:/Data/Property taxes/UK"
setwd(path)
require(plyr)

#read in price paid data
#https://www.gov.uk/government/statistical-data-sets/price-paid-data-downloads
#note 2015 pull in live link -- thru to May 2015
pp_2015 <- read.csv("http://publicdata.landregistry.gov.uk/market-trend-data/price-paid-data/b/pp-2015.csv", header = FALSE)
pp_2014 <- read.csv("Land reg/pp-2014.csv", header = FALSE)
pp_2013 <- read.csv("Land reg/pp-2013.csv", header = FALSE)
pp_2012 <- read.csv("Land reg/pp-2012.csv", header = FALSE) #-- new stamp duty threshold for upper band introduced in Mar 2012
pp_data <- rbind.fill(pp_2012, pp_2013, pp_2014, pp_2015)
rm(pp_2015, pp_2014, pp_2013, pp_2012)

#asign col names
colnames <- read.csv("Land reg/Landreg_col_names.csv", header = TRUE)
colnames(pp_data) <- colnames[,1]

####read in London postcodes###
#this data originally from:
#E:\N drive\Back in London\Data correspondent\Property\UK house prices\Britain property analysis

#pull in names
PC_region <- read.csv("Postcodes/PC_region.csv", header = TRUE)
PC_region$PC_dist <- as.character(PC_region$PC_dist)

#loop round to pull in postcode files
PC = NULL
for (i in 1:nrow(PC_region)){
  PC1 <- read.csv(paste("Postcodes/", PC_region[i,1], ".csv", sep=""), header=FALSE)
  PC <- rbind.fill(PC, PC1) 
  }
rm(PC1)

colnames <- read.csv("Postcodes/Col_names.csv", header = FALSE)
colnames(PC) <- colnames[,2]

#remove London borough only postcodes
London_codes <- read.csv("Postcodes/London code list.csv", header = TRUE)
#London_codes[,1] <- gsub(" London Boro", "", London_codes[,1])
LDN_PC <- merge(PC, London_codes, by.x = "Admin_district_code", by.y = "Code")
LDN_PC <- LDN_PC[,c(1,2,10,11)]
colnames(LDN_PC)[4] <- "Borough"
#summary(LDN_PC$Borough)

#remove these
rm(PC, PC_region, colnames)

###cleaning postcodes###
#reg_ex <- "([A-PR-UWYZ0-9][A-HK-Y0-9][AEHMNPRTVXY0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}|GIR 0AA)$"

#trim white edges
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#replace double space with single
double <- function (x) gsub("  ", " ", x)

#for PC with no spaces, insert a space
space <- function (x){
  if (grepl(" ", x) == FALSE){
  paste(substr(x, 1, nchar(x)-3), " ", substr(x, nchar(x)-2, nchar(x)), sep="")
  } else {
  x <- x 
  }}

#run thru the functions
LDN_PC$PC2 <- as.character(LDN_PC$Postcode)
LDN_PC$PC2 <- sapply(LDN_PC$PC2, trim)
LDN_PC$PC2 <- sapply(LDN_PC$PC2, double)
LDN_PC$PC2 <- sapply(LDN_PC$PC2, space)
write.csv(LDN_PC, "R analysis/LDN_PC.csv")

#create postcode sector data set for each Borough
#LDN_PC <- read.csv("R analysis/LDN_PC.csv", header = TRUE)
#head(LDN_PC)
#LDN_PC$PC2 <- as.character(LDN_PC$PC2)
PC_sector <- function (x) { substr(x, 1, nchar(x)-2) }
LDN_PC$PCS <- sapply(LDN_PC$PC2, PC_sector)
LDN_PCS <- data.frame(LDN_PC[!duplicated(LDN_PC[,c("PCS", "Borough")]),], row.names = NULL)
#LDN_PCS <- LDN_PCS[, c(1:2, 5,7)] #error
write.csv(LDN_PCS, "R analysis/London_postcode_sectors.csv")
rm(LDN_PCS)    

#merge postcodes with PP dataset for London only
pp_data_LDN <- merge(pp_data, LDN_PC, by.x = "Postcode", by.y = "PC2" )
head(pp_data_LDN, 3)

#remove these
rm(LDN_PC, London_codes, pp_data)
rm(i, double, space, trim)

#create price buckets
fivenum(pp_data_LDN$Price)
pp_data_LDN$Price.bins <- cut(pp_data_LDN$Price, breaks=c(0, 100000, 250000, 500000, 750000, 1000000, 1500000, 2000000, 3000000, 5000000, 10000000, 100000000),
                                  labels=c("0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+"))
summary(pp_data_LDN$Price.bins)

#city <- subset(pp_data_LDN, Borough == "City of London")

###calculate stampduty paid###

#new stamp duty thresholds
#125,000 = 0
#125,001 to 250,000 = 2%
#250,0001 to 925,000 = 5%
#925,001 to 1,500,000 = 10%
#1.5m + = 12%

#new stamp duty: contract exchange after Dec 5th 2014
new_stampduty  <- function (x) {
  if (x <= 125000) {
  x <- 0 
  } else if (x > 125000 & x <= 250000) {
  (x - 125000) * 0.02 
  } else if (x > 250000 & x <= 925000) {
  125000 * 0.02 + (x - 250000) * 0.05 
  } else if (x > 925000 & x <= 1500000) {
  125000 * 0.02 + (925000 - 250000) * 0.05 + (x - 925000) * 0.1
  } else {
  125000 * 0.02 + (925000 - 250000) * 0.05 + (1500000 - 925000) * 0.1 + (x - 1500000) * .12
}}

#old stamp duty thresholds - contract exchange 21/3/2012 to 5/12/2014
#http://www.stampdutyrates.co.uk/historic-rates.html
old_stampduty  <- function (x) {
  if (x <= 125000) {
    x <- 0 
  } else if (x > 125000 & x <= 250000) {
    x  * 0.01 
  } else if (x > 250000 & x <= 500000) {
    x  * 0.03 
  } else if (x > 500000 & x <= 1000000) {
    x  * 0.04 
  } else if (x > 1000000 & x <= 2000000) {
    x  * 0.05
  } else {
    x * 0.07
  }}

#testing
#x <- 2100000
#x1 <- old_stampduty(x)
#x2 <- new_stampduty(x)
#x1
#x2

####add in stamp duty paid###
#convert date from factor to date
pp_data_LDN[,4] <- as.Date(pp_data_LDN[,4])

#<£40,000 do not need to be notified (ie, no tax paid)

#let's look at sales volume by month
pp_data_LDN$date2 <- paste("1/", format(pp_data_LDN[, 4], "%m"), "/", format(pp_data_LDN[, 4], "%Y"), sep="")
salesvol <- data.frame(table(pp_data_LDN$date2))
colnames(salesvol) <- c("date", "vol")
salesvol$date <- as.Date(salesvol$date, "%d/%m/%Y")
salesvol <- salesvol[order(salesvol$date), ]

#looking at sales vol by price band:
salesvol2 <- data.frame(count(pp_data_LDN, c("date2", "Price.bins")))
salesvol2 <- reshape(salesvol2, idvar = "date2", timevar = "Price.bins",  direction = "wide")
colnames(salesvol2) <- c("date", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-5m", "5m-10m", "10m+")
salesvol2$date <- as.Date(salesvol2$date, "%d/%m/%Y")
salesvol2 <- salesvol2[order(salesvol2$date), ]
salesvol <- merge(salesvol, salesvol2, by = "date")
rm(salesvol2)
write.csv(salesvol, "sales_by_price.csv")

#we only care about this
#stampy duty assuming new threshold for all
pp_data_LDN$stamp_duty <- sapply(pp_data_LDN$Price, new_stampduty)

#calculate actual receipts, avoiding tedious looping
pp_data_LDN2 <- subset(pp_data_LDN, pp_data_LDN[4] <= "2014/12/15")
pp_data_LDN2$stamp_duty2 <- sapply(pp_data_LDN2$Price, old_stampduty)

pp_data_LDN3 <- subset(pp_data_LDN, pp_data_LDN[4] > "2014/12/15")
pp_data_LDN3$stamp_duty2 <- sapply(pp_data_LDN3$Price, new_stampduty)
pp_data_LDN2 <- rbind.fill(pp_data_LDN2, pp_data_LDN3)

pp_data_LDN <- pp_data_LDN2
rm(pp_data_LDN2, pp_data_LDN3)

#but otherwise, calculate actual receipts
# #pp_data_LDN2 <- pp_data_LDN[1:15000,]
# loop round to calculate stampy duty #time how long this takes
# start.time <- Sys.time()
# for (i in 1:nrow(pp_data_LDN2)) {
#   if (pp_data_LDN2[i,4] <= "2014/12/15") {
#   pp_data_LDN2[i, "stamp_duty2"] <- old_stampduty(pp_data_LDN2[i, "Price"])
#   } else {
#   pp_data_LDN2[i, "stamp_duty2"] <- new_stampduty(pp_data_LDN2[i, "Price"])
# }}
# end.time <- Sys.time()
# fun.time <- end.time - start.time
# fun.time  
# #takes 20.5mins to run

#summary
summary(pp_data_LDN$stamp_duty2)

#tax rate of stampduty
pp_data_LDN$stamp_tax <- pp_data_LDN$stamp_duty / pp_data_LDN$Price * 100
pp_data_LDN$stamp_tax2 <- pp_data_LDN$stamp_duty2 / pp_data_LDN$Price * 100
#check <- subset(pp_data_LDN, stamp_tax > 11)

#sum of stampy duty
sum(pp_data_LDN$stamp_duty) / 10^6 #tallies with national stats = 4.7bn btw FY2012-2014
sum(pp_data_LDN$stamp_duty2) / 10^6 #tallies with national stats = 4.7bn btw FY2012-2014
summary(pp_data_LDN$stamp_duty2)

#by month
salesvol <- data.frame(table(pp_data_LDN$date2))
sales_val <- aggregate(pp_data_LDN$Price, by = list(pp_data_LDN$date2), FUN = function(x){sum(as.numeric(x), na.rm = TRUE)} )
stamp_receipt <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$date2), FUN = sum, na.rm = TRUE)
stamp_receipt2 <- aggregate(pp_data_LDN$stamp_duty2, by = list(pp_data_LDN$date2), FUN = sum, na.rm = TRUE)
receipts <- merge(salesvol, sales_val, by.x = "Var1", by.y = "Group.1")
receipts <- merge(receipts, stamp_receipt, by.x = "Var1", by.y = "Group.1")
receipts <- merge(receipts, stamp_receipt2, by.x = "Var1", by.y = "Group.1")
colnames(receipts) <- c("date", "vol", "val", "stamp-hypo", "stamp2-act")
receipts$date <- as.Date(receipts$date, "%d/%m/%Y")
receipts <- receipts[order(receipts$date), ]
write.csv(receipts, "stamp_recipts_month.csv")
rm(salesvol, sales_val, stamp_receipt, stamp_receipt2)

####################################################
#reduce the table down to two years worth of records
data_save <- pp_data_LDN 
pp_data_LDN <- subset(data_save, data_save[4] > "2013/01/01")

###aggregate sales by price buckets and by borough###
sales <- data.frame(count(pp_data_LDN, c("Borough", "Price.bins")))
sales <- reshape(sales, idvar = "Borough", timevar = "Price.bins",  direction = "wide")
colnames(sales)[2:12] <- c("0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")
sales$count <- rowSums(sales[2:12], na.rm=TRUE, dims = 1)

###aggregate stamp duty receipts by price buckets and by borough###
stamp_receipt <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$Borough, pp_data_LDN$Price.bins), FUN = sum, na.rm = TRUE)
stamp_receipt <- reshape(stamp_receipt, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(stamp_receipt) <- c("Borough", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

###average stamp duty rate by price buckets and by borough###
stamp_tax <- aggregate(pp_data_LDN$stamp_tax, by = list(pp_data_LDN$Borough, pp_data_LDN$Price.bins), FUN = mean, na.rm = TRUE)
stamp_tax <- reshape(stamp_tax, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(stamp_tax) <- c("Borough", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

#############################################
###reduce down further to 12 months of data
data_save_v2 <- pp_data_LDN 
pp_data_LDN <- subset(data_save_v2, data_save_v2[4] > "2014/03/30" & data_save_v2[4] < "2015/04/01")
#pp_data_LDN <- data_save_v2

###average stamp duty rate by price buckets for London###
LDN_stamp1 <- data.frame(count(pp_data_LDN, "Price.bins"))
LDN_stamp1$rows <- seq(1, 11, 1)
LDN_stamp2 <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$Price.bins), FUN = sum, na.rm = TRUE)
LDN_stamp3 <- aggregate(pp_data_LDN$stamp_tax, by = list(pp_data_LDN$Price.bins), FUN = mean, na.rm = TRUE)
LDN_stamp4 <- aggregate(pp_data_LDN$stamp_duty2, by = list(pp_data_LDN$Price.bins), FUN = sum, na.rm = TRUE)

LDN_stamp <- merge(LDN_stamp1, LDN_stamp2, by.x = "Price.bins", by.y = "Group.1")
LDN_stamp <- merge(LDN_stamp, LDN_stamp3, by.x = "Price.bins", by.y = "Group.1")
LDN_stamp <- merge(LDN_stamp, LDN_stamp4, by.x = "Price.bins", by.y = "Group.1")

LDN_stamp <- LDN_stamp[order(LDN_stamp$rows),]
LDN_stamp <- LDN_stamp[,c(1:2,4:6)]
colnames(LDN_stamp) <- c("Price.bin", "Sales_count", "Stamp_paid_hypo", "stamp_rate", "Stamp_rate_act")
rm(LDN_stamp1, LDN_stamp2, LDN_stamp3, LDN_stamp4)

#ploting this data
barplot(LDN_stamp$Sales, main = "London sales count", horiz = FALSE, 
        names.arg = LDN_stamp$Price.bin, col = "blue")
barplot(LDN_stamp$Stamp_paid/10^6, main = "London duty paid £m", horiz = FALSE, 
        names.arg = LDN_stamp$Price.bin, col = "blue")
barplot(LDN_stamp$Stamp_rate, main = "London tax rate, %", horiz = FALSE, 
        names.arg = LDN_stamp$Price.bin, col = "blue")

#import the council tax rates for each Borough
bands1 <- read.csv("Ctax_band_thresholds.csv", header = TRUE) #new thresholds assuming HP inflation
bands2 <- read.csv("Ctax_costs.csv", header = TRUE) #council tax charge per band
bands3 <- read.csv("Ctax_dwell.csv", header = TRUE) #number of dwellings
bands4 <- read.csv("Ctax_dwell2.csv", header = TRUE) #number of chargable dwellings

#check that no borough has a band H that is geater than 2m threshold
#doesn't matter
bands1$G > 2000

###calculate council tax paid by each property in London###
#work out what Band each property sits in -- takes 17mins
start.time <- Sys.time()
match <- match(pp_data_LDN$Admin_district_code, bands1$Code)
match2 <- pp_data_LDN[,"Price"]/1000 > bands1[match, 3:9]
for (i in 1:nrow(pp_data_LDN)) {
  pp_data_LDN[i, "cBand"] <- colnames(bands1[3:10])[min(which(match2[i, ] == FALSE))] 
  if (is.na(pp_data_LDN[i, "cBand"])) {
    pp_data_LDN[i, "cBand"] <- "H" }
}
end.time <- Sys.time()
fun.time <- end.time - start.time
fun.time  

#work out the tax it pays each year -- takes 6mins
start.time <- Sys.time()
match <- match(pp_data_LDN$Admin_district_code, bands2$Code)
for (i in 1:nrow(pp_data_LDN)) {
  pp_data_LDN[i, "cBill"] <- bands2[match[i], match(pp_data_LDN[i, "cBand"], colnames(bands2[3:10]))+2]
}  
end.time <- Sys.time()
fun.time <- end.time - start.time
fun.time  
rm(match, match2)

#work out the council tax rate
pp_data_LDN$cRate <- pp_data_LDN$cBill / pp_data_LDN$Price * 100

###aggregate bands, total payment and rate by borough and property price
#count of bands by Band
cBands <- data.frame(count(pp_data_LDN, c("Admin_district_code", "Borough", "cBand")))
cBands <- reshape(cBands, idvar = c("Admin_district_code", "Borough"), timevar = "cBand",  direction = "wide")
colnames(cBands)[3:10] <- c("A", "B", "C", "D", "E", "F", "G", "H")
cBands$count <- rowSums(cBands[3:10], na.rm=TRUE, dims = 1)
write.csv(cBands, "LDN_prop_by_ctax_bands.csv", row.names = FALSE)

#sum of council tax revenue by cBand
cRev1 <- aggregate(pp_data_LDN$cBill, by = list(pp_data_LDN$Borough, pp_data_LDN$cBand), FUN = sum, na.rm = TRUE)
cRev1 <- reshape(cRev1, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(cRev1) <- c("Borough", "A", "B", "C", "D", "E", "F", "G", "H")

#sum of council tax revenue by price.bin
cRev <- aggregate(pp_data_LDN$cBill, by = list(pp_data_LDN$Borough, pp_data_LDN$Price.bins), FUN = sum, na.rm = TRUE)
cRev <- reshape(cRev, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
#colnames(cRev) <- c("Borough", "A", "B", "C", "D", "E", "F", "G", "H")
colnames(cRev) <- c("Borough", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

#average council tax rate by price.bin
cRate <- aggregate(pp_data_LDN$cRate, by = list(pp_data_LDN$Borough, pp_data_LDN$Price.bins), FUN = mean, na.rm = TRUE)
cRate <- reshape(cRate, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(cRate) <- c("Borough", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

#aggregate bands, total payment and rate by band and property price for London
LDN_bands <- data.frame(count(pp_data_LDN, c("Price.bins", "cBand")))
LDN_bands <- reshape(LDN_bands, idvar = "Price.bins", timevar = "cBand",  direction = "wide")
colnames(LDN_bands) <- c("Price", "A", "B", "C", "D", "E", "F", "G", "H")

#add council tax to stamp duty from line 179
LDN_tax <- LDN_stamp
LDN_tax$rows <- seq(1, 11, 1)
LDN_ctax1 <- aggregate(pp_data_LDN$cBill, by = list(pp_data_LDN$Price.bins), FUN = sum, na.rm = TRUE)
LDN_ctax2 <- aggregate(pp_data_LDN$cRate, by = list(pp_data_LDN$Price.bins), FUN = mean, na.rm = TRUE)
LDN_price <- aggregate(pp_data_LDN$Price, by = list(pp_data_LDN$Price.bins), FUN = mean, na.rm = TRUE)
LDN_tax <- merge(LDN_tax, LDN_ctax1, by.x = "Price.bin", by.y = "Group.1")
LDN_tax <- merge(LDN_tax, LDN_ctax2, by.x = "Price.bin", by.y = "Group.1")
LDN_tax <- merge(LDN_tax, LDN_price, by.x = "Price.bin", by.y = "Group.1")
colnames(LDN_tax)[7:9] <- c("Council_tax", "cTax_rate", "Av_price")
#LDN_tax$tTax <-LDN_tax$Stamp_rate / 15 + LDN_tax$cTax_rate #divide stamp by av tenure ~ 15 years ???
LDN_tax <- LDN_tax[order(LDN_tax$rows),]
LDN_tax <- LDN_tax[,c(1:5,7:9)]
rm(LDN_ctax1, LDN_ctax2, LDN_price)

#file for Jan 2013 onwards
#LDN_tax_all <- LDN_tax
#write.csv(LDN_tax_all, "LDN_tax_all.csv")
write.csv(LDN_tax, "LDN_tax.csv")

#ploting this data
barplot(LDN_tax$Sales/10^3, main = "London sales count, '000", horiz = FALSE, 
        names.arg = LDN_tax$Price.bin, col = "blue")
barplot(LDN_tax$Stamp_paid/10^6, main = "London stamp duty paid £m", horiz = FALSE, 
        names.arg = LDN_tax$Price.bin, col = "blue")
barplot(LDN_tax$Stamp_rate, main = "London stamp tax rate, %", horiz = FALSE, 
        names.arg = LDN_tax$Price.bin, col = "blue")
barplot(LDN_tax$Council_tax/10^6, main = "London council tax raised, £m", horiz = FALSE, 
        names.arg = LDN_tax$Price.bin, col = "blue")
barplot(LDN_tax$cTax_rate, main = "London council tax rate, %", horiz = FALSE, 
        names.arg = LDN_tax$Price.bin, col = "blue")
#barplot(LDN_tax$tTax, main = "London total property tax rate, %", horiz = FALSE, 
#        names.arg = LDN_tax$Price.bin, col = "blue")

####let's look at 2m homes only
# mill <- subset(pp_data_LDN, Price > 2000000)
# #stamp duty paid & v's all
# nrow(mill)/nrow(pp_data_LDN)
# sum(mill$stamp_duty) / 10^6
# sum(mill$stamp_duty) / sum(pp_data_LDN$stamp_duty)
# sum(mill$cBill) / sum(pp_data_LDN$cBill)
# 
# #order this by Price paid
# mill <- mill[order(mill$Price, decreasing = TRUE),]

#write all these files out -- good to get them in an xls
write.csv(pp_data_LDN, "R analysis/LDN_property_data.csv")
write.csv(LDN_tax, "R analysis/LDN_property_tax2.csv")
write.csv(sales, "R analysis/LDN_sales_by_price_bands.csv")
write.csv(stamp_receipt, "R analysis/LDN_stamp_recipts.csv")
write.csv(stamp_tax, "R analysis/LDN_stamp_tax_rate.csv")
write.csv(LDN_bands, "R analysis/LDN_ctax_bands.csv")
write.csv(cBands, "R analysis/LDN_ctax_bands.csv")
write.csv(cRate, "R analysis/LDN_ctax_price_x_Boro.csv")
write.csv(cRev, "R analysis/LDN_ctax_price_x_Boro.csv")
write.csv(cRev1, "R analysis/LDN_ctax_band_x_Boro.csv")
#write.csv(mill, "R analysis/LDN_2m_plus.csv")
write.csv(data_save, "R analysis/data_save.csv")
write.csv(data_save2, "R analysis/data_save2.csv")
#pp_data_LDN <- read.csv("R analysis/LDN_property_data.csv", header = TRUE)

#indicative stamp duty rate line chart
x = seq(10000, 10000000, by = 5000)
x <- data.frame(x)
x$new_stamp <- sapply(x$x, new_stampduty)
x$old_stamp <- sapply(x$x, old_stampduty)
x$new_stamp <- x$new_stamp / x$x
x$old_stamp <- x$old_stamp / x$x
plot(x$new_stamp)
plot(x$old_stamp)

###draw this for council tax too###
#deriving council tax rate from (weighted) average of bands etc...
#with the weights by land registry sales

#import the council tax rates for each Borough
t_bands <- merge(bands2, cBands, by.x = "Code", by.y = "Admin_district_code") #cBands uses Land reg count -- alt use VOA count
t_bands[,21:28] <- t_bands[,3:10] * t_bands[,12:19]
t_bands$total_ctax <- rowSums(t_bands[21:28], na.rm=TRUE, dims = 1)
t_bands$av_ctax <- t_bands$total_ctax / t_bands$count
Ctax_boro <- t_bands[,c(1:2, 20, 29:30)]

#distil this down for whole of London
Ctax_lon <- data.frame(colSums(t_bands[12:19], na.rm=TRUE, dims = 1))
colnames(Ctax_lon) <- c("x1")
Ctax_lon$x2 <- colSums(t_bands[21:28], na.rm=TRUE, dims = 1)
Ctax_lon$x3 <- Ctax_lon$x2 / Ctax_lon$x1

#weighted av threshold level >> as above but ignoring band H
t_bands <- merge(bands1, cBands, by.x = "Code", by.y = "Admin_district_code") #cBands uses Land reg count -- alt use VOA count
t_bands[,21:27] <- t_bands[,3:9] * t_bands[,12:18] 
t_bands$total_band <- rowSums(t_bands[21:27], na.rm=TRUE, dims = 1)
t_bands$av_band <- t_bands$total_band / t_bands$count
#merge with Ctax_boro
Ctax_boro <- merge(Ctax_boro, t_bands[,c(1, 28:29)], by = "Code")

#add to Ctax above
Ctax_lon$x4 <- c(colSums(t_bands[21:27], na.rm=TRUE, dims = 1), 1)
Ctax_lon$x5 <- round(Ctax_lon$x4 / Ctax_lon$x1, digits = 3)
Ctax_lon$Bands <- c("A", "B", "C", "D", "E", "F", "G", "H")
colnames(Ctax_lon) <- c("Prop_count", "Total_cTax", "Av_cTax", "Tot_prop_val", "Av_prop_val", "Band")
Ctax_lon <- Ctax_lon[,c(6, 2:5)]
write.csv(Ctax_lon, "Ctax_lon.csv")

#creating a function based on Ctax_lon$Av_prop_value & Av_cTax
#ie, what's the (weighted) average London council tax at a give property price
# y <- Ctax_lon$Av_prop_val*1000
# z <- Ctax_lon$Av_cTax
# council_tax <- function (x) {
#     if (x < y[1]) {
#       x <- z[1] / x * 100
#     } else if (x < y[2])  {
#       x <- z[2] / x * 100
#     } else if (x < y[3])  {
#       x <- z[3] / x * 100
#     } else if (x < y[4])  {
#       x <- z[4] / x * 100
#     } else if (x < y[5])  {
#       x <- z[5] / x * 100
#     } else if (x < y[6])  {
#       x <- z[6] / x * 100
#     } else if (x < y[7])  {
#       x <- z[7] / x * 100
#     } else {
#       x <- z[8] / x * 100
# }}

#jj <- 15138309
#new_stampduty(jj) / jj * 100

#this is a short version that doesn't work
#council_tax <- function (x) {
#  for (i in 1:7) { 
#    if (x < y[i]) {
#      x <- x / z[i] 
#    } else {
#    x <- x / z[8] 
#}}}


#indicative stamp duty rate line chart
# x = seq(100000, 2000000, by = 5000)
# x <- data.frame(x)
# x$new_stamp <- sapply(x$x, new_stampduty)
# x$new_stamp <- x$new_stamp / x$x * 100
# 
# #indicative council tax rate line chart
# x$cTax <- sapply(x$x, council_tax)
# #sum of tax
# x$ttax <- x$new_stamp / 12 + x$cTax

# ten = 15 #length of tenure
# require(ggplot2)
# plot <- ggplot() + 
# #  geom_line(data=x/1000000, aes(x=x, y=new_stamp/ten, group=1)) +
# #  geom_line(data=x/1000000, aes(x=x, y=cTax, group=1)) +
#   geom_line(data=x/1000000, aes(x=x, y=(new_stamp/ten + cTax), group=1)) +
# #  geom_line(data=x/1000000, aes(x=x, y=(cTax), group=1)) +
#   #  scale_y_continuous(breaks=c(0, .2, .4, .6, .8, 1), labels = c("0", "2", "4","6","8","10")) +
#   xlab("Price paid, £m") + ylab("Tax rate, %") + 
#   ggtitle("Property tax rates for London")
# plot


###looking at the stamp duty and price element -- how sales are grouped
# pp_data_LDN_stamp <- pp_data_LDN[,c(1:6)]
# pp_data_LDN_stamp$Price.bins <- cut(pp_data_LDN_stamp$Price, breaks=seq(0, 1020000, by = 10000))
# colnames(pp_data_LDN_stamp) <- make.names(colnames(pp_data_LDN_stamp))
# plot(table(subset(pp_data_LDN_stamp, Date.of.Transfer <= "2014/12/05")[,"Price.bins"]))
# plot(table(subset(pp_data_LDN_stamp, Date.of.Transfer > "2015/01/01")[,"Price.bins"]))
# plot(table(pp_data_LDN_stamp[,"Price.bins"]))
# #x <- data.frame(table(pp_data_LDN_stamp[,"Price.bins"]))
# 
# #search for indie properties
# test <- subset(pp_data_LDN, Postcode == "N6 6AS")
# test <- subset(pp_data_LDN, Age == "Y")
# summary(test$Price)
# test$Price.bins <- cut(test$Price, breaks=seq(0, 1100000, by = 10000))
# plot(table(test[,"Price.bins"]))


######################################
#steps to calculate the tenure length
#1. Annualised value of stamp duty in 2015 in band X
#2. Average value of Band X property according to Land Reg
#3. Total value of Band X = average value (2) * stock of properties in Band X
#4. Stamp duty rate = #1 / #2
#5. Average tenure length = stamp duty rate for Band X / stamp duty rate in #4
#cBands == property sales aggregated by boro and cBand

### going thru these step above
#set years var
years <- 1

#1. stamp duty by council tax bands
stamp_receipt2 <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$Admin_district_code, pp_data_LDN$Borough, pp_data_LDN$cBand), FUN = sum, na.rm = TRUE)
stamp_receipt2 <- reshape(stamp_receipt2, idvar = c("Group.1", "Group.2"), timevar = "Group.3",  direction = "wide")
colnames(stamp_receipt2) <- c("Code", "Borough", "A", "B", "C", "D", "E", "F", "G", "H")

#2. Average value of band property
price_av <- aggregate(pp_data_LDN$Price, by = list(pp_data_LDN$Admin_district_code, pp_data_LDN$Borough, pp_data_LDN$cBand), FUN = mean, na.rm = TRUE)
price_av <- reshape(price_av, idvar = c("Group.1", "Group.2"), timevar = "Group.3",  direction = "wide")
colnames(price_av) <- c("Code", "Borough", "A", "B", "C", "D", "E", "F", "G", "H")
write.csv(price_av, "price_av.csv")

#3. Total value of property in Band
total_prop <- merge(bands3, price_av, by = "Code")
total_prop[,21:28] <- total_prop[,3:10] * total_prop[,13:20]
total_prop <- total_prop[,c(1:2, 21:28)]

#4. stamp duty rate = #1 (annualised) / #3
max(pp_data_LDN[,4]) #<- 5/12 -- 2.417 = 2 years and 5 months of data
stamp_rate <- merge(stamp_receipt2, total_prop, by = "Code")
stamp_rate[,20:27] <- round(stamp_rate[,3:10] / years  / stamp_rate[,12:19] * 100, digits = 3)
stamp_rate[,20:27] <- round(stamp_rate[,20:27], digits = 3)
stamp_rate <- stamp_rate[, c(1,2,20:27)]

#5. Average tenure length = stamp duty rate for Band X at av price #2 / stamp duty rate in #4
tenure <- price_av
tenure <- tenure[,3:10]
tenure[is.na(tenure)] <- 0
tenure <- cbind(price_av[,1:2], tenure)
for (i in 3:ncol(tenure)) {
  tenure[,i+8] <- sapply(tenure[,i], new_stampduty) / tenure[,i] * 100
}  
tenure <- tenure[,c(1:2, 11:18)]
tenure <- merge(tenure, stamp_rate, by = "Code")
tenure[,20:27] <- tenure[,3:10] / tenure[,12:19]
tenure <- tenure[,c(1:2, 20:27)]
colnames(tenure) <- c("Code", "Borough", "A", "B", "C", "D", "E", "F", "G", "H")

############################################
###crunching all of the above for london only
#1. stamp duty by council tax bands
Lon_ten <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$cBand), FUN = sum, na.rm = TRUE)
colnames(Lon_ten) <- c("Band", "Stamp_receipts")
#2. Average value of band property
temp <- aggregate(pp_data_LDN$Price, by = list(pp_data_LDN$cBand), FUN = mean, na.rm = TRUE)
colnames(temp) <- c("Band", "Av_price")
Lon_ten <- merge(Lon_ten, temp, by = "Band")
#3. Total value of property in Band
temp <- colSums(bands3[3:10], na.rm = TRUE)
Lon_ten$prop <- temp
Lon_ten$prop_val <- Lon_ten$Av_price * Lon_ten$prop
#4. stamp duty rate = #1 (annualised) / #3
Lon_ten$stamp_rate <- Lon_ten$Stamp_receipts / years / Lon_ten$prop_val * 100
#5. Average tenure length = stamp duty rate for Band X at av price #2 / stamp duty rate in #4
Lon_ten$stamp <- sapply(Lon_ten$Av_price, new_stampduty) / Lon_ten$Av_price * 100
Lon_ten$tenure <- Lon_ten$stamp / Lon_ten$stamp_rate
write.csv(Lon_ten, "Lon_ten.csv")

###!you get some fanciful figures because so much of the housing stock isn't owner occupied!###
#let's just use English housing survey data


###########################
#USD analysis#

#create value bins for USD value#
USD <- .6074
pp_data_LDN$Price_USD <- pp_data_LDN$Price / USD
fivenum(pp_data_LDN$Price)
fivenum(pp_data_LDN$Price_USD)
pp_data_LDN$Price.bins_USD <- cut(pp_data_LDN$Price_USD, breaks=c(0, 100000, 250000, 500000, 750000, 1000000, 1500000, 2000000, 3000000, 5000000, 10000000, 100000000),
                              labels=c("0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+"))

summary(pp_data_LDN$Price.bins_USD)
summary(pp_data_LDN$Price.bins)


###creating USD version of LDN_tax table###
#average stamp duty rate by price buckets for London
LDN_stamp1 <- data.frame(count(pp_data_LDN, "Price.bins_USD"))
LDN_stamp1$rows <- seq(1, 11, 1)
LDN_stamp2 <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$Price.bins_USD), FUN = sum, na.rm = TRUE)
LDN_stamp3 <- aggregate(pp_data_LDN$stamp_tax, by = list(pp_data_LDN$Price.bins_USD), FUN = mean, na.rm = TRUE)
LDN_stamp4 <- aggregate(pp_data_LDN$stamp_duty2, by = list(pp_data_LDN$Price.bins_USD), FUN = sum, na.rm = TRUE)

LDN_stamp_USD <- merge(LDN_stamp1, LDN_stamp2, by.x = "Price.bins_USD", by.y = "Group.1")
LDN_stamp_USD <- merge(LDN_stamp_USD, LDN_stamp3, by.x = "Price.bins_USD", by.y = "Group.1")
LDN_stamp_USD <- merge(LDN_stamp_USD, LDN_stamp4, by.x = "Price.bins_USD", by.y = "Group.1")

LDN_stamp_USD <- LDN_stamp_USD[order(LDN_stamp_USD$rows),]
LDN_stamp_USD <- LDN_stamp_USD[,c(1:2,4:6)]
colnames(LDN_stamp_USD) <- c("Price.bin", "Sales_count", "Stamp_paid_hypo", "stamp_rate", "Stamp_rate_act")
rm(LDN_stamp1, LDN_stamp2, LDN_stamp3, LDN_stamp4)

#USD version of LDN_tax
LDN_tax_USD <- LDN_stamp_USD
LDN_tax_USD$rows <- seq(1, 11, 1)
LDN_ctax1 <- aggregate(pp_data_LDN$cBill, by = list(pp_data_LDN$Price.bins_USD), FUN = sum, na.rm = TRUE)
LDN_ctax2 <- aggregate(pp_data_LDN$cRate, by = list(pp_data_LDN$Price.bins_USD), FUN = mean, na.rm = TRUE)
LDN_price <- aggregate(pp_data_LDN$Price_USD, by = list(pp_data_LDN$Price.bins_USD), FUN = mean, na.rm = TRUE)
LDN_tax_USD <- merge(LDN_tax_USD, LDN_ctax1, by.x = "Price.bin", by.y = "Group.1")
LDN_tax_USD <- merge(LDN_tax_USD, LDN_ctax2, by.x = "Price.bin", by.y = "Group.1")
LDN_tax_USD <- merge(LDN_tax_USD, LDN_price, by.x = "Price.bin", by.y = "Group.1")
colnames(LDN_tax_USD)[7:9] <- c("Council_tax", "cTax_rate", "Av_price_USD")
#LDN_tax$tTax <-LDN_tax$Stamp_rate / 15 + LDN_tax$cTax_rate #divide stamp by av tenure ~ 15 years ???
LDN_tax_USD <- LDN_tax_USD[order(LDN_tax_USD$rows),]
LDN_tax_USD <- LDN_tax_USD[,c(1:5,7:9)]
rm(LDN_ctax1, LDN_ctax2, LDN_price)

#council tax to USD 
LDN_tax_USD$Council_tax <- LDN_tax_USD$Council_tax / USD

write.csv(LDN_tax_USD, "LDN_tax_USD.csv")

#LDN_ten table
#convert stamp recipts, av_price, prop_val to USD
#cols 2, 3, 5

###crunching all of the above for london only
#1. stamp duty by council tax bands
Lon_ten_USD <- aggregate(pp_data_LDN$stamp_duty, by = list(pp_data_LDN$cBand), FUN = sum, na.rm = TRUE)
colnames(Lon_ten_USD) <- c("Band", "Stamp_receipts_GBP")
Lon_ten_USD$Stamp_receipts_USD <- Lon_ten_USD$Stamp_receipts / USD
#2a. Average value of band property for GBP
temp <- aggregate(pp_data_LDN$Price, by = list(pp_data_LDN$cBand), FUN = mean, na.rm = TRUE)
colnames(temp) <- c("Band", "Av_price_GBP")
Lon_ten_USD <- merge(Lon_ten_USD, temp, by = "Band")
#2b. Average value of band property for USD
temp <- aggregate(pp_data_LDN$Price_USD, by = list(pp_data_LDN$cBand), FUN = mean, na.rm = TRUE)
colnames(temp) <- c("Band", "Av_price_USD")
Lon_ten_USD <- merge(Lon_ten_USD, temp, by = "Band")
#3. Total value of property in Band
temp <- colSums(bands3[3:10], na.rm = TRUE)
Lon_ten_USD$prop <- temp
Lon_ten_USD$prop_val_GBP <- Lon_ten_USD$Av_price_GBP * Lon_ten_USD$prop
Lon_ten_USD$prop_val_USD <- Lon_ten_USD$Av_price_USD * Lon_ten_USD$prop
#4. stamp duty rate = #1 (annualised) / #3
Lon_ten_USD$stamp_rate_GBP <- Lon_ten_USD$Stamp_receipts_GBP / years / Lon_ten_USD$prop_val_GBP * 100
Lon_ten_USD$stamp_rate_USD <- Lon_ten_USD$Stamp_receipts_USD / years / Lon_ten_USD$prop_val_USD * 100
#5. Average tenure length = stamp duty rate for Band X at av price #2 / stamp duty rate in #4
Lon_ten_USD$stamp <- sapply(Lon_ten_USD$Av_price_GBP, new_stampduty) / Lon_ten_USD$Av_price_GBP * 100
Lon_ten_USD$tenure <- Lon_ten_USD$stamp / Lon_ten_USD$stamp_rate_GBP
write.csv(Lon_ten_USD, "Lon_ten_USD.csv")


#ctax_lon
#convert cols 2:5
Ctax_lon_USD <- Ctax_lon
Ctax_lon_USD[,2:5] <- Ctax_lon[,2:5] / USD
write.csv(Ctax_lon_USD, "Ctax_lon_USD.csv")

write.csv(x, "stampilo.csv")

#modelling the path of tax rates at a given price #ignore?
plot(LDN_tax$Av_price/1000, LDN_tax$tTax)
fit <- lm(tTax ~ Av_price + Av_price ^ 2, data=LDN_tax)
summary(fit)
write.csv(LDN_tax, "LDN_tax_table.csv")

#write out final file
write.csv(pp_data_LDN, "pp_data_LDN.csv")
write.csv(mill, "mill_london.csv")

# new_stampduty(363984) * 114726
# 
# LDN_tax$old_stamp <- sapply(LDN_tax$Av_price, old_stampduty)
# LDN_tax$old_stamp <- LDN_tax$old_stamp * LDN_tax$Sales_count
# sum(LDN_tax$old_stamp)

###this is a mess###
# colnames(pp_data_LDN)
# london <- subset(pp_data_LDN, Price > 1500000 & pp_data_LDN[,4] > "2015/01/01")
# london <- subset(pp_data_LDN, cBand == "H" & pp_data_LDN[,4] > "2015/01/01")
# sum(london$stamp_duty, na.rm = TRUE) / 10^6 /.261215 
# sum(as.numeric(london$Price)) /10^6 /.261215 
# 
# x <- sum(as.numeric(london$Price)) /10^6 /.261215  / (nrow(london) / .26)
# x
# 201750 / x
# 
# x <- 201750 / (sum(as.numeric(london$Price)) /10^6 /.261215  / (nrow(london) / .26))
# x
# 
# x <- sum(as.numeric(london$Price)) /10^6 /.261215  / (nrow(london) / .26)
# x <- new_stampduty(x)
# x
# 
# write.csv(london, "londonprop.csv")
# str(london$Price)
# 
# #date 
# colnames(pp_data_LDN) <- make.names(colnames(pp_data_LDN))
# 
# test1 <- subset(pp_data_LDN, cBand == "H" & Date.of.Transfer > "2014/01/01" & Date.of.Transfer < "2014/04/28")
# test2 <- subset(pp_data_LDN, cBand == "H" & Date.of.Transfer > "2014/01/01" & Date.of.Transfer < "2015/01/01")
# sum(as.numeric(test1$Price)) / sum(as.numeric(test2$Price))
# nrow(test1) / nrow(test2)