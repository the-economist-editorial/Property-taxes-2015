path = "U:/data/property taxes/NYC"
setwd(path)
require(plyr)
require(xlsx)

#103713 | 204995

#read in sales data
NYC_sales_2013 <- read.csv("Sales/NYC_sales_2013.csv", header = TRUE)
NYC_sales_2014 <- read.csv("Sales/NYC_sales_2014.csv", header = TRUE)
#NYC_sales_T12m <- read.csv("Sales/NYC_sales_2015.csv", header = TRUE)
NYC_sales <- rbind.fill(NYC_sales_2013, NYC_sales_2014)

#read in t12m figures from xlsx
Bronx_t12m <- read.csv("Sales/rollingsales_bronx.csv", skip = 4)
Brooklyn_t12m <- read.csv("Sales/rollingsales_brooklyn.csv", skip = 4)
Manhattan_t12m <- read.csv("Sales/rollingsales_manhattan.csv", skip = 4)
Queens_t12m <- read.csv("Sales/rollingsales_queens.csv", skip = 4)
StatenIs_t12m <- read.csv("Sales/rollingsales_statenisland.csv", skip = 4)
NYC_sales_T12m <- rbind.fill(Bronx_t12m, Brooklyn_t12m, Manhattan_t12m, Queens_t12m, StatenIs_t12m)
rm(Bronx_t12m, Brooklyn_t12m, Manhattan_t12m, Queens_t12m, StatenIs_t12m)

#CONVERT DATE, SUBSET 2015 ONLY
NYC_sales_T12m$SALE.DATE <- as.Date(NYC_sales_T12m$SALE.DATE, "%d/%m/%Y")
NYC_sales_2015 <- subset(NYC_sales_T12m, SALE.DATE > "2014-12-31")
NYC_sales_2015$File <- "T12m"

#MAKE SOME TWEAKS
colnames(NYC_sales) <- colnames(NYC_sales_2015)
NYC_sales$SALE.DATE <- as.Date(NYC_sales$SALE.DATE, "%d/%m/%Y")

#ADD IN FILE
NYC_sales <- rbind.fill(NYC_sales, NYC_sales_2015)

#remove
rm(NYC_sales_2013, NYC_sales_2014, NYC_sales_2015, NYC_sales_T12m)

###residential buildings only -- TAX CLASSES ONE AND TWO & sales of > $125k & excluding vacant land
NYC_resi_sales <- subset(NYC_sales, TAX.CLASS.AT.TIME.OF.SALE < 3 & SALE.PRICE > 125000 
                         & BUILDING.CLASS.CATEGORY != "05  TAX CLASS 1 VACANT LAND                 "
                         )

#add in price per unit 
NYC_resi_sales$price_unit <- round(NYC_resi_sales$SALE.PRICE / NYC_resi_sales$RESIDENTIAL.UNITS, digits = 3)

for (i in 1:nrow(NYC_resi_sales)) {
  if (NYC_resi_sales[i, "RESIDENTIAL.UNITS"] == 0) {
    NYC_resi_sales[i, "price_unit"] <- NYC_resi_sales[i, "SALE.PRICE"]
}}

summary(NYC_resi_sales$price_unit)

#create price buckets
fivenum(NYC_resi_sales$price_unit/1000)
NYC_resi_sales$Price.bins <- cut(NYC_resi_sales$price_unit, breaks=c(0, 100000, 250000, 500000, 750000, 1000000, 1500000, 2000000, 3000000, 5000000, 10000000, 1000000000),
                              labels=c("0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+"))
summary(NYC_resi_sales$Price.bins)

###looking at the mansion tax and price element -- ie, how sales are grouped
NYC_resi_sales_stamp <- NYC_resi_sales[,c(1,21,23)]
NYC_resi_sales_stamp$Price.bins <- cut(NYC_resi_sales_stamp$price_unit, breaks=seq(0, 1500000, by = 10000))
test <- data.frame(table(NYC_resi_sales_stamp$Price.bins))
plot(table(NYC_resi_sales_stamp$Price.bins))
rm(NYC_resi_sales_stamp, test)

#create AV-BBLE for NYC resi sales
# this is: boro code + 000 + block + lot + easement -- 11 digits
for (i in 1:nrow(NYC_resi_sales)) {
  NYC_resi_sales[i, "AV.BBLE."] <- paste(NYC_resi_sales[i, "BOROUGH"], 
                                  paste(paste(replicate(5 - nchar(NYC_resi_sales[i, "BLOCK"]), "0"), collapse=""), NYC_resi_sales[i, "BLOCK"], sep=""), 
                                  paste(paste(replicate(4 - nchar(NYC_resi_sales[i, "LOT"]), "0"), collapse=""), NYC_resi_sales[i, "LOT"], sep=""), 
                                  #NYC_resi_sales[i, "EASE.MENT"], 
                                  sep = "")}

#subset $5m homes only
# mill_NYC <- subset(NYC_resi_sales, price_unit > 5000000)
# mill_NYC <- mill_NYC[order(mill_NYC$price_unit, decreasing = TRUE),]
# #exclude those that report number of units
# mill_NYC <- subset(mill_NYC, RESIDENTIAL.UNITS != 0)

####read in tax data###
#colnames
names <- read.csv("Tax rating/Tax_columns.csv", header = TRUE)

#read in tax class 1 file
tc1 <- read.csv("Tax rating/tc1.txt", header = FALSE)

#read in tax class 2+ file
tc2 <- read.csv("Tax rating/tc234.txt", header = FALSE)

#assign the names
colnames(tc1) <- make.names(names[,1])
colnames(tc2) <- make.names(names[,1])

#trim the files down
#test market value <- AV.NEW.FULLVAL. [12]
#billable assessed value <- AV.FINAL.AVT.ACT. [37]
#exempt value <- AV.FINAL.EXT.ACT. [39]

tc1.trim <- tc1[,c(1:7, 10, 12, 33, 37, 39, 35, 40, 43, 45:51)]
tc2.trim <- tc2[,c(1:7, 10, 12, 33, 37, 39, 35, 40, 43, 45:51)]
tc2.trim <- subset(tc2.trim, AV.TAX.CLASS. == "2" | AV.TAX.CLASS. == "2A" | AV.TAX.CLASS. == "2B" | AV.TAX.CLASS. == "2C" )
tc <- rbind.fill(tc1.trim, tc2.trim)
colnames(tc)
summary(tc$AV.TAX.CLASS.)
rm(tc1, tc2, tc1.trim, tc2.trim)

#convert to character
tc$AV.BBLE. <- as.character(tc$AV.BBLE.)

#summary stats from overall tax roll
#tax_summary <- data.frame(table(tc$AV.TAX.CLASS))
tax_summary <- data.frame(table(tc$AV.BLDGCL))
temp1 <- aggregate(tc$AV.NEW.FULLVAL, by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
temp2 <- aggregate(tc$AV.FINAL.AVT.ACT, by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
temp3 <- aggregate(tc$AV.FINAL.AVT., by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
temp4 <- aggregate(tc$AV.FINAL.EXT., by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
temp5 <- aggregate(tc$AV.RESIDENTIAL.UNITS, by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
temp6 <- aggregate(tc$AV.TOTAL.UNITS, by = list(tc$AV.BLDGCL), FUN = sum, na.rm = TRUE )
tax_summary <- merge(tax_summary, temp1, by.x = "Var1", by.y = "Group.1")
tax_summary <- merge(tax_summary, temp2, by.x = "Var1", by.y = "Group.1")
tax_summary <- merge(tax_summary, temp3, by.x = "Var1", by.y = "Group.1")
tax_summary <- merge(tax_summary, temp4, by.x = "Var1", by.y = "Group.1")
tax_summary <- merge(tax_summary, temp5, by.x = "Var1", by.y = "Group.1")
tax_summary <- merge(tax_summary, temp6, by.x = "Var1", by.y = "Group.1")
colnames(tax_summary) <- c("Build.class", "Count", "Market.value", "Assessed.val", "Assessed.val2", "Exempt.val", "Resi.units", "Total.units")
rm(temp1, temp2, temp3, temp4, temp5, temp6)
tax_summary$comm.units <- tax_summary$Total.units - tax_summary$Resi.units
write.csv(tax_summary, "tax_roll_summary.csv")

tax_summary2 <- data.frame(table(tc$AV.BLDGCL, tc$AV.TAX.CLASS))
tax_summary2 <- subset(tax_summary2, Freq >0)
write.csv(tax_summary2, "tax_roll_summary2.csv")

#add in summary statistics >> tax rate, tax paid, mean, max, min, variance ?

######################################
####merge sales file with tax file####
NYC_tax <- merge(tc, NYC_resi_sales[,c(2,3, 9:12, 16:25)], by = "AV.BBLE.")
#summary(NYC_tax)

#checking entries
# indie <- subset(NYC_tax, AV.BBLE. == "1000970044")
# indie <- subset(NYC_tax, AV.BBLE. == "1010101698")
# indie <- subset(NYC_tax, ADDRESS == "330 EAST 38TH   STREET                   ")

#remove duplicates
#summary(NYC_tax$SALE.DATE)
#NYC_tax <- NYC_tax[order(NYC_tax$SALE.DATE, na.last = TRUE, decreasing = TRUE),]
#NYC_tax2 <- data.frame(NYC_tax[duplicated(NYC_tax[,"AV.BBLE."]),], row.names=NULL)
#NYC_tax <- data.frame(NYC_tax[!duplicated(NYC_tax[,"AV.BBLE."]),], row.names=NULL)
#x <- NYC_tax2[5,1]
#x
#test <- subset(NYC_tax, AV.BBLE. == x)
#test2 <- subset(NYC_tax2, AV.BBLE. == x)

colnames(NYC_tax)
###calculate rates etc. 
#Class 1 = 19.157%
#class 2 = 12.855%
#tax = (assesssed value - exemption) x tax rate
#billable assessed value <- AV.FINAL.AVT.
#exempt value <- AV.FINAL.EXT.

#CALCULATE TAX RATE FOR EACH PROPERTY CLASS -- 19.15% OR 12.85%
start.time <- Sys.time()
for (i in 1:nrow(NYC_tax)) {
  if (NYC_tax[i, "TAX.CLASS.AT.TIME.OF.SALE"] == 1) {
    NYC_tax[i, "prop.tax"] <- round((NYC_tax[i, "AV.FINAL.AVT."] - NYC_tax[i, "AV.FINAL.EXT."]) * .19157, digits = 2)
  } else {
    NYC_tax[i, "prop.tax"] <- round((NYC_tax[i, "AV.FINAL.AVT."] - NYC_tax[i, "AV.FINAL.EXT."]) * .12855, digits = 2)
  }}  
end.time <- Sys.time()
fun.time <- end.time - start.time
fun.time  
# #takes 1 min to run

#AND THESE SUMMARY STATISTICS
NYC_tax$mkt_val <- NYC_tax$AV.NEW.FULLVAL.
NYC_tax$assessed_val <- NYC_tax$AV.FINAL.AVT.
NYC_tax$exempt_val <- NYC_tax$AV.FINAL.EXT.
NYC_tax$exempt_pct <- round(NYC_tax$AV.FINAL.EXT. / NYC_tax$AV.FINAL.AVT. * 100, digits = 3)
NYC_tax$tax.rate <- round(NYC_tax$prop.tax / NYC_tax$mkt_val * 100, digits = 3)

#trying to derive actual number of units
start.time <- Sys.time()
for (i in 1:nrow(NYC_tax)) {
  if (NYC_tax[i, "AV.TOTAL.UNITS."] == "0") { 
    NYC_tax[i, "tax_units"] <- 1
  } else {
    NYC_tax[i, "tax_units"] <- NYC_tax[i, "AV.TOTAL.UNITS."]
  }}
for (i in 1:nrow(NYC_tax)) {
  if (NYC_tax[i, "RESIDENTIAL.UNITS"] == "0") { 
    NYC_tax[i, "sales_units"] <- 1
  } else {
    NYC_tax[i, "sales_units"] <- NYC_tax[i, "RESIDENTIAL.UNITS"]
  }}
end.time <- Sys.time()
fun.time <- end.time - start.time
fun.time  
#takes 2 mins

#trim space
trim.trailing <- function (x) sub("\\s+$", "", x)
NYC_tax$BUILDING.CLASS.CATEGORY <- sapply(NYC_tax$BUILDING.CLASS.CATEGORY, trim.trailing)

#####################################
#narrow data down
head(NYC_tax, 5)
NYC_tax_save <- NYC_tax

#narrow columns
#NYC_tax <- NYC_tax[,c(1:2, 8, )]

#2014 only
NYC_tax <- subset(NYC_tax, SALE.DATE > "2013-12-31" & SALE.DATE < "2015-01-01")

#exclude stuff
#NYC_tax <- 

#calculating tax rate per unit for each property in file
NYC_tax$tax.rate2 <- round((NYC_tax$prop.tax / NYC_tax$tax_units) / (NYC_tax$SALE.PRICE / NYC_tax$sales_units) *100, digits = 3)
summary(NYC_tax$tax.rate2)

#reationship between market value and sales value
NYC_tax$mkt.sales <- round(NYC_tax$mkt_val / NYC_tax$tax_units / NYC_tax$price_unit  * 100, digits = 1)
summary(NYC_tax$mkt.sales)
test <- subset(NYC_tax, mkt.sales > 0 & mkt.sales < 1 )
test <- test[order(test$mkt.sales, decreasing = FALSE),]

#calculating tax rate per unit for each property in file
NYC_tax$tax.rate2 <- round((NYC_tax$prop.tax / NYC_tax$tax_units) / (NYC_tax$SALE.PRICE / NYC_tax$sales_units) *100, digits = 3)
summary(NYC_tax$tax.rate2)

#reationship between market value and sales value
NYC_tax$mkt.sales <- round(NYC_tax$mkt_val / NYC_tax$tax_units / NYC_tax$price_unit  * 100, digits = 1)
summary(NYC_tax$mkt.sales)
#test <- subset(NYC_tax, mkt.sales > 0 & mkt.sales < 1 )
#test <- test[order(test$mkt.sales, decreasing = FALSE),]



###let's aggregate them up to see trends in buildings classess
buildings <- data.frame(table(NYC_tax$BUILDING.CLASS.CATEGORY))
colnames(buildings) <- c("Type", "count") 
temp1 <- aggregate(NYC_tax$SALE.PRICE, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp2 <- aggregate(NYC_tax$mkt_val, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp3 <- aggregate(NYC_tax$prop.tax, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp4 <- aggregate(NYC_tax$assessed_val, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp5 <- aggregate(NYC_tax$exempt_val, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp6 <- aggregate(NYC_tax$tax_units, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
temp7 <- aggregate(NYC_tax$sales_units, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = sum, na.rm = TRUE)
buildings <- merge(buildings, temp1, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp2, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp3, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp4, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp5, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp6, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp7, by.x = "Type", by.y = "Group.1")
colnames(buildings) <- c("Type", "Count", "Sales.price", "Market.val", "Property.tax", "Assessed.val", "Exempt.val", "Tax.units", "Sales.units")
rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7)
buildings$tax.rate <- round(buildings$Property.tax / buildings$Market.val * 100, digits = 2)
buildings$tax.rate2 <- round(buildings$Property.tax / buildings$Sales.price * 100, digits = 2)
buildings$tax.rate3 <- round((buildings$Property.tax / buildings$Tax.units) / (buildings$Sales.price / buildings$Sales.units) * 100, digits = 2)
write.csv(buildings, "buildings_summary.csv")


# ###properties to mark out and inspect: 
# #1. where price.bin is less than 125k
# temp <- subset(NYC_tax, Price.bins == "0-125")
# table(temp$BUILDING.CLASS.CATEGORY)
# 
# #2. where market price to sales price is greater than 1.5
# temp <- subset(NYC_tax, mkt.sales > 250)
# 
# #3 where market price to sales price is greater than 250
# temp <- subset(NYC_tax, mkt.sales < 1 & mkt.sales > 0)
# 
# #4. where there is discrepancy in the number of tax / sales units of greater than 75%, and the property is coop
# temp <- subset(NYC_tax, (BUILDING.CLASS.CATEGORY == "09  COOPS - WALKUP APARTMENTS" |
#                            BUILDING.CLASS.CATEGORY == "10  COOPS - ELEVATOR APARTMENTS" |
#                            BUILDING.CLASS.CATEGORY == "17  CONDO COOPS") 
#                #& (tax_units / sales_units > 2 ))
#                & Price.bins == "0-125")
# 
# #5 where the sale price of the property needs to be divided by the number of tax units
# #as the price is nuts
# temp <- subset(NYC_tax, sales_units == 1 & tax_units > 4 & SALE.PRICE > 10000000)
# 
# #6: remove: 1013730001 & 1009431002 & 1013790021 -- oddities
# 
# #remove some of these subset from the main file
# removed <- subset(NYC_tax, Price.bins == "0-125")
# remove <- subset(NYC_tax, price_unit > 110000000)
# removed <- rbind.fill(removed, remove)
# NYC_tax <- subset(NYC_tax, Price.bins != "0-125" & price_unit < 110000000)
# rm(remove)

#add these summaries to tax rate
temp1 <- aggregate(NYC_tax$tax.rate, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = mean, na.rm = TRUE)
temp2 <- aggregate(NYC_tax$tax.rate2, by = list(NYC_tax$BUILDING.CLASS.CATEGORY), FUN = mean, na.rm = TRUE)
buildings <- merge(buildings, temp1, by.x = "Type", by.y = "Group.1")
buildings <- merge(buildings, temp2, by.x = "Type", by.y = "Group.1")
rm(temp1, temp2)
colnames(buildings)[13:14] <- c("tax.rate4", "tax.rate5")
buildings[,13:14] <- round(buildings[,13:14], digits = 2)

#adding in this useful info
buildings$mkt_sal_pct <- round(buildings$Market.val / buildings$Sales.price * 100, digits = 2)
buildings$exmp_pct <- round(buildings$Exempt.val / buildings$Assessed.val * 100, digits = 2)

write.csv(buildings, "buildings_summary.csv")

###summary statistics by price category###
sales <- data.frame(count(NYC_tax, c("BUILDING.CLASS.CATEGORY", "Price.bins")))
sales <- reshape(sales, idvar = "BUILDING.CLASS.CATEGORY", timevar = "Price.bins",  direction = "wide")
colnames(sales) <- c("Type", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")
sales$count <- rowSums(sales[2:9], na.rm=TRUE, dims = 1)

tax.rate <- aggregate(NYC_tax$tax.rate2, by = list(NYC_tax$BUILDING.CLASS.CATEGORY, NYC_tax$Price.bins), FUN = mean, na.rm = TRUE)
tax.rate <- reshape(tax.rate, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(tax.rate) <- c("Type", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

tax.rate2 <- aggregate(NYC_tax$tax.rate2, by = list(NYC_tax$BUILDING.CLASS.CATEGORY, NYC_tax$Price.bins), FUN = mean, na.rm = TRUE)
tax.rate2 <- reshape(tax.rate2, idvar = "Group.1", timevar = "Group.2",  direction = "wide")
colnames(tax.rate2) <- c("Type", "0-100", "100-250", "250-500", "500-750", "750-1m", "1m-1.5m", "1.5m-2m", "2m-3m", "3m-5m", "5m-10m", "10m+")

#and again, ditching building class type
NYC_sum <- data.frame(count(NYC_tax, c("Price.bins")))
NYC_sum$rows <- seq(1, 11, 1)
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$SALE.PRICE, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$mkt_val, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$assessed_val, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$exempt_val, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$prop.tax, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$tax_units, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$sales_units, by = list(NYC_tax$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$tax.rate, by = list(NYC_tax$Price.bins), FUN = mean, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$tax.rate2, by = list(NYC_tax$Price.bins), FUN = mean, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum <- merge(NYC_sum, aggregate(NYC_tax$tax.rate2, by = list(NYC_tax$Price.bins), FUN = sd, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
colnames(NYC_sum) <- c("cat", "count", "rows", "sales.price", "market.val", "assessed.val", "exempt.val", "prop.tax", "tax.units", "sales.units", "tax.rate", "tax.rate2_mean", "sd.tax.rate2")
NYC_sum$tax.var <- NYC_sum$sd.tax.rate2 / NYC_sum$tax.rate2_mean
NYC_sum$av_price <- NYC_sum$sales.price / NYC_sum$sales.unit

#variance of tax rate
tax_rate_var <- data.frame(daply(NYC_tax, .(Price.bins), function(x) quantile(x$tax.rate2, c(.1, .25, 0.5, .75, .9))))

NYC_sum <- merge(NYC_sum, tax_rate_var, by.x = "cat", by.y = "row.names")
NYC_sum <- NYC_sum[order(NYC_sum$rows),]
NYC_sum <- NYC_sum[,c(1:2, 4:20)]
write.csv(NYC_sum, "NYC_tax_summary.csv")


#variance of tax rate data
tax_rate_var_dat <- data.frame(daply(NYC_tax, .(Price.bins), function(x) quantile(x$tax.rate2, seq(.05, 1, 0.05 ))))
write.csv(tax_rate_var_dat, "tax_rate_var_dat.csv")


####let's aggregate them up to see trends by tax class
#Family homes
#NYC_tax_t1 <- subset(NYC_tax, BUILDING.CLASS.CATEGORY == "01  ONE FAMILY DWELLINGS" | BUILDING.CLASS.CATEGORY == "02  TWO FAMILY DWELLINGS" | BUILDING.CLASS.CATEGORY == "03  THREE FAMILY DWELLINGS")
#Rentals
#NYC_tax_t1 <- subset(NYC_tax, BUILDING.CLASS.CATEGORY == "07  RENTALS - WALKUP APARTMENTS" | BUILDING.CLASS.CATEGORY == "08  RENTALS - ELEVATOR APARTMENTS" | BUILDING.CLASS.CATEGORY == "14  RENTALS - 4-10 UNIT")
#Condos
#NYC_tax_t1 <- subset(NYC_tax, BUILDING.CLASS.CATEGORY == "12  CONDOS - WALKUP APARTMENTS" | BUILDING.CLASS.CATEGORY == "13  CONDOS - ELEVATOR APARTMENTS" | BUILDING.CLASS.CATEGORY == "15  CONDOS - 2-10 UNIT RESIDENTIAL")
#Coops
NYC_tax_t1 <- subset(NYC_tax, BUILDING.CLASS.CATEGORY == "09  COOPS - WALKUP APARTMENTS" | BUILDING.CLASS.CATEGORY == "10  COOPS - ELEVATOR APARTMENTS")

#select var run thru...
ptype <- "coop"
NYC_sum_t1 <- data.frame(count(NYC_tax_t1, c("Price.bins")))
NYC_sum_t1$rows <- seq(1, 11, 1)
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$SALE.PRICE, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$mkt_val, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$assessed_val, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$exempt_val, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$prop.tax, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$tax_units, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$sales_units, by = list(NYC_tax_t1$Price.bins), FUN = sum, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$tax.rate, by = list(NYC_tax_t1$Price.bins), FUN = mean, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$tax.rate2, by = list(NYC_tax_t1$Price.bins), FUN = mean, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
NYC_sum_t1 <- merge(NYC_sum_t1, aggregate(NYC_tax_t1$tax.rate2, by = list(NYC_tax_t1$Price.bins), FUN = sd, na.rm = TRUE), by.x = "Price.bins", by.y = "Group.1")
colnames(NYC_sum_t1) <- c("cat", "count", "rows", "sales.price", "market.val", "assessed.val", "exempt.val", "prop.tax", "tax.units", "sales.units", "tax.rate", "tax.rate2_mean", "sd.tax.rate2")
NYC_sum_t1$tax.var <- NYC_sum_t1$sd.tax.rate2 / NYC_sum_t1$tax.rate2_mean
NYC_sum_t1$av_price <- NYC_sum_t1$sales.price / NYC_sum_t1$sales.unit

#variance of tax rate
tax_rate_var_t1 <- data.frame(daply(NYC_tax_t1, .(Price.bins), function(x) quantile(x$tax.rate2, c(.1, .25, 0.5, .75, .9))))

NYC_sum_t1 <- merge(NYC_sum_t1, tax_rate_var_t1, by.x = "cat", by.y = "row.names")
NYC_sum_t1 <- NYC_sum_t1[order(NYC_sum_t1$rows),]
NYC_sum_t1 <- NYC_sum_t1[,c(1:2, 4:20)]

write.csv(NYC_sum_t1, paste("NYC_tax_summary_", ptype, ".csv", sep=""))


# #subset $5m homes only
# mill_NYC <- subset(NYC_tax, price_unit > 5000000)
# mill_NYC <- mill_NYC[order(mill_NYC$price_unit, decreasing = TRUE),]

###modelling a function to get from sales value to market value###
#create a dummy for tax class
summary(NYC_tax$TAX.CLASS.AT.TIME.OF.SALE)
NYC_tax$tax.class <- NYC_tax$TAX.CLASS.AT.TIME.OF.SALE - 1
table(NYC_tax$tax.class)

#create temp file and inspect relationships
temp <- subset(NYC_tax, tax.class == 0 & SALE.PRICE < 10000000)
plot(temp$SALE.PRICE, temp$mkt_val)
abline(lm(temp$SALE.PRICE ~ temp$mkt_val), col = "red")

#multiple linear regression
fit <- lm(mkt_val ~ SALE.PRICE + tax_units + tax.class, data=temp)
summary(fit)
regression <- data.frame(coefficients(fit))
layout(matrix(c(1,2,3,4),2,2))
plot(fit)
layout(1)

#create a function to describe this relationship in the regression
x = seq(10000, 10000000, by = 5000)
x <- data.frame(x)

#for tax class 1
x$tc1 <- x$x / (regression[1,1] + x$x * regression[2,1])
plot(x$x, x$tc1)
x$tc1.tax <- x$tc1 * .06 * .19157 * 100
plot(x$x, x$tc1.tax)

#for tax class 2
x$tc2 <- x$x / (regression[1,1] + x$x * regression[2,1])
plot(x$x, x$tc2)
x$tc2.tax <- x$tc2 * .45 * .12855 * 100
plot(x$x, x$tc2.tax)

###modelling the relationship between market value and assessed value
plot(temp$mkt_val / 10^6, temp$assessed_val / 10^6)

######################################
###add in various transaction taxes###
#NYC property-transfer tax


#mansion tax#


#search for individuals: 
indie <- subset(tc, AV.OWNER. == "DE BLASIO, BILL")
indie <- subset(NYC_tax, AV.BBLE. == "1010101698")
indie <- subset(tc, AV.BBLE. == "1010101698")
indie <- subset(NYC_tax, AV.BBLE. == "1009431745")
