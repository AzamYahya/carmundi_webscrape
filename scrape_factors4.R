library(rvest)
library(tidyr)
library(RCurl)
library(httr)
library(curl)
library(xml2)
library(XML)
library(stringi)
library(stringr)
library(purrr)
library(plyr)
url <- "http://www.carmudi.pk/2005-suzuki-alto-139251-40.html"

total <- getURLContent(url, useragent = str_c(R.version$platform,
                                              R.version$version.string, sep = ","),
                       httpheader = c(from = "azam.yahya@economist.com"),
                       customrequest = "GET",header = I(TRUE),
                       followlocation = TRUE, connecttimeout = 5000)
doc2 <- htmlParse(total, useInternalNodes = TRUE)

val2 <- xpathApply(doc2, 
                   '//*[@id="details"]/div[2]',
                   fun = function(x) gsub("\n|\t", " ",
                   xmlValue(x)))
library(qdapRegex)
val2 <- rm_white(val2)

val3 <- xpathApply(doc2, 
                   '/html/body/aside/div[1]/main/section[2]/section/div[2]/div[1]/div[4]',
                   fun = function(x) gsub("\n|\t|\r", " " ,xmlValue(x)))

val3 <- rm_white(val3)

price <- xpathApply(doc2, "/html/body/aside/div[1]/main/section[2]/section/div[2]/div[1]/div[2]/div[1]/div",
                    fun = function(x) gsub("\n|\t", " ", xmlValue(x)))

price <- rm_white(price)


int <- xpathApply(doc2, '//*[@id="Interior>"]',fun = function(x) gsub("\n", " ", xmlValue(x)))

int <- rm_white(int)

int <- unlist(strsplit(int, "(?<=[^\a]Front)", perl = TRUE))

ext <- xpathApply(doc2, '//*[@id="exterior>"]',fun = function(x) gsub("\n", " ", xmlValue(x)))

ext <- rm_white(ext)

ext <- unlist(strsplit(ext, "(?<=[^\a]Wheels)", perl = TRUE))
date <- xpathApply(doc2, '/html/body/aside/div[1]/main/section[2]/section/div[2]/div[2]/div[2]/div',
                   fun = function(x) gsub("\n|\t", " ", xmlValue(x)))
date <- rm_white(date)

# date <- unlist(strsplit(date, "(?<=[^\a]:)", perl = TRUE))[2]

#carmudi ID 

carid <- unlist(strsplit(date, "(?<=[^\a]:)", perl = TRUE))[3]
#for date
date <- unlist(strsplit(date, "(?=[^\a]Listing)", perl =  TRUE))[1]

#good date
date <- unlist(strsplit(date, "(?<=[^\a]:)", perl  = TRUE))[2]

#split split without removing the delimiter
#http://stackoverflow.com/questions/13511869/split-a-string-but-keep-the-delimiter?rq=1

val2 <- as.data.frame(unlist(strsplit(val2, "(?=[^\a]Doors)|(?=[^\a]Type)", perl=TRUE)))
colnames(val2)[1] <- "value"

val2$main <- as.data.frame(gsub("([A-Za-z]+).*", "\\1", val2$value))

val2$value <-  gsub("^\\s*\\w*", "", val2$value)

val2 <- val2[!val2$value =="",] 

val2[2,1] <- as.numeric(as.character(str_extract_all(val2,"\\(?[0-9]+\\)?")[[1]]))

colnames(val2)[2] <- "feature"

val2 <- val2[,c(2,1)]

#extract kilometers
km <- unlist(strsplit(val3, "(?<=[^\a]Km) | (?=[^\a+]Report)", perl = TRUE))[1]

#extract other features
var3 <- unlist(strsplit(val3, "(?<=[^\a]Km) | (?=[^\a+]Report)", perl = TRUE))[2]


#remove the reduntant words

var3 <- unlist(strsplit(var3, "(?<=[^\a+]cc)", perl = TRUE))[1]
#for transmission
trans <- stri_split_boundaries(var3, n = -2L, simplify = TRUE)[1]
#for fuel type
fuel_type <- stri_split_boundaries(var3, n = -2L, simplify = TRUE)[2]

#for horse power
horse_power <- stri_split_boundaries(var3, n = -2L, simplify = TRUE)[3]


# Combine all the features

feature2 <- data.frame(feature = c("carid", "date", ext[1], "fuel_type",
                                   "horse_power", int[1],"km", "price",
                                   "transmission"), value = c(carid, date,
                                    ext[2], fuel_type, horse_power,int[2],
                                    km, price, trans))


#cleanup the colnames in val2
feature <- data.frame(val2[,1])
colnames(feature)[1] <- "feature"

val2$feature <- feature$feature


#name 

name <- unlist(xpathApply(doc2,'/html/body/aside/div[1]/main/section[2]/section/div[2]/div[1]/div[1]/span',
                          fun = function(x) gsub("\n|\t", " ", xmlValue(x))))

#row bind all factors

all_feature <- rbind.data.frame(val2, feature2)
