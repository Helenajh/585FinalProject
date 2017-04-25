## set working directory as the project direcctory 
## This code is used to pre-processed the datasets downloaded from https://beacon.schneidercorp.com
## filtering by number of bedrooms upto 18 months 
## output: clearned csv file StoryCountyIA-NEW_house-LatLong.csv into data folder 
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)
library(tidyverse)

##### Data Processing
## Combine datasets with different bedrooms
z_bed <- read.csv("./data/rawData/0bed.csv", stringsAsFactors = FALSE)
one_bed <- read.csv("./data/rawData/1bed.csv", stringsAsFactors = FALSE)
two_bed <- read.csv("./data/rawData/2beds.csv", stringsAsFactors = FALSE)
three_bed <- read.csv("./data/rawData/3beds.csv", stringsAsFactors = FALSE)
four_bed <- read.csv("./data/rawData/4beds.csv", stringsAsFactors = FALSE)
five_bed <- read.csv("./data/rawData/5beds.csv", stringsAsFactors = FALSE)
six_bed <- read.csv("./data/rawData/6beds.csv", stringsAsFactors = FALSE)
s_e_bed <- read.csv("./data/rawData/7&8beds.csv", stringsAsFactors = FALSE)

house <- do.call("rbind", list(z_bed, one_bed, two_bed, three_bed, four_bed, five_bed,
                             six_bed, s_e_bed))

## Get new variable month from Sale.Date
dates  = mdy_hms(house$Sale.Date)

days = data.frame(data = dates,
                  year = as.numeric(format(dates, format = "%Y")),
                  month = as.numeric(format(dates, format = "%m")),
                  day = as.numeric(format(dates, format = "%d"))
)
head(days)
head(dates)

new_house = cbind(house, days)
head(new_house)

## Get rid of year 2015 obs
new_house <- new_house %>% filter(year == 2016)

colnames(new_house)[19] <- "Date"

## Calculate house age, new variable age
new_house$age <- 2016 - new_house$Year.Built

## Creat new variable class, which make ages into 13 groups
new_house$class <- "no information"

for (i in 1:length(new_house$age)) {
  if (any(new_house$age[i] > 100 & new_house$age[i] < 200)) {
    new_house$class[i] <- "over 100 years"
  } 
  if (any(new_house$age[i] > 90 & new_house$age[i] <= 100)) {
    new_house$class[i] <- "91~100 years"
  }
  if (any(new_house$age[i] > 80 & new_house$age[i] <= 90)) {
    new_house$class[i] <- "81~90 years"
  }
  if (any(new_house$age[i] > 70 & new_house$age[i] <= 80)) {
    new_house$class[i] <- "71~80 years"
  }
  if (any(new_house$age[i] > 60 & new_house$age[i] <= 70)) {
    new_house$class[i] <- "61~70 years"
  }
  if (any(new_house$age[i] > 50 & new_house$age[i] <= 60)) {
    new_house$class[i] <- "51~60 years"
  }
  if (any(new_house$age[i] > 40 & new_house$age[i] <= 50)) {
    new_house$class[i] <- "41~50 years"
  }
  if (any(new_house$age[i] > 30 & new_house$age[i] <= 40)) {
    new_house$class[i] <- "31~40 years"
  }
  if (any(new_house$age[i] > 20 & new_house$age[i] <= 30)) {
    new_house$class[i] <- "21~30 years"
  }
  if (any(new_house$age[i] > 10 & new_house$age[i] <= 20)) {
    new_house$class[i] <- "11~20 years"
  }
  if (any(new_house$age[i] > 0 & new_house$age[i] <= 10)) {
    new_house$class[i] <- "1~10 years"
  }
  if (any(new_house$age[i] == 0)) {
    new_house$class[i] <- "new"
  }
  else {
    new_house$class[i] <- new_house$class[i]
  }
}

## Get Lat Long from Address 
## ref:http://stackoverflow.com/questions/32504880/street-address-to-geolocation-lat-long
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.5)  # API only allows 5 requests per second
  out
}

## adding latitude & longitude from address -- takes a little bit of  time to run 
getGeo <- matrix(NA, ncol = 2, nrow = nrow(new_house))
for (i in 1:nrow(new_house)) {
  cat("i is ", i, " and address is ", new_house$Address[i], "\n")
  getGeo[i, ] <- geocodeAdddress(paste0(new_house$Address[i], ", StoryCounty, IA"))
}

new_house$long <- getGeo[, 1]
new_house$lat <- getGeo[, 2]

## write data set 
write.csv(new_house, file = "./data/StoryCountyIA-NEW_house-LatLong.csv",
          quote = F, row.names = F)