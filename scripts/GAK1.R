library(ncdf4)
library(zoo)
library(gplots)
library(dplyr)
library(maps)
library(mapdata)
library(chron)
library(fields)
library(tidyr)
library(nlme)
library(ggplot2)
library(oce)

# now GAK1 salinity....
data <- read.csv("./data/GAK1.csv")

str(data)

data$year <- floor(data$dec.yr)

require(lubridate)
data$day <- yday(date_decimal(data$dec.yr)) # get Julian date

sal.set <- as.data.frame(tapply(data$Sal, list(data$dec.yr, data$Depth), mean))
colnames(sal.set) <- c("d0", "d10", "d20", "d30", "d50", "d75", "d100", "d150", "d200", "d250")

sal.set$year <- floor(as.numeric(rownames(sal.set))) 
sal.set$day <- yday(date_decimal(as.numeric(rownames(sal.set))))

unloadNamespace("lubridate")

salFMA <- filter(sal.set, day>31 & day <=120)

sal20mu <- tapply(salFMA$d20, salFMA$year, mean, na.rm=T)
plot(names(sal20mu), sal20mu, type="b")

gak <- data.frame(year = names(sal20mu),
                  salinity20m_FMA = sal20mu)

write.csv(gak, "./data/GAK_salinity.csv", row.names = F)
