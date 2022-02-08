library(data.table)
library(WDI)
library(COVID19)
library(ggplot2)
library(stargazer)
library(plm)

country_info <- WDI(indicator = "NY.GDP.PCAP.PP.CD",
                    start = 2020,
                    end = 2020)
setDT(country_info)
cinfo <- country_info[order(country_info$NY.GDP.PCAP.PP.CD,decreasing = TRUE)]
isoc <- c('XU','V4','XD','OE','XC','EU','Z7','B8','S4','Z4','S1')
for (isocode in isoc) {
  cinfo <- cinfo[iso2c != isocode]
}
cinfo <- cinfo[1:60]
country_names <- cinfo[['country']]

data <- covid19(country = country_names)
