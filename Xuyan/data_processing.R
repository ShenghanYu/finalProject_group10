setwd("C:/Users/oweni/OneDrive/Documents/GitHub/finalProject_group10")

load("complain.RData")

# find top 5 types
library(dplyr)
type_group = group_by(complain,Complaint.Type)
type_sum = summarize(type_group,cnt = n())
top10 = type_sum[order(type_sum$cnt, decreasing = T),][1:10,]

types = unique(top10[1:5,]$Complaint.Type)

complain_top5 = complain[complain$Complaint.Type %in% types,]
complain_top5_c = complain_top5[!(is.na(complain_top5$Latitude) | is.na(complain_top5$Longitude)),]

# find the sum in zip
zips = read.csv("zip_map.csv")
comp_zip = complain %>% group_by(Incident.Zip) %>% summarize(cnt = n())
zip_sum = comp_zip[order(comp_zip$cnt, decreasing = T),]

# clean zip_sum
zip_sum$Incident.Zip = substr(zip_sum$Incident.Zip,1,5)

# zip_code plots
library(choroplethrZip)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
data(zip.regions)
zip_list = data.frame(zip = unique(zip.regions[zip.regions$county.fips.numeric %in% nyc_fips,][,1]))
zip_list$zip = as.character(zip_list$zip)
zip_summary = left_join(zip_list,zip_sum,by=c("zip"="Incident.Zip"))
zip_summary$cnt[is.na(zip_summary$cnt)] = 0
zip_sum_f = zip_summary %>% group_by(zip) %>% summarize(count = sum(cnt))
zip_sum_f = as.data.frame(zip_sum_f)

# set the data and title
zip_sum_f$value = zip_sum_f[,2]
zip_sum_f$region = as.character(zip_sum_f$zip)
title = "2010-present New York City complain count\n"

# print the map
choro = zip_choropleth(zip_sum_f, title=title, county_zoom=nyc_fips)
print(choro)



# consider population
data(df_zip_demographics)

ny_demo = left_join(zip_list,df_zip_demographics,by=c("zip" = "region"))
comp_per_capita = data.frame(zip = zip_list$zip, avg_comp = zip_sum_f$count/ny_demo$total_population)
comp_per_capita$value = comp_per_capita[,2]
comp_per_capita$region = comp_per_capita$zip

title = "2010-present New York City complain count per person"
choro = zip_choropleth(comp_per_capita, title=title, county_zoom=nyc_fips)
print(choro)

price = read.csv("Xuyan/city_data_clean.csv")
price$region = as.character(price$region)


pprice = price[price$price>0,]
pprice$value = pprice$price

title = "2015-12 home value per sqft"
choro = zip_choropleth(pprice, title=title, county_zoom=nyc_fips)
print(choro)

city_data = left_join(zip_list,price,by=c("zip"="region"))

pprice$avg_metro = pprice$metro/pprice$population
pprice$avg_complain = pprice$complain/pprice$population

ll = lm(data = pprice, price ~ avg_income+avg_complain+avg_metro+crime_rate)
lml =step(ll)

pprice$pred = predict(lml,pprice)
pprice$rate = pprice$pred/pprice$price
summary(pprice$rate)
summary(pprice$avg_complain)
summary(pprice$crime_rate)


candidate = pprice[pprice$rate<0.85 & pprice$crime_rate<median(pprice$crime_rate) & pprice$avg_complain<median(pprice$avg_complain),]
cand = data.frame(region = candidate[order(candidate$price),]$region, value = 1)
cand$region = as.character(cand$region)


title = "underrated and safe communities"
choro = zip_choropleth(cand, title=title, county_zoom=nyc_fips)
print(choro)
