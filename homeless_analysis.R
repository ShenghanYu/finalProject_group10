
#install.packages("leaflet")
#library("leaflet")
require(leaflet)
require(plotly)
require(dplyr)
setwd("/Users/vishaljuneja/Desktop/EDAV/EDAV_Final/")


#df = read.csv("311_Service_Requests_from_2010_to_Present-3.csv", strip.white=TRUE)
#df_manhattan = df[grep("manhattan", df$Borough, ignore.case = TRUE),]
dfh = df[grep("homeless", df$Complaint.Type, ignore.case = TRUE), ]


library(ggmap)
theme_set(theme_bw())
#qmplot(Longitude, Latitude, data=dfh[1:100,])


dfh$year = as.numeric(format(as.Date(dfh$Created.Date, format= "%m/%d/%Y"), "%Y"))
dfh$month = as.numeric(format(as.Date(dfh$Created.Date, format= "%m/%d/%Y"), "%m"))

qmplot(Longitude, Latitude, data=dfh, extent="panel") + facet_wrap(~year)

qmplot(Longitude, Latitude, data=dfh[dfh$year=="2015",], extent="panel") + facet_wrap(~month)


dfh2015 = dfh[dfh$year=="2015",]

leaflet(dfh2015) %>% 
  addTiles() %>% 
  addMarkers(lng=~Longitude, lat=~Latitude, clusterOptions=markerClusterOptions(), popup="homeless")

dfh[dfh$year=="2013",] %>%
  group_by(month) %>%
  summarise(total=n())

year_wise = dfh %>%
            group_by(year) %>%
            summarise(total=n())

dfh2015jan = dfh2015[dfh2015$month == 1,]
dfh2015aug = dfh2015[dfh2015$month == 8,]

qmplot(Longitude, Latitude, data=dfh2015jan, geom = c("point","density2d"))
qmplot(Longitude, Latitude, data=dfh2015[dfh2015$month == 8,] , geom = c("point","density2d"))

monthwise = dfh2015 %>%
            group_by(month) %>%
            summarise(total=n())

g3 = ggplot(data=monthwise) + geom_bar(aes(x=reorder(month_name, month), y=total), stat="identity", fill="blue", alpha=0.5)
g3 = g3 + labs(title="Monthwise Complaints for 2015", x="Month", y="Complaints")
g3

g3 = ggplot(data=year_wise) + geom_bar(aes(x=year, y=total), stat="identity", fill="purple", alpha=0.5)
g3 = g3 + labs(title="Yearwise Complaints", x="Year", y="Complaints")
g3






