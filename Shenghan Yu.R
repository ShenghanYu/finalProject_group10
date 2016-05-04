setwd("D:/edav_final")

library(devtools)
install_github('arilamstein/choroplethrZip@v1.5.0')
library(choroplethr)
library(choroplethrZip)


?df_pop_zip
data(df_pop_zip)
data(zip.regions)


nyc_fips = c(36005, 36047, 36061, 36081, 36085)

df_general <- zip_code
colnames(df_general)[1] <- 'region'
colnames(df_general)[2] <- 'value'
for (i in 1:dim(zip_code)[1]){
  df_general[i,2] = general_stats$x[general_stats$Category == zip_code[i,2]]
}

df_general$region <- as.character(df_general$region)

df_general <- df_general[df_general$region!= 11695,]
df_general <- df_general[df_general$region!= 10317,]

zip_choropleth(df_general,
               county_zoom=nyc_fips,
               title="2008-2012 General Health Condition in NYC",
               legend="General Health Condition")


df_mental <- zip_code
colnames(df_mental)[1] <- 'region'
colnames(df_mental)[2] <- 'value'
for (i in 1:dim(zip_code)[1]){
  df_mental[i,2] = mental_stats$x[mental_stats$Category == zip_code[i,2]]
}

df_mental$region <- as.character(df_mental$region)
df_mental$value <- 2 - df_mental$value
df_mental <- df_mental[df_mental$region!= 11695,]
df_mental <- df_mental[df_mental$region!= 10317,]

zip_choropleth(df_mental,
               county_zoom=nyc_fips,
               title="2008-2012 Average Mental Healthn in NYC",
               legend="Mental Health Condition")


library(ggplot2)

choro = ZipChoropleth$new(df_general)
choro$title = "2008-2012 General Health Condition in NYC"
choro$ggplot_scale = scale_fill_brewer(name="Health Condition (1 Best - 5 Worst)", palette=3, drop=FALSE)
choro$set_zoom_zip(state_zoom="new york", county_zoom=nyc_fips, msa_zoom=NULL, zip_zoom=NULL)
choro$render()


######################################################


library(ggplot2)
test = 
  
health <- read.csv("output_health.csv")
zip_code <- read.csv("zipcode_list.csv")

hist(health$borough)
hist(health$generalhealth)
hist(health$uhf34)
hist(health$year)

library(ggmap)
mymap <- get_map(location = "New York", maptype = "roadmap")
ggmap(mymap)

health<- health[!is.na(health$generalhealth),]
health<- health[!is.na(health$mentalillness),]
health<- health[!is.na(health$borough),]
health<- health[!is.na(health$uhf34),]

general_stats = aggregate(health$generalhealth, by=list(Category=health$uhf34), FUN=mean)
mental_stats = aggregate(health$mentalillness, by=list(Category=health$uhf34), FUN=mean)















ggplot(general_stats, aes(x = Category, y = x)) +
        geom_histogram(stat="identity", binwidth = 10,bins = 35)

ggplot(health, aes(year, fill = borough)) +
  geom_bar()

ggplot(health, aes(x = generalhealth)) +
  geom_histogram()

qplot()


