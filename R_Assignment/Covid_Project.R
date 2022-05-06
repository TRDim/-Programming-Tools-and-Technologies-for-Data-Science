library(data.table);
library(lubridate);
library(rworldmap);
library(tmap);
library(ggplot2);
library(grid);
library(dplyr);
library(tidyr);
library(ggrepel);
library(tidyverse);
library(scales);



#Dataset Saved in documents until 31/12/2020
Covid_1 = fread('C:/Users/DTryfonopoulos/Downloads/global_cases_covid19.csv') 
Covid_2 = fread('C:/Users/DTryfonopoulos/Downloads/global_deaths_covid19.csv')

numCountries = length(unique(Covid_1$Country))
numDays = length(unique(Covid_1$date))
# 1.Remove columns with names Province, State, Lat and Long 
Covid_2[, c("Province/State", "Lat","Long"):=NULL]
Covid_1[, c("Province/State","Lat","Long"):=NULL]

# 2.Convert data from wide to long format 
Covid_2 = melt(Covid_2)
Covid_1 = melt(Covid_1)

# 3.Rename variable Country/Region to Country 
setnames(Covid_1, 'Country/Region','Country')
setnames(Covid_2, 'Country/Region','Country')

# 4.Name the variable with the cumulative confirmed cases as confirmed and 
# the variable with the cumulative number of deaths as deaths 
setnames(Covid_1, 'value','confirmed')
setnames(Covid_2, 'value','deaths')

setnames(Covid_1, 'variable','date')
setnames(Covid_2, 'variable','date')

# 5.Convert the variable date from character to a date object (mdy()).
#library(lubridate)
Covid_1$date = mdy(Covid_1$date)
Covid_2$date = mdy(Covid_2$date)

# 6.Group by coutry and date 
Covid_1 <- Covid_1[,lapply(.SD,sum), 
                   by = .(Country,date),.SDcols = c('confirmed')] 
Covid_2 <- Covid_2[,lapply(.SD,sum), 
                   by = .(Country,date),.SDcols = c('deaths')] 

# 7.Merge the 2 datasets into one
Covid_All = merge(Covid_1,Covid_2)

# 8.Calculate counts (confirmed and deaths) for the whole world.
#All Confirmed Cases & Deaths for each day 
totalPday = Covid_All[,lapply(.SD,sum), 
                      by = .(date),.SDcols = c('confirmed','deaths')] 

#Dataset with variables in q10 
TheDataset = merge(Covid_All,totalPday, by='date')

# 9.Sort again by Country and date 
answer_9 = Covid_All[order(Country,date),]

# 10. Create: confirmed.ind & deaths.inc with the daily confirmed cases 
# and daily deaths respectively  (lag()?)

namesOld =c('confirmed.x','deaths.x', 'confirmed.y', 'deaths.y')
namesNew = c('confirmed','deaths', 'confirmed.ind', 'deaths.inc')

setnames(TheDataset, namesOld,namesNew)


NewDataset= TheDataset 
NewDataset[Country=='United States of America']$Country= "United States"     
NewDataset[Country=='North Macedonia']$Country= "Macedonia"  
NewDataset[Country=='Czechia']$Country = 'Czech Republic'

#NewDataset$'DailyConf' = NewDataset[,confirmed/confirmed.ind *100,]
#NewDataset$'DailyDeaths' = NewDataset[,deaths/deaths.inc *100,]
######################################################################
######################################################################
# Match Countries with Continents (CSV downladed from the web)
continents = fread('C:/Users/DTryfonopoulos/Downloads/CountriesContinents.csv')
Country = data.frame(t(do.call("cbind", 
                               strsplit(as.character(continents$Country_Name),
                                                 ",", fixed = TRUE))))
continents$Country = data.frame(Country$X1)
continent = select(continents, Continent_Name, Country)
# Remove Turkey from being in Europe and Asia (leave only input in Asia)
continent=continent[-c(235),] 
continent[Country=='United States of America']$Country= "United States" 

DatasetCont = merge(NewDataset, continent, by='Country')

######################################################################
######################################################################

# Create Column Deaths & Confirmed Cases per day
DatasetCont$DailyConfirmed = DatasetCont$confirmed - 
  lag(DatasetCont$confirmed,1)
DatasetCont$DailyDeaths = DatasetCont$deaths - lag(DatasetCont$deaths,1)

# Deaths & Confirmed Cases  Replace NA and <0 Numbers 
DatasetCont$'DailyConfirmed'[is.na(DatasetCont$'DailyConfirmed')] = 0 
DatasetCont$'DailyConfirmed'[(DatasetCont$'DailyConfirmed')<0] = 0 
DatasetCont$'DailyDeaths'[is.na(DatasetCont$'DailyDeaths')] = 0 
DatasetCont$'DailyDeaths'[(DatasetCont$'DailyDeaths')<0] = 0 

# Separate DATE (Y-M-D)
DatasetContDATES = separate(DatasetCont, "date", c("Year", "Month", "Day"), sep = "-")


######################################################################
######################## START ANALYSIS ##############################
######################################################################

# All Conf & Deaths per Country for the whole Year 
DatasetCont=as.data.table(DatasetCont)
ConfDeathPerCountry =(DatasetCont[, lapply(.SD,sum),
                                  by = .(Country,Continent_Name ),
                                  .SDcols = c('DailyConfirmed', 'DailyDeaths')])
ConfDeathPerCountry = ConfDeathPerCountry %>% 
  rename( AnnualConfirmed = DailyConfirmed ,AnnualDeaths = DailyDeaths)

ConfDeathPerCountryPerMonth = (DatasetContDATES[, lapply(.SD,sum),
                                                by = .(Country,Month ),
                                                .SDcols = 
                                                  c('DailyConfirmed', 'DailyDeaths')])
######################################################################
# Confirmed-Deaths Per Continent
 
ConfDeathEurope = ConfDeathPerCountry[Continent_Name =='Europe']
ConfDeathAsia = ConfDeathPerCountry[Continent_Name =='Asia']
ConfDeathAfrica = ConfDeathPerCountry[Continent_Name =='Africa']
ConfDeathNAmerica = ConfDeathPerCountry[Continent_Name =='North America']
ConfDeathSAmerica = ConfDeathPerCountry[Continent_Name =='South America']
ConfDeathOceania = ConfDeathPerCountry[Continent_Name =='Oceania']

# TOP 8 Countries/Continent Confirmed % Deaths 
#We will keep the top3 and top8 countries 

#Europe
TopConfEurope = ConfDeathEurope[order(-AnnualConfirmed)]%>% head(3)
TopDeathEurope = ConfDeathEurope[order(-AnnualDeaths)]%>% head(3)

TopConfEurope2 = ConfDeathEurope[order(-AnnualConfirmed)]%>% head(8)
TopDeathEurope2 = ConfDeathEurope[order(-AnnualDeaths)]%>% head(8)
#Asia
TopConfAsia = ConfDeathAsia[order(-AnnualConfirmed)]%>% head(3)
TopDeathAsia = ConfDeathAsia[order(-AnnualDeaths)]%>% head(3)

TopConfAsia2 = ConfDeathAsia[order(-AnnualConfirmed)]%>% head(8)
TopDeathAsia2 = ConfDeathAsia[order(-AnnualDeaths)]%>% head(8)
#Africa
TopConfAfrica = ConfDeathAfrica[order(-AnnualConfirmed)]%>% head(3)
TopDeathAfrica = ConfDeathAfrica[order(-AnnualDeaths)]%>% head(3)

TopConfAfrica2 = ConfDeathAfrica[order(-AnnualConfirmed)]%>% head(8)
TopDeathAfrica2 = ConfDeathAfrica[order(-AnnualDeaths)]%>% head(8)
#NAmerica
TopConfNAmerica = ConfDeathNAmerica[order(-AnnualConfirmed)]%>% head(3)
TopDeathNAmerica = ConfDeathNAmerica[order(-AnnualDeaths)]%>% head(3)

TopConfNAmerica2 = ConfDeathNAmerica[order(-AnnualConfirmed)]%>% head(8)
TopDeathNAmerica2 = ConfDeathNAmerica[order(-AnnualDeaths)]%>% head(8)
#SAmerica
TopConfSAmerica = ConfDeathSAmerica[order(-AnnualConfirmed)]%>% head(3)
TopDeathSAmerica = ConfDeathSAmerica[order(-AnnualDeaths)]%>% head(3)

TopConfSAmerica2 = ConfDeathSAmerica[order(-AnnualConfirmed)]%>% head(8)
TopDeathSAmerica2 = ConfDeathSAmerica[order(-AnnualDeaths)]%>% head(8)

#Oceania
TopConfOceania = ConfDeathOceania[order(-AnnualConfirmed)]%>% head(3)
TopDeathOceania = ConfDeathOceania[order(-AnnualDeaths)]%>% head(3)

TopConfOceania2 = ConfDeathOceania[order(-AnnualConfirmed)]%>% head(8)
TopDeathOceania2 = ConfDeathOceania[order(-AnnualDeaths)]%>% head(8)

# TOP 3 Countries - Confirmed Cases 
worldTopConf = rbind(TopConfEurope, TopConfAsia, TopConfAfrica, TopConfNAmerica,
                     TopConfSAmerica, TopConfOceania)

# TOP 3 Countries - Deaths Cases 
worldTopDeath = rbind(TopDeathEurope, TopDeathAsia, TopDeathAfrica, TopDeathNAmerica, 
                      TopDeathSAmerica, TopDeathOceania)

#GRAPH 1 : 3 Top Countries/Continent Conf&Deaths  "Rplot01"
g1=ggplot(worldTopConf, aes(Continent_Name, AnnualConfirmed, size=AnnualDeaths)) +
geom_point(aes(color=Country))+ geom_text_repel( aes(label = AnnualDeaths),angle=270,
                                                 colour='red', hjust='left') +
labs(title = 'Top 3 Conf&Deaths - Countries/Continent Annualy',
subtitle = 'Horizontal Number: Confirmed
     Vertical Number: Deaths ', x= 'Continent', y = ' Annual Confirmed Cases') +
geom_text(aes(label =AnnualConfirmed ),check_overlap = TRUE,hjust='right')

#######################################################################
########################################################################
# Top Effected Countries Worldwide
MONTHS= unique(ConfDeathPerCountryPerMonth$Month) # List with all the months 

rm(df)
k=1;
Country = vector(mode = "list", length = 12)
DailyConfirmed =vector(mode = "numeric", length = 12)
DailyDeaths = vector(mode = "numeric", length = 12)

for (i in MONTHS){
  
  Country[k] =ConfDeathPerCountryPerMonth[Month==i][order(-DailyConfirmed)][1]$Country
  DailyConfirmed[k] =
    ConfDeathPerCountryPerMonth[Month==i][order(-DailyConfirmed)][1]$DailyConfirmed
  DailyDeaths[k] =
    ConfDeathPerCountryPerMonth[Month==i][order(-DailyConfirmed)][1]$DailyDeaths
  k=k+1
}

WorldTopConfPerMonth <- data.frame(Country=character(12), DailyConfirmed=numeric(12),
                                   DailyDeaths=numeric(12), stringsAsFactors=FALSE) 
WorldTopConfPerMonth$Country = Country
WorldTopConfPerMonth$DailyConfirmed =as.matrix(DailyConfirmed)
WorldTopConfPerMonth$DailyDeaths = DailyDeaths
WorldTopConfPerMonth$Month = MONTHS

WorldTopConfPerMonth = melt(as.data.table(WorldTopConfPerMonth))
dat_long <- WorldTopConfPerMonth %>% gather("Variable", "Value", Month)

#GRAPH_2: World Top Conf&Deaths - Countries Monthy    'Rplot02' 
g2 =ggplot(WorldTopConfPerMonth, aes(x=Month, y=value, fill = variable)) +
geom_bar(position = "dodge", stat = "identity") +
geom_label(data = WorldTopConfPerMonth, aes(label = Country), vjust = -0.5)+
labs(title = "World Top Conf&Deaths - Countries Monthy", subtitle = '2020',
     x = "Month", y = "Cases", color = "Country")+
geom_text(aes(label =value ),check_overlap = TRUE,hjust='bottom')

######################################################################
########################################################################
# All Conf & Deaths per Country per Month 
ConfDeathPerCountryPerMonth= DatasetContDATES[, lapply(.SD,sum),
                                              by=.(Country, Continent_Name, Month),
                                              .SDcols = 
                                                c('DailyConfirmed', 'DailyDeaths')]

#Europe
EuropeMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='Europe']
EuropeMonthly= merge(EuropeMonthlyAll,TopConfEurope2, by='Country' )
# Conf
c_EU=ggplot(EuropeMonthly, aes(x = Month, y=Country)) + 
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in Europe per Month')
# Deaths
d_EU=ggplot(EuropeMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths))+
  labs(title='Death Cases:', subtitle='Top 8 Countries in Europe per Month')
######################################################################
#Asia
AsiaMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='Asia']
AsiaMonthly= merge(AsiaMonthlyAll,TopConfAsia2, by='Country' )
# Conf
c_AS=ggplot(AsiaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in Asia per Month')
# Deaths
d_AS=ggplot(AsiaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths))+
  labs(title='Deaths Cases:', subtitle='Top 8 Countries in Asia per Month')
######################################################################
#Africa
AfricaMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='Africa']
AfricaMonthly= merge(AfricaMonthlyAll,TopConfAfrica2, by='Country' )
# Conf
c_AF=ggplot(AfricaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in Africa per Month')
# Deaths
d_AF=ggplot(AfricaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths)) +
  labs(title='Deaths Cases:', subtitle='Top 8 Countries in Africa per Month')
######################################################################
#NAmerica
NAmericaMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='North America']
NAmericaMonthly= merge(NAmericaMonthlyAll,TopConfNAmerica2, by='Country' )
# Conf
c_NA=ggplot(NAmericaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in N.America per Month')
# Deaths
d_NA=ggplot(NAmericaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths)) +
  labs(title='Death Cases:', subtitle='Top 8 Countries in N.America per Month')
######################################################################
#S America
SAmericaMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='South America']
SAmericaMonthly=merge(SAmericaMonthlyAll,TopConfSAmerica2, by='Country' )
# Conf
c_SA=ggplot(SAmericaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in S.America per Month')
# Deaths
d_SA=ggplot(SAmericaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths))+
  labs(title='Death Cases:', subtitle='Top 8 Countries in S.America per Month')
######################################################################
#Oceania
OceaniaMonthlyAll=ConfDeathPerCountryPerMonth[Continent_Name=='Oceania']
OceaniaMonthly=merge(OceaniaMonthlyAll,TopConfOceania2, by='Country' )
# Conf
c_OC=ggplot(OceaniaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyConfirmed))+
  labs(title='Confirmed Cases:', subtitle='Top 8 Countries in Oceania per Month')
# Deaths
d_OC=ggplot(OceaniaMonthly, aes(x = Month, y=Country))+
  geom_point(aes(col=Country,size=DailyDeaths))+
  labs(title='Death Cases:', subtitle='Top 8 Countries in Oceania per Month')

                      
######################################################################
######################################################################
######################################################################
#Greece Analysis 
GreecePerMonth = data.frame(DatasetContDATES[Country== 'Greece', lapply(.SD,sum),
                                             by = .(Month), .SDcols =
                                               c('DailyConfirmed', 'DailyDeaths')])
GreecePerMonth = GreecePerMonth %>% rename(MonthlyConfirmed = DailyConfirmed,
                                           MonthlyDeaths =DailyDeaths)
GreecePerMonth = melt(as.data.table(GreecePerMonth))
# PLOT GREECE Monthly
GR_Month01=ggplot(GreecePerMonth, aes(x = Month, y = value)) + 
  geom_point(aes(color = variable)) + 
  labs(title="Monthly Confirmed & Death Cases", 
       subtitle="Greece 2020",
       caption = 'RedNumbers:Deaths/Month')+
  geom_text(data=(GreecePerMonth[variable=='MonthlyDeaths']),
            aes(label = value, angle =0, hjust='bottom', color='MonthlyDeaths' ))
GR_Monthly=GR_Month01 + geom_text(data=(GreecePerMonth[variable=='MonthlyConfirmed']),
                       aes(label = value, angle =0, hjust='bottom', 
                           color='MonthlyConfirmed' ))

# PLOT GREECE Daily
GreecePerDay = data.frame(DatasetContDATES[Country== 'Greece', lapply(.SD,sum),
                                           by = .(Day, Month),
                                           .SDcols = 
                                             c('DailyConfirmed', 'DailyDeaths')])
GreecePerDay = melt(as.data.table(GreecePerDay))

# Mean Confirmed & Deaths per Month 
GreeceMeanCnD = data.frame(DatasetContDATES[Country== 'Greece', lapply(.SD,mean),
                                            by = .(Month),
                                            .SDcols = 
                                              c('DailyConfirmed', 'DailyDeaths')])
GreeceMeanCnD = GreeceMeanCnD %>% 
  rename(MeanConfirmed  = DailyConfirmed ,  MeanDeaths=DailyDeaths)
GreeceMeanCnD[,-1] <-round(GreeceMeanCnD[,-1],2) # Round the values 


# PLOT GREECE per Day
plotGr=ggplot(GreecePerDay, aes(x = Day, y = value)) + 
  geom_point(aes(color = variable)) + 
  labs(title="Daily Confirmed & Death Cases", subtitle="Greece 2020",
       caption = 'Green: Mean Confirmed per Day
       Red:Mean Deaths per Day', x='All Days per Month',
       y= 'Number of Cases')+
    facet_grid(~ Month)+
    theme(axis.text.x = element_blank())
  plotGr2=plotGr + geom_text(x=5, y=2000, aes(label = MeanConfirmed ),
                             data = GreeceMeanCnD, angle=90,color='green')
  GR_Daily=plotGr2 +geom_text(x=20, y=2000, aes(label = MeanDeaths ),
                              data = GreeceMeanCnD, angle=90, color='red')
  
########################################################################
########################################################################
  
# Create Maps with data  
allconts = unique(DatasetCont$Continent_Name)
for (j in allconts){
  
  worldMap <- getMap()
  
  # Find the Countries of each Continent
  EuropeCountries = unique(DatasetCont[c(Continent_Name==j)]$Country)
  indEU <- which(worldMap$NAME%in%EuropeCountries)
  
  
  # Keep The coordinates for each Country in the Map 
  europeCoords <- lapply(indEU, function(i){
    df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
    df$region =as.character(worldMap$NAME[i])
    colnames(df) <- list("long", "lat", "region")
    return(df)
  })
  
  europeCoords <- do.call("rbind", europeCoords)
  ########################################################################
  # Create data.frame for Confirmed and Deaths
  AnnualConfirmed = ConfDeathPerCountry[Continent_Name==j]$AnnualConfirmed
  AnnualDeaths = ConfDeathPerCountry[Continent_Name==j]$AnnualDeaths
  
  ######
  europeanUnionTable <- data.frame(country = EuropeCountries,
                                   AnnualConfirmed, AnnualDeaths)
  europeanUnionTable=europeanUnionTable %>% rename(region = country,)
  
  europeCoords = merge(europeCoords,europeanUnionTable, by= 'region')
  
  # Change fill AnnualDeaths to calculate deaths
  P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat,
                                                        group = region,
                                                        fill = AnnualConfirmed),
                               colour = "black", size = 0.1)
    
  P<- P + labs(title="Annual Confirmed Cases")
  #P<- P + labs(title="Annual Death Cases")
               
  
  #These Colors for Confirmed
  P <- P + scale_fill_gradient(name = "Annual Confirmed", 
                               high = "#FF0000FF", low = "#FFFF00FF", na.value ="grey50",
                               labels=comma)
  
  #These Colors for Deaths 
  #P <- P + scale_fill_gradient(name = "Annual DeaTHS", 
   #                            low = "#FFFFFF", high = "#000000", na.value = "grey50",
    #                           labels=comma)
  
  
  P <- P + theme(axis.text.x = element_blank(),
    axis.text.y = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(), axis.title = element_blank(),
    plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
  assign(paste("Map_C_", j, sep = ""), P) 
}

########################################################################  
########################################################################
########################################################################
# ALL FIGURES:
  print('GRAPH 1 : 3 Top Countries/Continent Conf&Deaths');g1
  
  print('GRAPH_2: World Top Conf&Deaths - Countries Monthy');g2
  
  print('Top8 Counties per Continent - Confirmed Cases')
  print('Europe Conf'); c_EU
  print('Asia Conf'); c_AS
  print('Africa Conf'); c_AF
  print('N.America Conf'); c_NA
  print('S.America Conf'); c_SA
  print('Oceania Conf'); c_OC
  
  print('Top8 Counties per Continent - Deaths Cases')
  print('Europe Deaths'); d_EU
  print('Asia Deaths'); d_AS
  print('Africa Deaths'); d_AF
  print('N.America Deaths'); d_NA
  print('S.America Deaths'); d_SA
  print('Oceania Deaths'); d_OC
  
  print('GREECE: Monthly Confirmed & Death Cases'); GR_Monthly
  print('GREECE: Daily Confirmed & Death Cases'); GR_Daily
  
  print('WorldMaps: Annual Confirmed Cases')
  Map_C_Europe;Map_C_Asia;Map_C_Africa;`Map_C_North America`;`Map_C_South America`;Map_C_Oceania;
  print('WorldMaps: Annual Death Cases')
  Map_D_Europe;Map_D_Asia;Map_D_Africa;`Map_D_North America`;`Map_D_South America`;Map_D_Oceania