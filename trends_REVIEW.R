##library(devtools)
##dev_mode(on=T)
##devtools::install_github("PMassicotte/gtrendsR")
library(dplyr)
library(ggplot2)
library(gtrendsR)
library(reshape2)
library(changepoint)
library(tibble)
library(tidyr)
library(readr)
library(tidycovid19)
library(scales)
library(curl)
library(tseries)
library(bfast)
library(lubridate)
library(ggpubr)
library(ggsci)
library(dplyr)

# load workspace from 6/30/2020 that contains Google Trends search data
# redundant code that was used to create the workspace is muted below

load("covidgtrendsdata630.RData")

###Case data download and preprocessing
###Coronavirus case data-Johns Hopkins case records
# covidcases<-tidycovid19::download_jhu_csse_covid19_data(cached=TRUE)
# covidcasemarch<-tidycovid19::download_jhu_csse_covid19_data(cached=TRUE)

 # covidcasemarch<-covidcasemarch$country%>%
 #   #dplyr::mutate(date=as.POSIXct(date))%>%
 #   dplyr::group_by(date)%>%dplyr::mutate(tconfirmed=sum(confirmed))%>%
 #   dplyr::filter(date=='2020-03-01')%>%
 #   filter(confirmed>0)

covidcasesfilt<-covidcases%>%
  #dplyr::mutate(country=as.factor(country))%>%
  #dplyr::mutate(date=as.Date(date))%>%
  #dplyr::group_by(date)%>%
  #dplyr::mutate(confirmed=as.double(confirmed))
  #dplyr::mutate(tconfirmed=sum(confirmed))%>%
  dplyr::filter(country%in% c('US','China','Italy'))%>%
  dplyr::filter(date<="2020-05-01")

covidcaseschina<-covidcasesfilt%>%dplyr::filter(country=='China')
covidcasesitaly<-covidcasesfilt%>%dplyr::filter(country=='Italy')
covidcasesus<-covidcasesfilt%>%dplyr::filter(country=='US')
covidcaseschina$confirmedscale <- rescale(covidcaseschina$confirmed, to = c(0, 100))
covidcasesitaly$confirmedscale <- rescale(covidcasesitaly$confirmed, to = c(0, 100))
covidcasesus$confirmedscale<- rescale(covidcasesus$confirmed, to = c(0, 100))

###Pull Google Trends data from server for 2016-2020

###Reset cookies to get around Google's query limits; requires the curl package
##Add the follow lines of code, either in or out of a loop, before using the gtrends function
##If used in a loop, the code should run in each iteration before the gtrends function
# h <- new_handle()
# req <- curl_fetch_memory("http://apis.google.com/Cookies/OTZ", handle = h)
# handle_cookies(h)


# covidcasemarch<-covidcasemarch$country%>%
#   #dplyr::mutate(date=as.POSIXct(date))%>%
#   dplyr::group_by(date)%>%dplyr::mutate(tconfirmed=sum(confirmed))%>%
#   dplyr::filter(date=='2020-03-01')%>%
#   filter(confirmed>0)
# 
# covidcases<-covidcases$country%>%
#   #dplyr::mutate(date=as.POSIXct(date))%>%
#   dplyr::group_by(date)%>%dplyr::mutate(tconfirmed=sum(confirmed))%>%
#   dplyr::filter(country%in% c('US','China','Italy'))%>%
#   dplyr::filter(date<='2020-05-01')
# 
# covidcaseschina<-covidcases%>%dplyr::filter(country=='China')
# covidcasesitaly<-covidcases%>%dplyr::filter(country=='Italy')
# covidcasesus<-covidcases%>%dplyr::filter(country=='US')
# covidcaseschina$confirmedscale <- rescale(covidcaseschina$confirmed, to = c(0, 100))
# covidcasesitaly$confirmedscale <- rescale(covidcasesitaly$confirmed, to = c(0, 100))
# covidcasesus$confirmedscale<- rescale(covidcasesus$confirmed, to = c(0, 100))

###Pull Google Trends data from server for 2016-2020


#Aggregate Time Series for web searches in whole US (separate by geo region below)
# these data are stored in the .RData file at the top of the script because
# search volume and search queries can be re-normalized by Google leading to slight variation in
# values for each keyword
# script below can be used to directly pull data from the Google Servers
########################################CORONAVIRUS WEB#########################

# trendscv1620 <- gtrends(keyword=c("coronavirus+corona virus+covid"),
#                         geo = "US",
#                         low_search_volume=TRUE,# include low search volume regions
#                         time = "2016-01-01 2020-05-01")

##Interest over time for coronavirus
 cvweb1620<-trendscv1620$interest_over_time%>%
   dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%#assign value '<1' to .5
#for coronavirus alone, we evaluate search volume beginning in Dec. 2019
   dplyr::filter(date>='2019-12-01')

###Convert to time series object
cvweb1620.ts <- ts(cvweb1620$hits,
                   start=c(2016,1),
                   frequency=52
)



cvweb21620<-cvweb1620%>%
  dplyr::rename(web=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcpt=if_else(index%in%cvweb_time,1,0))%>%
  dplyr::select(date,web,webcpt)




# cvrecent <- gtrends(keyword=c("coronavirus+corona virus+covid"),#+sars-cov-2+sars cov 2",
#                     geo = "US",
#                     low_search_volume=TRUE,
#                     time = "2020-01-21 2020-05-01")
#write.csv(trendscv,"trendscv.csv")

cvrecent2 <- gtrends(keyword=c("coronavirus+corona virus+covid"),
                     geo = "US",
                     low_search_volume=TRUE,
                     time = "2020-01-01 2020-05-01")



cvnewsrecent <- gtrends(keyword=c("coronavirus+corona virus+covid"),
                        geo = "US",
                        low_search_volume=TRUE,
                        time = "2020-01-21 2020-05-01",
                        gprop="news"
)

cvwebrecent<-cvrecent$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)%>%mutate(daterev=as.Date(date))

cvwebrecent2<-cvrecent2$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)%>%mutate(daterev=as.Date(date))%>%
  dplyr::mutate(difftrend=c(0,diff(hits)))

cvnewsrecent2<-cvnewsrecent$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)%>%
  dplyr::rename(newshits=hits)

uscovid<-covidcasesus%>%
  select(date,confirmed,confirmedscale)%>%
  #mutate(daterev=as.Date(date))%>%
  ungroup()%>%
  mutate(diffscale=c(0,diff(confirmedscale)))

chinacovid<-covidcaseschina%>%
  select(date,confirmed,confirmedscale)%>%
  #mutate(daterev=as.Date(date))%>%
  ungroup()%>%
  mutate(diffscale=c(0,diff(confirmedscale)))

italycovid<-covidcasesitaly%>%
  select(date,confirmed,confirmedscale)%>%
  #mutate(daterev=as.Date(date))%>%
  ungroup()%>%
  mutate(diffscale=c(0,diff(confirmedscale)))

totalcasesearchus<-left_join(cvwebrecent,covidcasesus,by="date")%>%drop_na()%>%
  dplyr::mutate(deltaconfirmed=confirmed-lag(confirmed, default = 0))%>%
  drop_na()#%>%select(-date.x,-date.y)

totalcasessearchchina<-left_join(cvwebrecent,covidcaseschina,by="date")%>%drop_na()%>%
  dplyr::mutate(deltaconfirmed=confirmed-lag(confirmed, default = 0))%>%
  drop_na()#%>%select(-date.x,-date.y)

totalcasessearchitaly<-left_join(cvwebrecent,covidcasesitaly,by='date')%>%drop_na()%>%
  dplyr::mutate(deltaconfirmed=confirmed-lag(confirmed, default = 0))%>%
  drop_na()#%>%select(-date.x,-date.y)

searchnewsus<-left_join(cvwebrecent,cvnewsrecent2,by="date")

###############Time series crosscorrelation
corrcvUS<-ccf(totalcasesearchus$hits,totalcasesearchus$deltaconfirmed)
#corrcvUSaccel<-ccf(totalcasesearchus$hits,totalcasesearchus$accelconfirmed)
corrcvUScase<-ccf(totalcasesearchus$hits,totalcasesearchus$confirmed)
corrcvCHINA<-ccf(totalcasessearchchina$hits,totalcasessearchchina$deltaconfirmed)
corrcvITALY<-ccf(totalcasessearchitaly$hits,totalcasessearchitaly$deltaconfirmed)
corrnewssearch<-ccf(searchnewsus$hits,searchnewsus$newshits)
#corrcv<-ccf(totalcasesearch$confirmed,totalcasesearch$hits)





#############Apply to taxonomic group search terms
###########################BATS WEB
# 
# trendsbatweb1620 <- gtrends(keyword="bat+bats",
#                             geo = "US",
#                             category="66",#search category refinement
#                             low_search_volume=TRUE,
#                             time = "2016-01-01 2020-05-01")

batweb1620<-trendsbatweb1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)

#need ts representation so convert here
batweb1620.ts <- ts(batweb1620$hits,
                    start=c(2016,1),
                    frequency=52
)

batweb.bfast<-bfast(batweb1620.ts,h=0.05,max.iter=1,season="dummy")
batweb_brkpt <- batweb.bfast$output[[1]]$ci.Vt$confint%>%as.data.frame()
batweb_maxmag<-batweb.bfast$Magnitude
batweb_time<-batweb.bfast$Time
batweb_season<-batweb.bfast$output[[1]]$St%>%as.data.frame()
batweb_trend<-batweb.bfast$output[[1]]$Tt%>%as.data.frame()


plot(
  batweb.bfast,
  type="components",
  ylim=c(3,max(batweb1620$hits)+1),
  #ANOVA=TRUE,
  #largest=TRUE,
  main=""
)

#look at the stl Seasonal-Trend decomposition procedure already in R
#batweb1620.stl <- stl(batweb1620.ts,s.window="periodic")



batweb21620<-batweb1620%>%
  dplyr::rename(web=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcpt=if_else(index%in%batweb_time,1,0))%>%
  dplyr::select(date,web,webcpt)

batusdate<-batweb21620[batweb21620$webcpt == 1, "date"]



###########################Pangolin WEB
# 
# trendspangolinweb1620 <- gtrends(keyword="pangolin+pangolins",
#                                  geo = "US",
#                                  low_search_volume=TRUE,
#                                  time = "2016-01-01 2020-05-01")

pangolinweb1620<-trendspangolinweb1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)



#need ts representation
pangolinweb1620.ts <- ts(pangolinweb1620$hits,
                         start=c(2016,1),
                         frequency=52
)

pangolinweb.bfast<-bfast(pangolinweb1620.ts,h=0.05,max.iter=1,season="none")
pangolinweb_brkpt <- pangolinweb.bfast$output[[1]]$ci.Vt$confint%>%as.data.frame()
pangolinweb_maxmag<-pangolinweb.bfast$Magnitude
pangolinweb_time<-pangolinweb.bfast$Time
pangolinweb_trend<-pangolinweb.bfast$output[[1]]$Tt%>%as.data.frame()


plot(
  pangolinweb.bfast,
  type="components",
  #largest=TRUE,
  #ylim=c(3,max(pangolinweb1620$hits)+1),
  main=""
)

pangolinweb21620<-pangolinweb1620%>%
  dplyr::rename(web=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcpt=if_else(index%in%pangolinweb_time,1,0))%>%
  dplyr::select(date,web,webcpt)

pangolinusdate<-pangolinweb21620[pangolinweb21620$webcpt == 1, "date"]


#############Apply to conservation policy search terms
###########################WILDLIFE TRADE WEB

# trendswildtradeweb1620 <- gtrends(keyword="wildlife trade+bushmeat",
#                                   geo = "US",
#                                   #category="66",
#                                   low_search_volume=TRUE,
#                                   time = "2016-01-01 2020-05-01")

wildtradeweb1620<-trendswildtradeweb1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)



#need ts representation 
wildtradeweb1620.ts <- ts(wildtradeweb1620$hits,
                          start=c(2016,1),
                          frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradeweb.bfast<-bfast(wildtradeweb1620.ts,h=0.05,max.iter=1,season="none")
wildtradeweb_brkpt <- wildtradeweb.bfast$output[[1]]$ci.Vt$confint%>%as.data.frame()
wildtradeweb_maxmag<-wildtradeweb.bfast$Magnitude
wildtradeweb_time<-wildtradeweb.bfast$Time
wildtradeweb_trend<-wildtradeweb.bfast$output[[1]]$Tt%>%as.data.frame()

plot(
  wildtradeweb.bfast,
  type="components",
  #ylim=c(3,max(wildtradeweb1620$hits)+1),
  main=""
)


wildtradeweb21620<-wildtradeweb1620%>%
  dplyr::rename(web=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcpt=if_else(index%in%wildtradeweb_time,1,0))%>%
  dplyr::select(date,web,webcpt)

wildtradeusdate<-wildtradeweb21620[wildtradeweb21620$webcpt == 1, "date"]

###########################CONSERVATION WEB

# trendsconservationweb1620 <- gtrends(keyword="conservation",
#                                      geo = "US",
#                                      category="66",# search category refinement
#                                      low_search_volume=TRUE,
#                                      time = "2016-01-01 2020-05-01")

conservationweb1620<-trendsconservationweb1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))%>%
  dplyr::select(date,hits)

#need ts representation
conservationweb1620.ts <- ts(conservationweb1620$hits,
                             start=c(2016,1),
                             frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationweb.bfast<-bfast(conservationweb1620.ts,h=0.05,max.iter=1,season="dummy")
conservationweb_brkpt <- conservationweb.bfast$output[[1]]$ci.Vt$confint%>%as.data.frame()
conservationweb_maxmag<-conservationweb.bfast$Magnitude
conservationweb_time<-conservationweb.bfast$Time
conservationweb_trend<-conservationweb.bfast$output[[1]]$Tt%>%as.data.frame()
conservationweb_season<-conservationweb.bfast$output[[1]]$St%>%as.data.frame()

plot(
  conservationweb.bfast,
  type="components",
  #ylim=c(3,max(wildtradeweb1620$hits)+1),
  main=""
)

conservationweb2<-conservationweb%>%
  dplyr::rename(web=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcpt=if_else(index%in%conservationwebcpt,1,0))%>%
  dplyr::select(date,web,webcpt)

conservationusdate<-conservation.df[conservation.df$webcpt == 1, "date"]


################Code for Time series analysis Figures (Fig. 1, Fig. 2)

#########################Fig. 1B and Fig. 1C
##########Comparison between daily COVID-19 cases and search interest for coronavirus (Fig. 1B)
colcases<- c("US web searches" = "black", "US confirmed cases" = "#2e6f8e", "China confirmed cases" = "#29af7f", "Italy confirmed cases" = "#482173")

caseusweb2<-ggplot()+
  geom_line(data=covidcasesus,aes(x=as.POSIXct(date),y=confirmedscale,color="US confirmed cases"),linetype="solid",size=1)+
  geom_line(data=covidcaseschina,aes(x=as.POSIXct(date),y=confirmedscale,color="China confirmed cases"),linetype="solid",size=1)+
  geom_line(data=covidcasesitaly,aes(x=as.POSIXct(date),y=confirmedscale,color="Italy confirmed cases"),linetype="solid",size=1)+
  geom_line(data=cvwebrecent2,aes(x=as.POSIXct(date),y=hits,color="US web searches"),size=1.2)+
  #geom_vline(xintercept=as.POSIXct("2020-01-12"),linetype="dashed",size=1,color="black")+
  labs(title="B",x="",y="Coronavirus Trends")+
  theme_bw()+
  scale_color_manual(values=colcases,breaks=c("US web searches","US confirmed cases","Italy confirmed cases","China confirmed cases"),name=NULL)+
  theme(legend.title = element_blank(),legend.position="bottom")+
  theme(legend.text=element_text(size=12))+
  xlim(as.POSIXct("2020-01-01"),as.POSIXct("2020-05-01"))

caseusweb2



#####Pandemic search trends (Fig. 1C)
colsearch <- c("Coronavirus\nsearches" = "black", "Bat searches" = "#2e6f8e", "Pangolin searches" = "#29af7f", "Wildlife trade\nsearches" = "#482173","Conservation\nsearches"="#bddf26")
pandemictimeseries<-ggplot()+
  geom_line(data=batweb1620,aes(x=date,y=hits,color="Bat searches"),linetype="solid",size=1)+
  geom_point(data=batweb1620,aes(x=date,y=hits,color="Bat searches"))+
  geom_line(data=pangolinweb1620,aes(x=date,y=hits,color="Pangolin searches"),linetype="solid",size=1)+
  geom_point(data=pangolinweb1620,aes(x=date,y=hits,color="Pangolin searches"))+
  geom_line(data=wildtradeweb1620,aes(x=date,y=hits,color="Wildlife trade\nsearches"),linetype="solid",size=1)+
  geom_point(data=wildtradeweb1620,aes(x=date,y=hits,color="Wildlife trade\nsearches"))+
  geom_line(data=conservationweb1620,aes(x=date,y=hits,color="Conservation\nsearches"),linetype="solid",size=1)+
  geom_point(data=conservationweb1620,aes(x=date,y=hits,color="Conservation\nsearches"))+
  geom_line(data=cvweb,aes(x=date,y=hits,color="Coronavirus\nsearches"),size=1.2)+
  geom_point(data=cvweb,aes(x=date,y=hits,color="Coronavirus\nsearches"))+
  #geom_vline(xintercept=as.POSIXct("2020-01-12"),linetype="dashed",size=1,color="black")+
  labs(title="C",x="",y="Search Trends")+
  theme_bw()+
  #scale_color_viridis_d(name=NULL,breaks=c("Coronavirus searches","Bat searches","Pangolin searches","Wildlife trade searches","Conservation searches"))+
  scale_color_manual(values=colsearch,name=NULL,breaks=c("Coronavirus\nsearches","Bat searches","Pangolin searches","Wildlife trade\nsearches","Conservation\nsearches"))+
  theme(legend.title = element_blank(),legend.position="bottom")+
  theme(legend.text=element_text(size=12))+
  xlim(as.POSIXct("2020-01-01"),as.POSIXct("2020-05-01"))#+
#expand_limits(x = as.POSIXct("2020-01-01"))

pandemictimeseries

ggarrange(caseusweb2,pandemictimeseries,nrow=2, align="hv")

###cross correlation between time series during pandemic
cvwebpando<-cvweb1620%>%filter(date>=as.POSIXct('2020-01-01')) 
batwebpando<-batweb1620%>%filter(date>=as.POSIXct('2020-01-01')) 
pangolinwebpando<-pangolinweb1620%>%filter(date>=as.POSIXct('2020-01-01')) 
wildtradewebpando<-wildtradeweb1620%>%filter(date>=as.POSIXct('2020-01-01')) 
conservationwebpando<-conservationweb1620%>%filter(date>=as.POSIXct('2020-01-01')) 


corrcvbat<-ccf(cvwebpando$hits,batwebpando$hits)
corrbatpangolin<-ccf(batwebpando$hits,pangolinwebpando$hits)
corrcvpangolin<-ccf(cvwebpando$hits,pangolinwebpando$hits)
corrpangolinwildtrade<-ccf(pangolinwebpando$hits,wildtradewebpando$hits)
corrcvwildtrade<-ccf(cvwebpando$hits,wildtradewebpando$hits)
corrwildtradecons<-ccf(wildtradewebpando$hits,conservationwebpando$hits)
corrcvconservation<-ccf(cvwebpando$hits,conservationwebpando$hits)
#corrcvITALY<-ccf(totalcasessearchitaly$hits,totalcasessearchitaly$confirmedscale)
corrnewssearch<-ccf(searchnewsus$hits,searchnewsus$newshits)


############Timeline of US interest breakpoints (from bfast), 
############peaks and current events (From Fig. 2E)
library(ggpmisc)
library(timelineS)
library(anytime)

####Annotate with Main Coronavirus events
events<-c("WHO declaration of COVID-19 pandemic","Coronavirus search interest increases","Earth Day","Coronavirus identified","First US. case (WA)","First CA case","Global health emergency declared","Bat origin suggested","Pangolin origin suggested","China bans illegal wildlife trade","First NY & FL case","First LA case","Earth Day")
dates<-c("2020-03-11","2020-01-19","2019-04-22","2020-01-08","2020-01-21","2020-01-26","2020-01-30","2020-02-03","2020-02-07","2020-02-24","2020-03-01","2020-03-09","2020-04-22")
#search<-c(5,10,13,7,7,5,5,35,35,35)
Events<-as.data.frame(cbind(events,dates))

USevents<-Events%>%filter(!events%in%c('First CA case','First NY & FL case','First LA case'))%>%
  dplyr::mutate(dates=as.POSIXct(dates))
UScvpeak<-cvweb%>%filter(date>=as.POSIXct('2019-12-01'))%>%filter(hits==max(hits))%>%dplyr::select(date,hits)%>%dplyr::mutate(term="coronavirus")
USbatpeak<-batweb1620%>%filter(date>=as.POSIXct('2019-12-01'))%>%filter(hits==max(hits))%>%dplyr::mutate(term="bat")
USpangolinpeak<-pangolinweb1620%>%filter(date>=as.POSIXct('2019-12-01'))%>%filter(hits==max(hits))%>%dplyr::mutate(term="pangolin")
USwildtradepeak<-wildtradeweb1620%>%filter(date>=as.POSIXct('2019-12-01'))%>%filter(hits==max(hits))%>%dplyr::mutate(term="wildlife trade")
#datebkpt<-c(usdate,batusdate,pangolinusdate,wildtradeusdate)
datebkpt<-c("2020-01-19","2020-01-26","2020-01-12")
termbkpt<-c("bat","pangolin","wildlife\ntrade")
USbreakpoints<-as.data.frame(cbind(datebkpt,termbkpt))%>%mutate(datebkpt=as.POSIXct(datebkpt))
#USconservationpeak<-conservationweb1620%>%filter(date>=as.POSIXct('2019-12-01'))%>%filter(hits==max(hits))%>%dplyr::mutate(term="conservation")
USinterestpeaks<-as.data.frame(rbind(UScvpeak,USbatpeak,USpangolinpeak,USwildtradepeak))#,USconservationpeak))
# 
# USinterestplot<-ggplot()+
#   geom_line(data=cvweb,aes(x=date,y=hits),color="black",size=1.2)+
#   geom_line(data=batweb1620,aes(x=date,y=hits),color="blue",size=.5)+
#    stat_peaks(data=batweb,aes(x=date,y=hits))+
# geom_line(data=pangolinweb,aes(x=date,y=hits),color="red",size=.5)+
# geom_line(data=wildtradeweb,aes(x=date,y=hits),color="green",size=.5)+
# #   geom_line(data=conservationweb,aes(x=date,y=hits),color="purple",size=.5)+
# #   theme(legend.title = element_blank())+
# #   theme_bw()
# USinterestplot
shift_axis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), ymax=y, ymin=y)+
    
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.ticks.x=element_blank())+
    theme(axis.line.y = element_blank(), axis.line.x=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(), axis.ticks.y=element_blank(),
          axis.text.y= element_blank(),axis.text.x = element_blank(), legend.title=element_blank(),
          plot.background = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.border= element_blank())
}

timelineplot<-ggplot()+
  geom_segment(data=USinterestpeaks,aes(x=as.POSIXct(date), y=-15, xend=as.POSIXct(date),yend=0),color="red",linetype="dashed",size=1)+
  geom_segment(data=USbreakpoints,aes(x=datebkpt, y=-15, xend=datebkpt,yend=0),color="black",linetype="dashed",size=1)+
  geom_segment(data=USevents,aes(x = dates, y = 10, xend = dates, yend = 0),size=.5)+
  geom_text(data = USevents, mapping=aes(x=dates, y= 10, label=events), hjust=-0.01, vjust=0.25,angle = 45,size=5) +
  #geom_text(data= USbreakpoints,mapping=aes(x=datebkpt, y= -15, label=termbkpt), vjust=1.5,size=3)+
  #geom_text(data = USinterestpeaks, mapping=aes(x=as.POSIXct(date), y=-35, label=hits), hjust=-0.01, vjust=0.25,size=3) +
  #geom_vline(xintercept=as.POSIXct("2020-01-12"),linetype="dashed",size=1,color="black")+
  #geom_vline(xintercept = usdate,linetype="dashed",size=1,color="lightgrey")+
  #geom_segment(x=as.POSIXct("2020-01-12"),y=0,xend=as.POSIXct("2020-01-12"),yend=70,linetype="dashed",size=1,color="grey")+
  #labs(x="",y="Coronavirus Searches")+
  theme_classic()+
  theme(axis.text.x = element_text(face="bold"))+
  scale_x_datetime(date_breaks = "2 weeks", labels = date_format("%b %d"),limits = c(as.POSIXct("2020-01-01"),as.POSIXct("2020-05-01")),expand=c(0,0))+
  theme(axis.text=element_text(size=12))+
  #scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+#,limits=c(as.POSIXct("2020-01-01"),as.POSIXct("2020-05-01")))+
  #scale_x_date(limit=as.Date(c('2020-01-01', '2020-05-01')), expand=c(0,0))+
  #xlim(as.POSIXct("2020-01-01"),as.POSIXct("2020-05-01"))+
  annotate("text", x = as.POSIXct("2020-04-15"), y = c(-25,-32,-39,-46), label = c("coronavirus","bat","pangolin","wildlife trade"),size=5)+
  ylim(-50,60)
shift_axis(timelineplot, 0)

###News and web search correlation Fig. S1
newsweb<-ggplot()+
  geom_line(data=cvwebrecent,aes(x=date,y=hits,color="US web searches"),size=1.2)+
  geom_line(data=cvnewsrecent2,aes(x=date,y=newshits,color="US news searches"),size=1)+
  #geom_vline(xintercept=as.POSIXct("2020-01-12"),linetype="dashed",size=1,color="black")+
  labs(x="",y="Coronavirus Trends")+
  theme(legend.title = element_blank())+
  theme_bw()+
  scale_color_grey(breaks=c("US web searches","US news searches"),start=.8,end=0,name=NULL)+
  xlim(as.POSIXct("2020-01-21"),as.POSIXct("2020-05-01"))

newsweb

#########################################################################
######################End main text analysis#############################
##################Sensitivity Testing at State Level#########
###################Disaggregated trend in High Volume States###########

########################################CORONAVIRUS WEB
# trendsstatecv <- gtrends(keyword=c("coronavirus+corona virus+covid"),
#                          geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
#                          low_search_volume=TRUE,
#                          time = "2019-12-01 2020-05-01")

##Interest over time for coronavirus
cvstateweb<-trendsstatecv$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))

#plot(cvweb$hits,x=cvweb$date)
#lines(cvweb,type='l',col='red')
#loesscvny<-predict(loess(hits~index,data=cvny,span=0.10)) # 10% smoothing span

###Estimate regime shift by state using trend mean
cvwaweb<-cvstateweb%>%filter(geo=="US-WA")
cvwaweb.cpt<-changepoint::cpt.mean(cvwaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)",
#test.stat="CUSUM")
cvwawebcpt<-cpts(cvwaweb.cpt)
cvwawebcpt
plot(cvwaweb.cpt)

cvnyweb<-cvstateweb%>%filter(geo=="US-NY")
cvnyweb.cpt<-changepoint::cpt.mean(cvnyweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)",
#test.stat="CUSUM")
cvnywebcpt<-cpts(cvnyweb.cpt)
cvnywebcpt
plot(cvnyweb.cpt)

cvcaweb<-cvstateweb%>%filter(geo=="US-CA")
cvcaweb.cpt<-changepoint::cpt.mean(cvcaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)",
#test.stat="CUSUM")
cvcawebcpt<-cpts(cvcaweb.cpt)
cvcawebcpt
plot(cvcaweb.cpt)

cvlaweb<-cvstateweb%>%filter(geo=="US-LA")
cvlaweb.cpt<-changepoint::cpt.mean(cvlaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)",
#test.stat="CUSUM")
cvlawebcpt<-cpts(cvlaweb.cpt)
cvlawebcpt
plot(cvlaweb.cpt)

cvflweb<-cvstateweb%>%filter(geo=="US-FL")
cvflweb.cpt<-changepoint::cpt.mean(cvflweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)",
#test.stat="CUSUM")
cvflwebcpt<-cpts(cvflweb.cpt)
cvflwebcpt
plot(cvflweb.cpt)

###VISUALIZE CORONAVIRUS CHANGEPOINTS
###For news and web

cvwaweb2<-cvwaweb%>%
  dplyr::rename(webwa=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webwacpt=if_else(index%in%cvwawebcpt,1,0))%>%
  dplyr::select(date,webwa,webwacpt)
cvnyweb2<-cvnyweb%>%
  dplyr::rename(webny=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webnycpt=if_else(index%in%cvnywebcpt,1,0))%>%
  dplyr::select(date,webny,webnycpt)
cvcaweb2<-cvcaweb%>%
  dplyr::rename(webca=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcacpt=if_else(index%in%cvcawebcpt,1,0))%>%
  dplyr::select(date,webca,webcacpt)
cvlaweb2<-cvlaweb%>%
  dplyr::rename(webla=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(weblacpt=if_else(index%in%cvlawebcpt,1,0))%>%
  dplyr::select(date,webla,weblacpt)
cvflweb2<-cvflweb%>%
  dplyr::rename(webfl=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webflcpt=if_else(index%in%cvflwebcpt,1,0))%>%
  dplyr::select(date,webfl,webflcpt)

cvstate.df<-left_join(cvwaweb2,cvnyweb2,by="date")%>%left_join(cvcaweb2,by="date")%>%left_join(cvlaweb2,by="date")%>%left_join(cvflweb2,by="date")

cols <- c("US trend" = "black", "WA Trend" = "dodgerblue4", "NY Trend" = "skyblue1", "CA Trend" = "darkgreen","LA Trend"= "mediumseagreen","FL Trend"="maroon")

########################################BAT STATE WEB
# trendsstatebat <- gtrends(keyword="bat+bats",
#                          geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
#                          category="66",
#                          low_search_volume=TRUE,
#                          time = "2018-01-01 2020-05-01")

trendsstatebat1620 <- gtrends(keyword="bat+bats",
                              geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
                              category="66",
                              low_search_volume=TRUE,
                              time = "2016-01-01 2020-05-01")


##Interest over time for coronavirus
batstateweb1620<-trendsstatebat1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))


#plot(cvweb$hits,x=cvweb$date)
#lines(cvweb,type='l',col='red')
#loesscvny<-predict(loess(hits~index,data=cvny,span=0.10)) # 10% smoothing span

###Estimate regime shift by state using trend variance
batwaweb1620<-batstateweb1620%>%filter(geo=="US-WA")
batwaweb1620.ts <- ts(batwaweb1620$hits,
                      start=c(2016,1),
                      frequency=52
)
#seg<-10/length(batweb1620.ts)
batwaweb.bfast<-bfast(batwaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")
batwaweb_brkpt <- batwaweb.bfast$output[[1]]$ci.Vt$confint%>%as.data.frame()
batwaweb_maxmag<-batwaweb.bfast$Magnitude
bawatweb_time<-batwaweb.bfast$Time


plot(
  batwaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="bat-WA"
)
# batwaweb.cpt<-changepoint::cpt.mean(batwaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
#                                     #test.stat="CUSUM")
# #batwaweb2.cpt<-changepoint::cpt.mean(batwaweb$hits,method="AMOC",penalty="Manual",pen.value="3*log(n)",
#                                      #test.stat="CUSUM")
# batwawebcpt<-cpts(batwaweb.cpt)
# batwawebcpt
# plot(batwaweb.cpt)
batnyweb1620<-batstateweb1620%>%filter(geo=="US-NY")
batnyweb1620.ts <- ts(batnyweb1620$hits,
                      start=c(2016,1),
                      frequency=52
)
#seg<-10/length(batweb1620.ts)
batnyweb.bfast<-bfast(batnyweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  batnyweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="bat-NY"
)
# batnyweb<-batstateweb%>%filter(geo=="US-NY")
# batnyweb.cpt<-changepoint::cpt.mean(batnyweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
#                                     #test.stat="CUSUM")
# batnyweb2.cpt<-changepoint::cpt.mean(batnyweb$hits,method="AMOC",penalty="Manual",pen.value="3*log(n)")
# batnywebcpt<-cpts(batnyweb.cpt)
# batnywebcpt
# plot(batnyweb.cpt)
batcaweb1620<-batstateweb1620%>%filter(geo=="US-CA")
batcaweb1620.ts <- ts(batcaweb1620$hits,
                      start=c(2016,1),
                      frequency=52
)
#seg<-10/length(batweb1620.ts)
batcaweb.bfast<-bfast(batcaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  batcaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="bat-CA"
)
# batcaweb<-batstateweb%>%filter(geo=="US-CA")
# batcaweb.cpt<-changepoint::cpt.mean(batcaweb$hits,method="AMOC",penalty="BIC")#,pen.value="3*log(n)")
#                                     #test.stat="CUSUM")
# batcaweb2.cpt<-changepoint::cpt.mean(batcaweb$hits,method="AMOC",penalty="Manual",pen.value="3*log(n)")
# batcawebcpt<-cpts(batcaweb.cpt)
# batcawebcpt
# plot(batcaweb.cpt)

batlaweb1620<-batstateweb1620%>%filter(geo=="US-LA")
batlaweb1620.ts <- ts(batlaweb1620$hits,
                      start=c(2016,1),
                      frequency=52
)
#seg<-10/length(batweb1620.ts)
batlaweb.bfast<-bfast(batlaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  batlaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="bat-LA"
)
# batlaweb<-batstateweb%>%filter(geo=="US-LA")
# batlaweb.cpt<-changepoint::cpt.mean(batlaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
#                                     #test.stat="CUSUM")
# batlaweb2.cpt<-changepoint::cpt.mean(batlaweb$hits,method="AMOC",penalty="Manual",pen.value="3*log(n)")
# batlawebcpt<-cpts(batlaweb.cpt)
# batlawebcpt
# plot(batlaweb.cpt)
batflweb1620<-batstateweb1620%>%filter(geo=="US-FL")
batflweb1620.ts <- ts(batflweb1620$hits,
                      start=c(2016,1),
                      frequency=52
)
#seg<-10/length(batweb1620.ts)
batflweb.bfast<-bfast(batflweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  batflweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="bat-FL"
)
# batflweb<-batstateweb%>%filter(geo=="US-FL")
# batflweb.cpt<-changepoint::cpt.mean(batflweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# #                                     #test.stat="CUSUM")
# # batflweb2.cpt<-changepoint::cpt.mean(batflweb$hits,method="AMOC",penalty="Manual",pen.value="3*log(n)")
# # batflwebcpt<-cpts(batflweb.cpt)
# # batflwebcpt
# # plot(batflweb.cpt)
# 
# ###VISUALIZE BAT CHANGEPOINTS
# 
# batwaweb2<-batwaweb%>%
#   dplyr::rename(webwa=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webwacpt=if_else(index%in%batwawebcpt,1,0))%>%
#   dplyr::select(date,webwa,webwacpt)
# batnyweb2<-batnyweb%>%
#   dplyr::rename(webny=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webnycpt=if_else(index%in%batnywebcpt,1,0))%>%
#   dplyr::select(date,webny,webnycpt)
# batcaweb2<-batcaweb%>%
#   dplyr::rename(webca=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webcacpt=if_else(index%in%batcawebcpt,1,0))%>%
#   dplyr::select(date,webca,webcacpt)
# batlaweb2<-batlaweb%>%
#   dplyr::rename(webla=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(weblacpt=if_else(index%in%batlawebcpt,1,0))%>%
#   dplyr::select(date,webla,weblacpt)
# batflweb2<-batflweb%>%
#   dplyr::rename(webfl=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webflcpt=if_else(index%in%batflwebcpt,1,0))%>%
#   dplyr::select(date,webfl,webflcpt)
# 
# batstate.df<-left_join(batwaweb2,batnyweb2,by="date")%>%left_join(batcaweb2,by="date")%>%left_join(batlaweb2,by="date")%>%left_join(batflweb2,by="date")

# batcadate<-bat.df[bat.df$webcacpt == 1, "date"]
# batwadate<-bat.df[bat.df$webwacpt == 1, "date"]
# batnydate<-bat.df[bat.df$webnycpt == 1, "date"]
# batladate<-bat.df[bat.df$weblacpt == 1, "date"]
# batfldate<-bat.df[bat.df$webflcpt == 1, "date"]

#batdates<-rbind(usdate,cadate,wadate,nydate,)


########################################PANGOLIN STATE WEB
# trendsstatepangolin <- gtrends(keyword="pangolin+pangolins",
#                           geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
#                           low_search_volume=TRUE,
#                           time = "2018-01-01 2020-05-01")

trendsstatepangolin1620 <- gtrends(keyword="pangolin+pangolins",
                                   geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
                                   low_search_volume=TRUE,
                                   time = "2016-01-01 2020-05-01")

pangolinstateweb1620<-trendsstatepangolin1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))


pangolinwaweb1620<-pangolinstateweb1620%>%filter(geo=="US-WA")
pangolinwaweb1620.ts <- ts(pangolinwaweb1620$hits,
                           start=c(2016,1),
                           frequency=52
)
#seg<-10/length(batweb1620.ts)
pangolinwaweb.bfast<-bfast(pangolinwaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  pangolinwaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="pangolin-WA"
)

# ##Interest over time for coronavirus
# pangolinstateweb<-trendsstatepangolin$interest_over_time%>%
#   #dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))

#plot(cvweb$hits,x=cvweb$date)
#lines(cvweb,type='l',col='red')
#loesscvny<-predict(loess(hits~index,data=cvny,span=0.10)) # 10% smoothing span

# ###Estimate regime shift by state using trend variance
# pangolinwaweb<-pangolinstateweb%>%filter(geo=="US-WA")
# pangolinwaweb.cpt<-changepoint::cpt.mean(pangolinwaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# pangolinwawebcpt<-cpts(pangolinwaweb.cpt)
# pangolinwawebcpt
# plot(pangolinwaweb.cpt)
# 
# write.csv(pangolinwaweb,'pangolinwaweb.csv')
pangolinnyweb1620<-pangolinstateweb1620%>%filter(geo=="US-NY")
pangolinnyweb1620.ts <- ts(pangolinnyweb1620$hits,
                           start=c(2016,1),
                           frequency=52
)
#seg<-10/length(batweb1620.ts)
pangolinnyweb.bfast<-bfast(pangolinnyweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  pangolinnyweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="pangolin-NY"
)
# pangolinnyweb<-pangolinstateweb%>%filter(geo=="US-NY")
# pangolinnyweb.cpt<-changepoint::cpt.mean(pangolinnyweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# pangolinnywebcpt<-cpts(pangolinnyweb.cpt)
# pangolinnywebcpt
# plot(pangolinnyweb.cpt)

pangolincaweb1620<-pangolinstateweb1620%>%filter(geo=="US-CA")
pangolincaweb1620.ts <- ts(pangolincaweb1620$hits,
                           start=c(2016,1),
                           frequency=52
)
#seg<-10/length(batweb1620.ts)
pangolincaweb.bfast<-bfast(pangolincaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  pangolincaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="pangolin-CA"
)

# pangolincaweb<-pangolinstateweb%>%filter(geo=="US-CA")
# pangolincaweb.cpt<-changepoint::cpt.mean(pangolincaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# pangolincawebcpt<-cpts(pangolincaweb.cpt)
# pangolincawebcpt
# plot(pangolincaweb.cpt)

pangolinlaweb1620<-pangolinstateweb1620%>%filter(geo=="US-LA")
pangolinlaweb1620.ts <- ts(pangolinlaweb1620$hits,
                           start=c(2016,1),
                           frequency=52
)
#seg<-10/length(batweb1620.ts)
pangolinlaweb.bfast<-bfast(pangolinlaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  pangolinlaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="pangolin-LA"
)
# pangolinlaweb<-pangolinstateweb%>%filter(geo=="US-LA")
# pangolinlaweb.cpt<-changepoint::cpt.mean(pangolinlaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# pangolinlawebcpt<-cpts(pangolinlaweb.cpt)
# pangolinlawebcpt
# plot(pangolinlaweb.cpt)
pangolinflweb1620<-pangolinstateweb1620%>%filter(geo=="US-FL")
pangolinflweb1620.ts <- ts(pangolinflweb1620$hits,
                           start=c(2016,1),
                           frequency=52
)
#seg<-10/length(batweb1620.ts)
pangolinflweb.bfast<-bfast(pangolinflweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  pangolinflweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="pangolin-FL"
)
# pangolinflweb<-pangolinstateweb%>%filter(geo=="US-FL")
# pangolinflweb.cpt<-changepoint::cpt.mean(pangolinflweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# pangolinflwebcpt<-cpts(pangolinflweb.cpt)
# pangolinflwebcpt
# plot(pangolinflweb.cpt)

###VISUALIZE PANGOLIN CHANGEPOINTS

pangolinwaweb2<-pangolinwaweb%>%
  dplyr::rename(webwa=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webwacpt=if_else(index%in%pangolinwawebcpt,1,0))%>%
  dplyr::select(date,webwa,webwacpt)
pangolinnyweb2<-pangolinnyweb%>%
  dplyr::rename(webny=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webnycpt=if_else(index%in%pangolinnywebcpt,1,0))%>%
  dplyr::select(date,webny,webnycpt)
pangolincaweb2<-pangolincaweb%>%
  dplyr::rename(webca=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcacpt=if_else(index%in%pangolincawebcpt,1,0))%>%
  dplyr::select(date,webca,webcacpt)
pangolinlaweb2<-pangolinlaweb%>%
  dplyr::rename(webla=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(weblacpt=if_else(index%in%pangolinlawebcpt,1,0))%>%
  dplyr::select(date,webla,weblacpt)
pangolinflweb2<-pangolinflweb%>%
  dplyr::rename(webfl=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webflcpt=if_else(index%in%pangolinflwebcpt,1,0))%>%
  dplyr::select(date,webfl,webflcpt)
pangolinstate.df<-left_join(pangolinwaweb2,pangolinnyweb2,by="date")%>%left_join(pangolincaweb2,by="date")%>%left_join(pangolinlaweb2,by="date")%>%left_join(pangolinflweb2,by="date")

# pangolincadate<-pangolin.df[pangolin.df$webcacpt == 1, "date"]
# pangolinwadate<-pangolin.df[pangolin.df$webwacpt == 1, "date"]
# pangolinnydate<-pangolin.df[pangolin.df$webnycpt == 1, "date"]
# pangolinladate<-pangolin.df[pangolin.df$weblacpt == 1, "date"]
# pangolinfldate<-pangolin.df[pangolin.df$webflcpt == 1, "date"]

########################################Wildlife trade STATE WEB
# trendsstatewildtrade <- gtrends(keyword="wildlife trade+bushmeat",
#                                 geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
#                                 #category="66",
#                                 low_search_volume=TRUE,
#                                 time = "2018-01-01 2020-05-01")

trendsstatewildtrade1620 <- gtrends(keyword="wildlife trade+bushmeat",
                                    geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
                                    #category="66",
                                    low_search_volume=TRUE,
                                    time = "2016-01-01 2020-05-01")


##Interest over time for coronavirus
wildtradestateweb1620<-trendsstatewildtrade1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))

#plot(cvweb$hits,x=cvweb$date)
#lines(cvweb,type='l',col='red')
#loesscvny<-predict(loess(hits~index,data=cvny,span=0.10)) # 10% smoothing span

###Estimate regime shift by state using trend variance

wildtradewaweb1620<-wildtradestateweb1620%>%filter(geo=="US-WA")
wildtradewaweb1620.ts <- ts(wildtradewaweb1620$hits,
                            start=c(2016,1),
                            frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradewaweb.bfast<-bfast(wildtradewaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  wildtradewaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="Wildlife Trade-WA"
)

# wildtradewaweb<-wildtradestateweb%>%filter(geo=="US-WA")
# wildtradewaweb.cpt<-changepoint::cpt.mean(wildtradewaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# wildtradewawebcpt<-cpts(wildtradewaweb.cpt)
# wildtradewawebcpt
# plot(wildtradewaweb.cpt)

wildtradenyweb1620<-wildtradestateweb1620%>%filter(geo=="US-NY")
wildtradenyweb1620.ts <- ts(wildtradenyweb1620$hits,
                            start=c(2016,1),
                            frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradenyweb.bfast<-bfast(wildtradenyweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  wildtradenyweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="Wildlife Trade-NY"
)

# wildtradenyweb<-wildtradestateweb%>%filter(geo=="US-NY")
# wildtradenyweb.cpt<-changepoint::cpt.mean(wildtradenyweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# wildtradenywebcpt<-cpts(wildtradenyweb.cpt)
# wildtradenywebcpt
# plot(wildtradenyweb.cpt)

wildtradecaweb1620<-wildtradestateweb1620%>%filter(geo=="US-CA")
wildtradecaweb1620.ts <- ts(wildtradecaweb1620$hits,
                            start=c(2016,1),
                            frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradecaweb.bfast<-bfast(wildtradecaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  wildtradecaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="Wildlife Trade-CA"
)

# wildtradecaweb<-wildtradestateweb%>%filter(geo=="US-CA")
# wildtradecaweb.cpt<-changepoint::cpt.mean(wildtradecaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# wildtradecawebcpt<-cpts(wildtradecaweb.cpt)
# wildtradecawebcpt
# plot(wildtradecaweb.cpt)

wildtradelaweb1620<-wildtradestateweb1620%>%filter(geo=="US-LA")
wildtradelaweb1620.ts <- ts(wildtradelaweb1620$hits,
                            start=c(2016,1),
                            frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradelaweb.bfast<-bfast(wildtradelaweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  wildtradelaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="Wildlife Trade-LA"
)

# wildtradelaweb<-wildtradestateweb%>%filter(geo=="US-LA")
# wildtradelaweb.cpt<-changepoint::cpt.mean(wildtradelaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# wildtradelawebcpt<-cpts(wildtradelaweb.cpt)
# wildtradelawebcpt
# plot(wildtradelaweb.cpt)

wildtradeflweb1620<-wildtradestateweb1620%>%filter(geo=="US-FL")
wildtradeflweb1620.ts <- ts(wildtradeflweb1620$hits,
                            start=c(2016,1),
                            frequency=52
)
#seg<-10/length(batweb1620.ts)
wildtradeflweb.bfast<-bfast(wildtradeflweb1620.ts,h=0.05,max.iter=1,season="none")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  wildtradeflweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="Wildlife Trade-FL"
)

# wildtradeflweb<-wildtradestateweb%>%filter(geo=="US-FL")
# wildtradeflweb.cpt<-changepoint::cpt.mean(wildtradeflweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# wildtradeflwebcpt<-cpts(wildtradeflweb.cpt)
# wildtradeflwebcpt
# plot(wildtradeflweb.cpt)


###VISUALIZE WILDLIFE TRADE CHANGEPOINTS

# wildtradewaweb2<-wildtradewaweb%>%
#   dplyr::rename(webwa=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webwacpt=if_else(index%in%wildtradewawebcpt,1,0))%>%
#   dplyr::select(date,webwa,webwacpt)
# wildtradenyweb2<-wildtradenyweb%>%
#   dplyr::rename(webny=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webnycpt=if_else(index%in%wildtradenywebcpt,1,0))%>%
#   dplyr::select(date,webny,webnycpt)
# wildtradecaweb2<-wildtradecaweb%>%
#   dplyr::rename(webca=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webcacpt=if_else(index%in%wildtradecawebcpt,1,0))%>%
#   dplyr::select(date,webca,webcacpt)
# wildtradelaweb2<-wildtradelaweb%>%
#   dplyr::rename(webla=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(weblacpt=if_else(index%in%wildtradelawebcpt,1,0))%>%
#   dplyr::select(date,webla,weblacpt)
# wildtradeflweb2<-wildtradeflweb%>%
#   dplyr::rename(webfl=hits)%>%
#   dplyr::mutate(index=row_number())%>%
#   dplyr::mutate(webflcpt=if_else(index%in%wildtradeflwebcpt,1,0))%>%
#   dplyr::select(date,webfl,webflcpt)
# 
# wildtradestate.df<-left_join(wildtradewaweb2,wildtradenyweb2,by="date")%>%left_join(wildtradecaweb2,by="date")%>%left_join(wildtradelaweb2,by="date")%>%left_join(wildtradeflweb2,by="date")

# wildtradecadate<-wildtrade.df[wildtrade.df$webcacpt == 1, "date"]
# wildtradewadate<-wildtrade.df[wildtrade.df$webwacpt == 1, "date"]
# wildtradenydate<-wildtrade.df[wildtrade.df$webnycpt == 1, "date"]
# wildtradeladate<-wildtrade.df[wildtrade.df$weblacpt == 1, "date"]
# wildtradefldate<-wildtrade.df[wildtrade.df$webflcpt == 1, "date"]


########################################CONSERVATION STATE WEB
# trendsstateconservation <- gtrends(keyword="conservation",
#                                 geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
#                                 category="66",
#                                 low_search_volume=TRUE,
#                                 time = "2018-01-01 2020-05-01")

trendsstateconservation1620 <- gtrends(keyword="conservation",
                                       geo = c("US-WA","US-NY","US-CA","US-LA","US-FL"),
                                       category="66",
                                       low_search_volume=TRUE,
                                       time = "2016-01-01 2020-05-01")

##Interest over time for coronavirus
conservationstateweb1620<-trendsstateconservation1620$interest_over_time%>%
  #dplyr::mutate(index=row_number())%>%
  dplyr::mutate(hits=if_else(hits=="<1",0.5,as.numeric(hits)))

#plot(cvweb$hits,x=cvweb$date)
#lines(cvweb,type='l',col='red')
#loesscvny<-predict(loess(hits~index,data=cvny,span=0.10)) # 10% smoothing span

###Estimate regime shift by state using trend variance

conservationwaweb1620<-conservationstateweb1620%>%filter(geo=="US-WA")
conservationwaweb1620.ts <- ts(conservationwaweb1620$hits,
                               start=c(2016,1),
                               frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationwaweb.bfast<-bfast(conservationwaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")
conservationwaweb_season<-conservationwaweb.bfast$output[[1]]$St%>%as.data.frame()


plot(
  conservationwaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="conservation-WA"
)

# conservationwaweb<-conservationstateweb%>%filter(geo=="US-WA")
# conservationwaweb.cpt<-changepoint::cpt.mean(conservationwaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# conservationwawebcpt<-cpts(conservationwaweb.cpt)
# conservationwawebcpt
# plot(conservationwaweb.cpt)

conservationnyweb1620<-wildtradestateweb1620%>%filter(geo=="US-NY")
conservationnyweb1620.ts <- ts(conservationnyweb1620$hits,
                               start=c(2016,1),
                               frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationnyweb.bfast<-bfast(conservationnyweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")
conservationnyweb_season<-conservationnyweb.bfast$output[[1]]$St%>%as.data.frame()

plot(
  conservationnyweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="conservation-NY"
)

# conservationnyweb<-conservationstateweb%>%filter(geo=="US-NY")
# conservationnyweb.cpt<-changepoint::cpt.mean(conservationnyweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# conservationnywebcpt<-cpts(conservationnyweb.cpt)
# conservationnywebcpt
# plot(conservationnyweb.cpt)

conservationcaweb1620<-conservationstateweb1620%>%filter(geo=="US-CA")
conservationcaweb1620.ts <- ts(conservationcaweb1620$hits,
                               start=c(2016,1),
                               frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationcaweb.bfast<-bfast(conservationcaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")
conservationcaweb_season<-conservationcaweb.bfast$output[[1]]$St%>%as.data.frame()

plot(
  conservationcaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="conservation-CA"
)

# conservationcaweb<-conservationstateweb%>%filter(geo=="US-CA")
# conservationcaweb.cpt<-changepoint::cpt.mean(conservationcaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# conservationcawebcpt<-cpts(conservationcaweb.cpt)
# conservationcawebcpt
# plot(conservationcaweb.cpt)

conservationlaweb1620<-conservationstateweb1620%>%filter(geo=="US-LA")
conservationlaweb1620.ts <- ts(conservationlaweb1620$hits,
                               start=c(2016,1),
                               frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationlaweb.bfast<-bfast(conservationlaweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")
conservationlaweb_season<-conservationlaweb.bfast$output[[1]]$St%>%as.data.frame()

plot(
  conservationlaweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="conservation-LA"
)

# conservationlaweb<-conservationstateweb%>%filter(geo=="US-LA")
# conservationlaweb.cpt<-changepoint::cpt.mean(conservationlaweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# conservationlawebcpt<-cpts(conservationlaweb.cpt)
# conservationlawebcpt
# plot(conservationlaweb.cpt)

conservationflweb1620<-conservationstateweb1620%>%filter(geo=="US-FL")
conservationflweb1620.ts <- ts(conservationflweb1620$hits,
                               start=c(2016,1),
                               frequency=52
)
#seg<-10/length(batweb1620.ts)
conservationflweb.bfast<-bfast(conservationflweb1620.ts,h=0.05,max.iter=1,season="dummy")
#batweb.bfast1<-bfast01(batweb1620.ts,h=0.1,max.iter=1,season="dummy")

plot(
  conservationflweb.bfast,
  type="components",
  #ylim=c(3,max(batweb1620$hits)+1),
  main="conservation-FL"
)

# conservationflweb<-conservationstateweb%>%filter(geo=="US-FL")
# conservationflweb.cpt<-changepoint::cpt.mean(conservationflweb$hits,method="AMOC",penalty="BIC")#penalty="Manual",pen.value="3*log(n)")
# conservationflwebcpt<-cpts(conservationflweb.cpt)
# conservationflwebcpt
# plot(conservationflweb.cpt)


###VISUALIZE CONSERVATION CHANGEPOINTS

conservationwaweb2<-conservationwaweb%>%
  dplyr::rename(webwa=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webwacpt=if_else(index%in%conservationwawebcpt,1,0))%>%
  dplyr::select(date,webwa,webwacpt)
conservationnyweb2<-conservationnyweb%>%
  dplyr::rename(webny=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webnycpt=if_else(index%in%conservationnywebcpt,1,0))%>%
  dplyr::select(date,webny,webnycpt)
conservationcaweb2<-conservationcaweb%>%
  dplyr::rename(webca=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webcacpt=if_else(index%in%conservationcawebcpt,1,0))%>%
  dplyr::select(date,webca,webcacpt)
conservationlaweb2<-conservationlaweb%>%
  dplyr::rename(webla=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(weblacpt=if_else(index%in%conservationlawebcpt,1,0))%>%
  dplyr::select(date,webla,weblacpt)
conservationflweb2<-conservationflweb%>%
  dplyr::rename(webfl=hits)%>%
  dplyr::mutate(index=row_number())%>%
  dplyr::mutate(webflcpt=if_else(index%in%conservationflwebcpt,1,0))%>%
  dplyr::select(date,webfl,webflcpt)

conservationstate.df<-left_join(conservationwaweb2,conservationnyweb2,by="date")%>%left_join(conservationcaweb2,by="date")%>%left_join(conservationlaweb2,by="date")%>%left_join(conservationflweb2,by="date")

conservationcadate<-conservation.df[conservation.df$webcacpt == 1, "date"]
conservationwadate<-conservation.df[conservation.df$webwacpt == 1, "date"]
conservationnydate<-conservation.df[conservation.df$webnycpt == 1, "date"]
conservationladate<-conservation.df[conservation.df$weblacpt == 1, "date"]
conservationfldate<-conservation.df[conservation.df$webflcpt == 1, "date"]

#####################SEGMENTATION TESTS
batcpt.seg=cbind(c(0,cpts(batweb.cpt)),seg.len(batweb.cpt))
batdata=data.set(batweb.cpt)
shapiro.func=function(x){
  out=shapiro.test(batdata[(x[1]+1):(x[1]+x[2])])
  return(c(out$statistic,p=out$p.value))}
apply(batcpt.seg,1,shapiro.func)
batcptwa.seg=cbind(c(0,cpts(batwaweb.cpt)),seg.len(batwaweb.cpt))
batwadata=data.set(batwaweb.cpt)
apply(batcptwa.seg,1,shapiro.func)
