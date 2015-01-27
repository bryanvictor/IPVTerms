setwd("C:/Users/Tom/Documents/GitHub/IPVTerms")

load("IPV title search as short titles.R")

library(dplyr)
library(stringr)
library(ggplot2)

IPV.years<-IPV.df%>%
  filter(attributes =="YR")%>%
  select(-attributes, articleID, Year=record)

titles.df<-merge(compressed, IPV.years, by="articleID")
titles.df<-select(titles.df, -X)
titles.df$Year<-as.character(titles.df$Year)
titles.df$Year<-as.numeric(titles.df$Year)

#DOMESTIC VIOLENCE

DV.set<-filter(titles.df, grepl("domesticviolence", short.title))
DV.set<-filter(DV.set, Year<2014)

n.DV.year <- DV.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.DV.year<-select(n.DV.year, Year, DV=n)

#INTIMATE PARTNER VIOLENCE

IPV.set<-filter(titles.df, grepl("intimatepartnerviolence", short.title))
IPV.set<-filter(IPV.set, Year<2002)

n.IPV.year <- IPV.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.IPV.year<-select(n.IPV.year, Year, IPV=n)

#FAMILY VIOLENCE

FV.set<-filter(titles.df, grepl("familyviolence", short.title))
FV.set<-filter(FV.set, Year<2014)

n.FV.year <- FV.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.FV.year<-select(n.FV.year, Year, FV=n)

#BATTERING

BAT.set<-filter(titles.df, grepl("battering", short.title))
BAT.set<-filter(BAT.set, Year<2014)

n.BAT.year <- BAT.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.BAT.year<-select(n.BAT.year, Year, BAT=n)

#MERGER

merged<-merge(n.DV.year, n.IPV.year, by="Year", all=TRUE)
merged<-merge(merged, n.BAT.year, by="Year", all=TRUE)
merged<-merge(merged, n.FV.year, by="Year", all=TRUE)
merged[is.na(merged)] <- 0

term.count <- ggplot(merged, aes(as.numeric(Year), y=n, group=1, color=Terms)) + 
  geom_line(aes(y = DV, colour="Domestic Violence")) +
  geom_line(aes(y = IPV, colour="Intimate Partner Violence")) + 
  geom_line(aes(y = BAT, colour="Battering"))+
  geom_line(aes(y = FV, colour="Family Violence"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Year") + 
  ylab("frequency") + 
  ggtitle("Term Usage in Peer-Reviewed Article Titles") + 
  scale_x_continuous(breaks=seq(1963, 2013, 10))

print(term.count)

#Google Trend Data 

google<-read.csv("googletrend.csv")
google$Week<-strtrim(google$Week, 10)
google<-google[1:575,]
google$Week<-as.Date(google$Week)
google$domestic.violence<-as.numeric(google$domestic.violence)

google.count <- ggplot(google, aes(x=Week)) + 
  geom_line(aes(y = domestic.violence, colour="Domestic Violence")) +
  geom_line(aes(y = intimate.partner.violence, colour="Intimate Partner Violence")) + 
  geom_line(aes(y = battering, colour="Battering"))+
  geom_line(aes(y = family.violence, colour="Family Violence"))+ scale_x_date() + 
  xlab("Year") + 
  ylab("frequency") + 
  ggtitle("Search Term Popularity")

google.count<- google.count + scale_color_discrete(name="Google Search Terms")

