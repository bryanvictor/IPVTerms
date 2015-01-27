setwd("C:/Users/Tom/Documents/GitHub/IPVTerms")

library(dplyr)
library(stringr)
library(ggplot2)

IPV <- read.csv("IPV.csv") 

terms <- IPV %>%
  filter(attributes=="KY")

years<-IPV %>%
  filter(attributes=="YR")%>%
  select(articleID, year=record)

years$year<-as.character(years$year)
years$year<-as.numeric(years$year)

ID <- terms[,3]

terms.split<-str_split_fixed(terms$record, ";", 5)

terms.split<-as.data.frame(terms.split)

IPV.Terms<-cbind(ID,years,terms.split)

colnames(IPV.Terms)<- c("ID", "Year", "Term1", "Term2", "Term3", "Term4", "Term5")


set1 = select(IPV.Terms, ID, Year, Term=Term1)
set2 = select(IPV.Terms, ID, Year, Term=Term2)
set3 = select(IPV.Terms, ID, Year, Term=Term3)
set4 = select(IPV.Terms, ID, Year, Term=Term4)
set5 = select(IPV.Terms, ID, Year, Term=Term5)

full.set<-rbind(set1, set2, set3, set4, set5)

#DOMESTIC VIOLENCE

DV.set<-filter(full.set, Term=="Domestic Violence")
DV.set<-filter(DV.set, Year<2014)

n.DV.year <- DV.set %>% 
  group_by(Year) %>%
   summarise(n = n())

n.DV.year<-select(n.DV.year, Year, DV=n)

# INTIMATE PARTNER VIOLENCE

IPV.set<-filter(full.set, Term=="Intimate Partner Violence")
IPV.set<-filter(IPV.set, Year<2014)

n.IPV.year <- IPV.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.IPV.year<-select(n.IPV.year, Year, IPV=n)

#BATTERING

BAT.set<-filter(full.set, grepl("Batter", Term))
BAT.set<-filter(BAT.set, Year<2014)

n.BAT.year <- BAT.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.BAT.year<-select(n.BAT.year, Year, BAT=n)

#FAMILY

FAM.set<-filter(full.set, grepl("Family", Term))
FAM.set<-filter(FAM.set, Year<2014)

n.FAM.year <- FAM.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.FAM.year<-select(n.FAM.year, Year, FAM=n)


#MERGER

merged<-merge(n.DV.year, n.IPV.year, by="Year", all=TRUE)
merged<-merge(merged, n.BAT.year, by="Year", all=TRUE)
merged<-merge(merged, n.FAM.year, by="Year", all=TRUE)
merged[is.na(merged)] <- 0

term.count <- ggplot(merged, aes(as.numeric(Year), y=n, group=1, color=Terms)) + 
  geom_line(aes(y = DV, colour="Domestic Violence")) +
  geom_line(aes(y = IPV, colour="Intimate Partner Violence")) + 
  geom_line(aes(y = BAT, colour="Battered"))+
  geom_line(aes(y = FAM, colour="Family"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Year") + 
  ylab("frequency") + 
  ggtitle("Term Count as Article Subject") + 
  scale_x_continuous(breaks=seq(1963, 2013, 10))

print(term.count)