IPV <- read.csv("KeywordWrangle_Expanded.csv") 

terms <- IPV %>%
  filter(attributes=="KP")

years<-IPV %>%
  filter(attributes=="YR")%>%
  select(articleID, year=record)

years$year<-as.character(years$year)
years$year<-as.numeric(years$year)

merged<-merge(terms, years, by="articleID")
merged<-select(merged, keyword=record, year)

#DOMESTIC VIOLENCE

DV.set<-filter(merged, grepl("domestic violence|Domestic Violence", keyword))
DV.set<-filter(DV.set, year<2014)

n.DV.year <- DV.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.DV.year<-select(n.DV.year, year, DV=n)

#INTIMATE PARTNER VIOLENCE

IPV.set<-filter(merged, grepl("intimate partner violence|Intimate Partner Violence", keyword))
IPV.set<-filter(IPV.set, year<2014)

n.IPV.year <- IPV.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.IPV.year<-select(n.IPV.year, year, IPV=n)

#FAMILY VIOLENCE

FV.set<-filter(merged, grepl("family violence|Family Violence", keyword))
FV.set<-filter(FV.set, year<2014)

n.FV.year <- FV.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.FV.year<-select(n.FV.year, year, FV=n)

#BATTERING

BAT.set<-filter(merged, grepl("batter|Batter", keyword))
BAT.set<-filter(BAT.set, year<2014)

n.BAT.year <- BAT.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.BAT.year<-select(n.BAT.year, year, BAT=n)

#WIFE ABUSE

WA.set<-filter(merged, grepl("wife abuse|Wife Abuse", keyword))
WA.set<-filter(WA.set, year<2014)

n.WA.year <- WA.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.WA.year<-select(n.WA.year, year, WA=n)

#SPOUSE ABUSE

SA.set<-filter(merged, grepl("spouse abuse|Spouse Abuse", keyword))
SA.set<-filter(SA.set, year<2014)

n.SA.year <- SA.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.SA.year<-select(n.SA.year, year, SA=n)

#PARTNER ABUSE

PA.set<-filter(merged, grepl("partner abuse|Partner Abuse", keyword))
PA.set<-filter(PA.set, year<2014)

n.PA.year <- PA.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.PA.year<-select(n.PA.year, year, PA=n)

#DATING VIOLENCE

DATING.set<-filter(merged, grepl("dating violence|Dating Violence", keyword))
DATING.set<-filter(DATING.set, year<2014)

n.DATING.year <- SA.set %>% 
  group_by(year) %>%
  summarise(n = n())

n.DATING.year<-select(n.DATING.year, year, DATING=n)

KW<-merge(n.DV.year, n.IPV.year, by="year", all=TRUE)
KW<-merge(KW, n.BAT.year, by="year", all=TRUE)
KW<-merge(KW, n.FV.year, by="year", all=TRUE)
KW<-merge(KW, n.WA.year, by="year", all=TRUE)
KW<-merge(KW, n.SA.year, by="year", all=TRUE)
KW<-merge(KW, n.PA.year, by="year", all=TRUE)
KW<-merge(KW, n.DATING.year, by="year", all=TRUE)
KW[is.na(KW)] <- 0

term.count <- ggplot(KW, aes(as.numeric(year), y=n, group=1, color=Keywords)) + 
  geom_line(aes(y = DV, colour="Domestic Violence")) +
  geom_line(aes(y = IPV, colour="Intimate Partner Violence")) + 
  geom_line(aes(y = BAT, colour="Battered/Battering"))+
  geom_line(aes(y = FV, colour="Family Violence"))+
  geom_line(aes(y = WA, colour="Wife Abuse"))+
  geom_line(aes(y = SA, colour="Spouse Abuse"))+
  geom_line(aes(y = PA, colour="Partner Abuse"))+
  geom_line(aes(y = DATING, colour="Dating Violence"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Year") + 
  ylab("frequency") + 
  ggtitle("Term Count as Article Subject") + 
  scale_x_continuous(breaks=seq(1963, 2013, 10))

print(term.count)