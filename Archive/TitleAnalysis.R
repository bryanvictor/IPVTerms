IPV.years<-IPV.df%>%
  filter(attributes == "YR")%>%
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
IPV.set<-filter(IPV.set, Year<2014)

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
BAT.set<-filter(BAT.set, Year<1970)

n.BAT.year <- BAT.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.BAT.year<-select(n.BAT.year, Year, BAT=n)

#WIFE ABUSE

WA.set<-filter(titles.df, grepl("wifeabuse", short.title))
WA.set<-filter(WA.set, Year<2014)

n.WA.year <- WA.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.WA.year<-select(n.WA.year, Year, WA=n)

#SPOUSE ABUSE

SA.set<-filter(titles.df, grepl("spouseabuse", short.title))
SA.set<-filter(SA.set, Year<2014)

n.SA.year <- SA.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.SA.year<-select(n.SA.year, Year, SA=n)

#PARTNER ABUSE

PA.set<-filter(titles.df, grepl("wifeabuse", short.title))
PA.set<-filter(PA.set, Year<2014)

n.PA.year <- PA.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.PA.year<-select(n.PA.year, Year, PA=n)

#DATING VIOLENCE

DATING.set<-filter(titles.df, grepl("datingviolence", short.title))
DATING.set<-filter(DATING.set, Year<2014)

n.DATING.year <- DATING.set %>% 
  group_by(Year) %>%
  summarise(n = n())

n.DATING.year<-select(n.DATING.year, Year, DATING=n)

#MERGER

merged<-merge(n.DV.year, n.IPV.year, by="Year", all=TRUE)
merged<-merge(merged, n.BAT.year, by="Year", all=TRUE)
merged<-merge(merged, n.FV.year, by="Year", all=TRUE)
merged<-merge(merged, n.WA.year, by="Year", all=TRUE)
merged<-merge(merged, n.SA.year, by="Year", all=TRUE)
merged<-merge(merged, n.PA.year, by="Year", all=TRUE)
merged<-merge(merged, n.DATING.year, by="Year", all=TRUE)
merged[is.na(merged)] <- 0

term.count <- ggplot(merged, aes(as.numeric(Year), y=n, group=1, color=Terms)) + 
  geom_line(aes(y = DV, colour="Domestic Violence")) +
  geom_line(aes(y = IPV, colour="Intimate Partner Violence")) + 
  geom_line(aes(y = BAT, colour="Battering"))+
  geom_line(aes(y = FV, colour="Family Violence"))+
  geom_line(aes(y = WA, colour="Wife Abuse"))+
  geom_line(aes(y = SA, colour="Spouse Abuse"))+
  geom_line(aes(y = PA, colour="Partner Abuse"))+
  geom_line(aes(y = DATING, colour="Dating Violence"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Year") + 
  ylab("Frequency") + 
  ggtitle("") + 
  scale_x_continuous(breaks=seq(1963, 2013, 10))

print(term.count)