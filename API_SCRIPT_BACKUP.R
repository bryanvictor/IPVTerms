library(dplyr)

article_key <- "577ee1a30fa99b09acbf6adb46a0e82d:11:72817964"
url <- 'http://api.nytimes.com/svc/search/v2/articlesearch.json?q="domestic+violence"'
pageRange<-0:100

DV1 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&end_date=19800101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV1<-rbind(DV1, articles)
}

DV2 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=19800101&end_date=19900101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV2<-rbind(DV2, articles)
}

DV3 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=19900101&end_date=19950101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV3<-rbind(DV3, articles)
}

DV4 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=19950101&end_date=19980101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV4<-rbind(DV4, articles)
}

DV5 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=19980101&end_date=20010101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV5<-rbind(DV5, articles)
}

DV6 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20010101&end_date=20050101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV6<-rbind(DV6, articles)
}

DV7 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20050101&end_date=20090101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV7<-rbind(DV7, articles)
}

DV8 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20090101&end_date=20120101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV8<-rbind(DV8, articles)
}

DV9 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20120101&end_date=20140101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV9<-rbind(DV9, articles)
}

DV10 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20140101&end_date=20141101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV10<-rbind(DV10, articles)
}

DV11 <- as.data.frame(c())

for (i in pageRange) {
  # concatenate URL for each page
  req <- fromJSON(paste0(url, "&page=", i, "&begin_date=20141101&api-key=", article_key, sep=""))
  articles <- as.data.frame(req$response$docs)  # convert the dates to a vector and append
  articles<-select(articles, web_url, pub_date)
  DV11<-rbind(DV11, articles)
}


DV.combined <- rbind(DV1, DV2, DV3, DV4, DV5, DV6, DV7, DV8, DV9, DV10, DV11)

DV.DF <-unique(DV.combined)

DV.annualCount <- DV.DF %>%
  group_by(year) %>%
  summarise(N = n())

term.count <- ggplot(DV.annualCount, aes(as.numeric(year), y=N)) + 
  geom_line()


