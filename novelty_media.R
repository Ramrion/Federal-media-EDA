library(tidyverse)
library(purrr)
library(LaplacesDemon)
library(zoo)
library(fpp2)
library(TTR)

load("year_0.RData")
spiting_data <- out$meta           # News metadata
thetas <- year_ctm_xplore$theta    # Distribution of 73(?) topics in every news

rm(out)
rm(year_ctm_xplore)

media_x_news <- spiting_data %>%
  mutate(id = 1:nrow(spiting_data)) %>% 
  filter(pubDate >= "2019-09-01")

media_x_news <- media_x_news[order(media_x_news$pubDate),]

media_x_news_count <- plyr::count(media_x_news, "pubDate")

media_x_news_topic_distr <- thetas[media_x_news$id,]

KLD_novelty <-  function(data, w, date) {
  # data ~ distribution of topics in the document, it's kinda similar to probability as it sums to 1
  # w ~ number of news for each day
  # so sum of w should be equal length(data)
  # date ~ period or posting dates
  
  # Creating indexes for each frame
  # Each news in frame t should be compared with every news in frame t-1
  # example of data in m_frame: 
  # news with id 64 should be compared with news 1:63
  # but news with id 1052 should be compared with news 851:1051
  # because the day before news 1052 there were 200 news, namely, news 851:1051
  m_frame <- c()
  for (i in 1:length(w) - 1) {
    m_frame[i] <- list(rep(list(seq(if (i == 1) 1 else sum(w[1:i-1]) + 1, sum(w[1:i]))), times = w[i+1]))
  }
  
  m_frame <- unlist(m_frame, recursive = FALSE)  
  px <- seq(w[1] + 1, nrow(data))
  # Indexes of documents with wich we compare
  # Example: Documents from 1:100 we will compare with 101
  
  
  # KLD of Moving frame
  # There might be some issue that p and x should change their places...
  kld_news <- mapply(function(frame, p) {
    mean(unlist(lapply(frame, function(x) {
      KLD_frame <-  KLD(data[p,], data[x,])$sum.KLD.px.py
    })))
  }, frame = m_frame, p = px)
  
  # Creating list with indexes of each news for each day
  d_frame <- c()
  for (i in 2:length(w)) {
    d_frame[i-1] <- list(seq(if (i == 2) 1 else sum(w[2:(i-1)]) + 1,
                             sum(w[2:i])))
  }
  
  # Calculate mean KLD for each day
  kld_mean_day <- sapply(d_frame, function(x) {
    mean(kld_news[x])
  })
  
  # date[2:length(date)] ~ as we calculate KLD based on the news of the day before
  # it means that we cannot calculate KLD for the first day
  
  novelty_media <- data.frame(mean_kld = kld_mean_day, pubDate = date[2:length(date)])
  
  # Saving novelty of each news and mean novelty of the day

finale <- list("news_novelty" = kld_news, "novelty_mean_day" = novelty_media)
  return(finale)
}

# the whole period:

whole_novelty <- KLD_novelty(media_x_news_topic_distr, media_x_news_count$freq, media_x_news_count$pubDate)

save(whole_novelty, file = "whole_media_novelty.RData")

# for every media separetly:

test2 <- lapply(levels(as.factor(spiting_data$source))[c(14,16)], function(x) {
  media_x_news <- spiting_data %>%
    mutate(id = 1:nrow(spiting_data)) %>% 
    filter(pubDate >= "2019-09-01", source == x)
  
  media_x_news <- media_x_news[order(media_x_news$pubDate),]
  
  media_x_news_count <- plyr::count(media_x_news, "pubDate")
  
  media_x_news_topic_distr <- thetas[media_x_news$id,]
  
  KLD_novelty(media_x_news_topic_distr, media_x_news_count$freq, media_x_news_count$pubDate)
})

# Changes in novelty from september 2019 to december 2020
ggplot(whole_novelty$novelty_mean_day, aes(x = as_date(pubDate), y = mean_kld)) +
  geom_line() +
  # geom_point(data = kld_mean_day[kld_mean_day$day_week %in% c("Сб", "Вс"),],
  #            aes(x = as_date(id), y = kld_day), color = "red") +
  # geom_point(data = kld_mean_day[kld_mean_day$day_week == "Пн",],
  #            aes(x = as_date(id), y = kld_day), color = "green") +
  scale_x_date(date_breaks = 'month')

# Calculating correlation between resonance and novelty
load("whole_media_resonance.RData")

plot.ts(kld_mean_day)

cor(kld_mean_day[2:424], whole_novelty$novelty_mean_day$mean_kld[1:423])
