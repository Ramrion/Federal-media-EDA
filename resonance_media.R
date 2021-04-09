library(tidyverse)
library(purrr)
library(LaplacesDemon)
library(zoo)
library(fpp2)
library(TTR)

load("year_0.RData")
spiting_data <- out$meta
thetas <- year_ctm_xplore$theta

rm(out)
rm(year_ctm_xplore)

levels(as.factor(spiting_data$source))

media_x_news <- spiting_data %>%
  mutate(id = 1:nrow(spiting_data)) %>% 
  filter(pubDate >= "2019-09-01", source == "Zona.media")

media_x_news <- media_x_news[order(media_x_news$pubDate),]

media_x_news_count <- plyr::count(media_x_news, "pubDate")

KLD_resonance <-  function(data, w, date) {
  
  # w ~ number of news for each day
  # so sum of w should be equal length(data)
  
  # Creating indexes for each frame
  # So, each news in frame t should be compared with every news in frame t-1
  # example of data in m_frame: 
  # news with id 64 should be compared with news 1:63
  # but news with id 1052 should be compared with news 851:1051
  # because the day before news 1052 there were 200 news, namely, news 851:1051
  m_frame <- c()
  for (i in 1:(length(w) - 1)) {
    m_frame[i] <- list(rep(list(seq(sum(w[1:i]) + 1, sum(w[1:(i+1)]))),
                             times = w[i]))
  }
  
  m_frame <- unlist(m_frame, recursive = FALSE)  
  px <- seq(1, (nrow(data) - w[length(w)]))
  # Indexes of documents with wich we compare
  # Example: Documents from 1:100 will be compared with 101
  
  
  # KLD of Moving frame
  # There might be some issue that p and x should change their places...
  kld_news <- mapply(function(frame, p) {
    mean(unlist(lapply(frame, function(x) {
      KLD_frame <-  KLD(data[p,], data[x,])$sum.KLD.px.py
    })))
  }, frame = m_frame, p = px)
  
  d_frame <- c()
  for (i in 1:(length(w) - 1)) {
    d_frame[i] <- list(seq(sum(w[1:i]) + 1, sum(w[1:(i+1)])))
  }
  
  # Calculate mean KLD for each day
  kld_mean_day <- sapply(d_frame, function(x) {
    mean(kld_news[x])
  })
  
  resonance_media <- data.frame(mean_kld = kld_mean_day, pubDate = date[1:(length(date) - 1)])

finale <- list("news_resonance" = kld_news, "resonance_mean_day" = resonance_media)
  return(finale)
}



# KLD_resonance for every particular media
test <- lapply(levels(as.factor(spiting_data$source))[c(14,16)], function(x) {
  media_x_news <- spiting_data %>%
    mutate(id = 1:nrow(spiting_data)) %>% 
    filter(pubDate >= "2019-09-01", source == x)
  
  media_x_news <- media_x_news[order(media_x_news$pubDate),]
  
  media_x_news_count <- plyr::count(media_x_news, "pubDate")
  
  media_x_news_topic_distr <- thetas[media_x_news$id,]
  
  KLD_resonance(media_x_news_topic_distr, media_x_news_count$freq, media_x_news_count$pubDate)
})

cor(test[[1]]$resonance_mean_day$mean_kld[2:423], test2[[1]]$novelty_mean_day$mean_kld[1:422])


# kld_media <- data.frame(mean_kld = kld_mean_day, pubDate = date[2:length(date)])

# save(every_media_kld, file = "sep_media_novelty.RData")

# save(media_resonance, kld_mean_day, file = "whole_media_resonance.RData")
