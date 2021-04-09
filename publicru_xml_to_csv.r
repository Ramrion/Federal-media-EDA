library(xml2)
library(tidyverse)
library(lubridate)
library(magrittr)
library(readr)

## stating where the xml files are located
filenames <- list.files(path = "~/media_nlp/xmlki/vestifm_may_sep_xmlki", pattern="*.xml", full.names=TRUE)

## reading in the data in a single object
df <- do.call(rbind, lapply(filenames, function(x) {
  doc <- read_xml(x)
  # deleting <author>, <url>, <page>, <issue> tags as irrelevant 
  xml_remove(xml_find_all(doc, xpath = ".//item/author | .//item/url | .//item/page | .//item/issue"))
  # deleting items with empty <description> field
  special_nodes <- xml_find_all(doc, ".//item[child::description[text()]]")
  list_of_lists <- lapply(special_nodes, function(x) {
    as_list(x, ns = c("title", "description",
                      "pubDate", "source", "place", "genre", "type", "period"))
  })
  df <- as.data.frame(t(matrix(unlist(list_of_lists), nrow=length(list_of_lists[[1]]))))
}))

names(df) <- c("title", "description", "pubDate", "source", "place", "genre", "type", "period")

## diagnostics
head(df)

## visual diagnostics for suddenly missed data
df %>% 
  # this takes from 6th to 16th character from the pubDate column
  mutate(pubDate = dmy(substr(pubDate, 6, 16))) %>%
  # introducing breaks by month as diagnostic feature
  mutate(posted_week = as.Date(cut(pubDate, breaks = "week"))) %>% 
  count(posted_week) %>% 
  ggplot(aes (x = posted_week, y = n)) + geom_col()

## reorganizing and cleaning the data a bit
df %<>% 
  # this takes from 6th to 16th character from the pubDate column
  mutate(pubDate = dmy(substr(pubDate, 6, 16))) %>%
  # introducing breaks by month as diagnostic feature
  mutate(posted_mon = as.Date(cut(pubDate, breaks = "month"))) %>% 
  # deleting items with duplicated titles
 filter(!duplicated(title)) %>% ## for the future: implement smart filtering
  # cleaning the text a bit
  mutate(description = gsub("\\n|\\r|Следующий слайд", " ", description))


## writing the cleaned data into csv 
write.csv(df, "~/media_nlp/vestifm_df_may_sep.csv", row.names = FALSE)


# creating big dataset for may-septermber on 9 medias

filenames <- list.files(path = "~/media_nlp/may_sep", pattern="*.csv", full.names=TRUE)

test <- do.call(rbind, lapply(filenames, function(x) {
  filler <- read_csv(x, locale = readr::locale(encoding = "UTF-8"))
}))



filenames <- list.files(path = "~/media_nlp/may_sep_lem", pattern="*.csv", full.names=TRUE)

test_2 <- unlist(do.call(c, lapply(filenames, function(x) {
  filler <- read_csv(x, col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
})))

# Do the same as previous function, but there was something wrong with it...
test_1 <- read_csv(filenames[1], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_2 <- read_csv(filenames[2], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_3 <- read_csv(filenames[3], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_4 <- read_csv(filenames[4], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_5 <- read_csv(filenames[5], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_6 <- read_csv(filenames[6], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_7 <- read_csv(filenames[7], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))
test_9 <- read_csv(filenames[9], col_names = FALSE, locale = readr::locale(encoding = "UTF-8"))

gl_test <- rbind(test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_9)

# Binding news info and their lemmas
may_sep <- cbind(gl_test, test)

# fultering too short news
may_sep <- may_sep %>%
  mutate(X1 = gsub("[[:punct:]]|S", replacement = "", X1)) %>% 
  filter(nchar(X1) >= 125)









#######
#
# Playing with data
#
#######

write.csv(may_sep, file = "~/media_nlp/may_sep_full.csv", fileEncoding = "UTF-8")

library(stm)

may_sep_proc <- textProcessor(may_sep$X1, lowercase = TRUE,
                          removestopwords = TRUE, removenumbers = TRUE, language = "russian",
                          stem = FALSE, removepunctuation = FALSE,
                          metadata = may_sep)

plotRemoved(may_sep_proc$documents, lower.thresh=seq(0,5000, by=5))

ggplot(may_sep, aes(nchar(X1))) +
  geom_histogram(bins = 500) +
  scale_x_continuous(limits = c(0, 1000))

# lower.thresh = 15, upper.thresh = ??? ~230000 words removed

may_sep_out <- prepDocuments(may_sep_proc$documents, may_sep_proc$vocab, may_sep_proc$meta, lower.thresh = 10)

# Removing 219661 of 249966 terms (519006 of 10027530 tokens) due to frequency
# Your corpus now has 35063 documents, 30305 terms and 9508524 tokens.

may_sep_100 <- stm(documents = may_sep_out$documents, vocab = may_sep_out$vocab,
                  K = 100, max.em.its = 50, data = may_sep_out$meta,
                  init.type = "Spectral", seed = 42)

save(may_sep_100, file = "~/media_nlp/may_sep_100.RData")

may_sep_120 <- stm(documents = may_sep_out$documents, vocab = may_sep_out$vocab,
                   K = 120, max.em.its = 50, data = may_sep_out$meta,
                   init.type = "Spectral", seed = 42)

save(may_sep_120, file = "~/media_nlp/may_sep_120.RData")

may_sep_60 <- stm(documents = may_sep_out$documents, vocab = may_sep_out$vocab,
                   K = 60, max.em.its = 50, data = may_sep_out$meta,
                   init.type = "Spectral", seed = 42)

save(may_sep_60, file = "~/media_nlp/may_sep_60.RData")

may_sep_80 <- stm(documents = may_sep_out$documents, vocab = may_sep_out$vocab,
                  K = 80, max.em.its = 50, data = may_sep_out$meta,
                  init.type = "Spectral", seed = 42)

save(may_sep_80, file = "~/media_nlp/may_sep_80.RData")

may_sep_92 <- stm(documents = may_sep_out$documents, vocab = may_sep_out$vocab,
                  K =92, max.em.its = 50, data = may_sep_out$meta,
                  init.type = "Spectral", seed = 42)

save(may_sep_92, file = "~/media_nlp/may_sep_92.RData")


plot(may_sep_100, type = "summary", labeltype = "frex", topics = 1:30, n = 5)

findTopic(may_sep_100, type = "frex", n = 20, c("covid", "sars", "ковид", "коронавирус"))

toLDAvis(may_sep_100, may_sep_out$documents, reorder.topics = F)

plot(topicCorr(may_sep_100, cutoff = 0.15))

heatmap(topicCorr(may_sep_100)$cor, scale = "none")

fviz_nbclust(topicCorr(frb_ctm_80)$cor, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(topicCorr(frb_ctm_80)$cor, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(topicCorr(frb_ctm_80)$cor, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#stmCorrViz
# stmCorrViz(may_sep_100, "corrviz.html", documents_raw = forbes$description, documents_matrix = frb_proc$documents, verbose = T)

# toLDAvis
toLDAvis(frb_ctm_80, frb_out$documents, reorder.topics = FALSE)
