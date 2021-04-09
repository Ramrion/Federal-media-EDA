library(stm)
library(tidystm)
library(readr)
library(tidyverse)
library(plyr)

full_may_sep <- read_csv("~/year2019.csv")

# clearing from blue cluster & fontanka
# full_may_sep <- full_may_sep %>% 
#   filter(!(names_source %in% c("Ridus.ru", "Pnp.ru", "Fontanka.ru", "Komsa_pravda_msc"))) %>%
#   droplevels()


full_may_sep <- textProcessor(full_may_sep$lemma, lowercase = FALSE,
                              removestopwords = FALSE, removenumbers = FALSE, language = "na",
                              stem = FALSE, removepunctuation = FALSE,
                              metadata = full_may_sep)

out <- prepDocuments(full_may_sep$documents, full_may_sep$vocab, full_may_sep$meta, lower.thresh = 15)


# Correlated topic model
may_sep_ctm_xplore <- stm(documents = out$documents, vocab = out$vocab,
                          K = 0, max.em.its = 50, data = out$meta,
                          init.type = "Spectral", seed = 42)

# Part with estimating covariate effects, but we don't use it here
# x <- may_sep_ctm_xplore$beta$logbeta[[1]] %>% nrow
# 
# prep_n <- estimateEffect(formula = 1:x ~ cluster, stmobj = may_sep_ctm_xplore,
#                           metadata = out$meta,
#                           uncertainty = "Global")
# 
# effect_n <- extract.estimateEffect(prep_n, "cluster", method = "pointestimate")



save(out, may_sep_ctm_xplore, file = "year2019.RData")
