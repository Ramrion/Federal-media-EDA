load("~/media_nlp/topic_dynamics.rdata")
load("~/media_nlp/topic_dynamics_binary.rdata")
load("~/media_nlp/thesaurus_media.rdata")

library(tidyverse)
library(data.table)

# We create matrix with rows - media and columns - month
# the value inside it is the share of topics related to corona
covid_share <- topic_dynamics %>% 
  select(-covid, -total) %>% 
  pivot_wider(names_from = pubmon, values_from = share)

covid_binary_share <- plotting %>% 
  select(-n_cov, -n_tot) %>% 
  pivot_wider(names_from = pubmon, values_from = share)

row.names(covid_share) <- as.character(covid_share$source)

covid_share[,1]

heatmap(as.matrix(covid_share[,-1]), scale = NULL, labRow = rownames(covid_share),
        hclustfun = hclust(, method = "ward.D")

library(superheat)
library(gplots)
library(pheatmap)

superheat(as.matrix(covid_share[,-1]), dist.method = "euclidean",
          clustering.method = "hierarchical", scale = FALSE, )

covid_share$source <- plyr::mapvalues(covid_share$source, thesaurus$node, thesaurus$label) 
names(covid_share) <- c('source',
                 'янв',
                 'фев',
                 'мар',
                 'апр',
                 'май',
                 'июн',
                 'июл',
                 'авг',
                 'сен',
                 'окт'
)
library(RColorBrewer)
par(mar=(c(4,4,4,5)))

pheatmap(as.matrix(covid_share[,-1]), scale = "none", clustering_method = "ward.D2", cutree_rows = 3,
         clustering_distance_rows = "euclidean", cluster_cols = FALSE,
         show_rownames = TRUE,
         labels_row = rownames(covid_share),
         fontsize = 14, cellwidth = 60, 
         cellheight = 20,  angle_col = 0, 
         color = brewer.pal(10, 'RdPu'), legend = T
         # color = viridis::plasma(8, begin = 0.1, end = 0.95, alpha = 0.9)
         )

row.names(covid_share)
