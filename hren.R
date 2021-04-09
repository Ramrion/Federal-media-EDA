load("~/media_nlp/topic_dynamics.rdata")
load("~/media_nlp/topic_dynamics_binary.rdata")
load("~/media_nlp/thesaurus_media.rdata")

library(tidyverse)
library(data.table)

test <- topic_dynamics %>% 
  select(-covid, -total) %>% 
  pivot_wider(names_from = pubmon, values_from = share)

test <- plotting %>% 
  select(-n_cov, -n_tot) %>% 
  pivot_wider(names_from = pubmon, values_from = share)

row.names(test) <- as.character(test$source)

test[,1]

heatmap(as.matrix(test[,-1]), scale = NULL, labRow = rownames(test),
        hclustfun = hclust(, method = "ward.D")

library(superheat)
library(gplots)
library(pheatmap)

superheat(as.matrix(test[,-1]), dist.method = "euclidean",
          clustering.method = "hierarchical", scale = FALSE, )

test$source <- plyr::mapvalues(test$source, thesaurus$node, thesaurus$label) 
names(test) <- c('source',
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

pheatmap(as.matrix(test[,-1]), scale = "none", clustering_method = "ward.D2", cutree_rows = 3,
         clustering_distance_rows = "euclidean", cluster_cols = FALSE,
         show_rownames = TRUE,
         labels_row = rownames(test),
         fontsize = 14, cellwidth = 60, 
         cellheight = 20,  angle_col = 0, 
         color = brewer.pal(10, 'RdPu'), legend = T
         # color = viridis::plasma(8, begin = 0.1, end = 0.95, alpha = 0.9)
         )

row.names(test)
