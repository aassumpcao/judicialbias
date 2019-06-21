# script creating last graph in the paper
rm(list = ls())
library(tidyverse)

# load results
gini <- readRDS('data/decrease_gini.Rds') %>%
        arrange(desc(MeanDecreaseGini)) %>%
        slice(1:6) %>%
        mutate_at(1, as.character)

# insert last row to cover eveything else
gini[7,] <- c('All Others', 50, rep(NA_character_, 2))
gini %<>% mutate_at(2, as.integer)

# create labels
labs <- c('Judge Tenure', 'Case Claim', 'Judge Pay', 'Election Share',
          'Politician is Defendant', 'Candidate Age', 'All Others')

# produce plot
ggplot(data = gini) +
  geom_point(aes(MeanDecreaseGini, 1:7), size = 2) +
  geom_segment(aes(x = 0, xend = MeanDecreaseGini, y = 1:7, yend = 1:7),
    size = 1) +
  geom_segment(aes(x = 0, xend = 300, y = 4.5, yend = 4.5), linetype = 'dashed',
    color = 'gray79') +
  geom_text(aes(MeanDecreaseGini, 1:7), label = c(unlist(gini[1:6, 2]), '< 50'),
    nudge_x = 15, family = 'LM Roman 10') +
  scale_y_reverse(name = NULL, breaks = 1:7, labels = labs) +
  theme_bw() +
  xlab('Mean Decrease in Gini') +
  theme(axis.title = element_text(size = 10),
          axis.title.y = element_text(margin = margin(r = 12)),
          axis.title.x = element_text(margin = margin(t = 12)),
          axis.text.y = element_text(size = 10, lineheight = 1, face = 'bold'),
          axis.text.x = element_text(size = 10, lineheight = 1, face = 'bold'),
          text = element_text(family = 'LM Roman 10'),
          panel.border = element_rect(color = 'black', size = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
    )

# save plot
ggsave('rf-varimp-aa.pdf', device = cairo_pdf, path = 'plots', dpi = 100,
       width = 7, height = 5)
