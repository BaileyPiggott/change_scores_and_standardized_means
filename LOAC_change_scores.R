# change scores for all tests

source("LOAC_CAT_paired.R")
source("LOAC_CLA_paired.R")
source("LOAC_VALUE_paired.R")

all_change <- bind_rows(cat_change, cla_change, value_change)


# PLOT ---------------

ggplot(data = all_change, 
  aes(x = test, y = change, fill = subject)
  ) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  coord_cartesian(ylim = c(-0.2, 1)) +
  labs(title = "Change Scores") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.major.y = element_line(colour = "grey"),
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
  ) +
  scale_fill_discrete(
    name = "Discipline", 
    labels = c("Engineering", "Drama", "Psychology")
    )
  