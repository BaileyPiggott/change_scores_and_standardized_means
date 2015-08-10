# CAT paired data

library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

# LOAD DATA ---------------------
cat <- read.csv("LOAC CAT Paired.csv") %>% mutate(scaled_score = scale(score))
write.csv(cat, "Scaled CAT scores.csv") # scaling the score column makes this not a data frame anymore

cat <- read.csv("Scaled CAT scores.csv")# reload as a data frame
unlink("Scaled CAT scores.csv") # delete file

cat_1 <- cat %>% 
  filter(project_year == 1) %>% 
  select(-c(course, test, X, X.1, year, consent)) 

cat_2 <- cat %>% 
  filter(project_year == 2) %>% 
  select(-c(course, test, X, X.1, year, consent)) 

all_cat <- inner_join(cat_1, cat_2, by = c("studentid", "subject", "discipline"))  %>%
  mutate(change = scaled_score.y - scaled_score.x)


cat_eng <- all_cat %>% filter(subject == "APSC") 
n_eng <- nrow(cat_eng)
cat_dram <- all_cat %>% filter(subject == "DRAM")
n_dram <- nrow(cat_dram)


#CALCULATE AVERAGE CHANGE SCORES ---------------
eng_change <- mean(cat_eng$change)
dram_change <- mean(cat_dram$change)

cat_change <- data.frame(subject = c("APSC", "DRAM"), change = c(eng_change, dram_change))

# CREATE PLOT ---------------

ggplot(
  data = cat_change, 
  aes(x = subject, y = change, fill = subject)) +
  geom_bar(stat = "identity", width = 0.3) +
  coord_cartesian(ylim = c(0, 0.4)) +
  labs(title = "CAT Change Scores") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.major.y = element_line(colour = "grey"),
    legend.position = "none",
    plot.title = element_text(size = 15),
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
    )
  


#HISTOGRAM OF CHANGE SCORES ----------------------

ggplot(data = cat_dram, aes(x = change)) +
  geom_histogram() +
  coord_cartesian(xlim = c(-2,2))

