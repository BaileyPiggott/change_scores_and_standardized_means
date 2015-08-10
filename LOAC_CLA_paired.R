# CLA paired data
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

all_cla <- read.csv("LOAC CLA Paired.csv") %>% 
  filter(score_total > 0) %>%
  select(studentid, subject, discipline, project_year, score_total) %>%
  mutate(scaled_score = scale(score_total))
write.csv(all_cla, "Scaled CLA scores.csv") # scaling the score column makes this not a data frame anymore

all_cla <- read.csv("Scaled CLA scores.csv")# reload as a data frame
unlink("Scaled CLA scores.csv") # delete file

cla_1 <- all_cla %>% 
  filter(project_year == 1)

cla_2 <- all_cla %>% 
  filter(project_year == 2)

cla <- inner_join(cla_1, cla_2, by = c("studentid", "subject", "discipline"))  %>%
  mutate(change = scaled_score.y - scaled_score.x)


cla_eng <- cla %>% filter(subject == "APSC") 
n_cla_eng <- nrow(cla_eng)
cla_psyc <- cla %>% filter(subject == "PSYC")
n_cla_psyc <- nrow(cla_psyc)


#CALCULATE AVERAGE CHANGE SCORES ---------------
eng_change_cla <- mean(cla_eng$change)
psyc_change_cla <- mean(cla_psyc$change)

cla_change <- data.frame(subject = c("APSC", "PSYC"), change = c(eng_change_cla, psyc_change_cla), test = "CLA")



# CREATE PLOT --------------------------


ggplot(
  data = cla_change, 
  aes(x = subject, y = change, fill = subject)) +
  geom_bar(stat = "identity", width = 0.3) +
  #coord_cartesian(ylim = c(0, 0.4)) +
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
