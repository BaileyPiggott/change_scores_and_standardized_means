# VALUE paired data
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

all_value <- read.csv("LOAC VALUE Paired.csv") %>%
  gather(learning_outcome, score, Critical.Thinking_Conclusions.and.outcomes:Written.Communication_Sources.of.evidence) %>%
  filter(score >= 0) %>%
  mutate(scaled_score = scale(score))

write.csv(all_value, "Scaled VALUE scores.csv") # scaling the score column makes this not a data frame anymore

all_value <- read.csv("Scaled VALUE scores.csv")# reload as a data frame
unlink("Scaled VALUE scores.csv") # delete file

  
value_1 <- all_value %>% filter(program_year == 1) %>% select(-c(X.1, X, consent, course))
value_2 <- all_value %>% filter(program_year == 2) %>% select(-c(X.1, X, consent, course))

value <- inner_join(value_1, value_2, by = c("studentid", "subject", "learning_outcome"))  %>%
  mutate(change = scaled_score.y - scaled_score.x)

# separate by discipline ---------------------
value_eng <- value %>% filter(subject == "APSC") 
value_dram <- value %>% filter(subject == "DRAM") 
value_psyc <- value %>% filter(subject == "PSYC") 


#CALCULATE AVERAGE CHANGE SCORES ---------------
eng_change_value <- mean(value_eng$change)
psyc_change_value <- mean(value_psyc$change)
dram_change_value <- mean(value_dram$change)

value_change <- data.frame(subject = c("APSC", "PSYC", "DRAM"), change = c(eng_change_value, psyc_change_value, dram_change_value), test = "VALUE")



# CREATE PLOT -----------------------

ggplot(
  data = value_change, 
  aes(x = subject, y = change, fill = subject)) +
  geom_bar(stat = "identity", width = 0.3) +
  #coord_cartesian(ylim = c(0, 0.4)) +
  labs(title = "VALUE Change Scores") +
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
