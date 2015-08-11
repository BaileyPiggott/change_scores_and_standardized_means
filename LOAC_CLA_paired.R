# CLA paired data
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)

# LOAD DATA ----------------------
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

# SEPARATE BY PROGRAM -----------------
cla_eng <- cla %>% filter(subject == "APSC") 
n_cla_eng <- nrow(cla_eng)
cla_psyc <- cla %>% filter(subject == "PSYC")
n_cla_psyc <- nrow(cla_psyc)

#CALCULATE AVERAGE CHANGE SCORES ---------------
eng_change_cla <- mean(cla_eng$change)
psyc_change_cla <- mean(cla_psyc$change)

cla_change <- data.frame(subject = c("APSC", "PSYC"), change = c(eng_change_cla, psyc_change_cla), test = "CLA")

# SEPARATE BY ENGINEERING DISCIPLINE -------------------

cla_mech <- cla %>% filter(discipline == "MECH-M-BSE")
cla_elec <- cla %>% filter(discipline == "ELEC-M-BSE")
cla_cmpe <- cla %>% filter(discipline == "CMPE-M-BSE")
cla_civl <- cla %>% filter(discipline == "CIVL-M-BSE")
cla_chee <- cla %>% filter(discipline == "CHEE-M-BSE")
cla_ench <- cla %>% filter(discipline == "ENCH-M-BSE")
cla_mine <- cla %>% filter(discipline == "MINE-M-BSE")
cla_geoe <- cla %>% filter(discipline == "GEOE-M-BSE")
cla_enph <- cla %>% filter(discipline == "ENPH-M-BSE")
cla_mthe <- cla %>% filter(discipline == "MTHE-M-BSE")

# ENGINEERING DISCIPLINE CHANGE SCORES -----------------------

mech_change_cla <- mean(cla_mech$change)
elec_change_cla <- mean(cla_elec$change)
civl_change_cla <- mean(cla_civl$change)
cmpe_change_cla <- mean(cla_cmpe$change)
chee_change_cla <- mean(cla_chee$change)
ench_change_cla <- mean(cla_ench$change)
mine_change_cla <- mean(cla_mine$change)
geoe_change_cla <- mean(cla_geoe$change)
enph_change_cla <- mean(cla_enph$change)
mthe_change_cla <- mean(cla_mthe$change)

eng_disp_cla_change <- data.frame(discipline = c("MECH", "ELEC", "CMPE", "CIVL", "CHEE", "ENCH", "MINE", "GEOE", "ENPH", "MTHE"), 
                                  change = c(mech_change_cla,elec_change_cla,cmpe_change_cla,civl_change_cla,chee_change_cla,ench_change_cla,mine_change_cla,geoe_change_cla,enph_change_cla,mthe_change_cla), 
                                  test = "CLA")

# CREATE PLOT --------------------------
ggplot(
  data = eng_disp_cla_change, 
  aes(x = discipline, y = change)) +
  geom_bar(stat = "identity", width = 0.3) +
  coord_cartesian(ylim = c(-0.8, 0.40)) +
  labs(title = "Standardized CLA Change Scores Year 1-Year 2 per Engineering Discipline") +
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
