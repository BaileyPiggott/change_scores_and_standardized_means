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

# separate by program ----------------
cat_eng <- all_cat %>% filter(subject == "APSC") 
n_cat_eng <- nrow(cat_eng)
cat_dram <- all_cat %>% filter(subject == "DRAM")
n_cat_dram <- nrow(cat_dram)

#CALCULATE AVERAGE CHANGE SCORES ---------------
eng_change_cat <- mean(cat_eng$change)
dram_change_cat <- mean(cat_dram$change)

cat_change <- data.frame(subject = c("APSC", "DRAM"), change = c(eng_change_cat, dram_change_cat), test = "CAT")

# SEPARATE BY ENGINEERING DISCIPLINE -------------------

cat_mech <- all_cat %>% filter(discipline == "MECH-M-BSE")
cat_elec <- all_cat %>% filter(discipline == "ELEC-M-BSE")
cat_cmpe <- all_cat %>% filter(discipline == "CMPE-M-BSE")
cat_civl <- all_cat %>% filter(discipline == "CIVL-M-BSE")
cat_chee <- all_cat %>% filter(discipline == "CHEE-M-BSE")
cat_ench <- all_cat %>% filter(discipline == "ENCH-M-BSE")
cat_mine <- all_cat %>% filter(discipline == "MINE-M-BSE")
cat_geoe <- all_cat %>% filter(discipline == "GEOE-M-BSE")
cat_enph <- all_cat %>% filter(discipline == "ENPH-M-BSE")
cat_mthe <- all_cat %>% filter(discipline == "MTHE-M-BSE")

# ENGINEERING DISCIPLINE CHANGE SCORES -----------------------

mech_change_cat <- mean(cat_mech$change)
elec_change_cat <- mean(cat_elec$change)
civl_change_cat <- mean(cat_civl$change)
cmpe_change_cat <- mean(cat_cmpe$change)
chee_change_cat <- mean(cat_chee$change)
ench_change_cat <- mean(cat_ench$change)
mine_change_cat <- mean(cat_mine$change)
geoe_change_cat <- mean(cat_geoe$change)
enph_change_cat <- mean(cat_enph$change)
mthe_change_cat <- mean(cat_mthe$change)

eng_disp_cat_change <- data.frame(discipline = c("MECH", "ELEC", "CMPE", "CIVL", "CHEE", "ENCH", "MINE", "GEOE", "ENPH", "MTHE"), 
                                  change = c(mech_change_cat,elec_change_cat,cmpe_change_cat,civl_change_cat,chee_change_cat,ench_change_cat,mine_change_cat,geoe_change_cat,enph_change_cat,mthe_change_cat), 
                                  test = "CAT"
                                  )


# CREATE PLOT ---------------

ggplot(
  data = eng_disp_cat_change, 
  aes(x = discipline, y = change)) +
  geom_bar(stat = "identity", width = 0.3) +
  coord_cartesian(ylim = c(-0.2, 0.8)) +
  labs(title = "Standardized CAT Change Scores Year 1-Year 2 per Engineering Discipline") +
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
  

