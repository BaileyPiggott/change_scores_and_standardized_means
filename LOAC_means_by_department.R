# LOAC mean scores by department

source("LOAC_CAT_paired.R")
source("LOAC_CLA_paired.R")
source("LOAC_VALUE_paired.R")

# CALCULATE MEAN CHANGE SCORES -----------

value_eng_1 <- mean(value_eng$scaled_score.x)
value_eng_2 <- mean(value_eng$scaled_score.y)
cat_eng_1 <- mean(cat_eng$scaled_score.x)
cat_eng_2 <- mean(cat_eng$scaled_score.y)
cla_eng_1 <- mean(cla_eng$scaled_score.x)
cla_eng_2 <- mean(cla_eng$scaled_score.y)

value_dram_1 <- mean(value_dram$scaled_score.x)
value_dram_2 <- mean(value_dram$scaled_score.y)
cat_dram_1 <- mean(cat_dram$scaled_score.x)
cat_dram_2 <- mean(cat_dram$scaled_score.y)

value_psyc_1 <- mean(value_psyc$scaled_score.x)
value_psyc_2 <- mean(value_psyc$scaled_score.y)
cla_psyc_1 <- mean(cla_psyc$scaled_score.x)
cla_psyc_2 <- mean(cla_psyc$scaled_score.y)

# ENGINEERING DISCIPLINE CHANGE SCORES -----------------

cat_mech_1 <- mean(cat_mech$scaled_score.x)
cat_mech_2 <- mean(cat_mech$scaled_score.y)
cla_mech_1 <- mean(cla_mech$scaled_score.x)
cla_mech_2 <- mean(cla_mech$scaled_score.y)

cat_elec_1 <- mean(cat_elec$scaled_score.x)
cat_elec_2 <- mean(cat_elec$scaled_score.y)
cla_elec_1 <- mean(cla_elec$scaled_score.x)
cla_elec_2 <- mean(cla_elec$scaled_score.y)

cat_cmpe_1 <- mean(cat_cmpe$scaled_score.x)
cat_cmpe_2 <- mean(cat_cmpe$scaled_score.y)
cla_cmpe_1 <- mean(cla_cmpe$scaled_score.x)
cla_cmpe_2 <- mean(cla_cmpe$scaled_score.y)

cat_civl_1 <- mean(cat_civl$scaled_score.x)
cat_civl_2 <- mean(cat_civl$scaled_score.y)
cla_civl_1 <- mean(cla_civl$scaled_score.x)
cla_civl_2 <- mean(cla_civl$scaled_score.y)

cat_chee_1 <- mean(cat_chee$scaled_score.x)
cat_chee_2 <- mean(cat_chee$scaled_score.y)
cla_chee_1 <- mean(cla_chee$scaled_score.x)
cla_chee_2 <- mean(cla_chee$scaled_score.y)

cat_ench_1 <- mean(cat_ench$scaled_score.x)
cat_ench_2 <- mean(cat_ench$scaled_score.y)
cla_ench_1 <- mean(cla_ench$scaled_score.x)
cla_ench_2 <- mean(cla_ench$scaled_score.y)

cat_mine_1 <- mean(cat_mine$scaled_score.x)
cat_mine_2 <- mean(cat_mine$scaled_score.y)
cla_mine_1 <- mean(cla_mine$scaled_score.x)
cla_mine_2 <- mean(cla_mine$scaled_score.y)

cat_geoe_1 <- mean(cat_geoe$scaled_score.x)
cat_geoe_2 <- mean(cat_geoe$scaled_score.y)
cla_geoe_1 <- mean(cla_geoe$scaled_score.x)
cla_geoe_2 <- mean(cla_geoe$scaled_score.y)

cat_enph_1 <- mean(cat_enph$scaled_score.x)
cat_enph_2 <- mean(cat_enph$scaled_score.y)
cla_enph_1 <- mean(cla_enph$scaled_score.x)
cla_enph_2 <- mean(cla_enph$scaled_score.y)

cat_mthe_1 <- mean(cat_mthe$scaled_score.x)
cat_mthe_2 <- mean(cat_mthe$scaled_score.y)
cla_mthe_1 <- mean(cla_mthe$scaled_score.x)
cla_mthe_2 <- mean(cla_mthe$scaled_score.y)

# CREATE DATA FRAME ----------------------

std_means <- data.frame(
  subject = c("APSC", "DRAM", "PSYC"),
  cat_year1 = c(cat_eng_1, cat_dram_1, NA),
  cat_year2 = c(cat_eng_2, cat_dram_2, NA),
  cla_year1 = c(cla_eng_1, NA, cla_psyc_1),
  cla_year2 = c(cla_eng_2, NA, cla_psyc_2),
  value_year1 = c(value_eng_1, value_dram_1, value_psyc_1),
  value_year2 = c(value_eng_2, value_dram_2, value_psyc_2)
  ) %>%
  gather(test, change, -subject)# convert to long form

# ENGINEERING DISCIPLINE DATA FRAME ------------------

std_eng_means <- data.frame(
  discipline = c("MECH", "ELEC", "CMPE", "CIVL", "CHEE", "ENCH", "MINE", "GEOE", "ENPH", "MTHE"),        
  year = c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2),
  CAT = c(cat_mech_1, cat_elec_1, cat_cmpe_1, cat_civl_1, cat_chee_1, cat_ench_1, cat_mine_1, cat_geoe_1, cat_enph_1, cat_mthe_1,
    cat_mech_2, cat_elec_2, cat_cmpe_2, cat_civl_2, cat_chee_2, cat_ench_2, cat_mine_2, cat_geoe_2, cat_enph_2, cat_mthe_2),
  CLA = c(cla_mech_1, cla_elec_1, cla_cmpe_1, cla_civl_1, cla_chee_1, cla_ench_1, cla_mine_1, cla_geoe_1, cla_enph_1, cla_mthe_1,
    cla_mech_2, cla_elec_2, cla_cmpe_2, cla_civl_2, cla_chee_2, cla_ench_2, cla_mine_2, cla_geoe_2, cla_enph_2, cla_mthe_2)
  ) %>%
  gather(test, std_mean, CAT:CLA) 

# CREATE PLOT FOR DEPARTMENTS ----------------------

ggplot(data = std_means, 
  aes(x = test, y = change, fill = subject)
  ) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_vline(aes(xintercept = c(2.5, 4.5)),colour = "grey") +
  coord_cartesian(ylim = c(-1.3, 1)) +
  labs(title = "Standardized Mean per Year, by Department", y = "Standardized Mean") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.major.y = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11) #size of x axis labels
  ) +
  annotate(
    "text", 
    x = c(1.5, 3.5, 5.5, 1, 2, 3, 4, 5, 6), 
    y = c(-1.05,-1.05, -1.05, -1.15, -1.15, -1.15, -1.15, -1.15, -1.15),
    size = 4.5,
    vjust = 1, 
    label = c("CAT", "CLA", "VALUE", "Year 1", "Year 2", "Year 1", "Year 2", "Year 1", "Year 2")
    )  

# ENGINEERING PLOT -------------------
ggplot(data = std_eng_means, 
       aes(x = discipline, y = std_mean, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  facet_grid(test~.) + #separate vertically
  coord_cartesian(ylim = c(-0.6, 0.6)) +
  labs(title = "Standardized Mean per Year, by Discipline", x = "Engineering Discipline", y = "Standardized Mean") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.major.y = element_line(colour = "grey"),
    axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11), #size of x axis labels
    strip.text.y = element_text(size = 12, face = "bold") # facet text
    ) +
  scale_fill_discrete(
    labels = c("First Year", "Second Year")
    )