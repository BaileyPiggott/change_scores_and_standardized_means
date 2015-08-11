# change scores for all tests
source("LOAC_CAT_paired.R")
source("LOAC_CLA_paired.R")
source("LOAC_VALUE_paired.R")

# PLOT ---------------
all_change <- bind_rows(cat_change, cla_change, value_change)

ggplot(data = all_change, 
  aes(x = test, y = change, fill = subject)
  ) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  coord_cartesian(ylim = c(-0.25, 1)) +
  labs(title = "Standardized Change Scores Year 1-Year 2 per Instrument") +
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

# COMPARE ENGINEERING DISCIPLINES ------------------

eng_disp_change <- bind_rows(eng_disp_cat_change, eng_disp_cla_change) %>%
  arrange(test, change)

sample_sizes <- c(paste0("CHEE\nn=", nrow(cat_chee), "  n=", nrow(cla_chee)),
                  paste0("CIVL\nn=", nrow(cat_civl), "  n=", nrow(cla_civl)),
                  paste0("CMPE\nn=", nrow(cat_cmpe), "  n=", nrow(cla_cmpe)),
                  paste0("ELEC\nn=", nrow(cat_elec), "  n=", nrow(cla_elec)),
                  paste0("ENCH\nn=", nrow(cat_ench), "  n=", nrow(cla_ench)),
                  paste0("ENPH\nn=", nrow(cat_enph), "  n=", nrow(cla_enph)),
                  paste0("GEOE\nn=", nrow(cat_geoe), "  n=", nrow(cla_geoe)),
                  paste0("MECH\nn=", nrow(cat_mech), "  n=", nrow(cla_mech)),
                  paste0("MINE\nn=", nrow(cat_mine), "  n=", nrow(cla_mine)),
                  paste0("MTHE\nn=", nrow(cat_mthe), "  n=", nrow(cla_mthe))                  
                  )

# PLOT ENGINEERING DISCIPLINE CHANGE -----------------
ggplot(data = eng_disp_change, 
  aes(x = discipline, y = change, fill = test)
  ) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  coord_cartesian(ylim = c(-0.8, 0.8)) +
  scale_x_discrete(labels = sample_sizes) +
  labs(title = "Standardized Change Scores Year 1-Year 2") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(), # remove vertical lines
    panel.grid.major.y = element_line(colour = "grey"),
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11) #size of x axis labels
  ) 