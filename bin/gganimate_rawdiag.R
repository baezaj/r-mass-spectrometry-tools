# setup -------------------------------------------------------------------



library(tidyverse)
library(gganimate)
library(rio)
library(lubridate)
library(ggsci)
library(glue)



# data import -------------------------------------------------------------



raw <- import("../intermediate_data/EXP22-271443_rawdiag_data.csv")
rando_color <- import("../../R/Color_Pallette.txt")





# Formatting --------------------------------------------------------------



# Formatting color pallette
rando_color <- rando_color %>% filter(template_type == "new") %>% 
  select(`Color Pallette`, Hex) %>% 
  column_to_rownames("Color Pallette") %>% 
  pull()



names(rando_color) <- c("Batch 1", "Ref", "Batch 2", "pool", "GPF", "Batch 3", "Batch 4")





rando_color <- rando_color[c("Batch 1", "Batch 2", "Batch 3", "Batch 4", "Ref", "pool", "GPF")]





raw$creation_date <- ymd_hms(raw$creation_date)





annot <- raw %>% 
  select(filename:creation_date) %>% 
  distinct()





raw <- raw %>% 
  mutate(batch = case_when(ms_type == "gpf" ~ "GPF", 
                           time_point == "" ~ "Ref",
                           batch == "1" ~ "Batch 1",
                           batch == "2" ~ "Batch 2",
                           batch == "3" ~ "Batch 3",
                           batch == "4" ~ "Batch 4",
                           TRUE ~ batch))





# gganimation -------------------------------------------------------------





tic_gif <- raw %>% 
  filter(
    ms_type == "dia",
    MSOrder == "Ms"
  ) %>%
  unite(condition_id, condition_1, time_point, batch, sep = "_", remove = FALSE) %>%
  ggplot() +
  geom_line(aes(x = StartTime, y = TIC, group = run_order), size = 0.03) +
  theme_classic(base_size = 12) +
  labs(#title = "Bioreactor: {closest_layer}",
    x = "Retention Time (min)") +
  transition_time(creation_date) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.05)



animate(tic_gif, height = 4, width = 5, units = "in", res = 100)
anim_save(filename = "../figures/RawDiag_TIC_Chromatogram_DIA.gif", animation = last_animation())
ggsave(filename = "../figures/RawDiag_TIC_Chromatogram_DIA.png", height = 4, width = 5, units = "in")





# Boxplot - grouped by batch
tic_boxplot_gif <- raw %>% 
  filter(
    ms_type == "dia",
    MSOrder == "Ms2"
    # run_order < 10
  ) %>% 
  unite(condition_id, condition_1, time_point, batch, run_order, sep = "_", remove = FALSE) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(condition_id, creation_date), y = TIC, fill = factor(batch)), outlier.size = 0.5) +
  geom_hline(yintercept = median(raw$TIC[raw$MSOrder == "Ms2"], na.rm = TRUE)) +
  scale_fill_manual(values = rando_color[-length(rando_color)]) +
  scale_y_log10() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(fill = "Sample\nBatch",
       x = "MS Runs",
       y = "MS2 TIC") +
  transition_time(creation_date) +
  shadow_mark(past = TRUE, future = FALSE)





animate(tic_boxplot_gif, height = 4, width = 7, units = "in", res = 100)



anim_save(filename = "../figures/RawDiag_TIC_boxplot_DIA.gif", animation = last_animation())
# ggsave(filename = "../figures/RawDiag_TIC_boxplot_DIA.png", height = 4, width = 7, units = "in")



# with GPF ----------------------------------------------------------------





tic2_gif <- raw %>% 
  filter(
    # ms_type == "dia",
    MSOrder == "Ms"
  ) %>%
  unite(condition_id, condition_1, time_point, batch, sep = "_", remove = FALSE) %>%
  ggplot() +
  geom_line(aes(x = StartTime, y = TIC, group = run_order), size = 0.03) +
  theme_classic(base_size = 12) +
  labs(#title = "Bioreactor: {closest_layer}",
    x = "Retention Time (min)") +
  transition_time(creation_date) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.05)







animate(tic2_gif, height = 4, width = 5, units = "in", res = 100)
anim_save(filename = "../figures/RawDiag_DIA_GPF_TIC_Chromatogram_DIA.gif", animation = last_animation())
# ggsave(filename = "../figures/RawDiag_DIA_GPF_TIC_Chromatogram_DIA.png", height = 4, width = 5, units = "in")





# Boxplot - grouped by batch
tic2_boxplot_gif <- raw %>% 
  filter(
    # ms_type == "dia",
    MSOrder == "Ms2"
    # run_order < 10
  ) %>% 
  unite(condition_id, condition_1, time_point, batch, run_order, sep = "_", remove = FALSE) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(condition_id, creation_date), y = TIC, fill = factor(batch)), outlier.size = 0.5) +
  geom_hline(yintercept = median(raw$TIC[raw$MSOrder == "Ms2"], na.rm = TRUE)) +
  scale_fill_manual(values = rando_color) +
  scale_y_log10() +
  theme_classic(base_size = 14) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(fill = "Sample\nBatch",
       x = "MS Runs",
       y = "MS2 TIC") +
  transition_time(creation_date) +
  shadow_mark(past = TRUE, future = FALSE)





animate(tic2_boxplot_gif, height = 4, width = 7, units = "in", res = 100)
anim_save(filename = "../figures/RawDiag_DIA_GPF_TIC_boxplot_DIA.gif", animation = last_animation())
# ggsave(filename = "../figures/RawDiag_DIA_GPF_TIC_boxplot_DIA.png", height = 4, width = 7, units = "in")

