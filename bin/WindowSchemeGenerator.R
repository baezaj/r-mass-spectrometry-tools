
# -------------------------------------------------------------------------


library(tidyverse)

# -------------------------------------------------------------------------


start_mz <- 400
stop_mz <- 1000

# For the HFX, Exploris480 and Ecclipse use 75 windows
# For the QE, QE-HF use 25 windows

num_windows <- 75

optimalMzIncrement <- 1.00045475

optimalMzConstant <- 0.25
# optimalMzConstant <- 0.241469079

increment <- ceiling((stop_mz - start_mz)/num_windows)


# Cycle 1 -----------------------------------------------------------------


cycle_1 <- data.frame(index = 0:(num_windows-1))

cycle_1$left <- start_mz + increment*cycle_1$index
cycle_1$right <- start_mz + increment*(cycle_1$index + 1)

cycle_1 <- cycle_1 %>% 
  mutate(start_mz = ceiling(left / optimalMzIncrement) * optimalMzIncrement + optimalMzConstant,
         stop_mz = ceiling(right / optimalMzIncrement) * optimalMzIncrement + optimalMzConstant,
         delta_mz = stop_mz - start_mz,
         center_mz = (stop_mz + start_mz) / 2) %>% 
  select(index, start_mz, center_mz, stop_mz, delta_mz)

 

# Cycle 2 -----------------------------------------------------------------


cycle_2 <- data.frame(index = 0:(num_windows))

cycle_2$left <- start_mz + increment*cycle_2$index - (0.5*increment)
cycle_2$right <- start_mz + increment*(cycle_2$index + 1) - (0.5*increment)

cycle_2 <- cycle_2 %>% 
  mutate(start_mz = ceiling(left / optimalMzIncrement) * optimalMzIncrement + optimalMzConstant,
         stop_mz = ceiling(right / optimalMzIncrement) * optimalMzIncrement + optimalMzConstant,
         delta_mz = stop_mz - start_mz,
         center_mz = (stop_mz + start_mz) / 2) %>% 
  select(index, start_mz, center_mz, stop_mz, delta_mz)


# Window Scheme -----------------------------------------------------------

window_scheme <- rbind(cycle_1, cycle_2) %>% 
  mutate(time = 1:nrow(.)) %>% 
  select(time, everything(), -index)

ggplot(window_scheme) +
  geom_linerange(aes(y = time, xmin = start_mz, xmax = stop_mz), size = 1, color = "blue") +
  theme_bw(base_size = 14) +
  labs(y = "Scan Number",
       x = "m/z")


# export ------------------------------------------------------------------


write_csv(window_scheme, file = paste0("DIA_wide-window_", start_mz, "_", stop_mz, "mz_", num_windows, "x", increment, "mz-ol",".csv"))
write_csv(window_scheme, file = paste0("DIA_GPF_", start_mz, "_", stop_mz, "mz_", num_windows, "x", increment, "mz-ol",".csv"))


