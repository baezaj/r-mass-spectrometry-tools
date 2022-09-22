require(tidyverse)
require(patchwork)


# power analysis ----------------------------------------------------------


prot_glance <- prot_nest %>% 
  mutate(glance = map(lm_data, broom::glance)) %>% 
  unnest(glance) %>% 
  select(-lm_data, -data)

FDR <- 0.05
power <- 0.9
quintile_variance <- round(boxplot.stats(prot_glance$sigma^2, coef = 1.5)$stats, digits = 5)

desired_FC <- c(1.5,8)
delta <- log2(seq(desired_FC[1], desired_FC[2], 0.01))
desired_FC <- 2^delta

m0_m1 <- 99
alpha <- power * FDR / (1 + (1 - FDR) * m0_m1)
z_alpha <- qnorm(1 - alpha/2)
z_beta <- qnorm(power)

power_df <- data.frame(desired_FC, delta, FDR, power)
variance_df <- data.frame(quintile_variance, FDR, power)
power_df <- full_join(power_df, variance_df)

power_df$numSample <- ceiling((2 * (z_alpha + z_beta)^2 * power_df$quintile_variance) / (power_df$delta)^2)
power_df$CV <- 2 * (power_df$quintile_variance/power_df$numSample) / power_df$desired_FC * 100


g1 <- as.data.frame(quintile_variance) %>% 
  ggplot() +
  geom_boxplot(aes(x = "a", y = quintile_variance)) +
  labs(x = NULL,
       y = expression(Variance~(sigma^2))) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Power analysis",
       subtitle = "Variance quintiles")


# Power analysis using 
g2 <- ggplot(power_df) +
  geom_line(aes(x = desired_FC, y = numSample, group = quintile_variance, color = factor(quintile_variance))) +
  scale_y_continuous(limits = c(0,20), n.breaks = 11) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 14) +
  labs(title = "Power analysis",
       subtitle = "grouped by variance quintiles",
       color = "Variance levels",
       y = "minimum number of samples needed",
       x = "Desired Fold change") +
  annotate("text", x = 6.5, y = c(19, 17), label = c(paste0("FDR - ", FDR), paste0("Power - ", power)), size = 5)

g1 + g2 + plot_annotation(tag_levels = "A")
