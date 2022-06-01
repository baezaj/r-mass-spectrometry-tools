
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
