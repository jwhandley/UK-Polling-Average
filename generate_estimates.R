library(tidyverse)
library(cmdstanr)

# Load pollbase data
pollbase <- read_csv("pollbase.csv")

# Pivot to long format
data <- pollbase %>%
  as_tibble() %>%
  pivot_longer(cols = c(con,lab,lib,grn,ref), names_to = "party", values_to = "vote") %>%
  filter(!is.na(vote)) %>%
  mutate(pollster = case_when(pollster == "Opinium" & date <= as.Date("2022-02-12") ~ "Opinium-Old",
                              pollster == "Opinium" & date >= as.Date("2022-02-12") ~ "Opinium-New",
                              TRUE ~ pollster))

# Create integer time values for use in Stan
# Using weeks as the default time step to compromise between timeliness and computational cost
data$t <- interval(as.Date("2019-12-12"), data$date) %/% weeks(1) + 1

# Compile model
model <- cmdstan_model("polling_average_multivariate.stan")

# Data for Stan
data_list <- list(
  T = max(data$t),
  K = length(unique(data$pollster)),
  J = length(unique(data$party)),
  N = nrow(data),
  k = as_factor(data$pollster) %>% as.numeric(),
  j = as_factor(data$party) %>% as.numeric(),
  t = data$t,
  vi = data$vote,
  vote0 = c(43.6, 32.1, 11.6, 2.6, 2) # Taken from GE2019 results
)

# Fit model
fit <- model$sample(
  data = data_list,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 2000
)

# Extract estimates vote shares and 95% CIs
fit$draws("vote", format = "matrix") %>%
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() -> vote

# Plot vote shares
tibble(party = rep(c("con","lab","lib","grn","ref"),max(data$t)),
       t = rep(1:max(data$t), each = 5)) %>%
  mutate(party = factor(party, levels = c("con","lab","lib","grn","ref")),
         date = weeks(t-1) + as.Date("2019-12-12")) %>%
  bind_cols(vote) %>%
  ggplot(aes(x = date, group = party)) +
  geom_line(aes(y = `50%`, color = party)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = party), alpha = 0.3) +
  scale_color_manual(values = c("#2c88df","#d90f32","#f3a600","#3aa859","#42b6d1"),
                     labels = c("Conservative","Labour","Lib Dem","Green","Ref UK")) +
  scale_fill_manual(values = c("#2c88df","#d90f32","#f3a600","#3aa859","#42b6d1"),
                    labels = c("Conservative","Labour","Lib Dem","Green","Ref UK")) +
  labs(x = NULL,
       y = NULL,
       fill = "Party",
       color = "Party",
       title = "Estimated vote share by party, 2020-present",
       subtitle = "Pollbase Q1 2023 release",
       caption = "@jwhandley17")
ggsave("polling_average.png",width=8,height=5)

# Extract pollster house effects
fit$draws("mu", format = "matrix") %>% t() -> mu
house_effects <- expand_grid(pollster = levels(as_factor(data$pollster)),
                             party = levels(as_factor(data$party))) %>%
  mutate(party = factor(party, levels = levels(as_factor(data$party))),
         pollster = factor(pollster, levels = levels(as_factor(data$pollster)))) %>%
  bind_cols(mu) %>%
  pivot_longer(cols = c(everything(),-pollster,-party), names_to = "draw_number", values_to = "draw")

# Plot house effects
house_effects %>%
  mutate(party = fct_rev(party)) %>%
  ggplot(aes(x = party, fill = party)) +
  geom_violin(aes(y = draw)) +
  coord_flip() +
  facet_wrap(~pollster) +
  scale_fill_manual(values = c("#42b6d1","#3aa859","#f3a600","#d90f32","#2c88df"),
                    labels = c("Ref UK","Green","Lib Dem","Labour","Conservative")) +
  scale_x_discrete(labels = c("Ref UK","Green","Lib Dem","Labour","Conservative")) +
  labs(x = "Party",
       y = "House effect",
       fill = "Party",
       title = "House effects by pollster in the UK, 2020-present")
ggsave("house_effects.png",width=12,height=9)
