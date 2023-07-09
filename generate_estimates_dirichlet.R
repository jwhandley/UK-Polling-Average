library(tidyverse)
library(cmdstanr)
library(britpol)

pollbase <- read_csv("pollbase.csv")

data <- pollbase %>%
  select(-ref) %>%
  filter(!is.na(grn)) %>%
  mutate(oth = 100 - con - lab - lib - grn) %>%
  mutate(across(c(con,lab,lib,grn,oth),~.x/100)) %>%
  filter(date >= as.Date("2019-12-12")) %>%
  mutate(t = interval(as.Date("2019-12-12"), date) %/% weeks(1)) %>%
  mutate(pollster = case_when(pollster == "Opinium" & date <= as.Date("2022-02-12") ~ "Opinium-Old",
                              pollster == "Opinium" & date >= as.Date("2022-02-12") ~ "Opinium-New",
                              TRUE ~ pollster))

data_list <- list(
  T = max(data$t),
  P = length(unique(data$pollster)),
  J = 5,
  N = nrow(data),
  pollster = as_factor(data$pollster) %>% as.numeric(),
  t = data$t,
  vi = data[,c("con","lab","lib","grn","oth")] %>% as.matrix(),
  vote0 = c(0.436, 0.321, 0.116, 0.0261, 1 - 0.436 - 0.321 - 0.116 - 0.0261) # Taken from GE2019 results
)

model <- cmdstan_model("polling_average_dirichlet.stan")

fit <- model$sample(
  data = data_list,
  iter_warmup = 500,
  iter_sampling = 500,
  chains = 4,
  parallel_chains = 4
)

pollster_error <- fit$draws("alpha_obs", format = "df") %>%
  as_tibble() %>%
  select(-c(".chain",".iteration",".draw"))
colnames(pollster_error) <- levels(as_factor(data$pollster))
pollster_error %>%
  pivot_longer(cols = everything(),
               names_to = "pollster",
               values_to = "sample") %>%
  ggplot(aes(x = log(sample))) +
  geom_density() +
  facet_wrap(~pollster)


fit$draws("house_effects", format = "matrix") %>%
  t() %>%
  as_tibble() -> house_effects

expand_grid(party = c("con","lab","lib","grn","oth"),
                             pollster = levels(as_factor(data$pollster))) %>%
  mutate(party = factor(party, levels = c("con","lab","lib","grn","oth")),
         pollster = factor(pollster, levels = levels(as_factor(data$pollster)))) %>%
  bind_cols(house_effects) %>%
  pivot_longer(cols = c(everything(),-pollster,-party), names_to = "draw_number", values_to = "draw") %>%
  ggplot(aes(x = fct_rev(party), fill = party)) +
  geom_hline(yintercept = 0) +
  geom_violin(aes(y = draw)) +
  coord_flip() +
  facet_wrap(~pollster) +
  scale_fill_manual(values = c("#2c88df","#d90f32","#f3a600","#3aa859","black"),
                    labels = c("Conservative","Labour","Lib Dem","Green","Other")) +
  scale_x_discrete(labels = c("Other","Green","Lib Dem","Labour","Conservative")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = NULL,
       x = NULL,
       fill = "Party",
       title = "Estimated pollster house effects in the Dirichlet model",
       caption = "@jwhandley17")
ggsave("house_effects_dirichlet.png",width=12,height=9)


fit$draws("alpha_obs", format = "df") %>%
  as_tibble() -> house_scales
colnames(house_scales)[1:18] <- unique(data$pollster)
house_scales %>%
  pivot_longer(cols = c(everything(),-c(".draw",".chain",".iteration")),
               names_to = "pollster", 
               values_to = "draw") %>%
  ggplot(aes(x = fct_reorder(pollster, draw), y = draw)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = NULL,
       y = "Dirichlet scale parameter",
       title = "Estimated pollster-specific scale parameter (higher is lower variance)")
ggsave("dirichlet_scale.png")

data_long <- data %>% 
  pivot_longer(c(con,lab,lib,grn,oth), names_to = "party", values_to = "vote") %>% 
  mutate(party = factor(party, levels = c("con","lab","lib","grn","oth"))) %>%
  select(-t,-pollster)

fit$draws("vote", format = "matrix") %>% 
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  bind_cols(
    expand_grid(party = factor(c("con","lab","lib","grn","oth"),
                               levels = c("con","lab","lib","grn","oth")),
                t = 1:max(data$t))
  ) %>%
  mutate(date = as.Date("2019-12-12") + weeks(t)) %>%
  left_join(data_long) %>%
  ggplot(aes(x = date)) +
  geom_jitter(aes(y = vote, color = party), alpha = 0.2, width = 0.5, height = 0) +
  geom_line(aes(y = `50%`, color = party)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = party), alpha = 0.3) +
  scale_color_manual(values = c("#2c88df","#d90f32","#f3a600","#3aa859","black"),
                     labels = c("Conservative","Labour","Lib Dem","Green","Other")) +
  scale_fill_manual(values = c("#2c88df","#d90f32","#f3a600","#3aa859","black"),
                    labels = c("Conservative","Labour","Lib Dem","Green","Other")) +
  scale_y_continuous(labels = scales::percent) +
  labs(color = "Party",
       fill = "Party",
       x = NULL,
       y = NULL,
       title = "Dirichlet-based UK poll aggregator",
       subtitle = "Pollbase Q1 2023 & Wikipedia",
       caption = "@jwhandley17")
ggsave("dirichlet_average.png",width=8,height=5)
  
