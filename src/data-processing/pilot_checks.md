Pilot Study
================

-   [Required Packages &
    Reproducibility](#required-packages--reproducibility)
-   [Pilot Data](#pilot-data)
-   [Differences by Treatment](#differences-by-treatment)
    -   [Populist Party Message or Anonymous Twitter
        User?](#populist-party-message-or-anonymous-twitter-user)
    -   [Nosstalgia Question Pre- or
        Post-Treatment?](#nosstalgia-question-pre--or-post-treatment)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
#renv::snapshot()
```

## Pilot Data

``` r
load(here("data/intermediate/pilot1.RData")) 
d1 <- d1 %>% 
  select(education, gender, age, ideology, collective_nostalgia,
         populist:elitist, nostalgia:message)
load(here("data/intermediate/pilot2.RData"))
d2 <- d2 %>% 
  select(education, gender, age, ideology, collective_nostalgia,
         populist, pluralist, elitist, nostalgia, issue,
         nostalgic_message, mc1, mc2, mc3, check_nostalgia,
         check_immigration, check_corruption, message)

d <- d1 %>% add_row(d2)
```

## Differences by Treatment

### Populist Party Message or Anonymous Twitter User?

``` r
m1 <- tidy(lm(populist ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Populist Attitude")

m2 <- tidy(lm(pluralist ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Pluralist Attitude")

m3 <- tidy(lm(elitist ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Elitist Attitude")

m1 %>% add_row(m2) %>% add_row(m3) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = recode(term, 
                       `as.factor(message)party` = "Populist Party Message",
                       `as.factor(issue)immigration` = "Issue: Immigration",
                       `as.factor(nostalgic_message)yes` = "Nostalgic Message"),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper,
             color = y)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicting Y") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
```

<img src="../../report/figures/Diff-DV-Treat-1.png" style="display: block; margin: auto;" />

``` r
m4 <- tidy(lm(check_nostalgia ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Message Perceived as Nostalgic")

m5 <- tidy(lm(check_immigration ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Message Perceived as about Immigration")

m6 <- tidy(lm(check_corruption ~ as.factor(message) +
                as.factor(issue) + as.factor(nostalgic_message), 
              data = d)) %>% 
  mutate(y = "Message Perceived as about Corruption")

m4 %>% add_row(m5) %>% add_row(m6) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = recode(term, 
                       `as.factor(message)party` = "Populist Party Message",
                       `as.factor(issue)immigration` = "Issue: Immigration",
                       `as.factor(nostalgic_message)yes` = "Nostalgic Message"),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper,
             color = y)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicting Y") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
```

<img src="../../report/figures/Diff-DV-Treat-2.png" style="display: block; margin: auto;" />

### Nosstalgia Question Pre- or Post-Treatment?

``` r
df <- d %>% 
  mutate(nostalgia = ifelse(nostalgia == "pre-treatment", 0, 1))
m7 <- lm(collective_nostalgia ~ 
                as.factor(nostalgia) * as.factor(nostalgic_message) +
                as.factor(message) + as.factor(issue), 
              data = df)

m7 <- summary(margins(m7, variables = "nostalgic_message", 
                      at = list(nostalgia = 0:1))) %>%
        mutate(lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) 

m7 %>% 
  mutate(nostalgia = recode(nostalgia, 
                       `0.0000` = "Nostalgic Attitudes: Pre-Treatment",
                       `1.0000` = "Nostalgic Attitudes: Post-Treatment")) %>% 
  ggplot(aes(x = nostalgia, 
             y = AME,
             ymin = lower,
             ymax = upper,
             color = factor)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects on Collective Nostalgic Attitudes for Exposure to Nostalgic Message") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
```

<img src="../../report/figures/nost-pre-post-1.png" style="display: block; margin: auto;" />
