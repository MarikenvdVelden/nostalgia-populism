#H4
#The left ideological in-groups (and right ideological out-groups) are: 
#left-wing people, Muslims, people who get state subsidies, climate activists;
#The right ideological in-groups (and left ideological out-groups) are: 
#right-wing people, neoliberal politicians, materialistic people, and big companies.
d_left <- d %>% 
  filter(ideology==0)
d_right <- d %>% 
  filter(ideology==1)

h4 <- tidy(lm(H19_1 ~  factor(nostalgia) +
                factor(scapegoat), d_right)) %>% 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

tmp <- tidy(lm(H19_1 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  factor(nostalgia) +
          factor(scapegoat), d_right)) %>% 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "People on State Subsidies",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "People on State Subsidies",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  factor(nostalgia) +
                 factor(scapegoat), d_right)) %>% 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  factor(nostalgia) +
                 factor(scapegoat), d_left)) %>% 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower, var)

h4 <- h4 %>% 
  add_case(tmp) %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `factor(nostalgia)1` = "Received Nostalgic Message",
                       `factor(scapegoat)1` = "Received Message with Scapegoat"),
         hyp = "Hypothesis 4 & 5") %>% 
  filter(term != "Intercept") %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(hyp~var, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Support for Social Group",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=3,byrow=TRUE))
