#H1
#The left ideological in-groups (and right ideological out-groups) are: 
#left-wing people, Muslims, people who get state subsidies, climate activists;
#The right ideological in-groups (and left ideological out-groups) are: 
#right-wing people, neoliberal politicians, materialistic people, and big companies.
d_left <- d |> 
  filter(ideology==0)
d_right <- d |>  
  filter(ideology==1)

h1 <- tidy(lm(H19_1 ~  treatment, d_right)) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Right-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

tmp <- tidy(lm(H19_1 ~  treatment, d_left)) |> 
  mutate(y = "Multinationals",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Left-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  treatment, d_right)) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Right-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_2 ~  treatment, d_left)) |> 
  mutate(y = "Neo-Liberal Politicians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Left-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  treatment, d_right)) |> 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Right-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_3 ~  treatment, d_left)) |> 
  mutate(y = "Right-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Left-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  treatment, d_left)) |> 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_4 ~  treatment, d_right)) |> 
  mutate(y = "Left-Wing Voters",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  treatment, d_right)) |> 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Right-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_5 ~  treatment, d_left)) |> 
  mutate(y = "Materialistic People",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Left-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  treatment, d_left)) |> 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_6 ~  treatment, d_right)) |> 
  mutate(y = "Climate Activists",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  treatment, d_left)) |> 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_7 ~  treatment, d_right)) |> 
  mutate(y = "Immigrants",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  treatment, d_left)) |> 
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_8 ~  treatment, d_right)) |> 
  mutate(y = "Living on Benefits",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)


h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_9 ~  treatment, d_left)) |> 
  mutate(y = "Goverment",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_9 ~  treatment, d_right)) |> 
  mutate(y = "Goverment",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  treatment, d_left)) |> 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Left-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_10 ~  treatment, d_right)) |> 
  mutate(y = "Muslims",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Right-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  treatment, d_right)) |> 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social In-Groups",
         id = "Right-Wing",
         hyp = "a",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |> 
  add_case(tmp)

tmp <- tidy(lm(H19_11 ~  treatment, d_left)) |> 
  mutate(y = "Christians",
         var = "Affective Sentiment for Ideological Social Out-Groups",
         id = "Left-Wing",
         hyp = "b",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) |> 
  dplyr::select(term, y, estimate, upper, lower, var, id, hyp)

h1 <- h1 |>  
  add_case(tmp) |> 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                       `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat",
                       `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat"))

p1a_l <- h1 |> 
  filter(term != "Intercept", hyp == "a", id == "Left-Wing") |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", x = "") + 
       #x = "Predicted Effect for Support for Social In-Groups",
       #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1a_r <- h1 |> 
  filter(term != "Intercept", hyp == "a", id == "Right-Wing") |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Support for Social In-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1b_l <- h1 |> 
  filter(term != "Intercept", hyp == "b", id == "Left-Wing") |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", x = "") + 
  #x = "Predicted Effect for Support for Social Out-Groups",
  #caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1b_r <- h1 |> 
  filter(term != "Intercept", hyp == "b", id == "Right-Wing") |> 
  ggplot(aes(y = y, x = estimate,
             xmin = lower, xmax = upper,
             color = term)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  facet_grid(.~id, scales = "free") +
  labs(y = "", 
       x = "Predicted Effect for Support for Social Out-Groups",
       caption = "Results are based on analyses controlled for other experimental conditions as well as unbalanced covariates") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_vline(xintercept = 0, size = .2, linetype = "dashed") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
