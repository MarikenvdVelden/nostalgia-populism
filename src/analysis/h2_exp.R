#H2
h2 <- lm(H18 ~  E2_2*treatment +
           ideology, d)

pred_nostalgia1 <- interact_plot(model = h2, pred = E2_2, 
                                    modx = treatment, interval = FALSE,
                                    data = d, colors=fig_cols) +
  labs(y = "Predicted Probabilities of Support for Message",
       x = "Ideology \n (0 = Progressive, 1 = Conservative)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  geom_curve(
    aes(x = .2, y = 8.92, xend = .15, yend = 8.5),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[2]) +
  annotate("text", x = .2, y = 8.45, label = "Nostalgic Message",
           color = fig_cols[2]) +
  geom_curve(
    aes(x = .7, y = 8.68, xend = .75, yend = 9.08),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[1]) +
  annotate("text", x = .7, y = 9.12, label = "Not Nostalgic Message",
           color = fig_cols[1])

h2 <- lm(H18 ~  right_vote*treatment +
           ideology, d)

pred_nostalgia2 <- interact_plot(model = h2, pred = right_vote, 
                                 modx = treatment, interval = FALSE,
                                 data = d, colors=fig_cols) +
  labs(y = "Predicted Probabilities of Support for Message",
       x = "Vote for Right-Wing Party \n (0 = No, 1 = Yes)") +
  theme_ipsum() +
  theme(legend.position = "none") +
  geom_curve(
    aes(x = .2, y = 9.23, xend = .15, yend = 8.83),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[2]) +
  annotate("text", x = .2, y = 8.78, label = "Nostalgic Message",
           color = fig_cols[2]) +
  geom_curve(
    aes(x = .7, y = 9.59, xend = .75, yend = 9.99),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[1]) +
  annotate("text", x = .7, y = 10.05, label = "Not Nostalgic Message",
           color = fig_cols[1])

h2 <- lm(H18 ~  E2_2*treatment +
           ideology, d)
h2 <- summary(margins(h2, variables = "treatment", at = list(E2_2 = 0:1)))
h2 <- as_tibble(h2) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Support for Message",
         var = "Ideology") %>% 
  dplyr::select(factor, var, value = E2_2, y, AME, upper, lower)

tmp <- lm(H18 ~  right_vote*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(right_vote = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Support for Message",
         var = "Party Supporter") %>% 
  dplyr::select(factor, var, value = right_vote, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_self ~  E2_2*treatment+
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(E2_2 = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Respondent",
         var = "Ideology") %>% 
  dplyr::select(factor, var, value = E2_2, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_self ~  right_vote*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(right_vote = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Respondent",
         var = "Party Supporter") %>% 
  dplyr::select(factor, var, value = right_vote, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_dutch ~  E2_2*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(E2_2 = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Dutch Voters",
         var = "Ideology") %>% 
  dplyr::select(factor, var, value = E2_2, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_dutch ~  right_vote*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(right_vote = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Dutch Voters",
         var = "Party Supporter") %>% 
  dplyr::select(factor, var, value = right_vote, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_ingroup ~  E2_2*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(E2_2 = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for [Leftwing/Rightwing] Voters",
         var = "Ideology") %>% 
  dplyr::select(factor, var, value = E2_2, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp)

tmp <- lm(nostalgia_support_ingroup ~  right_vote*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(right_vote = 0:1)))
tmp <- as_tibble(tmp) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for [Leftwing/Rightwing] Voters",
         var = "Party Supporter") %>% 
  dplyr::select(factor, var, value = right_vote, y, AME, upper, lower)

h2 <- h2 %>% 
  add_case(tmp) %>% 
  mutate(value = ifelse(var == "Party Supporter" & value == 1,
                      "Voted for Right-Wing Party", value),
         value = ifelse(var == "Party Supporter" & value == 0,
                      "Voted for Left-Wing Party", value),
         value = ifelse(var == "Ideology" & value == 1,
                      "Self-Placement: Conservative", value),
         value = ifelse(var == "Ideology" & value == 0,
                      "Self-Placement: Progressive", value),
         hyp = "Heterogeneous Treatment Effect",
         factor = recode(factor,
                         `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                         `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat",
                         `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat"))  %>% 
  dplyr::select(factor,
                var = value,
                y,
                estimate = AME,
                upper:lower, hyp)

