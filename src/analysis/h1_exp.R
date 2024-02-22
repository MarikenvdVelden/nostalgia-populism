h1 <- lm(H18 ~ pop_vote*treatment +
           ideology, d)

pred_scapegoat <- interact_plot(model = h1, pred = pop_vote, 
                                modx = treatment, interval = FALSE,
                                data = d, colors = fig_cols)+
  labs(y = "Predicted Probabilities of Support for Scapegoat Messages",
       x = "Popolist Supporter \n (0 = No Supporter, 1 = Supporter)") +
  theme_ipsum() +
  theme(legend.position = "none",
        axis.text=element_text(size=15)) +
  geom_curve(
    aes(x = .2, y = 8.78, xend = .15, yend = 8.35),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[2]) +
  annotate("text", x = .2, y = 8.3, label = "Scapegoat Message",
           color = fig_cols[2]) +
  geom_curve(
    aes(x = .37, y = 9.77, xend = .32, yend = 10.17),
    arrow = arrow(length = unit(0.03, "npc")),
    color = fig_cols[1]) +
  annotate("text", x = .3, y = 10.22, label = "No Scapegoat Message",
           color = fig_cols[1])

#H1
h1 <- lm(H18 ~ pop_vote*treatment +
          ideology, d)
h1 <- summary(margins(h1, variables = "treatment", at = list(pop_vote = 0:1)))
h1 <- as_tibble(h1) %>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Support for Message") %>% 
  dplyr::select(factor, pop_vote, y, AME, upper, lower)

tmp <- lm(scapegoat_support_self ~ pop_vote*treatment +
           scapegoat + ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(pop_vote = 0:1))) 
tmp <- as_tibble(tmp)%>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Respondent") %>% 
  dplyr::select(factor, pop_vote, y, AME, upper, lower)

h1 <- h1 %>% 
  add_case(tmp)

tmp <- lm(scapegoat_support_ingroup ~ pop_vote*treatment +
         ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(pop_vote = 0:1))) 
tmp <- as_tibble(tmp)%>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for Dutch Voters") %>% 
  dplyr::select(factor, pop_vote, y, AME, upper, lower)

h1 <- h1 %>% 
  add_case(tmp)

tmp <- lm(scapegoat_support_dutch ~ pop_vote*treatment +
            ideology, d)
tmp <- summary(margins(tmp, variables = "treatment", at = list(pop_vote = 0:1))) 
tmp <- as_tibble(tmp)%>% 
  mutate(upper = (AME + (SE * 1.56)),
         lower = (AME - (SE * 1.56)),
         y = "Best Campaign Message for [Leftwing/Rightwing] Voters") %>% 
  dplyr::select(factor, pop_vote, y, AME, upper, lower)

h1 <- h1 %>% 
  add_case(tmp) %>% 
  mutate(pop_vote = recode(pop_vote,
                           `0` = "Non-Populist Voter",
                           `1` = "Populist Voter"),
         hyp = "Heterogeneous Treatment Effect",
         factor = recode(factor,
                         `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                         `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat",
                         `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat")) %>% 
  dplyr::select(factor,
                var = pop_vote,
                y,
                estimate = AME,
                upper:lower, hyp)


