df <- df %>% 
  dplyr::select(id:H1a,
         polevent = `political event`,
         youth = `personal youth`,
         anti_eu = `anti-europe`,
         pre_ind = `pre-industrialized society...8`,
         nationalism,
         sol_soc = `solidar society`,
         pre_pol = `pre-polarized society`,
         colonialism,
         sports_event = `sports event`) %>% 
  mutate(polevent = replace_na(polevent, 0),
         youth = replace_na(youth, 0),
         anti_eu = replace_na(anti_eu, 0),
         pre_ind = replace_na(pre_ind, 0),
         nationalism = replace_na(nationalism, 0),
         sol_soc = replace_na(sol_soc, 0),
         pre_pol = replace_na(pre_pol, 0),
         colonialism = replace_na(colonialism, 0),
         sports_event = replace_na(sports_event, 0))

tmp <- df %>% 
  dplyr::select(youth:colonialism) %>% 
  pivot_longer(cols = everything(),
               names_to = "var") %>% 
  mutate(var = recode(var,
                      `colonialism` = "Colonialism",
                      `anti_eu` = "Euroskeptic",
                      `nationalism` = "Nationalism",
                      `sol_soc` = "Solidary Society", 
                      `youth` = "Personal Youth",  
                      `pre_ind` = "Pre-Industrialized Society", 
                      `pre_pol` = "Pre-Polarized Society")) %>% 
  filter(value == 1) %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                         `Colonialism` = "Reluctant Nostalgia",
                         `Euroskeptic` = "Reluctant Nostalgia",
                         `Nationalism` = "Reluctant Nostalgia",
                         `Solidary Society` = "Progressive Nostalgia", 
                         `Personal Youth` = "Playful Nostalgia",  
                         `Pre-Industrialized Society` = "Progressive Nostalgia", 
                         `Pre-Polarized Society` = "Progressive Nostalgia"),
         type = "Open-Ended Question \n Moment in history most longed for") %>% 
  ungroup()

tmp <- d1 %>% 
  dplyr::select(H1b_1:H1b_11) %>% 
  pivot_longer(cols = everything(),
               names_to = "var") %>% 
  filter(value==1) %>% 
  mutate(var = recode(var,
                      `H1b_1` = "More economic inequality",
                      `H1b_2` = "Fewer working class politicians",
                      `H1b_3` = "New ways of communication, such as Facebook or Instagram",
                      `H1b_4` = "Greater ethnic diversity in Dutch villages and cities",
                      `H1b_5` = "Fewer people attending church",
                      `H1b_6` = "New technological devices such as cell phones",
                      `H1b_7` = "More same-sex couples",
                      `H1b_8` = "More women working instead of staying at home",
                      `H1b_9` = "More choice in TV and entertainment",
                      `H1b_10` = "More immigrant and female politicians",
                      `H1b_11` = "More young people going to college")) %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                       `More economic inequality` = "Progressive Nostalgia",
                       `Fewer working class politicians` = "Progressive Nostalgia",
                       `New ways of communication, such as Facebook or Instagram` = "Playful Nostalgia",
                       `Greater ethnic diversity in Dutch villages and cities`  = "Reluctant Nostalgia",
                       `Fewer people attending church` = "Reluctant Nostalgia",
                       `New technological devices such as cell phones`= "Playful Nostalgia",
                       `More same-sex couples` = "Reluctant Nostalgia",
                       `More women working instead of staying at home`  = "Reluctant Nostalgia",
                       `More choice in TV and entertainment` = "Playful Nostalgia",
                       `More immigrant and female politicians`  = "Reluctant Nostalgia",
                       `More young people going to college` = "Progressive Nostalgia"),
         type = "Survey Items \n Developments made life worse") %>% 
  ungroup() %>% 
  add_case(tmp) %>% 
  mutate(voters= "All Voters")

tst <- d1 %>% 
  add_column(coded_youth = df$youth,
             coded_antieu = df$anti_eu,
             coded_indus = df$pre_ind,
             coded_nationalism = df$nationalism,
             coded_solsoc = df$sol_soc,
             coded_prepol = df$pre_pol,
             coded_colonialism = df$colonialism) %>% 
  dplyr::select(A2b, coded_youth:coded_colonialism) %>% 
  pivot_longer(cols = coded_youth:coded_colonialism,
               names_to = "var") %>% 
  mutate(var = recode(var,
                      `coded_colonialism` = "Colonialism",
                      `coded_antieu` = "Euroskeptic",
                      `coded_nationalism` = "Nationalism",
                      `coded_solsoc` = "Solidary Society", 
                      `coded_youth` = "Personal Youth",  
                      `coded_indus` = "Pre-Industrialized Society", 
                      `coded_prepol` = "Pre-Polarized Society")) %>% 
  filter(value == 1) %>%
  mutate(pop_vote = A2b,
         pop_vote = na_if(pop_vote, "Don't know"),
         pop_vote = na_if(pop_vote, "Not eligible"),
         pop_vote = na_if(pop_vote, "Other party"),
         pop_vote = na_if(pop_vote, "Won't vote"),
         pop_vote = recode(pop_vote,
                           `BBB` = 1,
                           `FvD` = 1,
                           `BVNL` = 1,
                           `JA21` = 1,
                           `PVV` = 1,
                           `SP` = 1,
                           .default = 0)) %>% 
  filter(pop_vote == 1) %>% 
  filter(value != "Na") %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                       `Colonialism` = "Reluctant Nostalgia",
                       `Euroskeptic` = "Reluctant Nostalgia",
                       `Nationalism` = "Reluctant Nostalgia",
                       `Solidary Society` = "Progressive Nostalgia", 
                       `Personal Youth` = "Playful Nostalgia",  
                       `Pre-Industrialized Society` = "Progressive Nostalgia", 
                       `Pre-Polarized Society` = "Progressive Nostalgia"),
         type = "Open-Ended Question \n Moment in history most longed for",
         voters = "Populist Voters") %>% 
  ungroup()

p1 <- d1 %>% 
  dplyr::select(A2b, H1b_1:H1b_11, E1) %>% 
  pivot_longer(cols = H1b_1:H1b_11,
               names_to = "var") %>% 
  filter(value==1) %>% 
  mutate(var = recode(var,
                      `H1b_1` = "More economic inequality",
                      `H1b_2` = "Fewer working class politicians",
                      `H1b_3` = "New ways of communication, such as Facebook or Instagram",
                      `H1b_4` = "Greater ethnic diversity in Dutch villages and cities",
                      `H1b_5` = "Fewer people attending church",
                      `H1b_6` = "New technological devices such as cell phones",
                      `H1b_7` = "More same-sex couples",
                      `H1b_8` = "More women working instead of staying at home",
                      `H1b_9` = "More choice in TV and entertainment",
                      `H1b_10` = "More immigrant and female politicians",
                      `H1b_11` = "More young people going to college"),
         pop_vote = A2b,
         pop_vote = na_if(pop_vote, "Don't know"),
         pop_vote = na_if(pop_vote, "Not eligible"),
         pop_vote = na_if(pop_vote, "Other party"),
         pop_vote = na_if(pop_vote, "Won't vote"),
         pop_vote = recode(pop_vote,
                           `BBB` = 1,
                           `FvD` = 1,
                           `BVNL` = 1,
                           `JA21` = 1,
                           `PVV` = 1,
                           `SP` = 1,
                           .default = 0)) %>% 
  filter(pop_vote == 1) %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                       `More economic inequality` = "Progressive Nostalgia",
                       `Fewer working class politicians` = "Progressive Nostalgia",
                       `New ways of communication, such as Facebook or Instagram` = "Playful Nostalgia",
                       `Greater ethnic diversity in Dutch villages and cities`  = "Reluctant Nostalgia",
                       `Fewer people attending church` = "Reluctant Nostalgia",
                       `New technological devices such as cell phones`= "Playful Nostalgia",
                       `More same-sex couples` = "Reluctant Nostalgia",
                       `More women working instead of staying at home`  = "Reluctant Nostalgia",
                       `More choice in TV and entertainment` = "Playful Nostalgia",
                       `More immigrant and female politicians`  = "Reluctant Nostalgia",
                       `More young people going to college` = "Progressive Nostalgia"),
         type = "Survey Items \n Developments made life worse",
         voters= "Populist Voters") %>% 
  ungroup() %>% 
  add_case(tmp) %>% 
  add_case(tst) %>% 
  ggplot(aes(y = fct_reorder(var, perc), 
             x = perc, fill = nost)) + 
  geom_col() +
  theme_ipsum() +
  facet_grid(type~voters, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text=element_text(size=15)) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

p2 <- tmp %>% 
  ggplot(aes(y = fct_reorder(var, perc), 
                         x = perc, fill = nost)) + 
  geom_col() +
  theme_ipsum() +
  facet_wrap(vars(type), ncol = 2, scales = "free") +
  #facet_grid(.~type, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text=element_text(size=15)) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
  
tmp <- d1 %>% 
  add_column(coded_youth = df$youth,
             coded_antieu = df$anti_eu,
             coded_indus = df$pre_ind,
             coded_nationalism = df$nationalism,
             coded_solsoc = df$sol_soc,
             coded_prepol = df$pre_pol,
             coded_colonialism = df$colonialism) %>% 
  dplyr::select(pop_vote = A2b, coded_youth:coded_colonialism,  H1b_1:H1b_11) %>% 
  pivot_longer(cols = coded_youth:coded_colonialism,
               names_to = "var") %>% 
  mutate(var = recode(var,
                      `coded_colonialism` = 1,
                      `coded_antieu` = 1,
                      `coded_nationalism` = 1,
                      `coded_solsoc` = 0, 
                      `coded_youth` = 0,  
                      `coded_indus` = 0, 
                      `coded_prepol` = 0)) %>% 
  drop_na(var) %>% 
  filter(value == 1) %>% 
  dplyr::select(-value) %>% 
  pivot_longer(cols = (H1b_1:H1b_11)) %>% 
  filter(value==1) %>% 
  mutate(value = ifelse(name == "H1b_1" ,0, value),
         value = ifelse(name == "H1b_2", 0, value),
         value = ifelse(name == "H1b_3", 0, value),
         value = ifelse(name == "H1b_4", 1, value),
         value = ifelse(name == "H1b_5", 1, value),
         value = ifelse(name == "H1b_6", 0, value),
         value = ifelse(name == "H1b_7", 1, value),
         value = ifelse(name == "H1b_8", 1, value),
         value = ifelse(name == "H1b_9", 0, value),
         value = ifelse(name == "H1b_10", 1, value),
         value = ifelse(name == "H1b_11", 0, value),
         pop_vote = na_if(pop_vote, "Don't know"),
         pop_vote = na_if(pop_vote, "Not eligible"),
         pop_vote = na_if(pop_vote, "Other party"),
         pop_vote = na_if(pop_vote, "Won't vote"),
         pop_vote = recode(pop_vote,
                           `BBB` = 1,
                           `FvD` = 1,
                           `BVNL` = 1,
                           `JA21` = 1,
                           `PVV` = 1,
                           `SP` = 1,
                           .default = 0),
         ) %>% 
  drop_na(pop_vote, var) %>% 
  mutate(pop_vote = ifelse(pop_vote == 1, "Populist Vote", "Other Vote"))

ttst1<- tibble(means = t.test(var ~ pop_vote, tmp)$estimate,
               groups = c("Other Vote", "Populist Vote"),
               pval = t.test(var ~ pop_vote, tmp)$p.value) %>% #open ended
  mutate(pval = ifelse(pval <0.05, "sign", "not"))
ttst2<- tibble(means = t.test(value ~ pop_vote, tmp)$estimate,
               groups = c("Other Vote", "Populist Vote"),
               pval = t.test(var ~ pop_vote, tmp)$p.value) %>% #survey
  mutate(pval = ifelse(pval <0.05, "sign", "not"))

ttest <- ttst1 %>% 
  add_row(ttst2)


df_tmp <- df %>% 
  dplyr::select(youth:colonialism)

dm <- d1 %>% 
  dplyr::select(E2, H1b_1:H1b_11) %>% 
  add_column(df_tmp) %>% 
  mutate(ideo = recode(E2,
                       `0` = "Left-Wing Respondents",
                       `1` = "Left-Wing Respondents",
                       `2` = "Left-Wing Respondents",
                       `3` = "Left-Wing Respondents",
                       `4` = "Center Respondents",
                       `5` = "Center Respondents",
                       `6` = "Center Respondents",
                       `7` = "Right-Wing Respondents",
                       `8` = "Right-Wing Respondents",
                       `9` = "Right-Wing Respondents",
                       `10` = "Right-Wing Respondents")) %>% 
  dplyr::select(-E2)

open1 <- dm %>% 
  dplyr::select(youth:colonialism) %>% 
  pivot_longer(cols = everything(),
               names_to = "var") %>% 
  mutate(var = recode(var,
                      `colonialism` = "Colonialism",
                      `anti_eu` = "Euroskeptic",
                      `nationalism` = "Nationalism",
                      `sol_soc` = "Solidary Society", 
                      `youth` = "Personal Youth",  
                      `pre_ind` = "Pre-Industrialized Society", 
                      `pre_pol` = "Pre-Polarized Society")) %>% 
  filter(value == 1) %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                       `Colonialism` = "Reluctant Nostalgia",
                       `Euroskeptic` = "Reluctant Nostalgia",
                       `Nationalism` = "Reluctant Nostalgia",
                       `Solidary Society` = "Progressive Nostalgia", 
                       `Personal Youth` = "Playful Nostalgia",  
                       `Pre-Industrialized Society` = "Progressive Nostalgia", 
                       `Pre-Polarized Society` = "Progressive Nostalgia"),
         type = "Open-Ended Question \n Moment in history most longed for",
         ideo = "Full Sample") %>% 
  ungroup()

open2 <- dm %>% 
  pivot_longer(cols = youth:colonialism,
               names_to = "var") %>% 
  mutate(var = recode(var,
                      `colonialism` = "Colonialism",
                      `anti_eu` = "Euroskeptic",
                      `nationalism` = "Nationalism",
                      `sol_soc` = "Solidary Society", 
                      `youth` = "Personal Youth",  
                      `pre_ind` = "Pre-Industrialized Society", 
                      `pre_pol` = "Pre-Polarized Society")) %>% 
  filter(value == 1) %>% 
  group_by(var, ideo) %>% 
  count(tot = n()) %>% 
  mutate(nost = recode(var,
                       `Colonialism` = "Reluctant Nostalgia",
                       `Euroskeptic` = "Reluctant Nostalgia",
                       `Nationalism` = "Reluctant Nostalgia",
                       `Solidary Society` = "Progressive Nostalgia", 
                       `Personal Youth` = "Playful Nostalgia",  
                       `Pre-Industrialized Society` = "Progressive Nostalgia", 
                       `Pre-Polarized Society` = "Progressive Nostalgia"),
         type = "Open-Ended Question \n Moment in history most longed for",
         perc = ifelse(ideo == "Left-Wing Respondents", n/635, 0),
         perc = ifelse(ideo == "Right-Wing Respondents", n/732, perc),
         perc = ifelse(ideo == "Center Respondents", n/817, perc)) %>% 
  ungroup()

items1 <- dm %>% 
  dplyr::select(H1b_1:H1b_11) %>% 
  pivot_longer(cols = everything(),
               names_to = "var") %>% 
  filter(value==1) %>% 
  mutate(var = recode(var,
                      `H1b_1` = "More economic inequality",
                      `H1b_2` = "Fewer working class politicians",
                      `H1b_3` = "New ways of communication, such as Facebook or Instagram",
                      `H1b_4` = "Greater ethnic diversity in Dutch villages and cities",
                      `H1b_5` = "Fewer people attending church",
                      `H1b_6` = "New technological devices such as cell phones",
                      `H1b_7` = "More same-sex couples",
                      `H1b_8` = "More women working instead of staying at home",
                      `H1b_9` = "More choice in TV and entertainment",
                      `H1b_10` = "More immigrant and female politicians",
                      `H1b_11` = "More young people going to college")) %>% 
  group_by(var) %>% 
  count(tot = n()) %>% 
  mutate(perc = n/tot,
         nost = recode(var,
                       `More economic inequality` = "Progressive Nostalgia",
                       `Fewer working class politicians` = "Progressive Nostalgia",
                       `New ways of communication, such as Facebook or Instagram` = "Playful Nostalgia",
                       `Greater ethnic diversity in Dutch villages and cities`  = "Reluctant Nostalgia",
                       `Fewer people attending church` = "Reluctant Nostalgia",
                       `New technological devices such as cell phones`= "Playful Nostalgia",
                       `More same-sex couples` = "Reluctant Nostalgia",
                       `More women working instead of staying at home`  = "Reluctant Nostalgia",
                       `More choice in TV and entertainment` = "Playful Nostalgia",
                       `More immigrant and female politicians`  = "Reluctant Nostalgia",
                       `More young people going to college` = "Progressive Nostalgia"),
         type = "Survey Items \n Developments made life worse",
         ideo = "Full Sample") %>% 
  ungroup()

items2 <- dm %>% 
  pivot_longer(cols = H1b_1:H1b_11,
               names_to = "var") %>% 
  filter(value==1) %>% 
  mutate(var = recode(var,
                      `H1b_1` = "More economic inequality",
                      `H1b_2` = "Fewer working class politicians",
                      `H1b_3` = "New ways of communication, such as Facebook or Instagram",
                      `H1b_4` = "Greater ethnic diversity in Dutch villages and cities",
                      `H1b_5` = "Fewer people attending church",
                      `H1b_6` = "New technological devices such as cell phones",
                      `H1b_7` = "More same-sex couples",
                      `H1b_8` = "More women working instead of staying at home",
                      `H1b_9` = "More choice in TV and entertainment",
                      `H1b_10` = "More immigrant and female politicians",
                      `H1b_11` = "More young people going to college")) %>% 
  group_by(var, ideo) %>% 
  count(tot = n()) %>% 
  mutate(nost = recode(var,
                       `More economic inequality` = "Progressive Nostalgia",
                       `Fewer working class politicians` = "Progressive Nostalgia",
                       `New ways of communication, such as Facebook or Instagram` = "Playful Nostalgia",
                       `Greater ethnic diversity in Dutch villages and cities`  = "Reluctant Nostalgia",
                       `Fewer people attending church` = "Reluctant Nostalgia",
                       `New technological devices such as cell phones`= "Playful Nostalgia",
                       `More same-sex couples` = "Reluctant Nostalgia",
                       `More women working instead of staying at home`  = "Reluctant Nostalgia",
                       `More choice in TV and entertainment` = "Playful Nostalgia",
                       `More immigrant and female politicians`  = "Reluctant Nostalgia",
                       `More young people going to college` = "Progressive Nostalgia"),
         type = "Survey Items \n Developments made life worse",
         perc =  n/1779) %>% 
  ungroup()

open1 %>% 
  add_case(open2) %>% 
  add_case(items1) %>% 
  add_case(items2) %>% 
  mutate(ideo = factor(ideo,
                       levels = c("Right-Wing Respondents",
                                  "Center Respondents",
                                  "Left-Wing Respondents",
                                  "Full Sample"))) %>% 
  filter(type == "Survey Items \n Developments made life worse") |> 
  filter(var == "More economic inequality")

p2b <- open1 %>% 
  add_case(open2) %>% 
  add_case(items1) %>% 
  add_case(items2) %>% 
  mutate(ideo = factor(ideo,
                       levels = c("Right-Wing Respondents",
                                  "Center Respondents",
                                  "Left-Wing Respondents",
                                  "Full Sample"))) %>% 
  ggplot(aes(y = fct_reorder(var, perc),
             x = perc, fill = ideo)) + 
  geom_col(position = "dodge2") +
  theme_ipsum() +
  facet_wrap(vars(type), ncol = 2, scales = "free") +
  scale_fill_manual(values = fig_cols) +
  labs(y = "", x = "") +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text=element_text(size=15)) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE))

tmp <- df |> 
  dplyr::select(polevent:sports_event)
ds1 <- d1 |> 
  dplyr::select(id:urbanisation, A1:A2b, F1_1:F1_9, H1a:H1b_11) |> 
  add_column(tmp)
