tmp <- d2 %>% 
  select(ideology:collective_nostalgia, message) %>%
  pivot_longer(cols = ideology:collective_nostalgia,
               names_to = "ivs") %>% 
  mutate(ivs = recode(ivs,
                      trust = "Trust in Institution",
                      life_satisf = "Life Satisfaction",
                      agreeable = "Agreeableness",
                      nationalism = "Nationalism",
                      collective_nostalgia = "Nostalgia",
                      ideology = "Ideology")) %>%
  group_by(ivs, message) %>%
  summarise(means = mean(value, na.rm=T),
            stdev = sd(value, na.rm=T)) %>%
  mutate(lower = means - (1.96 * stdev),
         upper = means + (1.96 * stdev)) %>% 
  ungroup()
  
p1 <- d1 %>% 
  select(subj_socialclass, polvuln, cultvuln, reldep,
         NfC, nativism, collective_narcism, collective_nostalgia, ideology, message) %>%
  pivot_longer(cols = subj_socialclass:ideology,
               names_to = "ivs") %>% 
  mutate(ivs = recode(ivs,
                      subj_socialclass = "Subjective Social Class",
                      polvuln = "Political Vulnerability",
                      cultvuln = "Cultural Vulnerability",
                      reldep = "Relative Deprivation",
                      NfC = "Need for Cognition",
                      nativism = "Nativism",
                      collective_narcism = "Collective Narcism",
                      collective_nostalgia = "Nostalgia",
                      ideology = "Ideology")) %>%
  group_by(ivs, message) %>%
  summarise(means = mean(value, na.rm=T),
            stdev = sd(value, na.rm=T)) %>%
  mutate(lower = means - (1.96 * stdev),
         upper = means + (1.96 * stdev)) %>% 
  ungroup() %>% 
  add_row(tmp) %>% 
  mutate(message = ifelse(message == "anonymous", "Anonymous Message", 
                          "Party Message"),
         scale = ifelse(ivs == "Ideology", "10-point scale",
                 ifelse(ivs == "Life Satisfaction", "10-point scale", 
                        "5-point scale"))) %>% 
  ggplot(aes(x = ivs, y = means, ymin = lower, ymax = upper, color = message)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  labs(y = "", 
       x = "") +
  theme_ipsum() +
  facet_grid(.~scale, scales = "free") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) 

tmp <- d2 %>% select(gender, message) %>% drop_na()
p2 <- d1 %>% 
  select(gender, message) %>%
  drop_na() %>% 
  add_row(tmp) %>% 
  group_by(gender, message) %>% 
  filter(gender != "NA") %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/(dim(d1)[1]+dim(d2)[1]),
         message = ifelse(message == "anonymous", "Anonymous Message", 
                                 "Party Message")) %>% 
  ggplot(aes(x = gender, y = perc, fill = message)) +
  geom_col(position = position_dodge(.5)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Gender") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  coord_flip()


tmp <- d2 %>% select(age, message) %>% drop_na()
p3 <- d1 %>% 
  select(age, message) %>%
  drop_na() %>% 
  add_row(tmp) %>% 
  group_by(age, message) %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/(dim(d1)[1]+dim(d2)[1]),
         message = ifelse(message == "anonymous", "Anonymous Message", 
                          "Party Message")) %>% 
  ggplot(aes(x = age, y = perc, fill = message)) +
  geom_col(position = position_dodge(.5)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Age") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  coord_flip()


tmp <- d2 %>% select(education, message) %>% drop_na()
p4 <- d1 %>% 
  select(education, message) %>%
  drop_na() %>% 
  add_row(tmp) %>% 
  group_by(education, message) %>% 
  filter(education != "NA") %>% 
  summarise(n = n()) %>% 
  mutate(perc = n/(dim(d1)[1]+dim(d2)[1]),
         message = ifelse(message == "anonymous", "Anonymous Message", 
                          "Party Message")) %>% 
  ggplot(aes(x = education, y = perc, fill = message)) +
  geom_col(position = position_dodge(.5)) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", title = "Education") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  coord_flip()


rm(tmp)
