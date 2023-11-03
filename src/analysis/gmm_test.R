dm <- d1 %>% 
  mutate_at(vars(c(F1_3, F1_5)), 
            funs(recode(., `1` = 5, 
                        `2` =4,`3` = 3,
                        `4` = 2, `5` = 1))) %>% 
  mutate_at(vars(starts_with("H1b_")),
            funs(recode(., 
                        `1` = 1,
                        `2` = 0,
                        `3` = 0))) %>% 
  mutate(pop = (F1_1 + F1_2 + F1_3 + F1_4 + F1_5 + F1_6 + F1_7 + F1_8 + F1_9)/9,
         reluct = (H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10)/5,
         play = (H1b_3 + H1b_6 + H1b_9)/3,
         progr = (H1b_1 + H1b_2 + H1b_11)/3) %>% 
  dplyr::select(reluct, play, progr, pop) %>% 
  drop_na() 
  

library(mclust)
set.seed(26111985)
mb = Mclust(dm,2)
plot(mb, what=c("density"))

tmp <- d1 %>% 
  mutate_at(vars(starts_with("H1b_")),
            funs(recode(., 
                        `1` = 1,
                        `2` = 0,
                        `3` = 0))) %>% 
  mutate(reluct = (H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10)/5,
         play = (H1b_3 + H1b_6 + H1b_9)/3,
         progr = (H1b_1 + H1b_2 + H1b_11)/3,
         groups = ifelse(reluct<.6,0,1),
         ideo = ifelse(E2 <5, "left", "center"),
         ideo = ifelse(E2>6, "right", ideo)) %>% 
  dplyr::select(reluct, play, progr,groups, region, urbanisation, education, sex, ideo) %>% 
  drop_na() %>% 
  group_by(groups, education, sex, ideo, urbanisation) %>% 
  summarise(tot = n()) %>% 
  ungroup() %>% 
  mutate(perc = tot/sum(tot)*100,
         id = paste(sex, education, urbanisation, ideo, sep = "-")) 
  

tmp %>% filter(groups==1) %>% arrange(desc(perc))



