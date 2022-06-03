d1 <- d1 %>% 
  mutate(region = ifelse(is.na(region), names(which.max(table(d1$region))),
                         region),
         education = ifelse(is.na(education), names(which.max(table(d1$education))),
                            education),
         employment = ifelse(employment == 3, round(mean(employment, na.rm = T), 0),
                            employment),
         job = ifelse(is.na(job), names(which.max(table(d1$job))),
                      job),
         income = ifelse(is.na(income), names(which.max(table(d1$income))),
                         income),
         subj_socialclass = ifelse(is.na(subj_socialclass),
                                   round(mean(subj_socialclass, na.rm = T), 0),
                                   subj_socialclass),
         gender = ifelse(is.na(gender), names(which.max(table(d1$gender))),
                         gender),
         ideology = ifelse(is.na(ideology), round(mean(ideology, na.rm = T), 0),
                           ideology))

d2 <- d2 %>% 
  mutate(education = ifelse(is.na(education), names(which.max(table(d2$education))),
                            education),
         job = ifelse(is.na(job), names(which.max(table(d2$job))),
                      job),
         gender = ifelse(is.na(gender), names(which.max(table(d2$gender))),
                         gender),
         ideology = ifelse(is.na(ideology), round(mean(ideology, na.rm = T), 0),
                           ideology),
         vote = ifelse(is.na(vote), names(which.max(table(d2$vote))),
                       vote))

