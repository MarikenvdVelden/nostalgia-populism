s1 <- d1 %>% select(matches("polvuln_")) %>% drop_na()
scales <- tibble(alpha = round(ltm::cronbach.alpha(s1)[1]$alpha, 2),
                 variable = "Political Vulnarability")

s2 <- d1 %>% select(matches("cultvuln_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s2)[1]$alpha, 2),
                             variable = "Cultural Vulnarability")

s3 <- d1 %>% select(matches("relative_deprivation_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s3)[1]$alpha, 2),
                             variable = "Relative Deprivation")

s4 <- d1 %>% select(matches("NfC_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s4)[1]$alpha, 2),
                             variable = "Need for Cognition")

s5 <- d1 %>% select(matches("nativism_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s5)[1]$alpha, 2),
                             variable = "Nativism")

s6 <- d1 %>% select(matches("collective_narcism_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s6)[1]$alpha, 2),
                             variable = "Collective Narcisism")

s7 <- d1 %>% select(matches("colnostalgia_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s7)[1]$alpha, 2),
                             variable = "Collective Nostalgia")

tmp <- d2 %>% select(matches("populist_attitudes_[12345678]")) %>% drop_na()
s8 <- d1 %>% select(matches("populist_attitudes_[12345678]")) %>% 
  drop_na() %>% add_row(tmp)
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s8)[1]$alpha, 2),
                             variable = "Populist Attitudes ")

tmp <- d2 %>% select(populist_attitudes_9, populist_attitudes_10,
                     populist_attitudes_11) %>% drop_na() %>% 
  mutate(populist_attitudes_11 = 6 - populist_attitudes_11)
s9 <- d1 %>% select(populist_attitudes_9, populist_attitudes_10,
                    populist_attitudes_11) %>% drop_na() %>% 
  mutate(populist_attitudes_11 = 6 - populist_attitudes_11)  %>% add_row(tmp)
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s9)[1]$alpha, 2),
                             variable = "Pluralist Attitudes")

tmp <- d2 %>% select(populist_attitudes_12, populist_attitudes_13,
                     populist_attitudes_14) %>% drop_na() 
s10 <- d1 %>% select(populist_attitudes_12, populist_attitudes_13,
                    populist_attitudes_14) %>% drop_na() %>% add_row(tmp)
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s10)[1]$alpha, 2),
                             variable = "Elitist Attitudes")

s11 <- d2 %>% select(matches("nationalism_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s11)[1]$alpha, 2),
                             variable = "Nationalism")

s12 <- d2 %>% select(matches("agreeable_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s12)[1]$alpha, 2),
                             variable = "Agreeableness")

s13 <- d2 %>% select(matches("trust_"), -trust_11) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s13)[1]$alpha, 2),
                             variable = "Trust in Institutions") 

s14 <- d2 %>% select(matches("colnostalgia_")) %>% drop_na()
scales <- scales %>% add_row(alpha = round(ltm::cronbach.alpha(s14)[1]$alpha, 2),
                             variable = "Collectiive Nostalgia")

d1 <- d1 %>% 
  mutate(polvuln = round((polvuln_1 + polvuln_2 + polvuln_3 + 
                      polvuln_4 + polvuln_5)/5, 0),
         cultvuln = round((cultvuln_1 + cultvuln_2 + cultvuln_3 + 
                             cultvuln_4)/4, 0),
         reldep = round((relative_deprivation_1 + relative_deprivation_2 + 
                           relative_deprivation_3 + relative_deprivation_1)/4, 
                        0),
         NfC = round((NfC_1 + NfC_2 + NfC_3 + NfC_4 + NfC_5 + NfC_6)/6, 0),
         nativism = round((nativism_1 + nativism_2 + nativism_3 + 
                             nativism_4 + nativism_5)/5, 0),
         collective_narcism = round((collective_narcism_1 + collective_narcism_2  + 
                                       collective_narcism_3  + collective_narcism_4  + 
                                       collective_narcism_5  + collective_narcism_6  + 
                                       collective_narcism_7  + collective_narcism_8 +
                                       collective_narcism_9)/9, 0),
         collective_nostalgia = round((colnostalgia_1 + colnostalgia_2 +
                                   colnostalgia_3 + colnostalgia_4)/4, 0),
         populist = round((populist_attitudes_1 + populist_attitudes_2  + 
                             populist_attitudes_3  + populist_attitudes_4  + 
                             populist_attitudes_5  + populist_attitudes_6  + 
                             populist_attitudes_7  + populist_attitudes_8)/8, 0),
         pluralist = round((populist_attitudes_9 + populist_attitudes_10  + 
                              populist_attitudes_11)/3, 0),
         elitist = round((populist_attitudes_12 + populist_attitudes_13  + 
                              populist_attitudes_14)/3, 0))  %>% 
  select(region:subj_socialclass, polvuln, cultvuln, reldep, NfC, 
         nativism, collective_narcism, collective_nostalgia,
         populist, pluralist, elitist, gender:ideology, nostalgia:message, mc1:mc3)

d2 <- d2 %>% 
  mutate(nationalism = round((nationalism_1 + nationalism_2 + nationalism_3 + 
                                nationalism_4 + nationalism_5 + nationalism_6 + 
                                nationalism_7)/7, 0),
         agreeable = round((agreeable_1 + agreeable_2 + agreeable_3 + 
                               agreeable_4 + agreeable_5 + agreeable_6 + 
                               agreeable_7 + agreeable_8 + agreeable_9 +
                               agreeable_10)/10, 0),
         trust = round((trust_1 + trust_2 + trust_3 + 
                          trust_4 + trust_5 + trust_6 + 
                          trust_7 + trust_8 + trust_9 +
                          trust_10)/10, 0),
         collective_nostalgia = round((colnostalgia_1 + colnostalgia_2 +
                                         colnostalgia_3 + colnostalgia_4)/4, 0),
         populist = round((populist_attitudes_1 + populist_attitudes_2  + 
                             populist_attitudes_3  + populist_attitudes_4  + 
                             populist_attitudes_5  + populist_attitudes_6  + 
                             populist_attitudes_7  + populist_attitudes_8)/8, 0),
         pluralist = round((populist_attitudes_9 + populist_attitudes_10  + 
                              populist_attitudes_11)/3, 0),
         elitist = round((populist_attitudes_12 + populist_attitudes_13  + 
                            populist_attitudes_14)/3, 0)) %>% 
  select(gender:ideology, nationalism, agreeable, life_satisf, trust, collective_nostalgia, 
         populist, pluralist, elitist, nostalgic_message:message, mc1:mc3)
  

rm(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, tmp)
