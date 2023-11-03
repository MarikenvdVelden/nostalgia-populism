dm <- d1 %>% 
  dplyr::select(contains("F1") | contains("H1b")) %>% 
  mutate_at(vars(c(F1_3, F1_5)), 
            funs(recode(., `1` = 5, 
                        `2` =4,`3` = 3,
                        `4` = 2, `5` = 1))) %>% 
  mutate_at(vars(starts_with("H1b_")),
            funs(recode(., 
                        `1` = 1,
                        `2` = 0,
                        `3` = 0))) %>% 
  drop_na()

#1 = makes life worse
#2 = no difference
#3 = makes life better
  
# Confirmatory Factor Analysis
# Defining models
cfa_model1 <- "
   Reluctant Nostalgia =~ H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10
   Playful Nostalgia =~     H1b_3 + H1b_6 + H1b_9
   Populist Attitude =~ F1_1 + F1_4 + F1_5 + F1_6 + F1_7 + F1_8 + F1_9
   Progressive Nostalgia =~ H1b_1 + H1b_2 + H1b_11
   Reluctant Nostalgia ~~ Populist Attitude
   Playful Nostalgia ~~ Populist Attitude
   Progressive Nostalgia ~~ Populist Attitude
"

fit1 <- cfa(cfa_model1, 
               estimator = "MLR", 
               data = dm)
fitMeasures(fit1, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                       "cfi.robust", "tli.robust", "rmsea.robust"))
summary(fit1, fit = T, std = T)
modindices(fit1) %>%
  arrange(-mi) %>%
  mutate_if(is.numeric, round, 2)

m1 <- parameterEstimates(fit1) %>% 
  filter(lhs %in% c("ReluctantNostalgia", "PopulistAttitude","ProgressiveNostalgia","PlayfulNostalgia")) %>% 
  dplyr::select(`Latent Variable` = lhs, Indicators = rhs,
                Loadings = est, 
                `Lowerbound` = ci.lower,
                `Upperbound` = ci.upper) %>% 
  mutate(Indicators = recode(Indicators,
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
                             `H1b_11` = "More young people going to college",
                             `F1_1` = "People with a Different Opinion are Bad",
                             `F1_4` = "Politicians Should Always Listen to the People",
                             `F1_5` = "Politicians Need to Spend Time with People",
                             `F1_6` = "Will of the People",
                             `F1_7` = "More Promised than Delivered",
                             `F1_8` = "Anti Compromise",
                             `F1_9` = "Strong Leader",
                             `PopulistAttitude` = "Populist Attitude",
                             `ProgressiveNostalgia` = "Progressive Nostalgia",
                             `ReluctantNostalgia` = "Reluctant Nostalgia",
                             `PlayfulNostalgia` = "Playful Nostalgia"),
         `Latent Variable` = recode(`Latent Variable`,
                                    `PopulistAttitude` = "Populist Attitude",
                                    `ProgressiveNostalgia` = "Progressive Nostalgia",
                                    `ReluctantNostalgia` = "Reluctant Nostalgia",
                                    `PlayfulNostalgia` = "Playful Nostalgia"),
         `Latent Variable` = factor(`Latent Variable`, 
                                    levels = c("Populist Attitude",
                                               "Reluctant Nostalgia",
                                               "Playful Nostalgia",
                                               "Progressive Nostalgia")),
         id = 0,
         id = ifelse(`Latent Variable` == "Populist Attitude" & Indicators == "Populist Attitude",
                     1, id),
         id = ifelse(`Latent Variable` == "Progressive Nostalgia" & Indicators == "Progressive Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Playful Nostalgia" & Indicators == "Playful Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Reluctant Nostalgia" & Indicators == "Reluctant Nostalgia",
                     1, id),         
         id = ifelse(`Latent Variable` == "Populist Attitude" & Indicators == "Progressive Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Populist Attitude" & Indicators == "Playful Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Populist Attitude" & Indicators == "Reluctant Nostalgia",
                    1, id),
         id = ifelse(`Latent Variable` == "Reluctant Nostalgia" & Indicators == "Progressive Nostalgia",
                    1, id),
         id = ifelse(`Latent Variable` == "Reluctant Nostalgia" & Indicators == "Playful Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Reluctant Nostalgia" & Indicators == "Populist Attitude",
                     1, id),
         id = ifelse(`Latent Variable` == "Playful Nostalgia" & Indicators == "Progressive Nostalgia",
                    1, id),
         id = ifelse(`Latent Variable` == "Playful Nostalgia" & Indicators == "Reluctant Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Playful Nostalgia" & Indicators == "Populist Attitude",
                     1, id),
         id = ifelse(`Latent Variable` == "Progressive Nostalgia" & Indicators == "Playful Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Progressive Nostalgia" & Indicators == "Reluctant Nostalgia",
                     1, id),
         id = ifelse(`Latent Variable` == "Progressive Nostalgia" & Indicators == "Populist Attitude",
                    1, id))

p1 <- m1 %>% filter(id==0) %>% 
  ggplot(aes(x = Loadings, y = reorder(Indicators, -Loadings),
               xmin = `Lowerbound`, xmax = `Upperbound`,
               color = `Latent Variable`)) +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_ipsum() +
  facet_wrap(.~`Latent Variable`, scales = "free") +
  labs(y = " ", x = "PCA Loadings" ) +
  theme(legend.position = "none")

p2 <- m1 %>% filter(id==1,
              `Latent Variable` == "Populist Attitude" | Indicators == "Populist Attitude") %>% 
  mutate(id = 0,
         id = ifelse(`Latent Variable` == "Populist Attitude" & Indicators == "Populist Attitude", 1, id),
         type = "Covariance between Latent Variables and Populist Attitude") %>% 
  filter(id==0) %>% 
  mutate(x = c("Reluctant Nostalgia", "Playful Nostalgia", "Progressive Nostalgia")) %>% 
  ggplot(aes(x = Loadings, y = reorder(x, -Loadings),
             xmin = `Lowerbound`, xmax = `Upperbound`,
             color = `type`)) +
  geom_point() +
  geom_errorbar(width = 0) +
  theme_ipsum() +
  facet_wrap(`type`~., scales = "free") +
  labs(y = " ", x = "PCA Loadings") +
  theme(legend.position = "none")


tmp <- d1 %>% 
  mutate_at(vars(starts_with("H1b_")),
            funs(recode(., 
                        `1` = 1,
                        `2` = 0,
                        `3` = 0))) %>% 
  mutate(pop_att = (F1_1 + F1_2 + F1_3 + F1_4 + F1_5 + F1_6 + F1_7 + F1_8 + F1_9)/9,
         rn = (H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10)/5,
         pn = ( H1b_3 + H1b_6 + H1b_9)/3,
         progn = (H1b_1 + H1b_2 + H1b_11)/3) %>% 
  dplyr::select(`Populist Attitudes` = pop_att, 
                `Reluctant Nostalgia` = rn, 
                `Playful Nostalgia` = pn,
                `Progressive Nostalgia` = progn) %>% 
  drop_na()

p3 <- ggcorrmat(tmp,
                type = "robust", # correlation method
                cor.vars = c(everything()), # a range of variables can be selected
                title = "Correlalogram for Variables under Study",
                colors = c(fig_cols[2], "white", fig_cols[3])
)
