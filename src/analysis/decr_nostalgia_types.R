df1 <- d1 %>% 
  mutate_at(vars(starts_with("H1b_")),
            funs(recode(., 
                        `1` = 1,
                        `2` = 0,
                        `3` = 0))) %>% 
  mutate(reluct = (H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10)/5,
         play = (H1b_3 + H1b_6 + H1b_9)/3,
         progr = (H1b_1 + H1b_2 + H1b_11)/3,
         group1 = "group",
         region = factor(region,
                         levels = c("North  of the country",
                                    "East  of the country",
                                    "South  of the country",
                                    "West of the country",
                                    "Three big cities (Amsterdam, Rotterdam, The Hague)")),
         education = factor(education,
                            levels = c("Low", "Medium", "High")),
         urbanisation = factor(urbanisation,
                               levels = c("Not urbanised",
                                          "Lowly urbanised",
                                          "Medium urbanised",
                                          "Strongly urbanised",
                                          "Very strongly urbanised"))) %>% 
  dplyr::select(`Reluctant Nostalgia` = reluct, 
                `Playful Nostalgia` = play, 
                `Progressive Nostalgia` = progr,
                Region = region, 
                Urbanisation = urbanisation, 
                Education = education, 
                Gender = sex, 
                Age = age,
                Ideology = E2,
                group1) %>% 
  drop_na()


p1 <- ggbivariate(df1, outcome = "Reluctant Nostalgia", 
                  explanatory = c("Gender", "Age","Education", 
                                  "Urbanisation", "Region",
                                  "Ideology"),
                  types = list(comboVertical = "autopoint"),
                  aes(color = group1)) +
  theme_ipsum() +
  scale_fill_manual(values = fig_cols[1]) +
  scale_color_manual(values = fig_cols[1]) +
  labs(y = "", x = "Levels of Nostalgia",
       title = "", subtitle = "") +
  theme(legend.title = element_blank(),
        legend.position = "none")

p2 <- ggbivariate(df1, outcome = "Playful Nostalgia", 
                  explanatory = c("Gender", "Age","Education", 
                                  "Urbanisation", "Region",
                                  "Ideology"),
                  types = list(comboVertical = "autopoint"),
                  aes(color = group1)) +
  theme_ipsum() +
  scale_fill_manual(values = fig_cols[2]) +
  scale_color_manual(values = fig_cols[2]) +
  labs(y = "", x = "Levels of Nostalgia",
       title = "", subtitle = "") +
  theme(legend.title = element_blank(),
        legend.position = "none")

p3 <- ggbivariate(df1, outcome = "Progressive Nostalgia", 
                  explanatory = c("Gender", "Age","Education", 
                                  "Urbanisation", "Region",
                                  "Ideology"),
                  types = list(comboVertical = "autopoint"),
                  aes(color = group1)) +
  theme_ipsum() +
  scale_fill_manual(values = fig_cols[3]) +
  scale_color_manual(values = fig_cols[3]) +
  labs(y = "", x = "Levels of Nostalgia",
       title = "", subtitle = "") +
  theme(legend.title = element_blank(),
        legend.position = "none")

df2 <- d1 %>% 
  add_column(coded_youth = df$youth,
             coded_antieu = df$anti_eu,
             coded_indus = df$pre_ind,
             coded_nationalism = df$nationalism,
             coded_solsoc = df$sol_soc,
             coded_prepol = df$pre_pol,
             coded_colonialism = df$colonialism) %>% 
  pivot_longer(cols = coded_youth:coded_colonialism,
               names_to = "var") %>%
  filter(value == 1) %>% 
  dplyr::select(-value) %>% 
  mutate(var = recode(var,
                      `coded_colonialism` = "Colonialism",
                      `coded_antieu` = "Euroskeptic",
                      `coded_nationalism` = "Nationalism",
                      `coded_solsoc` = "Solidary Society", 
                      `coded_youth` = "Personal Youth", 
                      `coded_indus` = "Pre-Industrialized Society", 
                      `coded_prepol` = "Pre-Polarized Society"),
         var = factor(var),
         group1 = "group",
         region = factor(region,
                         levels = c("North  of the country",
                                    "East  of the country",
                                    "South  of the country",
                                    "West of the country",
                                    "Three big cities (Amsterdam, Rotterdam, The Hague)")),
         education = factor(education,
                            levels = c("Low", "Medium", "High")),
         urbanisation = factor(urbanisation,
                               levels = c("Not urbanised",
                                          "Lowly urbanised",
                                          "Medium urbanised",
                                          "Strongly urbanised",
                                          "Very strongly urbanised"))) %>% 
  filter(var != "Na") %>% 
  dplyr::select(`Narratives` = var, 
                Region = region, 
                Urbanisation = urbanisation, 
                Education = education, 
                Gender = sex, 
                Age = age,
                Ideology = E2) %>% 
  drop_na() 

p4 <- ggbivariate(df2, 
                          outcome = "Narratives", 
                          explanatory = c("Gender", "Age","Education", 
                                          "Urbanisation", "Region",
                                          "Ideology"),
                          # aes(color = Narratives, fill = Narratives,
                          #     group = 1),
                            rowbar_args = list(
                            colour = "white",
                            size = 2,
                            show.legend = FALSE)) +
  theme_ipsum() +
  scale_fill_manual(values = fig_cols) +
  scale_color_manual(values = fig_cols) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
