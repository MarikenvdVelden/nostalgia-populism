#H3
d <- d |> 
  mutate(nostalgia = factor(nostalgia),
                 scapegoat = factor(scapegoat),
                 ideology = factor(ideology),
                 treatment = factor(treatment))
d <- within(d, treatment <- relevel(treatment, ref = "No Nostalgia, No Scapegoat"))

h3 <- tidy(lm(H18 ~  treatment +
          ideology, d)) %>% 
  mutate(y = "Support for Message",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower)

tmp <- tidy(lm(nostalgia_support_self ~  treatment +
            ideology, d)) %>% 
  mutate(y = "Best Campaign Message for Respondent",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower)

h3 <- h3 %>% 
  add_case(tmp)

tmp <- tidy(lm(nostalgia_support_dutch ~ treatment +
            ideology, d)) %>% 
  mutate(y = "Best Campaign Message for Dutch Voters",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower)

h3 <- h3 %>% 
  add_case(tmp)

tmp <- tidy(lm(nostalgia_support_ingroup ~  treatment +
            ideology, d)) %>% 
  mutate(y = "Best Campaign Message for [Leftwing/Rightwing] Voters",
         upper = (estimate + (std.error * 1.56)),
         lower = (estimate - (std.error * 1.56))) %>% 
  dplyr::select(term, y, estimate, upper, lower)

h3 <- h3 %>% 
  add_case(tmp) %>% 
  mutate(var = "Experimental Treatment",
         hyp = "Hypothesis 3",
         term = recode(term,
                         `treatmentNo Nostalgia, Scapegoat` = "No Nostalgia, Scapegoat",
                         `treatmentNostalgia, Scapegoat` = "Nostalgia, Scapegoat",
                         `treatmentNostalgia, No Scapegoat` = "Nostalgia, No Scapegoat")) %>% 
  filter(term %in% c("No Nostalgia, Scapegoat", 
                     "Nostalgia, Scapegoat",
                     "Nostalgia, No Scapegoat")) %>% 
  dplyr::select(factor = term,
                var,
                y,
                estimate,
                upper:lower, hyp)

