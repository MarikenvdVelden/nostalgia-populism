##### 0. packages ####
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
library(ggrepel)
library(readr)
library(lme4)
library(interactions)
 
library(ggridges)
library(jtools)
library(modelsummary)
library(colorspace)
library(interplot)
library(ggpubr)
library(margins)
library(estimatr)
library(tidycomm)
library(ggsurvey)
library(survey)
library(questionr)
library(ggplot2)
library(srvyr)
##### 0. data cleaning ####

d4_e <- d4 |>  
  mutate(support_bin= factor(case_when(H18<7 ~ "Opposes the message",
                                       H18>6 ~ "Supports the message")),
         nost_scap= factor(case_when(exp_nost %in% c("A1-left-no nostalgia",
                                                     "B1-right-no nostalgia") ~ "No nostalgia, no scapegoat",
                                     exp_nost %in% c("A2-left-no nostalgia-scapegoat",
                                                     "B2-right-no nostalgia-scapegoat") ~ "No nostalgia, yes scapegoat",
                                     exp_nost %in% c("A3-left-nostalgia",
                                                     "B3-right-nostalgia") ~ "Nostalgia, no scapegoat",
                                     exp_nost %in% c("A4-left-nostalgia-scapegoat",
                                                     "B4-right-nostalgia-scapegoat") ~ "Nostalgia, yes scapegoat")),
         nost= factor(case_when(nost_scap %in% c("Nostalgia, no scapegoat", "Nostalgia, yes scapegoat") ~ "Nostalgia",
                                nost_scap %in% c("No nostalgia, no scapegoat", "No nostalgia, yes scapegoat") ~ "No nostalgia")),
         scap= factor(case_when(nost_scap %in% c("No nostalgia, yes scapegoat", "Nostalgia, yes scapegoat") ~ "Scapegoat",
                                nost_scap %in% c("Nostalgia, no scapegoat","No nostalgia, no scapegoat") ~ "No scapegoat")),
         nost_scap_num= as.numeric(nost_scap),
         nost_num= as.numeric(nost),
         scap_num= as.numeric(scap),
         nost_scap_num= as.numeric(scap),
         
         respondent_conservativeness= factor(E2_2),
         respondent_conservativeness_num= as.numeric(respondent_conservativeness),
         message_support= H18,
         pop_vote = A2b,
         pop_vote = na_if(pop_vote, "Don't know"),
         pop_vote = na_if(pop_vote, "Not eligible"),
         pop_vote = na_if(pop_vote, "Other party"),
         pop_vote = na_if(pop_vote, "Won't vote"),
         pop_vote = recode(pop_vote,
                           `FvD` = 1,
                           `BVNL` = 1,
                           `JA21` = 1,
                           `PVV` = 1,
                           `SP` = 1,
                           .default = 0),
) |> 
  drop_na(weights, support_bin) |> 
  separate(exp_nost, c("group", "tmp")) |> 
  mutate(ideology = recode(group,#right == 1
                                    `A1` = 0,
                                    `A2` = 0,
                                    `A3` = 0,
                                    `A4` = 0,
                                    `B1` = 1,
                                    `B2` = 1,
                                    `B3` = 1,
                                    `B4` = 1),
                  scapegoat_support_self = recode(H21_1,
                                                  `No nostalgia + scapegoat` = 1,
                                                  `Nostalgia + scapegoat` = 1,
                                                  .default = 0),
                  nostalgia_support_self = recode(H21_1,
                                                  `Nostalgia` = 1,
                                                  `Nostalgia + scapegoat` = 1,
                                                  .default = 0),
                  scapegoat_support_self = recode(H21_1,
                                                  `No nostalgia + scapegoat` = 1,
                                                  `Nostalgia + scapegoat` = 1,
                                                  .default = 0),
                  nostalgia_support_dutch = recode(H21_2,
                                                   `Nostalgia` = 1,
                                                   `Nostalgia + scapegoat` = 1,
                                                   .default = 0),
                  scapegoat_support_dutch = recode(H21_2,
                                                   `No nostalgia + scapegoat` = 1,
                                                   `Nostalgia + scapegoat` = 1,
                                                   .default = 0),
                  nostalgia_support_ingroup = recode(H21_1,
                                                     `Nostalgia` = 1,
                                                     `Nostalgia + scapegoat` = 1,
                                                     .default = 0),
                  scapegoat_support_ingroup = recode(H21_2,
                                                     `No nostalgia + scapegoat` = 1,
                                                     `Nostalgia + scapegoat` = 1,
                                                     .default = 0))


d4_e_left <- d4_e %>% 
  filter(ideology==0)
d4_e_right <- d4_e %>% 
  filter(ideology==1)


d4_weighted <- svydesign(ids = ~1, data = d4_e, weights = d4_e$weights)

d4_e_left_weighted <- svydesign(ids = ~1, data = d4_e_left, weights = d4_e_left$weights)
d4_e_right_weighted <- svydesign(ids = ~1, data = d4_e_right, weights = d4_e_right$weights)



colors<- c("#FFB84C", "#F266AB")
colorsX= c( "#A459D1", "#2CD3E1")



table_result <- svytable(~support_bin + nost_scap, d4_weighted)

# Convert the table to proportions
prop_result <- prop.table(table_result, margin = 2) * 100

# Print the results
print(prop_result)


##### 1. SUPPORT | H1: Nostalgia & scapegoat x all voters ##### 

nostalgia_allvoters_h1 <- ggsurvey(d4_weighted) + 
  aes(x = nost_scap, fill = support_bin) + 
  geom_bar(position = "fill")+  
  scale_fill_manual(values = colors)+
  theme_ipsum() +

  labs(y="", x="",
       title="Support (%) message among conditional treatment arms")+
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face="bold"),
        legend.position = "top") +

  annotate("label", x = 1, y = .88, label = "92.2%",
           color="#F266AB", size=3, fontface="bold")+
  annotate("label", x = 2, y = .82, label = "86.1%",
           color="#F266AB", size=3, fontface="bold")+
  annotate("label", x = 3, y = .85, label = "89.0%",
           color="#F266AB", size=3, fontface="bold")+
  annotate("label", x = 4, y = .71, label = "74.97%",
           color="#F266AB", size=3, fontface="bold")


# H1: respondents in general support in-party messages. Our treatment worked!
# However, there is heterogeneity: they support the messages less when they are nostalgic/have scapegoats
# Especially when there are scapegoats
# This is against our expectations, although as we will see later the story has another side.

##### 1.1. SUPPORT | Nostalgia x all voters ##### 

ggsurvey(d4_weighted) + 
  aes(x = nost, fill = support_bin) + 
  geom_bar(position = "fill")+  
  scale_fill_manual(values = colors)+
  theme_minimal()+
  labs(y="", x="", 
       title="Support (%) message among nostalgic/non-nostalgic treatment")+
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face="bold"),
        legend.position = "top") 

##### 1.2. SUPPORT | H3: Scapegoat x all voters ##### 

ggsurvey(d4_weighted) + 
  aes(x = scap, fill = support_bin) + 
  geom_bar(position = "fill")+  
  scale_fill_manual(values = colors)+
  theme_minimal()+
  labs(y="", x="", 
       title="Support (%) message among scapegoat/non-scapegoat treatment")+
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, face="bold"),
        legend.position = "top")



# H1: We can REJECT it. 
# Voters are more supportive of non-nostalgic messages, although differences are minimal.
# What is way clear is that they are less supportive of messages that mention scapegoats.



##### 2.1. SUPPORT | H2: Nostalgia x ideology ####


lm_nostalgia <- lm(message_support ~  respondent_conservativeness_num*nost, weight=weights, d4_e)

d4_e$predictINT <-predict(lm_nostalgia, d4_e)


pred_nostalgia <-interact_plot(model = lm_nostalgia, pred = respondent_conservativeness_num, 
                               modx = nost, interval = FALSE,
                               data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of support")



margins_nostalgia <-
  lm_nostalgia %>%
  margins(at = list(respondent_conservativeness_num = seq(1, 5, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "nostNostalgia")


ame_nostalgia<- ggplot(margins_nostalgia, aes(respondent_conservativeness_num, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's conservativeness")+
  ylab("Conditional ATE") +
  theme_minimal()

 
support_nostalgia_h2 <- pred_nostalgia / ame_nostalgia


# H2: Yes! Right-wing voters (dis)like nostalgia more (less) than left-wing voters
# This is caused by the fact that they like all messages more.
# Because, actually, the gap in support between nostalgic and non-nostalgic is higher
# among right-wing voters.

##### 2.2. SUPPORT | H3: Scapegoat x populism ####

lm_scapegoat <- lm(message_support ~  pop_vote*scap, weight=weights, d4_e)


pred_scapegoat <-interact_plot(model = lm_scapegoat, pred = pop_vote, 
                               modx =scap, interval = FALSE,
                               data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of support")



margins_scapegoat <-
  lm_scapegoat %>%
  margins(at = list(pop_vote = seq(0, 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "scapScapegoat")


ame_scapegoat<- ggplot(margins_scapegoat, aes(pop_vote, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's populist voting")+
  ylab("Conditional ATE") +
  theme_minimal()

 
support_scapegoat_h3 <- pred_scapegoat / ame_scapegoat


#H3: Populists do dislike messages using scapegoats less than non-populists.
# They still slightly prefer a message without scapegoat but minimally




##### 3.1. PERSUASIVE SELF | H2: Nostalgia x ideology ####


lm_nostalgia_perself <- lm(nostalgia_support_self ~  respondent_conservativeness_num*nost, weight=weights, d4_e)


pred_nostalgia_perself <-interact_plot(model = lm_nostalgia_perself, pred = respondent_conservativeness_num, 
                               modx = nost, interval = FALSE,
                               data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of perceived persuasiveness for ideological in-group")



margins_nostalgia_perself <-
  lm_nostalgia_perself %>%
  margins(at = list(respondent_conservativeness_num = seq(1, 5, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "nostNostalgia")


ame_nostalgia_perself<- ggplot(margins_nostalgia_perself, aes(respondent_conservativeness_num, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's conservativeness")+
  ylab("Conditional ATE") +
  theme_minimal()

 
perself_nostalgia_h2 <- pred_nostalgia_perself / ame_nostalgia_perself


#Progressives seem to perceive that nostalgic messages are minimally more effective for the ideological-ingroup.


##### 3.2. PERSUASIVE SELF  | H3: Scapegoat x populism ####

lm_scapegoat_perself <- lm(scapegoat_support_self ~  pop_vote*scap, weight=weights, d4_e)


pred_scapegoat_perself <-interact_plot(model = lm_scapegoat_perself, pred = pop_vote, 
                               modx =scap, interval = FALSE,
                               data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of perceived persuasiveness for ideological in-group")



margins_scapegoat_perself <-
  lm_scapegoat_perself %>%
  margins(at = list(pop_vote = seq(0, 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "scapScapegoat")


ame_scapegoat_perself<- ggplot(margins_scapegoat_perself, aes(pop_vote, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's populist voting")+
  ylab("Conditional ATE") +
  theme_minimal()

perself_scapegoat_h3 <- pred_scapegoat_perself / ame_scapegoat_perself


# While in general populists seem to find messages as more convincing for ideological in-groups
# it seems that non-populists are more like to finding more convincing when it mentions scapegoats

##### 3.1. PERSUASIVE dutch | H2: Nostalgia x ideology ####


lm_nostalgia_perdutch <- lm(nostalgia_support_dutch ~  respondent_conservativeness_num*nost, weight=weights, d4_e)


pred_nostalgia_perdutch <-interact_plot(model = lm_nostalgia_perdutch, pred = respondent_conservativeness_num, 
                                       modx = nost, interval = FALSE,
                                       data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of perceived persuasiveness for the Dutch")



margins_nostalgia_perdutch <-
  lm_nostalgia_perdutch %>%
  margins(at = list(respondent_conservativeness_num = seq(1, 5, by = 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "nostNostalgia")


ame_nostalgia_perdutch<- ggplot(margins_nostalgia_perdutch, aes(respondent_conservativeness_num, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's conservativeness")+
  ylab("Conditional ATE") +
  theme_minimal()

perdutch_nostalgia_h2 <- pred_nostalgia_perdutch / ame_nostalgia_perdutch


# Conservative people feel nostalgic messages are more persuasive for the Dutch
# which is the opposite for the progressives


##### 3.2. PERSUASIVE dutch  | H3: Scapegoat x populism ####

lm_scapegoat_perdutch <- lm(scapegoat_support_dutch ~  pop_vote*scap, weight=weights, d4_e)


pred_scapegoat_perdutch <-interact_plot(model = lm_scapegoat_perdutch, pred = pop_vote, 
                                       modx =scap, interval = FALSE,
                                       data = d4_e, colors=colorsX) +
  labs(x="", y= "Predicted level of perceived persuasiveness for the Dutch")



margins_scapegoat_perdutch <-
  lm_scapegoat_perdutch %>%
  margins(at = list(pop_vote = seq(0, 1))) %>%
  summary %>%
  as.data.frame() %>%
  filter(factor == "scapScapegoat")


ame_scapegoat_perdutch<- ggplot(margins_scapegoat_perdutch, aes(pop_vote, AME)) +
  geom_point(colour="#2CD3E1") +
  geom_line(colour="#2CD3E1") +
  #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
  geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
  geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
  xlab("Pre-treatment respondent's populist voting")+
  ylab("Conditional ATE") +
  theme_minimal()

 
perdutch_scapegoat_h3 <- pred_scapegoat_perdutch / ame_scapegoat_perdutch


# Here, we see the opposite: populist voters seem to find more convincing for the Dutch
# the messages that do not mention a scapegoat, although the effect is hardly significant


## OVERALL: while people in general prefer messages that are not nostalgic or do not include scapegoats
##.         the perception that these messages might work better is quite widespread within certain ideological groups
##          and highly dependent on the group they think it should work for. 










##### EXTRA 1. SUPPORT | Scapegoat x ideology ####
  
  
lm_scapegoat <- lm(message_support ~  respondent_conservativeness_num*scap, weight=weights, d4_e)
  
  d4_e$predictINT <-predict(lm_scapegoat, d4_e)
  
  
  pred_scapegoat <-interact_plot(model = lm_scapegoat, pred = respondent_conservativeness_num, 
                                 modx =scap, interval = FALSE,
                                 data = d4_e, colors=colorsX) +
    labs(x="", y= "Predicted level of support")
  
  
  
  margins_scapegoat <-
    h2 %>%
    margins(at = list(respondent_conservativeness_num = seq(1, 5, by = 1))) %>%
    summary %>%
    as.data.frame() %>%
    filter(factor == "scapScapegoat")
  
  
  ame_scapegoat<- ggplot(margins_scapegoat, aes(respondent_conservativeness_num, AME)) +
    geom_point(colour="#2CD3E1") +
    geom_line(colour="#2CD3E1") +
    #coord_cartesian(xlim = c(1, 5), ylim = c(-3.5, .5)) +
    geom_errorbar(aes(ymax = (AME-SE*1.645), ymin = (AME+SE*1.645)), width = 0, colour="#2CD3E1") +
    geom_hline(yintercept = 0, linetype = "dashed", colour="#A459D1") +
    xlab("Pre-treatment respondent's conservativeness")+
    ylab("Conditional ATE") +
    theme_minimal()
  
   
  pred_scapegoat / ame_scapegoat
  
  # LOL! Right-wing voters don't make a difference between messages with/out scapegoats
  # The more right-wing the more likely you won't make a difference
  
  