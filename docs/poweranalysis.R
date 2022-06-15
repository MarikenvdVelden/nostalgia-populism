# Power Analysis
rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(scales)
library(here)

#H1 + H2: pooled over parties
N <- 100
assignment_prob <- 0.25 # 1 out of 4: mask vs. unmask, specified vs. unspecified 
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob, legacy = TRUE)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, inquiry = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, 
                    N=seq(3000,7000,1000), 
                    treatment_effect=0.1)

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha),
                                      keep_defaults=TRUE)
diagnosis1 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis1$diagnosands_df
diagnosis1  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis")

p <- diagnosis1 %>%
  ggplot(aes(x=N, y=power.onetailed, group=factor(treatment_effect), colour=factor(treatment_effect))) +
  geom_line() +
  geom_hline(yintercept=0.95, linetype="dashed") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 6000,linetype="dotdash", color = "darkgrey") +
  labs(x = "Number of Respondents", y = "Power: One-Tailed",
       title = "Pooled Issue Analysis",
       subtitle = "Various Levels of Treatment Effect") +
  scale_y_continuous(labels=percent) +
  theme_minimal() +
  scale_color_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
ggsave(here("report/figures/power_analysis.png"), p, width=8, height=5, dpi=900)


