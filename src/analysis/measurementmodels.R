dm <- d1 %>% 
  dplyr::select(contains("F1") | contains("H1b")) %>% 
  mutate_at(vars(c(F1_3, F1_5)), 
            funs(recode(., `1` = 5, 
                        `2` =4,`3` = 3,
                        `4` = 2, `5` = 1))) %>% 
  drop_na()

##Make CorrPlots
##All data
tmp <- dm %>% 
  rename(`Populist Attitudes 1` = F1_1,
         `Populist Attitudes 2` = F1_2,
         `Populist Attitudes 3` = F1_3,
         `Populist Attitudes 4` = F1_4,
         `Populist Attitudes 5` = F1_5,
         `Populist Attitudes 6` = F1_6,
         `Populist Attitudes 7` = F1_7,
         `Populist Attitudes 8` = F1_8,
         `Populist Attitudes 9` = F1_9,
         `Nostalgic Attitudes 1` = H1b_1, 
         `Nostalgic Attitudes 2` = H1b_2,
         `Nostalgic Attitudes 3` = H1b_3,
         `Nostalgic Attitudes 4` = H1b_4,
         `Nostalgic Attitudes 5` = H1b_5,
         `Nostalgic Attitudes 6` = H1b_6,
         `Nostalgic Attitudes 7` = H1b_7,
         `Nostalgic Attitudes 8` = H1b_8,
         `Nostalgic Attitudes 9` = H1b_9,
         `Nostalgic Attitudes 10` = H1b_10,
         `Nostalgic Attitudes 11` = H1b_11
  )

p1 <- ggcorrmat(tmp,
                type = "robust", # correlation method
                cor.vars = c(everything()), # a range of variables can be selected
                title = "Correlalogram for Variables under Study",
                colors = c(fig_cols[2], "white", fig_cols[3])
)

nos <- tmp %>% 
  dplyr::select(contains("Nostalgic Attitudes"))

p2 <- ggcorrmat(nos,
                type = "robust", # correlation method
                cor.vars = c(everything()), # a range of variables can be selected
                title = "Correlalogram for Nostalgic Attitude Variables",
                colors = c(fig_cols[2], "white", fig_cols[3])
)

pop <- tmp %>% 
  dplyr::select(contains("Populist Attitudes"))

p3 <- ggcorrmat(pop,
                type = "robust", # correlation method
                cor.vars = c(everything()), # a range of variables can be selected
                title = "Correlalogram for Populist Attitude Variables",
                colors = c(fig_cols[2], "white", fig_cols[3])
)

# How many factors should I use? --> Parallel Analysis 
nospanalysis <- fa.parallel(dm, fm = "ml", 
                            fa = "fa") # Scree Plots suggests 1 Factor (maybe 2), 

efa <- fa(tmp, nfactors = 2, rotate = "oblimin", fm = "ml")
#efa

# Confirmatory Factor Analysis
# Defining model
cfa_model <- "
   Playful Nostalgia =~     H1b_3 + H1b_6 + H1b_9
   Progressive Nostalgia =~ H1b_1 + H1b_2 + H1b_11 
   Reluctant Nostalgia =~ H1b_4 + H1b_5 + H1b_7 + H1b_8 + H1b_10
   Populist Attitude =~ F1_1 + F1_2 + F1_3 + F1_4 + F1_5 + F1_6 + F1_7 + F1_8 + F1_9
"
fit.cfa <- cfa(cfa_model, 
               estimator = "MLR", 
               data = dm)
fitMeasures(fit.cfa, c("chisq.scaled", "df.scaled", "pvalue.scaled", 
                       "cfi.robust", "tli.robust", "rmsea.robust"))
summary(fit.cfa, fit = T, std = T)
modindices(fit.cfa) %>%
  arrange(-mi) %>%
  mutate_if(is.numeric, round, 2)

# prcomp Function: Based on Eigenvalue
results <- prcomp(dm, scale = T)
#results

# Calculate the Eigenvalue- (>1, PC explains more 
# of the data than it is not explaining more of the data)
eig <- (results$sdev)^2
eig <- eig[which(eig>=1)]
#eig

# Variance 
variance <- eig*100/sum(eig)
#variance

# Cumulative Variance 
cumvar <- cumsum(variance)
#cumvar

# Percentage of Variance explained (Same as Cumulative Variance)
pca_var_per <- as_tibble(round(variance/sum(variance)*100, 1)) %>% 
  # Plotting Percentage of Variance for all PCs  
  mutate(pca = 1:5,
         pca = recode(pca,
                      `1` = "Principal Component 1",
                      `2` = "Principal Component 2",
                      `3` = "Principal Component 3",
                      `4` = "Principal Component 4",
                      `5` = "Principal Component 5",
                      `6` = "Principal Component 6"),
         value = value/100) %>% 
  drop_na(pca)

p4 <- pca_var_per %>% 
  ggplot(aes(y = pca, x = value)) +
  geom_col(fill = fig_cols[1]) +
  labs(x = "Explained Variance", y = "") +
  scale_x_continuous(labels = scales::percent) +
  theme_ipsum()

screeplot(results, type = "l", main = "Screeplot Principal Component Analysis")
abline(1,0, col = "red", lty = 2)

loading_scores1 <- results$rotation[,1]
variable_scores1 <- abs(loading_scores1) ## get the magnitudes
variable_score_ranked1 <- sort(variable_scores1, decreasing=TRUE)
top_10_variable_1 <- names(variable_score_ranked1[1:10])

top10 <- as_tibble(variable_score_ranked1) %>% 
  mutate(var = names(variable_score_ranked1),
         var = recode(var,
                      `F1_1` = "Populist Attitudes 1",
                      `F1_2` = "Populist Attitudes 2",
                      `F1_3` = "Populist Attitudes 3",
                      `F1_4` = "Populist Attitudes 4",
                      `F1_5` = "Populist Attitudes 5",
                      `F1_6` = "Populist Attitudes 6",
                      `F1_7` = "Populist Attitudes 7",
                      `F1_8` = "Populist Attitudes 8",
                      `F1_9` = "Populist Attitudes 9",
                      `H1b_1` = "Nostalgic Attitudes 1", 
                      `H1b_2` = "Nostalgic Attitudes 2",
                      `H1b_3` = "Nostalgic Attitudes 3",
                      `H1b_4` = "Nostalgic Attitudes 4",
                      `H1b_5` = "Nostalgic Attitudes 5",
                      `H1b_6` = "Nostalgic Attitudes 6",
                      `H1b_7` = "Nostalgic Attitudes 7",
                      `H1b_8` = "Nostalgic Attitudes 8",
                      `H1b_9` = "Nostalgic Attitudes 9",
                      `H1b_10` = "Nostalgic Attitudes 10",
                      `H1b_11` = "Nostalgic Attitudes 11"),
         factor = "Dimension 1") %>% 
  arrange() %>% 
  filter(value >0.162)

loading_scores2 <- results$rotation[,2]
variable_scores2 <- abs(loading_scores2) ## get the magnitudes
variable_score_ranked2 <- sort(variable_scores2, decreasing=TRUE)
tst <- as_tibble(variable_score_ranked2) %>% 
  mutate(var = names(variable_score_ranked2),
         var = recode(var,
                      `F1_1` = "Populist Attitudes 1",
                      `F1_2` = "Populist Attitudes 2",
                      `F1_3` = "Populist Attitudes 3",
                      `F1_4` = "Populist Attitudes 4",
                      `F1_5` = "Populist Attitudes 5",
                      `F1_6` = "Populist Attitudes 6",
                      `F1_7` = "Populist Attitudes 7",
                      `F1_8` = "Populist Attitudes 8",
                      `F1_9` = "Populist Attitudes 9",
                      `H1b_1` = "Nostalgic Attitudes 1", 
                      `H1b_2` = "Nostalgic Attitudes 2",
                      `H1b_3` = "Nostalgic Attitudes 3",
                      `H1b_4` = "Nostalgic Attitudes 4",
                      `H1b_5` = "Nostalgic Attitudes 5",
                      `H1b_6` = "Nostalgic Attitudes 6",
                      `H1b_7` = "Nostalgic Attitudes 7",
                      `H1b_8` = "Nostalgic Attitudes 8",
                      `H1b_9` = "Nostalgic Attitudes 9",
                      `H1b_10` = "Nostalgic Attitudes 10",
                      `H1b_11` = "Nostalgic Attitudes 11"),
         factor = "Dimension 2") %>% 
  arrange() %>% 
  filter(value >0.122)

p5 <- top10 %>% 
  add_case(tst) %>% 
  mutate(var = factor(var,
                      levels = c("Populist Attitudes 1",
                                 "Populist Attitudes 2",
                                 "Populist Attitudes 3",
                                 "Populist Attitudes 4",
                                 "Populist Attitudes 5",
                                 "Populist Attitudes 6",
                                 "Populist Attitudes 7",
                                 "Populist Attitudes 8",
                                 "Populist Attitudes 9",
                                 "Nostalgic Attitudes 1", 
                                 "Nostalgic Attitudes 2",
                                 "Nostalgic Attitudes 3",
                                 "Nostalgic Attitudes 4",
                                 "Nostalgic Attitudes 5",
                                 "Nostalgic Attitudes 6",
                                 "Nostalgic Attitudes 7",
                                 "Nostalgic Attitudes 8",
                                 "Nostalgic Attitudes 9",
                                 "Nostalgic Attitudes 10",
                                 "Nostalgic Attitudes 11"))) %>% 
  ggplot(aes(x = value, y = var, fill = factor)) +
  geom_col(position = "dodge") +
  labs(x = "10 Highest Loadings per Dimension", y = "") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values = fig_cols)

##LCA
f <- cbind(H1b_1, H1b_2, H1b_3, H1b_4,
           H1b_5, H1b_6, H1b_7, H1b_8,
           H1b_9, H1b_10, H1b_11,
           F1_1, F1_2, F1_3, F1_4,
           F1_5, F1_6, F1_7, F1_8,
           F1_9) ~1

lca8 <- poLCA(formula = f, maxiter=1000, nclass=5, nrep=75, data=dm, na.rm=T)
lca8$predclass <- as.numeric(lca8$predclass)

dm$class <- lca8$predclass # take the predictor for the class and add it to the data frame

mean_class1 <- apply(dm[which(dm$class==1),1:18], 2, mean, na.rm=T)
mean_class2 <- apply(dm[which(dm$class==2),1:18], 2, mean, na.rm=T)
mean_class3 <- apply(dm[which(dm$class==3),1:18], 2, mean, na.rm=T)
mean_class4 <- apply(dm[which(dm$class==4),1:18], 2, mean, na.rm=T)
mean_class5 <- apply(dm[which(dm$class==5),1:18], 2, mean, na.rm=T)
mean_class6 <- apply(dm[which(dm$class==6),1:18], 2, mean, na.rm=T)
mean_class7 <- apply(dm[which(dm$class==7),1:18], 2, mean, na.rm=T)
mean_class8 <- apply(dm[which(dm$class==8),1:18], 2, mean, na.rm=T)

# Creating data frame with Survey Items, Latent Class ID and mean scores on Survey Items
p6 <- tibble(items = c(names(mean_class1), names(mean_class2),
                      names(mean_class3), names(mean_class4),
                      names(mean_class5), names(mean_class6),
                      names(mean_class7), names(mean_class7)),
             id = c(rep("Latent Class 1", length(mean_class1)), # create id to separate lines in graph per class
                    rep("Latent Class 2", length(mean_class2)),
                    rep("Latent Class 3", length(mean_class3)),
                    rep("Latent Class 4", length(mean_class4)),
                    rep("Latent Class 5", length(mean_class5)),
                    rep("Latent Class 6", length(mean_class6)),
                    rep("Latent Class 7", length(mean_class7)),
                    rep("Latent Class 8", length(mean_class8))), 
             mean = c(mean_class1, mean_class2,
                      mean_class3, mean_class4, 
                      mean_class5, mean_class6,
                      mean_class7, mean_class8)) %>% 
  mutate(items = recode(items,
                              `F1_1` = "Populist Attitudes 1",
                              `F1_2` = "Populist Attitudes 2",
                              `F1_3` = "Populist Attitudes 3",
                              `F1_4` = "Populist Attitudes 4",
                              `F1_5` = "Populist Attitudes 5",
                              `F1_6` = "Populist Attitudes 6",
                              `F1_7` = "Populist Attitudes 7",
                              `F1_8` = "Populist Attitudes 8",
                              `F1_9` = "Populist Attitudes 9",
                              `H1b_1` = "Nostalgic Attitudes 1", 
                              `H1b_2` = "Nostalgic Attitudes 2",
                              `H1b_3` = "Nostalgic Attitudes 3",
                              `H1b_4` = "Nostalgic Attitudes 4",
                              `H1b_5` = "Nostalgic Attitudes 5",
                              `H1b_6` = "Nostalgic Attitudes 6",
                              `H1b_7` = "Nostalgic Attitudes 7",
                              `H1b_8` = "Nostalgic Attitudes 8",
                              `H1b_9` = "Nostalgic Attitudes 9",
                              `H1b_10` = "Nostalgic Attitudes 10",
                              `H1b_11` = "Nostalgic Attitudes 11"),
         items = factor(items,
                      levels = c("Populist Attitudes 1",
                                 "Populist Attitudes 2",
                                 "Populist Attitudes 3",
                                 "Populist Attitudes 4",
                                 "Populist Attitudes 5",
                                 "Populist Attitudes 6",
                                 "Populist Attitudes 7",
                                 "Populist Attitudes 8",
                                 "Populist Attitudes 9",
                                 "Nostalgic Attitudes 1", 
                                 "Nostalgic Attitudes 2",
                                 "Nostalgic Attitudes 3",
                                 "Nostalgic Attitudes 4",
                                 "Nostalgic Attitudes 5",
                                 "Nostalgic Attitudes 6",
                                 "Nostalgic Attitudes 7",
                                 "Nostalgic Attitudes 8",
                                 "Nostalgic Attitudes 9",
                                 "Nostalgic Attitudes 10",
                                 "Nostalgic Attitudes 11"))) %>% 
  ggplot(aes(x = items, y = mean, group = id,
                          color = id)) +
  geom_point() +
  geom_line() +
  theme_ipsum() +
  labs(x="", y = "Values of Latent Class") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)) +
  scale_color_manual(values = fig_cols) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
