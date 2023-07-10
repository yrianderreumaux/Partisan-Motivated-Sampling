# Load Data
Experiment1aData <- read.csv("data/Experiment1aData.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) 
Experiment1aData$Participant <- as.character(Experiment1aData$Participant) 

# Creating long format data set for sampling behavior
samp1 <- Experiment1aData %>%
  tidyr::pivot_longer(cols = c(in_samples, out_samples),
                      names_to = "Samp_Group",
                      values_to = "n_trials") %>%
  mutate(Samp_Group = factor(Samp_Group, levels = c("in_samples", "out_samples"), labels = c("In", "Out")))

# Creating a long format data set to look at evaluations
Evaluation.in.out <- Experiment1aData %>%
  tidyr::pivot_longer(cols = c(In.Est, Out.Est),
                      names_to = "Evaluated.Group",
                      values_to = "P.Estimates") %>%
  mutate(Evaluated.Group = factor(Evaluated.Group, levels = c("In.Est", "Out.Est"), labels = c("In", "Out")))

# Effects and Dummy coding categorical predictors for the sampling models
samp1 <- samp1 %>%
  mutate(Condition = factor(Condition, levels = 1:3, labels = c("Worse", "Same", "Better")),
         Condition_c = if_else(Condition == "Worse", -1, if_else(Condition == "Same", 0, 1)),
         Condition_c_eff = factor(Condition_c),
         Condition_c_dum = Condition) %>%
  fastDummies::dummy_cols(select_columns = "Condition_c_dum", remove_first_dummy = TRUE)

# Effects and Dummy coding categorical predictors for the point-estimate models.
Evaluation.in.out <- Evaluation.in.out %>%
  mutate(Condition = factor(Condition, levels = 1:3, labels = c("Worse", "Same", "Better")),
         Condition_c = if_else(Condition == "Worse", -1, if_else(Condition == "Same", 0, 1)),
         Condition_c_eff = factor(Condition_c),
         Condition_c_dum = Condition) %>%
  fastDummies::dummy_cols(select_columns = "Condition_c_dum", remove_first_dummy = TRUE)

# First sample model
Experiment1aData$First.Sample[Experiment1aData$First.Sample==-1] <- 0
Experiment1aData$First.Sample <- as.numeric(as.character(Experiment1aData$First.Sample))
Exp1.binom.test <- binom.test(length(Experiment1aData$First.Sample) - sum(Experiment1aData$First.Sample), length(Experiment1aData$First.Sample), p =.5)

# Define a function to build models
build_model <- function(base_model, variables) {
  model_list <- list()
  model <- base_model
  model_list[["Baseline"]] <- model
  for (variable in variables) {
    formula <- paste(". ~ . + ", variable)
    model <- update(model, formula)
    model_list[[variable]] <- model
  }
  return(model_list)
}

# Variables to add
variables_to_add <- c("Samp_GroupB_dum", "Valence_eff", "Condition_c_eff", 
                      "Group_eff", "Samp_GroupB_dum:Valence_eff", 
                      "Samp_GroupB_dum:Condition_c_eff", 
                      "Valence_eff:Condition_c_eff", 
                      "Samp_GroupB_dum:Condition_c_eff:Valence_eff", 
                      "Samp_GroupB_dum*Valence_eff*Condition_c_eff*Group_eff")

# Sampling models
Exp1.M1.Baseline <- glmer(n_trials~1+  (1|Participant), data = samp1, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp1.M1.models <- build_model(Exp1.M1.Baseline, variables_to_add)
Exp1.effects <- anova(Exp1.M1.models)
Exp1.coefficients <- summary(Exp1.M1.models[["Full"]])

# Eval models
Exp1.M2.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Evaluation.in.out, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
Exp1.M2.models <- build_model(Exp1.M2.Baseline, variables_to_add)
Exp1.M2.effects <- anova(Exp1.M2.models)
Exp1.M2.coef <- summary(Exp1.M2.models[["Full"]])

#evaluations
emm_SampGbyVal <- emmeans(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]], ~ Valence_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm_SampGbyVal, simple = "Evaluated.Group_eff")
eff_size(emm_SampGbyVal, sigma = sigma(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]]), edf = df.residual(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]]))

emm_SampGbyCond <- emmeans(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]], ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm_SampGbyCond, simple = "Evaluated.Group_eff")
eff_size(emm_SampGbyCond, sigma = sigma(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]), edf = df.residual(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]))

conditions <- c("Better", "Worse", "Same")
contrast_list <- lapply(conditions, function(cond) {
  subset_data <- subset(Evaluation.in.out, subset=Evaluation.in.out$Condition == cond)
  t.test(P.Estimates~Evaluated.Group, data = subset_data, paired = F)
  cohen.d(P.Estimates~Evaluated.Group, data = subset_data)
})
names(contrast_list) <- conditions

# Descriptives and simple contrasts
Exp1.contrasts.samp <- describeBy(samp1$n_trials, samp1$Samp_Group)
grouping_vars <- list(Evaluation.in.out$Evaluated.Group, Evaluation.in.out$Valence)
Exp1.contrasts <- lapply(grouping_vars, function(g) describeBy(Evaluation.in.out$P.Estimates, g))

# Again, wrapping the repeated code in a loop to avoid repetition
contrast_vars <- list(Evaluated.Group_eff, Valence_eff)
contrasts <- lapply(contrast_vars, function(var) {
  emmeans(Exp1.M2.models[[var]], specs = pairwise ~ var, type = "response")
})

# interaction of Eval by Valence
emm_SampGbyVal <- emmeans(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]], ~ Evaluated.Group_eff:Valence_eff, type = "response", lmer.df = "satterthwaite")
Exp1.EvalValContrast <- contrast(emm_SampGbyVal, "eff", by = "Evaluated.Group_eff")        
Exp1.EvalValContrast_d <- eff_size(emm_SampGbyVal, sigma = sigma(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]]), edf = df.residual(Exp1.M2.models[["Samp_GroupB_dum:Valence_eff"]]))

# interaction of Condition by Eval group
emm_SampGbyCond <- emmeans(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]], ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp1.evalCondContrast <- contrast(emm_SampGbyCond, "eff", by = "Condition_c_eff")          
Exp1.evalCondContrast_d <- eff_size(emm_SampGbyCond, sigma = sigma(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]), edf = df.residual(Exp1.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]))


#Visual for first sample 
# Common theme for plots
common_theme <- theme(
  plot.title = element_text(color = "black", size = 18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size = 40, face = "plain", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size = 40, face = "plain",family = "Times New Roman"),
  axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  legend.position = "none"
)

# Visual for first sample
Exp1.group <- c("in","out")
Exp1.mean <- c(71, 29)
Exp1.firstSample2 <- data.frame(Exp1.group, Exp1.mean)

firstSamplePlot2 <- ggplot(Exp1.firstSample2, aes(x=Exp1.group, y=Exp1.mean, fill = Exp1.group)) +
  geom_hline(yintercept = 107, linetype="solid") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_manual(name="group", values=c("in" = "grey","out"= "white")) +
  theme_bw() +  
  scale_x_discrete(labels=c('Ingroup', 'Outgroup')) +
  scale_y_continuous(limits = c(0, 100), labels = c("0%", "25%", "50%", "75%", "100%")) +
  common_theme
#ggsave("firstSamplePlot2.png", plot = firstSamplePlot2, width = 4, height = 4, dpi = 700)

# Visual for more ingroup samples
fullStudy1 <- samp1
fullStudy1$Condition <- factor(fullStudy1$Condition, levels = c("Worse", "Same", "Better"), labels = c(1, 2, 3))

moreIngroup  <- summarySEwithin(data=fullStudy1, idvar='Participant', measurevar = 'n_trials', withinvars = 'Samp_Group', na.rm = TRUE)

moreIngroupPlot <- ggplot(moreIngroup, aes(x=Samp_Group, y=n_trials, fill=Samp_Group))+
  geom_hline(yintercept = 107, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=n_trials-se, ymax=n_trials+se), width=.2, position=position_dodge(.9))+
  scale_fill_manual(name="Sampled Group", values=c("In" = "grey","Out"= "white")) +
  coord_cartesian(ylim = c(3, 8)) +
  theme_bw() +
  common_theme
#ggsave("moreIngroupPlot.png", plot = moreIngroupPlot, width = 4, height = 4, dpi = 700)

# Visuals for evals
fullStudy1Eval <- Evaluation.in.out
fullStudy1Eval$Condition <- factor(fullStudy1Eval$Condition, levels = c("Worse", "Same", "Better"), labels = c(1, 2, 3))

# Plotting Point-estimates for both with main effects of Valence
means_cond_both1Val <- summarySEwithin(data=fullStudy1Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Valence", withinvars = 'Evaluated.Group', na.rm = TRUE)

means_cond_both_graphVal <- ggplot(means_cond_both1Val, aes(x =Valence, y=P.Estimates, fill=Evaluated.Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimates-se, ymax=P.Estimates+se), width=.2, position=position_dodge(.9))+
  scale_fill_manual(name="Evaluated Group", values=c("In" = "grey","Out"= "white"), labels=c("Ingroup", "Outgroup")) +
  coord_cartesian(ylim = c(45, 75)) +
  theme_bw() +
  common_theme
#ggsave("means_cond_both_graphVal.png", plot = means_cond_both_graphVal)

# Plotting Point-estimates for both with main effects of Condition
means_cond_both1Cond <- summarySEwithin(data=fullStudy1Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Condition", withinvars = 'Evaluated.Group', na.rm = TRUE)

means_cond_both_graphCond <- ggplot(means_cond_both1Cond, aes(x =Condition, y=P.Estimates, fill=Evaluated.Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimates-se, ymax=P.Estimates+se), width=.2, position=position_dodge(.9))+
  scale_fill_manual(name="Evaluated Group", values=c("In" = "grey","Out"= "white"), labels=c("Ingroup", "Outgroup")) +
  coord_cartesian(ylim = c(45, 75)) +
  theme_bw() +
  common_theme
#ggsave("means_cond_both_graphCond.png", plot = means_cond_both_graphCond)

##### Print the results in the order they appear in the manuscript
cat('\nRESULTS FROM EXPERIMENT 1a\n')

# Binomial test for first sample
cat('\nBinomial test for first sample:\n')
print(Exp1.binom.test)

# Display the plot for first sample
cat('\nPlot for first sample:\n')
print(firstSamplePlot2)

# Mixed effects model on sampling behavior
cat('\nMixed effects model on sampling behavior:\n')
cat('\nDispersion parameter for Poisson:\n')
print(Exp1.disp)
cat('\nEffects of the model:\n')
print(Exp1.effects)
cat('\nCoefficients of the model:\n')
print(Exp1.coefficients)

# Display the plot for more ingroup samples
cat('\nPlot for more ingroup samples:\n')
print(moreIngroupPlot)

# Descriptive statistics for number of ingroup and outgroup samples
cat('\nDescriptive statistics for number of ingroup and outgroup samples:\n')
print(Exp1.contrasts.samp)

# Mixed effects model on ratings
cat('\nMixed effects model on ratings:\n')
print(Exp1.M2.effects)
cat('\nCoefficients of the model:\n')
print(Exp1.M2.coef)

# Display the plots for evaluations
cat('\nPlot for evaluations based on valence:\n')
print(means_cond_both_graphVal)
cat('\nPlot for evaluations based on real group difference:\n')
print(means_cond_both_graphCond)

# Contrast and effect size for evaluations by valence interaction
cat('\nContrast and effect size for evaluations by valence interaction:\n')
print(Exp1.EvalValContrast)
print(Exp1.EvalValContrast_d)

# Contrast and effect size for evaluated group by real group difference
cat('\nContrast and effect size for evaluated group by real group difference:\n')
print(Exp1.evalCondContrast)
print(Exp1.evalCondContrast_d)
