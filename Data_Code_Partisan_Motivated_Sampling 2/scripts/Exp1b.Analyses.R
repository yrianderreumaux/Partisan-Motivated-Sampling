# Load Data
Experiment1bData <- read.csv("data/Experiment1bData.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) 
Experiment1bData$Participant <- as.character(Experiment1bData$Participant) 

# Creating long format data set for sampling behavior
Samp2 <- Experiment1bData %>%
  tidyr::pivot_longer(cols = c(in_samples, out_samples),
                      names_to = "Samp_Group",
                      values_to = "n_trials") %>%
  mutate(Samp_Group = factor(Samp_Group, levels = c("in_samples", "out_samples"), labels = c("In", "Out")))

# Creating a long format data set to look at evaluations
Evaluation.in.out2 <- Experiment1bData %>%
  tidyr::pivot_longer(cols = c(In.Est, Out.Est),
                      names_to = "Evaluated.Group",
                      values_to = "P.Estimates") %>%
  mutate(Evaluated.Group = factor(Evaluated.Group, levels = c("In.Est", "Out.Est"), labels = c("In", "Out")))

# Effects and Dummy coding categorical predictors for the sampling models
Samp2 <- Samp2 %>%
  mutate(Condition = factor(Condition, levels = 1:3, labels = c("Worse", "Same", "Better")),
         Condition_c = if_else(Condition == "Worse", -1, if_else(Condition == "Same", 0, 1)),
         Condition_c_eff = factor(Condition_c),
         Condition_c_dum = Condition) %>%
  fastDummies::dummy_cols(select_columns = "Condition_c_dum", remove_first_dummy = TRUE)

# Effects and Dummy coding categorical predictors for the point-estimate models.
Evaluation.in.out2 <- Evaluation.in.out2 %>%
  mutate(Condition = factor(Condition, levels = 1:3, labels = c("Worse", "Same", "Better")),
         Condition_c = if_else(Condition == "Worse", -1, if_else(Condition == "Same", 0, 1)),
         Condition_c_eff = factor(Condition_c),
         Condition_c_dum = Condition) %>%
  fastDummies::dummy_cols(select_columns = "Condition_c_dum", remove_first_dummy = TRUE)

# First sample model
Experiment1bData$First.Sample[Experiment1bData$First.Sample==-1] <- 0
Experiment1bData$First.Sample <- as.numeric(as.character(Experiment1bData$First.Sample))
Exp1.binom.test <- binom.test(length(Experiment1bData$First.Sample) - sum(Experiment1bData$First.Sample), length(Experiment1bData$First.Sample), p =.5)

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
Exp1b.M1.Baseline <- glmer(n_trials~1+  (1|Participant), data = Samp2, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp1b.M1.models <- build_model(Exp1b.M1.Baseline, variables_to_add)
Exp1b.effects <- anova(Exp1b.M1.models)
Exp1b.coefficients <- summary(Exp1b.M1.models[["Full"]])

# Eval models
Exp1b.M2.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Evaluation.in.out2, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
Exp1b.M2.models <- build_model(Exp1b.M2.Baseline, variables_to_add)
Exp1b.M2.effects <- anova(Exp1b.M2.models)
Exp1b.M2.coef <- summary(Exp1b.M2.models[["Full"]])

# Evaluations
emm_SampGbyVal <- emmeans(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]], ~ Valence_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm_SampGbyVal, simple = "Evaluated.Group_eff")
eff_size(emm_SampGbyVal, sigma = sigma(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]]), edf = df.residual(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]]))

emm_SampGbyCond <- emmeans(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]], ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm_SampGbyCond, simple = "Evaluated.Group_eff")
eff_size(emm_SampGbyCond, sigma = sigma(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]), edf = df.residual(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]))

# Contrasts for different conditions
conditions <- c("Better", "Worse", "Same")
contrast_list <- lapply(conditions, function(cond) {
  subset_data <- subset(Evaluation.in.out2, subset=Evaluation.in.out2$Condition == cond)
  t.test(P.Estimates~Evaluated.Group, data = subset_data, paired = F)
  cohen.d(P.Estimates~Evaluated.Group, data = subset_data)
})
names(contrast_list) <- conditions

# Descriptives and simple contrasts
Exp1b.contrasts.samp <- describeBy(Samp2$n_trials, Samp2$Samp_Group)
grouping_vars <- list(Evaluation.in.out2$Evaluated.Group, Evaluation.in.out2$Valence)
Exp1b.contrasts <- lapply(grouping_vars, function(g) describeBy(Evaluation.in.out2$P.Estimates, g))

# Contrasts
contrast_vars <- list(Evaluated.Group_eff, Valence_eff)
contrasts <- lapply(contrast_vars, function(var) {
  emmeans(Exp1b.M2.models[[var]], specs = pairwise ~ var, type = "response")
})

# Interaction of Eval by Valence
emm_SampGbyVal <- emmeans(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]], ~ Evaluated.Group_eff:Valence_eff, type = "response", lmer.df = "satterthwaite")
Exp1b.EvalValContrast <- contrast(emm_SampGbyVal, "eff", by = "Evaluated.Group_eff")        
Exp1b.EvalValContrast_d <- eff_size(emm_SampGbyVal, sigma = sigma(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]]), edf = df.residual(Exp1b.M2.models[["Samp_GroupB_dum:Valence_eff"]]))

# Interaction of Condition by Eval group
emm_SampGbyCond <- emmeans(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]], ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp1b.evalCondContrast <- contrast(emm_SampGbyCond, "eff", by = "Condition_c_eff")          
Exp1b.evalCondContrast_d <- eff_size(emm_SampGbyCond, sigma = sigma(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]), edf = df.residual(Exp1b.M2.models[["Samp_GroupB_dum:Condition_c_eff"]]))


##Visuals for first sample
#####
#First sample plot
Exp2.group <- c("in","out")
Exp2.mean <- c(70, 30)
Exp2.firstSample <- data.frame(Exp2.group, Exp2.mean)

Exp2.firstSamplePlot <- ggplot(Exp2.firstSample, aes(x=Exp2.group, y=Exp2.mean, fill = Exp2.group)) +
  geom_hline(yintercept = 107, linetype="solid") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  coord_cartesian(ylim = c(0, 100))+scale_fill_manual(name="group", values=c("in" = "grey","out"= "white"),
                                                      labels=c("", ""))+theme_bw()+  
  theme(legend.justification=c(.05,.98),
        legend.position=c(.05,.98), panel.grid.major.x = element_blank())  + theme(legend.position = "none") + ylab("")+ xlab("")+
  scale_x_discrete(labels=c('Ingroup', 'Outgroup'))+ theme(
    axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))+ scale_y_continuous(limits = c(0, 100), labels = c("0%", "25%", "50%", "75%", "100%"))

#ggsave("firstSamplePlot1b.png", width = 4, height = 4, dpi = 700)

#####

#visuals for more ingroup samples
#####
fullStudy2 <- Samp2
fullStudy2$Condition[fullStudy2$Condition=="Better"] <- 3
fullStudy2$Condition[fullStudy2$Condition=="Same"] <- 2
fullStudy2$Condition[fullStudy2$Condition=="Worse"] <- 1

moreIngroup2  <- summarySEwithin(data=fullStudy2, idvar='Participant', measurevar = 'n_trials', withinvars = 'Samp_Group', na.rm = TRUE)
moreIngroup2plot <- ggplot(moreIngroup2, aes(x=Samp_Group, y=n_trials, fill=Samp_Group))+
  geom_hline(yintercept = 107, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=n_trials-se, ymax=n_trials+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  xlab("") +scale_x_discrete(labels=c('Ingroup', 'Outgroup'))+
  ylab("") +  scale_fill_manual(name="Sampled Group", values=c("In" = "grey","Out"= "white"),
                                labels=c("", "")) + # Use darker colors, lightness=40
  scale_y_continuous(limits = c(0, 100))+
  theme_bw() +
  theme(legend.justification=c(.05,.98),
        legend.position=c(.05,.98), panel.grid.major.x = element_blank())  +
  coord_cartesian(ylim = c(3, 8))+ theme(legend.position = "none")

moreIngroup2plot + theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40, face = "plain", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "plain",family = "Times New Roman"),
  axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("moreIngroup1bplot.png", width = 4, height = 4, dpi = 700)

#####

#Visuals for evals
#####
##Plotting Point-estimates for both with main effects of Condition
fullStudy2Eval <- Eval2
fullStudy2Eval$Condition[fullStudy2Eval$Condition=="Better"] <- 3
fullStudy2Eval$Condition[fullStudy2Eval$Condition=="Same"] <- 2
fullStudy2Eval$Condition[fullStudy2Eval$Condition=="Worse"] <- 1

means_cond_Val2 <- summarySEwithin(data=fullStudy2Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Valence", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_Val2Plot <- ggplot(means_cond_Val2, aes(x =Valence, y=P.Estimates, fill=Evaluated.Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimates-se, ymax=P.Estimates+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  xlab("") +scale_x_discrete(labels=c('Negative','Positive'))+
  ylab("") +
  scale_fill_manual(name="Evaluated Group", values=c("In" = "grey","Out"= "white"),
                    labels=c("Ingroup", "Outgroup")) + # Use darker colors, lightness=40
  scale_y_continuous(limits = c(0, 80), breaks = c(50,60,70))+
  theme_bw() +
  theme(legend.justification=c(.05,.98),
        legend.position=c(2,.90), panel.grid.major.x = element_blank())  +
  coord_cartesian(ylim = c(45, 75))+theme(legend.position = "none")

means_cond_Val2Plot <- means_cond_Val2Plot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 20,face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_Val2Plot.png")#save fill legend

means_cond_Cond2 <- summarySEwithin(data=fullStudy2Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Condition", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_Cond2Plot <- ggplot(means_cond_Cond2, aes(x =Condition, y=P.Estimates, fill=Evaluated.Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimates-se, ymax=P.Estimates+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  xlab("") +scale_x_discrete(labels=c('Ingroup Worse','No Difference', 'Ingroup Better'))+
  ylab("") +
  scale_fill_manual(name="Evaluated Group", values=c("In" = "grey","Out"= "white"),
                    labels=c("Ingroup", "Outgroup")) + # Use darker colors, lightness=40
  scale_y_continuous(limits = c(0, 80), breaks = c(50,60,70))+
  theme_bw()+
  theme_linedraw() +
  theme(legend.justification=c(.05,.98),
        legend.position=c(.05,.98), panel.grid.major.x = element_blank())  +
  coord_cartesian(ylim = c(45, 75)) +theme(legend.position = "none")

means_cond_Cond2Plot <- means_cond_Cond2Plot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 17, face = "plain",color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_Cond2Plot.png")#save fill legend
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FROM EXPERIMENT 1b')
print('Binomial test for first sample')
print(Exp2.binom.test)
print(Exp2.firstSamplePlot)

print('Mixed effects model on sampling behavior')
print('dispersion paramater for poisson')
print(Exp2.disp)
print(Exp2.effects)
print(Exp2.coefficients)
print(moreIngroup2)

print('Descriptive statistics for number of ingroup and outgroup samples')
print(Exp2.contrasts.samp)

print('Mixed effects model on ratings')
print(Exp2.M2.effects)
print(Exp2.M2.coefficients)
print(means_cond_Val2Plot)
print(means_cond_Cond2Plot)

print('Contrast and effect size for evaluations by valence interaction')
print(Exp2.ValContrast)
print(Exp2.ValContrast_d)
print('Contrast and effect size for evaluated group by real group difference')
print(Exp2.CondGroupContrast)
print(Exp2.CondGroupContrast_d)
