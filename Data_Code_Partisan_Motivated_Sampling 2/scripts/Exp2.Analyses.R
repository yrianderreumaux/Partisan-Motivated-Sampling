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

# Function for binomial test
do_binom_test <- function(df, column, condition=NULL){
  if(is.null(condition)){
    return(binom.test(length(df[[column]]) - sum(df[[column]]), length(df[[column]]), p = .5))
  } else {
    df_sub <- df[df$Val==condition, ]
    return(binom.test(length(df_sub[[column]]) - sum(df_sub[[column]]), length(df_sub[[column]]), p = .5))
  }
}

# Function to create models
create_model <- function(base_formula, base_model, data, factors, interactions, family = NULL){
  model <- base_model
  for (factor in factors){
    model <- update(model, formula(paste(". ~ . +", factor)))
  }
  
  for (interaction in interactions){
    model <- update(model, formula(paste(". ~ . +", interaction)))
  }
  
  if(!is.null(family)) {
    return(anova(base_model, model, dispersion_glmer(base_model)))
  } else {
    return(anova(base_model, model))
  }
}

# Now, let's use these functions to handle your binomial tests and models:

# Binomial tests
Experiment2Data$firstSample[Experiment2Data$firstSample == -1] <- 0
Experiment2Data$firstSample <- as.numeric(as.character(Experiment2Data$firstSample))
Exp3_binom_tests <- list(
  "Total" = do_binom_test(Experiment2Data, "firstSample"),
  "Pos" = do_binom_test(Experiment2Data, "firstSample", "pos"),
  "Neg" = do_binom_test(Experiment2Data, "firstSample", "neg")
)

# GLMER and LMER models
# Define factors and interactions for each model
model1_factors <- c("Samp_GroupB_dum", "Valence_eff", "Condition_c_eff", "Group_eff")
model1_interactions <- c("Samp_GroupB_dum:Valence_eff", "Samp_GroupB_dum:Condition_c_eff", "Valence_eff:Condition_c_eff", "Samp_GroupB_dum:Condition_c_eff:Valence_eff", "Samp_GroupB_dum*Valence_eff*Condition_c_eff*Group_eff")
model2_factors <- c("Evaluated.Group_eff", "Valence_eff", "Condition_c_eff", "Group_eff")
model2_interactions <- c("Evaluated.Group_eff:Valence_eff", "Evaluated.Group_eff:Condition_c_eff", "Valence_eff:Condition_c_eff", "Evaluated.Group_eff:Condition_c_eff:Valence_eff", "Evaluated.Group_eff*Valence_eff*Condition_c_eff*Group_eff")
model3_factors <- c("Samp_GroupB_dum", "PunditImp_eff", "Condition_c_eff", "Group_eff")
model3_interactions <- c("Samp_GroupB_dum:PunditImp_eff", "Samp_GroupB_dum:Condition_c_eff", "PunditImp_eff:Condition_c_eff", "Samp_GroupB_dum:Condition_c_eff:PunditImp_eff", "Samp_GroupB_dum*PunditImp_eff*Condition_c_eff*Group_eff")
model4_factors <- c("Evaluated.Group_eff", "PunditImp_eff", "Condition_c_eff", "Group_eff")
model4_interactions <- c("Evaluated.Group_eff:PunditImp_eff", "Evaluated.Group_eff:Condition_c_eff", "PunditImp_eff:Condition_c_eff", "Evaluated.Group_eff:Condition_c_eff:PunditImp_eff", "Evaluated.Group_eff*PunditImp_eff*Condition_c_eff*Group_eff")

# Create GLMER models
Exp3.M1.effects <- create_model(n_trials ~ 1 +  (1|Participant), glmer(n_trials ~ 1 +  (1|Participant), data = Samp3, family = 'poisson', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))), Samp3, model1_factors, model1_interactions, family = 'poisson')
Exp3.M3.effects <- create_model(n_trials ~ 1 +  (1|Participant), glmer(n_trials ~ 1 +  (1|Participant), data = Samp3, family = 'poisson', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5))), Samp3, model3_factors, model3_interactions, family = 'poisson')

# Create LMER models
Exp3.M2.effects <- create_model(P.Estimates ~ 1 +  (1|Participant), lmer(P.Estimates ~ 1 +  (1|Participant), data = Eval3), Eval3, model2_factors, model2_interactions)
Exp3.M4.effects <- create_model(P.Estimates ~ 1 +  (1|Participant), lmer(P.Estimates ~ 1 +  (1|Participant), data = Eval3), Eval3, model4_factors, model4_interactions)

# Print model summaries
Exp3.M1.coefficients <- summary(Exp3.M1.effects$Full)
Exp3.M2.coefficients <- summary(Exp3.M2.effects$Full)
Exp3.M3.coefficients <- summary(Exp3.M3.effects$Full)
Exp3.M4.pund.effect <- summary(Exp3.M4.effects$SampGbyPundit)
Exp3.M4.pund.cond.effect <- summary(Exp3.M4.effects$SampGbyCondbyPundit)

# Function for descriptive statistics
describe_data <- function(df, column, group){
  return(describeBy(df[[column]], df[[group]]))
}

# Function to calculate contrasts
calc_contrast <- function(model, formula_str, sigma, df_res){
  emm <- emmeans(model, formula_str, type = "response", lmer.df = "satterthwaite")
  contrast <- contrast(emm, simple = strsplit(formula_str, ":")[[1]][1])
  contrast_d <- eff_size(emm, sigma = sigma, edf = df_res)
  return(list("Contrast" = contrast, "Contrast_d" = contrast_d))
}

# Descriptive statistics
Exp3.contrasts.samp <- describe_data(Samp3, "n_trials", "Samp_Group")
Exp3.contrasts.EvalbyGroup <- describe_data(Eval3, "P.Estimates", "Evaluated.Group")
Exp3.contrasts.EvalbyValence <- describe_data(Eval3, "P.Estimates", "Valence")

# Contrast calculations
Exp3.ValCondContrast <- calc_contrast(Exp3.M2.CondbyVal, "~ Valence_eff:Condition_c_eff", sigma(Exp3.M2.CondbyVal), df.residual(Exp3.M2.CondbyVal))
Exp3.ValCondEvalContrast <- calc_contrast(Exp3.M2.SampGbyCondbyVal, "~ Valence_eff:Condition_c_eff:Evaluated.Group_eff", sigma(Exp3.M2.SampGbyCondbyVal), df.residual(Exp3.M2.SampGbyCondbyVal))
Exp3.CondEvalContrast <- calc_contrast(Exp3.M2.SampGbyCond, "~ Condition_c_eff:Evaluated.Group_eff", sigma(Exp3.M2.SampGbyCond), df.residual(Exp3.M2.SampGbyCond))

#visuals for first sample
#####
#calculate SE for first samples
p.estFull <- mean(Experiment2Data$firstSample)
varianceFull <- (p.estFull*(1-p.estFull))/nrow(Experiment2Data)
std.devFull <- sqrt(varianceFull)

p.estNeg <- mean(study3NEG$firstSample)
varianceNeg <- (p.estNeg*(1-p.estNeg))/nrow(study3NEG)
std.devNeg <- sqrt(varianceNeg)

p.estPos<- mean(study3POS$firstSample)
variancePos <- (p.estPos*(1-p.estPos))/nrow(study3POS)
std.devPos <- sqrt(variancePos)

Exp3.group <- c("in","out")
Exp3.mean <- c(65, 35)
Exp3.firstSample3 <- data.frame(Exp3.group)
firstSamplePlot3 <- ggplot(Exp3.firstSample3, aes(x=Exp3.group, y=Exp3.mean, fill = Exp3.group)) +
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
#ggsave("firstSamplePlot2.png", width = 4, height = 4, dpi = 700)

#when positive
Exp3.pos.group <- c("in","out")
Exp3.pos.mean <- c(77, 23)
Exp3.pos.firstSamplePOS <- data.frame(Exp3.pos.group, Exp3.pos.mean)
firstSamplePlotPos <- ggplot(Exp3.pos.firstSamplePOS, aes(x=Exp3.pos.group, y=Exp3.pos.mean, fill = Exp3.pos.group)) +
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
#ggsave("firstSamplePlotPos.png", width = 4, height = 4, dpi = 700)

#when negative
Exp3.neg.group <- c("in","out")
Exp3.neg.mean <- c(52, 48)
Exp3.neg.firstSampleNEG <- data.frame(Exp3.neg.group, Exp3.neg.mean)
firstSamplePlotNeg <- ggplot(Exp3.neg.firstSampleNEG, aes(x=Exp3.neg.group, y=Exp3.neg.mean, fill = Exp3.neg.group)) +
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
#ggsave("firstSamplePlotNeg.png", width = 4, height = 4,  dpi=700)
#####

#Visuals for more ingroup sampling
#####
#Plotting Sampling behavior for both collapsed: Both pos and neg
fullStudy3 <- Samp3
fullStudy3$Condition[fullStudy3$Condition=="Better"] <- 3
fullStudy3$Condition[fullStudy3$Condition=="Same"] <- 2
fullStudy3$Condition[fullStudy3$Condition=="Worse"] <- 1

moreIngroup3  <- summarySEwithin(data=fullStudy3, idvar='Participant', measurevar = 'n_trials', withinvars = 'Samp_Group', na.rm = TRUE)
moreIngroup3plot <- ggplot(moreIngroup3, aes(x=Samp_Group, y=n_trials, fill=Samp_Group))+
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

moreIngroup3plot + theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40, face = "plain", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "plain",family = "Times New Roman"),
  axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("moreIngroup2plot.png", width = 4, height = 4, dpi = 700)

#####

#Visuals for evaluations
#####
##Plotting Point-estimates for both with main effects of Condition
fullStudy3Eval <- Eval3
fullStudy3Eval$Condition[fullStudy3Eval$Condition=="Better"] <- 3
fullStudy3Eval$Condition[fullStudy3Eval$Condition=="Same"] <- 2
fullStudy3Eval$Condition[fullStudy3Eval$Condition=="Worse"] <- 1


means_cond_Val3 <- summarySEwithin(data=fullStudy3Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Valence", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_Val3Plot <- ggplot(means_cond_Val3, aes(x =Valence, y=P.Estimates, fill=Evaluated.Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimates-se, ymax=P.Estimates+se),
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

means_cond_Val3Plot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 20,face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_Val3Plot.png")#save fill legend

means_cond_Cond3 <- summarySEwithin(data=fullStudy3Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Condition", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_Cond3Plot <- ggplot(means_cond_Cond3, aes(x =Condition, y=P.Estimates, fill=Evaluated.Group)) +
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

means_cond_Cond3Plot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 17, face = "plain",color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_Cond3Plot.png")#save fill legend
#####

cat('RESULTS FROM EXPERIMENT 2\n')

# Binomial test results and bar plot for the first sample
cat('Binomial test for first sample\n')
print(Exp.3.binom.test)
cat('Standard Error for first sample\n')
print(seFull)
print(firstSamplePlot3)

# Binomial test results and bar plot for first sample after positive
cat('Binomial test for first sample after positive\n')
print(Exp.3.binom.test.pos)
cat('Standard Error for first sample after positive\n')
print(sePos)
print(firstSamplePlotPos)

# Binomial test results and bar plot for first sample after negative
cat('Binomial test for first sample after negative\n')
print(Exp.3.binom.test.neg)
cat('Standard Error for first sample after negative\n')
print(seNeg)
print(firstSamplePlotNeg)

cat('Mixed effects model on sampling behavior\n')
cat('dispersion paramater for poisson\n')
print(Exp3.M1.disp)
print(Exp3.M1.effects)
print(Exp3.M1.coefficients)
print(moreIngroup3plot)

cat('Descriptive statistics for number of ingroup and outgroup samples\n')
print(Exp3.contrasts.samp)

cat('Mixed effects model on ratings\n')
print(Exp3.M2.effects)
print(Exp3.M2.coefficients)
print(means_cond_Val3Plot)
print(means_cond_Cond3Plot)

cat('Contrast and effect size for real group difference by evaluated group\n')
print(Exp3.ValCondContrast)
print(Exp3.ValCondContrast_d)

cat('Contrast and effect size for real group difference by evaluated group by valence\n')
print(Exp3.ValCondEvalContrast)
print(Exp3.ValCondEvalContrast_d)

cat('Contrast and effect size for real group difference by valence\n')
print(Exp3.CondEvalContrast)
print(Exp3.CondEvalContrast_d)

cat('Sampling Model with Pundit as Factor\n')
cat('No Difference\n')
print(Exp3.M3.effects)
