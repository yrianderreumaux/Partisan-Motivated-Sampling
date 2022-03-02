#Load Data
Experiment1bData <- read.csv("data/Experiment1bData.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Experiment1bData$Participant <- factor(Experiment1bData$Participant) #make participant factor

##Creating long format data set for sampling behavior for full 
#####
Samp2 <- data.frame(rep(Experiment1bData$Participant,2), rep(Experiment1bData$Condition,2),
                    c(Experiment1bData$in_samples, Experiment1bData$out_samples), factor(rep(c(1,2), each=905), labels = c("In", "Out")),
                    rep(Experiment1bData$Val, 2), rep(Experiment1bData$Group, 2), rep(Experiment1bData$firstSample, 2))

names(Samp2) <- c("Participant", "Condition", "n_trials", "Samp_Group", "Valence", "Group", "firstSample")

##Making sampled group into character so that it can be effects coded
Samp2$Samp_GroupString <- as.character(Samp2$Samp_Group)
Samp2$GroupString <- as.character(Samp2$Group)
Samp2$ValenceString <- as.character(Samp2$Valence)

#####Creating a long format data set to look at Point-Estimates (DV) 
Eval2 <- data.frame(rep(Experiment1bData$Participant,2), rep(Experiment1bData$Condition,2),
                    c(Experiment1bData$In.Est, Experiment1bData$Out.Est), factor(rep(c(1,2), each=905), labels = c("In", "Out")),
                    rep(Experiment1bData$Val, 2), rep(Experiment1bData$Group, 2))

##Ranaming the variables 
names(Eval2) <- c("Participant", "Condition",  "P.Estimates", "Evaluated.Group", "Valence", "Group")

##Making sampled group into character so that it can be effects coded
Eval2$Evaluated.GroupString <- as.character(Eval2$Evaluated.Group)
#Eval2$diff_sourceString <- as.character(Eval2$diff_source)
Eval2$GroupString <- as.character(Eval2$Group)
Eval2$ValenceString <- as.character(Eval2$Valence)
#####

#Below is all the effects coding
######
#Change conditions to 1 2 3 for clarity
Samp2$Condition[Samp2$Condition == 1] <- "Worse"
Samp2$Condition[Samp2$Condition == 2] <- "Same"
Samp2$Condition[Samp2$Condition == 3] <- "Better"

#Effects + dummy coding, let's call it _c.
Samp2$Condition_c[Samp2$Condition == "Worse"] <- -1
Samp2$Condition_c[Samp2$Condition == "Same"] <- 0
Samp2$Condition_c[Samp2$Condition == "Better"] <- 1
Samp2$Condition_c_eff <- factor(Samp2$Condition_c)
Samp2$Condition_c_dum <- factor(Samp2$Condition,
                                levels = c("Worse", "Same", "Better"))

#effects + dummy coding In and Out group with out as thro-away (Samp_Group)
Samp2$Samp_GroupB_eff <- Samp2$Samp_GroupString
Samp2$Samp_GroupB_dum <- Samp2$Samp_GroupString
Samp2$Samp_GroupB_eff <- as.factor(Samp2$Samp_GroupB_eff)
Samp2$Samp_GroupB_dum <- as.factor(Samp2$Samp_GroupB_dum)
Samp2$Samp_GroupB_eff <- factor(Samp2$Samp_GroupB_eff, 
                                levels = c("In", "Out"))
Samp2$Samp_GroupB_dum <- factor(Samp2$Samp_GroupB_dum, 
                                levels = c("In", "Out"))

#effects + dummy coding group so that Rep is throw-away (Group)
Samp2$Group_dum <- as.factor(Samp2$Group)
Samp2$Group_eff <- as.factor(Samp2$Group)

#effects + dummy coding group so that Dem is reference (Group1)
Samp2$Group_eff1 <- as.factor(Samp2$Group)
Samp2$Group_dum1 <- as.factor(Samp2$Group)

#coding valence
Samp2$Valence_eff <- Samp2$ValenceString
Samp2$Valence_dum <- Samp2$ValenceString
Samp2$Valence_eff <- as.factor(Samp2$Valence_eff) 
Samp2$Valence_dum <- as.factor(Samp2$Valence_dum)

##Making the contrasts with dummy alternatives
contrasts(Samp2$Condition_c_eff) <-contr.sum(3)
contrasts(Samp2$Condition_c_dum) <-contr.sum(3)
contrasts(Samp2$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(Samp2$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(Samp2$Condition_c_dum)) = c("Worse", "Same")

contrasts(Samp2$Samp_GroupB_eff) <-contr.sum(2)
contrasts(Samp2$Samp_GroupB_dum) <-contr.sum(2)
contrasts(Samp2$Samp_GroupB_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Samp2$Samp_GroupB_eff)) = c("In.Group")
colnames(contrasts(Samp2$Samp_GroupB_dum)) = c("In.Group")

#change the dummy code reference group to DEM 
contrasts(Samp2$Group_dum) <-contr.treatment(2, base = 2)
contrasts(Samp2$Group_eff) <- contr.sum(2)
colnames(contrasts(Samp2$Group_eff)) = c("Dem")
colnames(contrasts(Samp2$Group_dum)) = c("Dem")

#change the dummy code reference to REP
contrasts(Samp2$Group_dum1) <-contr.treatment(2, base = 1)
colnames(contrasts(Samp2$Group_dum1)) = c("Rep")

contrasts(Samp2$Valence_eff) <-contr.sum(2)
contrasts(Samp2$Valence_dum) <-contr.sum(2)
contrasts(Samp2$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Samp2$Valence_eff)) = c("Neg")
colnames(contrasts(Samp2$Valence_dum)) = c("Neg")

#Change conditions to 1 2 3 for clarity
Eval2$Condition[Eval2$Condition == 1] <- "Worse"
Eval2$Condition[Eval2$Condition == 2] <- "Same"
Eval2$Condition[Eval2$Condition == 3] <- "Better"

Eval2$Condition_c[Eval2$Condition == "Worse"] <- -1
Eval2$Condition_c[Eval2$Condition == "Same"] <- 0
Eval2$Condition_c[Eval2$Condition == "Better"] <- 1
Eval2$Condition_c_eff <- factor(Eval2$Condition_c)
Eval2$Condition_c_dum <- factor(Eval2$Condition,
                                levels = c("Worse", "Same", "Better"))

Eval2$Evaluated.Group_eff <- Eval2$Evaluated.GroupString
Eval2$Evaluated.Group_dum <- Eval2$Evaluated.GroupString
Eval2$Evaluated.Group_eff <- as.factor(Eval2$Evaluated.Group_eff)
Eval2$Evaluated.Group_dum <- as.factor(Eval2$Evaluated.Group_dum)
Eval2$Evaluated.Group_eff <- factor(Eval2$Evaluated.Group_eff, 
                                    levels = c("In", "Out"))
Eval2$Evaluated.Group_dum <- factor(Eval2$Evaluated.Group_dum, 
                                    levels = c("In", "Out"))

Eval2$Group_eff <- as.factor(Eval2$Group)
Eval2$Group_dum <- as.factor(Eval2$Group)

Eval2$Valence_eff <- Eval2$ValenceString
Eval2$Valence_dum <- Eval2$ValenceString

Eval2$Valence_eff <- as.factor(Eval2$Valence_eff) 
Eval2$Valence_dum <- as.factor(Eval2$Valence_dum) 

##Making the contrasts with dummy alternatives
contrasts(Eval2$Condition_c_eff) <-contr.sum(3)
contrasts(Eval2$Condition_c_dum) <-contr.sum(3)
contrasts(Eval2$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(Eval2$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(Eval2$Condition_c_dum)) = c("Worse", "Same")

contrasts(Eval2$Evaluated.Group_eff) <-contr.sum(2)
contrasts(Eval2$Evaluated.Group_dum) <-contr.sum(2)
contrasts(Eval2$Evaluated.Group_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Eval2$Evaluated.Group_eff)) = c("In.Group")
contrasts(Eval2$Valence_eff) <-contr.sum(2)
contrasts(Eval2$Valence_dum) <-contr.sum(2)
contrasts(Eval2$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Eval2$Valence_eff)) = c("Neg")
colnames(contrasts(Eval2$Valence_dum)) = c("Neg") 
#######

#first sample model
#####
Experiment1bData$firstSample[Experiment1bData$firstSample==-1] <- 0
Experiment1bData$firstSample <- as.numeric(as.character(Experiment1bData$firstSample))
Exp2.binom.test <- binom.test(length(Experiment1bData$firstSample) - sum(Experiment1bData$firstSample), length(Experiment1bData$firstSample), p =.5)
#####

#Sampling Models
#####
Exp2.M1.Baseline <- glmer(n_trials~1+  (1|Participant), data = Samp2, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp2.disp <- dispersion_glmer(Exp2.M1.Baseline) # no evidence for overdispersion as it is not > 1.4

Exp2.M1.SampG <- update(Exp2.M1.Baseline, .~. + Samp_GroupB_dum)
Exp2.M1.Valence <- update(Exp2.M1.SampG, .~. + Valence_eff)
Exp2.M1.Cond <- update(Exp2.M1.Valence, .~. + Condition_c_eff)
Exp2.M1.PolAff <- update(Exp2.M1.Cond, .~. + Group_eff)
Exp2.M1.SampGbyVal <- update(Exp2.M1.PolAff, .~. + Samp_GroupB_dum:Valence_eff)
Exp2.M1.SampGbyCond <- update(Exp2.M1.SampGbyVal, .~. + Samp_GroupB_dum:Condition_c_eff)
Exp2.M1.CondbyVal <- update(Exp2.M1.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp2.M1.SampGbyCondbyVal <- update(Exp2.M1.CondbyVal, .~. + Samp_GroupB_dum:Condition_c_eff:Valence_eff)
Exp2.M1.Full <- update(Exp2.M1.SampGbyCondbyVal, .~. + Samp_GroupB_dum*Valence_eff*Condition_c_eff*Group_eff)

Exp2.effects <- anova(Exp2.M1.Baseline,Exp2.M1.SampG, Exp2.M1.Valence, Exp2.M1.Cond, Exp2.M1.PolAff, Exp2.M1.SampGbyVal, Exp2.M1.SampGbyCond, Exp2.M1.CondbyVal, Exp2.M1.SampGbyCondbyVal, Exp2.M1.Full)
Exp2.coefficients <- summary(Exp2.M1.Full)
#####

#Eval Models
#####
Exp2.effects <- anova(Exp2.M1.Baseline,Exp2.M1.SampG, Exp2.M1.Valence, Exp2.M1.Cond, Exp2.M1.PolAff, Exp2.M1.SampGbyVal, Exp2.M1.SampGbyCond, Exp2.M1.CondbyVal, Exp2.M1.SampGbyCondbyVal, Exp2.M1.Full)
Exp2.coefficients <- summary(Exp2.M1.Full)
#qqnorm(resid(Exp2.M1.Baseline), main="normal qq-plot, residuals")

#eval Models for 
Exp2.M2.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Eval2)
Exp2.M2.Affil <- update(Exp2.M2.Baseline, .~. + Group_eff)
Exp2.M2.SampG <- update(Exp2.M2.Baseline, .~. + Evaluated.Group_eff)
Exp2.M2.Valence <- update(Exp2.M2.SampG, .~. + Valence_eff)
Exp2.M2.Cond <- update(Exp2.M2.Valence, .~. + Condition_c_eff)
Exp2.M2.PolAff <- update(Exp2.M2.Cond, .~. + Group_eff)
Exp2.M2.SampGbyVal <- update(Exp2.M2.PolAff, .~. + Evaluated.Group_eff:Valence_eff)
Exp2.M2.SampGbyCond <- update(Exp2.M2.SampGbyVal, .~. + Evaluated.Group_eff:Condition_c_eff)
Exp2.M2.CondbyVal <- update(Exp2.M2.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp2.M2.SampGbyCondbyVal <- update(Exp2.M2.CondbyVal, .~. + Evaluated.Group_eff:Condition_c_eff:Valence_eff)
Exp2.M2.Full <- suppressMessages(update(Exp2.M2.SampGbyCondbyVal, .~. + Evaluated.Group_eff:Valence_eff:Condition_c_eff:Group_eff))

Exp2.M2.effects <- anova(Exp2.M2.Baseline,Exp2.M2.SampG, Exp2.M2.Valence, Exp2.M2.Cond, Exp2.M2.PolAff, Exp2.M2.SampGbyVal, Exp2.M2.SampGbyCond,
      Exp2.M2.CondbyVal,Exp2.M2.SampGbyCondbyVal, Exp2.M2.Full )
Exp2.M2.coefficients <- summary(Exp2.M2.Full)

#Contrasts

#simple contrasts
#####
emm <- emmeans(Exp2.M2.SampGbyVal, ~ Valence_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp2.ValContrast <- contrast(emm, simple = "Evaluated.Group_eff")
Exp2.ValContrast_d <- eff_size(emm, sigma = sigma(Exp2.M2.SampGbyVal), edf = df.residual(Exp2.M2.SampGbyVal))

emm <- emmeans(Exp2.M2.SampGbyCond, ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp2.CondGroupContrast <- contrast(emm, simple = "Evaluated.Group_eff")
Exp2.CondGroupContrast_d <- eff_size(emm, sigma = sigma(Exp2.M2.SampGbyCond), edf = df.residual(Exp2.M2.SampGbyCond))
#####

#descriptives for samples
#####
Exp2.contrasts.samp <- describeBy(Samp2$n_trials, Samp2$Samp_Group)
#####

#Descriptives for evaluations
#####
Exp2.contrasts.EvalbyGroup <- describeBy(Eval2$P.Estimates, Eval2$Evaluated.Group)
Exp2.contrasts.EvalbyValence <- describeBy(Eval2$P.Estimates, Eval2$Valence)


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
##**Plotting Point-estimates for both with main effects of Condition**
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
print('RESULTS FROM EXPERIMENT 1')
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
