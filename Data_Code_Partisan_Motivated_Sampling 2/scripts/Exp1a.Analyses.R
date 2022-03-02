#Load Data
Experiment1aData <- read.csv("data/Experiment1aData.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Experiment1aData$Participant <- as.character(Experiment1aData$Participant) #make participant factor

##Creating long format data set for sampling behavior
#####
samp1 <- data.frame(rep(Experiment1aData$Participant,2), rep(Experiment1aData$Condition,2),
                    c(Experiment1aData$in_samples, Experiment1aData$out_samples), factor(rep(c(1,2), each=540), labels = c("In", "Out")),
                    rep(Experiment1aData$Val, 2), rep(Experiment1aData$Group, 2))

names(samp1) <- c("Participant", "Condition", "n_trials", "Samp_Group", "Valence", "Group")

#####Ceating a long format data set to look at evaluations
Evaluation.in.out <- data.frame(rep(Experiment1aData$Participant,2), rep(Experiment1aData$Condition,2),
                                c(Experiment1aData$In.Est, Experiment1aData$Out.Est), factor(rep(c(1,2), each=540), labels = c("In", "Out")),
                                rep(Experiment1aData$Val, 2), rep(Experiment1aData$Group, 2))

##Ranaming the variables 
names(Evaluation.in.out) <- c("Participant", "Condition",  "P.Estimates", "Evaluated.Group", "Valence", "Group")

##Making sampled group into character so that it can be effects coded
samp1$Samp_GroupString <- as.character(samp1$Samp_Group)
samp1$GroupString <- as.character(samp1$Group)
samp1$ValenceString <- as.character(samp1$Valence)

##Making sampled group into character so that it can be effects coded
Evaluation.in.out$Evaluated.GroupString <- as.character(Evaluation.in.out$Evaluated.Group)
Evaluation.in.out$GroupString <- as.character(Evaluation.in.out$Group)
Evaluation.in.out$ValenceString <- as.character(Evaluation.in.out$Valence)
#####

##Effects and Dummy coding categorical predictors for the sampling models
#####
#Change conditions to 1 2 3 for clarity
samp1$Condition[samp1$Condition == 1] <- "Worse"
samp1$Condition[samp1$Condition == 2] <- "Same"
samp1$Condition[samp1$Condition == 3] <- "Better"

#Effects + dummy coding, let's call it _c.
samp1$Condition_c[samp1$Condition == "Worse"] <- -1
samp1$Condition_c[samp1$Condition == "Same"] <- 0
samp1$Condition_c[samp1$Condition == "Better"] <- 1
samp1$Condition_c_eff <- factor(samp1$Condition_c)
samp1$Condition_c_dum <- factor(samp1$Condition,
                                levels = c("Worse", "Same", "Better"))

#effects + dummy coding In and Out group with out as thro-away (Samp_Group)
samp1$Samp_GroupB_eff <- samp1$Samp_GroupString
samp1$Samp_GroupB_dum <- samp1$Samp_GroupString
samp1$Samp_GroupB_eff <- as.factor(samp1$Samp_GroupB_eff)
samp1$Samp_GroupB_dum <- as.factor(samp1$Samp_GroupB_dum)
samp1$Samp_GroupB_eff <- factor(samp1$Samp_GroupB_eff, 
                                levels = c("In", "Out"))
samp1$Samp_GroupB_dum <- factor(samp1$Samp_GroupB_dum, 
                                levels = c("In", "Out"))

#effects + dummy coding group so that Rep is throw-away (Group)
samp1$Group_dum <- as.factor(samp1$Group)
samp1$Group_eff <- as.factor(samp1$Group)

#effects + dummy coding group so that Dem is reference (Group1)
samp1$Group_eff1 <- as.factor(samp1$Group)
samp1$Group_dum1 <- as.factor(samp1$Group)

#coding valence
samp1$Valence_eff <- samp1$ValenceString
samp1$Valence_dum <- samp1$ValenceString
samp1$Valence_eff <- as.factor(samp1$Valence_eff) 
samp1$Valence_dum <- as.factor(samp1$Valence_dum)

##Making the contrasts with dummy alternatives
contrasts(samp1$Condition_c_eff) <-contr.sum(3)
contrasts(samp1$Condition_c_dum) <-contr.sum(3)
contrasts(samp1$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(samp1$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(samp1$Condition_c_dum)) = c("Worse", "Same")

contrasts(samp1$Samp_GroupB_eff) <-contr.sum(2)
contrasts(samp1$Samp_GroupB_dum) <-contr.sum(2)
contrasts(samp1$Samp_GroupB_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(samp1$Samp_GroupB_eff)) = c("In.Group")
colnames(contrasts(samp1$Samp_GroupB_dum)) = c("In.Group")

#change the dummy code reference group to DEM 
contrasts(samp1$Group_dum) <-contr.treatment(2, base = 2)
contrasts(samp1$Group_eff) <- contr.sum(2)
colnames(contrasts(samp1$Group_eff)) = c("Dem")
colnames(contrasts(samp1$Group_dum)) = c("Dem")

#change the dummy code reference to REP
contrasts(samp1$Group_dum1) <-contr.treatment(2, base = 1)
colnames(contrasts(samp1$Group_dum1)) = c("Rep")

contrasts(samp1$Valence_eff) <-contr.sum(2)
contrasts(samp1$Valence_dum) <-contr.sum(2)
contrasts(samp1$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(samp1$Valence_eff)) = c("Neg")
colnames(contrasts(samp1$Valence_dum)) = c("Neg")

##Effects and Dummy coding categorical predictors for the point-estimate models. 
Evaluation.in.out$Condition[Evaluation.in.out$Condition == 1] <- "Worse"
Evaluation.in.out$Condition[Evaluation.in.out$Condition == 2] <- "Same"
Evaluation.in.out$Condition[Evaluation.in.out$Condition == 3] <- "Better"

Evaluation.in.out$Condition_c[Evaluation.in.out$Condition == "Worse"] <- -1
Evaluation.in.out$Condition_c[Evaluation.in.out$Condition == "Same"] <- 0
Evaluation.in.out$Condition_c[Evaluation.in.out$Condition == "Better"] <- 1
Evaluation.in.out$Condition_c_eff <- factor(Evaluation.in.out$Condition_c)
Evaluation.in.out$Condition_c_dum <- factor(Evaluation.in.out$Condition,
                                            levels = c("Worse", "Same", "Better"))

Evaluation.in.out$Evaluated.Group_eff <- Evaluation.in.out$Evaluated.GroupString
Evaluation.in.out$Evaluated.Group_dum <- Evaluation.in.out$Evaluated.GroupString
Evaluation.in.out$Evaluated.Group_eff <- as.factor(Evaluation.in.out$Evaluated.Group_eff)
Evaluation.in.out$Evaluated.Group_dum <- as.factor(Evaluation.in.out$Evaluated.Group_dum)
Evaluation.in.out$Evaluated.Group_eff <- factor(Evaluation.in.out$Evaluated.Group_eff, 
                                                levels = c("In", "Out"))
Evaluation.in.out$Evaluated.Group_dum <- factor(Evaluation.in.out$Evaluated.Group_dum, 
                                                levels = c("In", "Out"))


Evaluation.in.out$Group_eff <- as.factor(Evaluation.in.out$Group)
Evaluation.in.out$Group_dum <- as.factor(Evaluation.in.out$Group)

Evaluation.in.out$Valence_eff <- Evaluation.in.out$ValenceString
Evaluation.in.out$Valence_dum <- Evaluation.in.out$ValenceString

Evaluation.in.out$Valence_eff <- as.factor(Evaluation.in.out$Valence_eff) 
Evaluation.in.out$Valence_dum <- as.factor(Evaluation.in.out$Valence_dum) 

##Making the contrasts with dummy alternatives
contrasts(Evaluation.in.out$Condition_c_eff) <-contr.sum(3)
contrasts(Evaluation.in.out$Condition_c_dum) <-contr.sum(3)
contrasts(Evaluation.in.out$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(Evaluation.in.out$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(Evaluation.in.out$Condition_c_dum)) = c("Worse", "Same")

contrasts(Evaluation.in.out$Evaluated.Group_eff) <-contr.sum(2)
contrasts(Evaluation.in.out$Evaluated.Group_dum) <-contr.sum(2)
contrasts(Evaluation.in.out$Evaluated.Group_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Evaluation.in.out$Evaluated.Group_eff)) = c("In.Group")
colnames(contrasts(Evaluation.in.out$Evaluated.Group_dum)) = c("In.Group")

contrasts(Evaluation.in.out$Group_eff) <-contr.sum(2)
contrasts(Evaluation.in.out$Group_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Evaluation.in.out$Group_eff)) = c("Dem")
colnames(contrasts(Evaluation.in.out$Group_dum)) = c("Dem")

contrasts(Evaluation.in.out$Valence_eff) <-contr.sum(2)
contrasts(Evaluation.in.out$Valence_dum) <-contr.sum(2)
contrasts(Evaluation.in.out$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Evaluation.in.out$Valence_eff)) = c("Neg")
colnames(contrasts(Evaluation.in.out$Valence_dum)) = c("Neg")
#####

#first sample model
#####
Experiment1aData$First.Sample[Experiment1aData$First.Sample==-1] <- 0
Experiment1aData$First.Sample <- as.numeric(as.character(Experiment1aData$First.Sample))
Exp1.binom.test <- binom.test(length(Experiment1aData$First.Sample) - sum(Experiment1aData$First.Sample), length(Experiment1aData$First.Sample), p =.5)
#####

#Sampling Models
#####
Exp1.M1.Baseline <- glmer(n_trials~1+  (1|Participant), data = samp1, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp1.disp <- dispersion_glmer(Exp1.M1.Baseline) # no evidence for overdispersion as it is not > 1.4
#qqnorm(resid(Exp1.M1.Baseline), main="normal qq-plot, residuals")
Exp1.M1.SampG <- update(Exp1.M1.Baseline, .~. + Samp_GroupB_dum)
Exp1.M1.Valence <- update(Exp1.M1.SampG, .~. + Valence_eff)
Exp1.M1.Cond <- update(Exp1.M1.Valence, .~. + Condition_c_eff)
Exp1.M1.PolAff <- update(Exp1.M1.Cond, .~. + Group_eff)
Exp1.M1.SampGbyVal <- update(Exp1.M1.PolAff, .~. + Samp_GroupB_dum:Valence_eff)
Exp1.M1.SampGbyCond <- update(Exp1.M1.SampGbyVal, .~. + Samp_GroupB_dum:Condition_c_eff)
Exp1.M1.CondbyVal <- update(Exp1.M1.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp1.M1.SampGbyCondbyVal <- update(Exp1.M1.CondbyVal, .~. + Samp_GroupB_dum:Condition_c_eff:Valence_eff)
Exp1.M1.Full <- update(Exp1.M1.SampGbyCondbyVal, .~. + Samp_GroupB_dum*Valence_eff*Condition_c_eff*Group_eff)
#attr(getME(Exp1.M1.Full,"X"),"col.dropped") #for rank deficient warning. Not an issue for estimation, see e.g., https://stackoverflow.com/questions/37090722/lme4lmer-reports-fixed-effect-model-matrix-is-rank-deficient-do-i-need-a-fi
Exp1.effects <- anova(Exp1.M1.Baseline,Exp1.M1.SampG, Exp1.M1.Valence, Exp1.M1.Cond, Exp1.M1.PolAff, Exp1.M1.SampGbyVal, Exp1.M1.SampGbyCond, Exp1.M1.CondbyVal, Exp1.M1.SampGbyCondbyVal, Exp1.M1.Full)
Exp1.coefficients <- summary(Exp1.M1.Full)
#model saved for power simulations
#Study1Power <- glmer(n_trials~Samp_GroupB_dum*Valence_eff*Condition_c_eff+Group_eff+  (1|Participant), data = samp1, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
#save(Study1Power, file = "study1model.rda")
#####

#Eval Models 
#####
Exp1.M2.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Evaluation.in.out, control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4))) #stopping singular warning due to low random intercepts
Exp1.M2.Affil <- update(Exp1.M2.Baseline, .~. + Group_eff)
Exp1.M2.SampG <- update(Exp1.M2.Affil, .~. + Evaluated.Group_eff)
Exp1.M2.Valence <- update(Exp1.M2.SampG, .~. + Valence_eff)
Exp1.M2.Cond <- update(Exp1.M2.Valence, .~. + Condition_c_eff)
Exp1.M2.PolAff <- update(Exp1.M2.Cond, .~. + Group_eff)
Exp1.M2.SampGbyVal <- update(Exp1.M2.PolAff, .~. + Evaluated.Group_eff:Valence_eff)
Exp1.M2.SampGbyCond <- update(Exp1.M2.SampGbyVal, .~. + Evaluated.Group_eff:Condition_c_eff)
Exp1.M2.CondbyVal <- update(Exp1.M2.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp1.M2.SampGbyCondbyVal <- update(Exp1.M2.CondbyVal, .~. + Evaluated.Group_eff:Condition_c_eff:Valence_eff)
Exp1.M2.Full <- update(Exp1.M2.SampGbyCondbyVal, .~. + Evaluated.Group_eff*Valence_eff*Condition_c_eff*Group_eff)
#attr(getME(Exp1.M2.Full,"X"),"col.dropped") #for rank deficient warning. Not an issue for estimation

Exp1.M2.effects <- anova(Exp1.M2.Baseline,Exp1.M2.SampG, Exp1.M2.Valence, Exp1.M2.Cond, Exp1.M2.PolAff, Exp1.M2.SampGbyVal, Exp1.M2.SampGbyCond,
      Exp1.M2.CondbyVal,Exp1.M2.SampGbyCondbyVal, Exp1.M2.Full)
Exp1.M2.coef <- summary(Exp1.M2.Full)

#Sampling 

#evaluations
emm <- emmeans(Exp1.M2.SampGbyVal, ~ Valence_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm, simple = "Evaluated.Group_eff")
eff_size(emm, sigma = sigma(Exp1.M2.SampGbyVal), edf = df.residual(Exp1.M2.SampGbyVal))

emm <- emmeans(Exp1.M2.SampGbyCond, ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
contrast(emm, simple = "Evaluated.Group_eff")
eff_size(emm, sigma = sigma(Exp1.M2.SampGbyCond), edf = df.residual(Exp1.M2.SampGbyCond))


Exp1.Contrast.better <- t.test(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Better"), paired = F)
Exp1.Contrast.better.d <- cohen.d(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Better"))

Exp1.Contrast.worse <- t.test(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Worse"), paired = F)
Exp1.Contrast.worse.s <- cohen.d(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Worse"))

Exp1.Contrast.same <- t.test(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Same"), paired = F)
Exp1.Contrast.same.d <- cohen.d(P.Estimates~Evaluated.Group, data = subset(Evaluation.in.out, subset=Evaluation.in.out$Condition=="Same"))
#####

#if anyone is interested in the singularity warning, run next 4 lines of code. 
#####
dd <- update(Exp1.M2.Baseline,devFunOnly=TRUE) #random intercepts for participants crashed to 0, basically means low variance and that we could probably drop random effect. However, it is of theoretical interest to keep
params <- getME(Exp1.M2.Baseline,"theta")
grad(dd,params) #value is small, <.003
eigen(solve(hessian(dd,params)),only.values=TRUE)$values #The warning in this case is not an issue, see e.g., https://stackoverflow.com/questions/20743453/obscure-warning-lme4-using-lmer-in-optwrap
#####

#Descriptives and simple contrasts
#####
#descriptives for samples
Exp1.contrasts.samp <- describeBy(samp1$n_trials, samp1$Samp_Group)

#means for evals
Exp1.contrasts.EvalbyGroup <- describeBy(Evaluation.in.out$P.Estimates, Evaluation.in.out$Evaluated.Group)
Exp1.contrasts.EvalbyValence <- describeBy(Evaluation.in.out$P.Estimates, Evaluation.in.out$Valence)

#Contrasts

#Main effect of Eval Group
evalContrast <- emmeans(Exp1.M2.SampG, specs = pairwise ~ Evaluated.Group_eff, type = "response")

#Main effect of Valence
valContrast <- emmeans(Exp1.M2.Valence , specs = pairwise ~ Valence_eff, type = "response")

#interaction of Eval by Valence
emm <- emmeans(Exp1.M2.SampGbyVal, ~ Evaluated.Group_eff:Valence_eff, type = "response", lmer.df = "satterthwaite")
Exp1.EvalValContrast <- contrast(emm, "eff", by = "Evaluated.Group_eff")        
Exp1.EvalValContrast_d <- eff_size(emm, sigma = sigma(Exp1.M2.SampGbyVal), edf = df.residual(Exp1.M2.SampGbyVal))

#interaction of Condition by Eval group
emm <- emmeans(Exp1.M2.SampGbyCond, ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp1.evalCondContrast <- contrast(emm, "eff", by = "Condition_c_eff")          
Exp1.evalCondContrast_d <- eff_size(emm, sigma = sigma(Exp1.M2.SampGbyCond), edf = df.residual(Exp1.M2.SampGbyCond))


#Visual for first sample 

#visual for first sample
#####
Exp1.group <- c("in","out")
Exp1.mean <- c(71, 29)
Exp1.firstSample2 <- data.frame(Exp1.group, Exp1.mean)

firstSamplePlot2 <- ggplot(Exp1.firstSample2, aes(x=Exp1.group, y=Exp1.mean, fill = Exp1.group)) +
  geom_hline(yintercept = 107, linetype="solid") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  coord_cartesian(ylim = c(0, 100))+scale_fill_manual(name="group", values=c("in" = "grey","out"= "white"),
                                                      labels=c("", ""))+theme_bw()+  
  theme(legend.justification=c(.05,.98),
        legend.position=c(.05,.98), panel.grid.major.x = element_blank())  + theme(legend.position = "none")+ ylab("")+ xlab("")+
  scale_x_discrete(labels=c('Ingroup', 'Outgroup'))+ theme(
    axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))+ scale_y_continuous(limits = c(0, 100), labels = c("0%", "25%", "50%", "75%", "100%"))
#ggsave("firstSamplePlot1a.png", width = 4, height = 4, dpi = 700)
#####

#Visual for more ingroup samples
#####
###**Plotting Sampling behavior** Main effects 
fullStudy1 <- samp1
fullStudy1$Condition[fullStudy1$Condition=="Better"] <- 3
fullStudy1$Condition[fullStudy1$Condition=="Same"] <- 2
fullStudy1$Condition[fullStudy1$Condition=="Worse"] <- 1

moreIngroup  <- summarySEwithin(data=fullStudy1, idvar='Participant', measurevar = 'n_trials', withinvars = 'Samp_Group', na.rm = TRUE)
moreIngroupPlot <- ggplot(moreIngroup, aes(x=Samp_Group, y=n_trials, fill=Samp_Group))+
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

moreIngroupPlot + theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40, face = "plain", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "plain",family = "Times New Roman"),
  axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("moreIngroup1aplot.png", width = 4, height = 4, dpi = 700)
#####

#Visuals for evals
#####
##**Plotting Point-estimates for both with main effects of Condition**
fullStudy1Eval <- Evaluation.in.out
fullStudy1Eval$Condition[fullStudy1Eval$Condition=="Better"] <- 3
fullStudy1Eval$Condition[fullStudy1Eval$Condition=="Same"] <- 2
fullStudy1Eval$Condition[fullStudy1Eval$Condition=="Worse"] <- 1

means_cond_both1Val <- summarySEwithin(data=fullStudy1Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Valence", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_both_graphVal <- ggplot(means_cond_both1Val, aes(x =Valence, y=P.Estimates, fill=Evaluated.Group)) +
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

means_cond_both_graphVal+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 20,face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_both_graphVal.png")#save fill legend

means_cond_both1Cond <- summarySEwithin(data=fullStudy1Eval, idvar='Participant', measurevar = 'P.Estimates', betweenvars =  "Condition", withinvars = 'Evaluated.Group', na.rm = TRUE)
means_cond_both_graphCond <- ggplot(means_cond_both1Cond, aes(x =Condition, y=P.Estimates, fill=Evaluated.Group)) +
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

means_cond_both_graphCond+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 17, face = "plain",color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("means_cond_both_graphCond.png")#save fill legend
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FROM EXPERIMENT 1a')
print('Binomial test for first sample')
print(Exp1.binom.test)
print(firstSamplePlot2)

print('Mixed effects model on sampling behavior')
print('dispersion paramater for poisson')
print(Exp1.disp)
print(Exp1.effects)
print(Exp1.coefficients)
print(moreIngroupPlot)

print('Descriptive statistics for number of ingroup and outgroup samples')
print(Exp1.contrasts.samp)

print('Mixed effects model on ratings')
print(Exp1.M2.effects)
print(Exp1.M2.coef)
print(means_cond_both_graphVal)
print(means_cond_both_graphCond)

print('Contrast and effect size for evaluations by valence interaction')
print(Exp1.EvalValContrast)
print(Exp1.EvalValContrast_d)
print('Contrast and effect size for evaluated group by real group difference')
print(Exp1.evalCondContrast)
print(Exp1.evalCondContrast_d)
