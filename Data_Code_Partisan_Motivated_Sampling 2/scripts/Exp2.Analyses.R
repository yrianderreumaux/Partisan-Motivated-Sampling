#Load Data
Experiment2Data <- read.csv("data/Experiment2Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Experiment2Data$Participant <- factor(Experiment2Data$Participant) #make participant factor

##Creating long format data set for sampling behavior for full 
#####
Samp3 <- data.frame(rep(Experiment2Data$Participant,2), rep(Experiment2Data$Condition,2),
                    c(Experiment2Data$in_samples, Experiment2Data$out_samples), factor(rep(c(1,2), each=986), labels = c("In", "Out")),
                    rep(Experiment2Data$Val, 2), rep(Experiment2Data$group, 2), rep(Experiment2Data$imp, 2))

names(Samp3) <- c("Participant", "Condition", "n_trials", "Samp_Group", "Valence", "Group", "PunditImp")
##Making sampled group into character so that it can be effects coded
Samp3$Samp_GroupString <- as.character(Samp3$Samp_Group)
Samp3$GroupString <- as.character(Samp3$Group)
Samp3$ValenceString <- as.character(Samp3$Valence)

#####Ceating a long format data set to look at Point-Estimates (DV) from the master data (both rep and dem)
Eval3 <- data.frame(rep(Experiment2Data$Participant,2), rep(Experiment2Data$Condition,2),
                    c(Experiment2Data$In.Est, Experiment2Data$Out.Est), factor(rep(c(1,2), each=986), labels = c("In", "Out")),
                    rep(Experiment2Data$Val, 2), rep(Experiment2Data$group, 2), rep(Experiment2Data$imp, 2))

##Ranaming the variables 
names(Eval3) <- c("Participant", "Condition",  "P.Estimates", "Evaluated.Group", "Valence", "Group", "PunditImp")

##Making sampled group into character so that it can be effects coded
Eval3$Evaluated.GroupString <- as.character(Eval3$Evaluated.Group)
#Eval3$diff_sourceString <- as.character(Eval3$diff_source)
Eval3$GroupString <- as.character(Eval3$Group)
Eval3$ValenceString <- as.character(Eval3$Valence)
#####

#Effects coding
#####
#Sampling
#Change conditions to 1 2 3 for clarity
Samp3$Condition[Samp3$Condition == 1] <- "Worse"
Samp3$Condition[Samp3$Condition == 2] <- "Same"
Samp3$Condition[Samp3$Condition == 3] <- "Better"

#Effects + dummy coding, let's call it _c.
Samp3$Condition_c[Samp3$Condition == "Worse"] <- -1
Samp3$Condition_c[Samp3$Condition == "Same"] <- 0
Samp3$Condition_c[Samp3$Condition == "Better"] <- 1
Samp3$Condition_c_eff <- factor(Samp3$Condition_c)
Samp3$Condition_c_dum <- factor(Samp3$Condition,
                                levels = c("Worse", "Same", "Better"))

#effects + dummy coding In and Out group with out as thro-away (Samp_Group)
Samp3$Samp_GroupB_eff <- Samp3$Samp_GroupString
Samp3$Samp_GroupB_dum <- Samp3$Samp_GroupString
Samp3$Samp_GroupB_eff <- as.factor(Samp3$Samp_GroupB_eff)
Samp3$Samp_GroupB_dum <- as.factor(Samp3$Samp_GroupB_dum)
Samp3$Samp_GroupB_eff <- factor(Samp3$Samp_GroupB_eff, 
                                levels = c("In", "Out"))
Samp3$Samp_GroupB_dum <- factor(Samp3$Samp_GroupB_dum, 
                                levels = c("In", "Out"))

#effects + dummy coding group so that Rep is throw-away (Group)
Samp3$Group_dum <- as.factor(Samp3$Group)
Samp3$Group_eff <- as.factor(Samp3$Group)

#effects + dummy coding group so that Dem is reference (Group1)
Samp3$Group_eff1 <- as.factor(Samp3$Group)
Samp3$Group_dum1 <- as.factor(Samp3$Group)

#coding valence
Samp3$Valence_eff <- Samp3$ValenceString
Samp3$Valence_dum <- Samp3$ValenceString
Samp3$Valence_eff <- as.factor(Samp3$Valence_eff) 
Samp3$Valence_dum <- as.factor(Samp3$Valence_dum)

#pundit impression
Samp3$PunditImp_eff <- as.factor(Samp3$PunditImp)

##Making the contrasts with dummy alternatives
contrasts(Samp3$Condition_c_eff) <-contr.sum(3)
contrasts(Samp3$Condition_c_dum) <-contr.sum(3)
contrasts(Samp3$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(Samp3$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(Samp3$Condition_c_dum)) = c("Worse", "Same")

contrasts(Samp3$Samp_GroupB_eff) <-contr.sum(2)
contrasts(Samp3$Samp_GroupB_dum) <-contr.sum(2)
contrasts(Samp3$Samp_GroupB_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Samp3$Samp_GroupB_eff)) = c("In.Group")
colnames(contrasts(Samp3$Samp_GroupB_dum)) = c("In.Group")

#change the dummy code reference group to DEM 
contrasts(Samp3$Group_dum) <-contr.treatment(2, base = 2)
contrasts(Samp3$Group_eff) <- contr.sum(2)
colnames(contrasts(Samp3$Group_eff)) = c("Dem")
colnames(contrasts(Samp3$Group_dum)) = c("Dem")

#change the dummy code reference to REP
contrasts(Samp3$Group_dum1) <-contr.treatment(2, base = 1)
colnames(contrasts(Samp3$Group_dum1)) = c("Rep")

contrasts(Samp3$Valence_eff) <-contr.sum(2)
contrasts(Samp3$Valence_dum) <-contr.sum(2)
contrasts(Samp3$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Samp3$Valence_eff)) = c("Neg")
colnames(contrasts(Samp3$Valence_dum)) = c("Neg")

#Pundit effects
contrasts(Samp3$PunditImp_eff) <-contr.sum(4)
#Coding looks like this:
#             [,1] [,2] [,3]
# negIngroup     1    0    0
# negOutgroup    0    1    0
# posIngroup     0    0    1
# posOutgroup   -1   -1   -1
colnames(contrasts(Samp3$PunditImp_eff)) = c("negIn", "negOut", "posIng")

#Evaluations
#Change conditions to 1 2 3 for clarity
Eval3$Condition[Eval3$Condition == 1] <- "Worse"
Eval3$Condition[Eval3$Condition == 2] <- "Same"
Eval3$Condition[Eval3$Condition == 3] <- "Better"

Eval3$Condition_c[Eval3$Condition == "Worse"] <- -1
Eval3$Condition_c[Eval3$Condition == "Same"] <- 0
Eval3$Condition_c[Eval3$Condition == "Better"] <- 1
Eval3$Condition_c_eff <- factor(Eval3$Condition_c)
Eval3$Condition_c_dum <- factor(Eval3$Condition,
                                levels = c("Worse", "Same", "Better"))

Eval3$Evaluated.Group_eff <- Eval3$Evaluated.GroupString
Eval3$Evaluated.Group_dum <- Eval3$Evaluated.GroupString
Eval3$Evaluated.Group_eff <- as.factor(Eval3$Evaluated.Group_eff)
Eval3$Evaluated.Group_dum <- as.factor(Eval3$Evaluated.Group_dum)
Eval3$Evaluated.Group_eff <- factor(Eval3$Evaluated.Group_eff, 
                                    levels = c("In", "Out"))
Eval3$Evaluated.Group_dum <- factor(Eval3$Evaluated.Group_dum, 
                                    levels = c("In", "Out"))

Eval3$Group_eff <- as.factor(Eval3$Group)
Eval3$Group_dum <- as.factor(Eval3$Group)

Eval3$Valence_eff <- Eval3$ValenceString
Eval3$Valence_dum <- Eval3$ValenceString

Eval3$Valence_eff <- as.factor(Eval3$Valence_eff) 
Eval3$Valence_dum <- as.factor(Eval3$Valence_dum) 

#pundit impression
Eval3$PunditImp_eff <- as.factor(Eval3$PunditImp)

##Making the contrasts with dummy alternatives
contrasts(Eval3$Condition_c_eff) <-contr.sum(3)
contrasts(Eval3$Condition_c_dum) <-contr.sum(3)
contrasts(Eval3$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(Eval3$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(Eval3$Condition_c_dum)) = c("Worse", "Same")

contrasts(Eval3$Evaluated.Group_eff) <-contr.sum(2)
contrasts(Eval3$Evaluated.Group_dum) <-contr.sum(2)
contrasts(Eval3$Evaluated.Group_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Eval3$Evaluated.Group_eff)) = c("In.Group")
colnames(contrasts(Eval3$Evaluated.Group_dum)) = c("In.Group")

contrasts(Eval3$Group_eff) <-contr.sum(2)
contrasts(Eval3$Group_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Eval3$Group_eff)) = c("Dem")
colnames(contrasts(Eval3$Group_dum)) = c("Dem")

contrasts(Eval3$Valence_eff) <-contr.sum(2)
contrasts(Eval3$Valence_dum) <-contr.sum(2)
contrasts(Eval3$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(Eval3$Valence_eff)) = c("Neg")
colnames(contrasts(Eval3$Valence_dum)) = c("Neg")

#Pundit effects
contrasts(Eval3$PunditImp_eff) <-contr.sum(4)
#Coding looks like this:
#             [,1] [,2] [,3]
# negIngroup     1    0    0
# negOutgroup    0    1    0
# posIngroup     0    0    1
# posOutgroup   -1   -1   -1
colnames(contrasts(Eval3$PunditImp_eff)) = c("negIn", "negOut", "posIng")
#####

#first sample models
#####
Experiment2Data$firstSample[Experiment2Data$firstSample==-1] <- 0
Experiment2Data$firstSample <- as.numeric(as.character(Experiment2Data$firstSample))
Exp.3.binom.test <- binom.test(length(Experiment2Data$firstSample) - sum(Experiment2Data$firstSample), length(Experiment2Data$firstSample), p =.5)
#no differences between dem and rep

#when positive
study3POS <- Experiment2Data[which(Experiment2Data$Val=="pos"),]
Exp.3.binom.test.pos <- binom.test(length(study3POS$firstSample) - sum(study3POS$firstSample), length(study3POS$firstSample), p =.5)

#when negative
study3NEG <- Experiment2Data[which(Experiment2Data$Val=="neg"),]
Exp.3.binom.test.neg <- binom.test(length(study3NEG$firstSample) - sum(study3NEG$firstSample), length(study3NEG$firstSample), p =.5)
#####

#Sampling Models without source as seen in manuscript
#####
Exp3.M1.Baseline <- glmer(n_trials~1+  (1|Participant), data = Samp3, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp3.M1.disp <- dispersion_glmer(Exp3.M1.Baseline) # no evidence for overdispersion as it is not > 1.4
Exp3.M1.SampG <- update(Exp3.M1.Baseline, .~. + Samp_GroupB_dum)
Exp3.M1.Valence <- update(Exp3.M1.SampG, .~. + Valence_eff)
Exp3.M1.Cond <- update(Exp3.M1.Valence, .~. + Condition_c_eff)
Exp3.M1.PolAff <- update(Exp3.M1.Cond, .~. + Group_eff)
Exp3.M1.SampGbyVal <- update(Exp3.M1.PolAff, .~. + Samp_GroupB_dum:Valence_eff)
Exp3.M1.SampGbyCond <- update(Exp3.M1.SampGbyVal, .~. + Samp_GroupB_dum:Condition_c_eff)
Exp3.M1.CondbyVal <- update(Exp3.M1.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp3.M1.SampGbyCondbyVal <- update(Exp3.M1.CondbyVal, .~. + Samp_GroupB_dum:Condition_c_eff:Valence_eff)
Exp3.M1.Full <- update(Exp3.M1.SampGbyCondbyVal, .~. + Samp_GroupB_dum*Valence_eff*Condition_c_eff*Group_eff)
Exp3.M1.effects <- anova(Exp3.M1.Baseline,Exp3.M1.SampG, Exp3.M1.Valence, Exp3.M1.Cond, Exp3.M1.PolAff, Exp3.M1.SampGbyVal, Exp3.M1.SampGbyCond, Exp3.M1.CondbyVal, Exp3.M1.SampGbyCondbyVal, Exp3.M1.Full)
Exp3.M1.coefficients <- summary(Exp3.M1.Full)
#####

#Eval models without pundit as seen in manuscript
#####
Exp3.M2.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Eval3)
Exp3.M2.SampG <- update(Exp3.M2.Baseline, .~. + Evaluated.Group_eff)
Exp3.M2.Valence <- update(Exp3.M2.SampG, .~. + Valence_eff)
Exp3.M2.Cond <- update(Exp3.M2.Valence, .~. + Condition_c_eff)
Exp3.M2.PolAff <- update(Exp3.M2.Cond, .~. + Group_eff)
Exp3.M2.SampGbyVal <- update(Exp3.M2.PolAff, .~. + Evaluated.Group_eff:Valence_eff)
Exp3.M2.SampGbyCond <- update(Exp3.M2.SampGbyVal, .~. + Evaluated.Group_eff:Condition_c_eff)
Exp3.M2.CondbyVal <- update(Exp3.M2.SampGbyCond, .~. + Valence_eff:Condition_c_eff)
Exp3.M2.SampGbyCondbyVal <- update(Exp3.M2.CondbyVal, .~. + Evaluated.Group_eff:Condition_c_eff:Valence_eff)
Exp3.M2.Full <- update(Exp3.M2.SampGbyCondbyVal, .~. + Evaluated.Group_eff*Valence_eff*Condition_c_eff*Group_eff)
Exp3.M2.effects <- anova(Exp3.M2.Baseline,Exp3.M2.SampG, Exp3.M2.Valence, Exp3.M2.Cond, Exp3.M2.PolAff, Exp3.M2.SampGbyVal, Exp3.M2.SampGbyCond,
      Exp3.M2.CondbyVal,Exp3.M2.SampGbyCondbyVal, Exp3.M2.Full )
Exp3.M2.coefficients <- summary(Exp3.M2.Full)
#####

#Models with source as factor
#####
#Sampling Models with source as factor
Exp3.M3.Baseline <- glmer(n_trials~1+  (1|Participant), data = Samp3, family = 'poisson',control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))
Exp3.M3.SampG <- update(Exp3.M3.Baseline, .~. + Samp_GroupB_dum)
Exp3.M3.PunditImp <- update(Exp3.M3.SampG, .~. + PunditImp_eff)
Exp3.M3.Cond <- update(Exp3.M3.PunditImp, .~. + Condition_c_eff)
Exp3.M3.PolAff <- update(Exp3.M3.Cond, .~. + Group_eff)
Exp3.M3.SampGbyPunditImp <- update(Exp3.M3.PolAff, .~. + Samp_GroupB_dum:PunditImp_eff)
Exp3.M3.SampGbyCond <- update(Exp3.M3.SampGbyPunditImp, .~. + Samp_GroupB_dum:Condition_c_eff)
Exp3.M3.CondbyVal <- update(Exp3.M3.SampGbyCond, .~. + PunditImp_eff:Condition_c_eff)
Exp3.M3.SampGbyCondbyPunditImp <- update(Exp3.M3.CondbyVal, .~. + Samp_GroupB_dum:Condition_c_eff:PunditImp_eff)
Exp3.M3.Full <- update(Exp3.M3.SampGbyCondbyPunditImp, .~. + Samp_GroupB_dum*PunditImp_eff*Condition_c_eff*Group_eff)
Exp3.M3.effects <- anova(Exp3.M3.Baseline, Exp3.M3.SampG, Exp3.M3.PunditImp, Exp3.M3.Cond, Exp3.M3.PolAff, Exp3.M3.SampGbyPunditImp, Exp3.M3.SampGbyCond, Exp3.M3.CondbyVal, Exp3.M3.SampGbyCondbyPunditImp, Exp3.M3.Full)

#Eval models with pundit
Exp3.M4.Baseline <- lmer(P.Estimates~1+  (1|Participant), data = Eval3)
Exp3.M4.Affil <- update(Exp3.M4.Baseline, .~. + Group_eff)
Exp3.M4.SampG <- update(Exp3.M4.Baseline, .~. + Evaluated.Group_eff)
Exp3.M4.Pundit <- update(Exp3.M4.SampG, .~. + PunditImp_eff)
Exp3.M4.Cond <- update(Exp3.M4.Pundit, .~. + Condition_c_eff)
Exp3.M4.PolAff <- update(Exp3.M4.Cond, .~. + Group_eff)
Exp3.M4.SampGbyPundit <- update(Exp3.M4.PolAff, .~. + Evaluated.Group_eff:PunditImp_eff)
Exp3.M4.SampGbyCond <- update(Exp3.M4.SampGbyPundit, .~. + Evaluated.Group_eff:Condition_c_eff)
Exp3.M4.CondbyPundit <- update(Exp3.M4.SampGbyCond, .~. + PunditImp_eff:Condition_c_eff)
Exp3.M4.SampGbyCondbyPundit <- update(Exp3.M4.CondbyPundit, .~. + Evaluated.Group_eff:Condition_c_eff:PunditImp_eff)
Exp3.M4.Full <- update(Exp3.M4.SampGbyCondbyPundit, .~. + Evaluated.Group_eff*PunditImp_eff*Condition_c_eff*Group_eff)
Exp3.M4.effects <- anova(Exp3.M4.Baseline,Exp3.M4.SampG, Exp3.M4.Pundit, Exp3.M4.Cond, Exp3.M4.PolAff, Exp3.M4.SampGbyPundit, Exp3.M4.SampGbyCond,
      Exp3.M4.CondbyPundit,Exp3.M4.SampGbyCondbyPundit, Exp3.M4.Full )
Exp3.M4.pund.effect <- summary(Exp3.M4.SampGbyPundit)
Exp3.M4.pund.cond.effect <- summary(Exp3.M4.SampGbyCondbyPundit)
#####

#Descriptives and simple contrasts
#####
Exp3.contrasts.samp <- describeBy(Samp3$n_trials, Samp3$Samp_Group)

#means for evals
Exp3.contrasts.EvalbyGroup <- describeBy(Eval3$P.Estimates, Eval3$Evaluated.Group)
Exp3.contrasts.EvalbyValence <- describeBy(Eval3$P.Estimates, Eval3$Valence)

#Contrasts
emm <- emmeans(Exp3.M2.CondbyVal, ~ Valence_eff:Condition_c_eff, type = "response", lmer.df = "satterthwaite")
Exp3.ValCondContrast <- contrast(emm, simple = "Valence_eff")
Exp3.ValCondContrast_d <- eff_size(emm, sigma = sigma(Exp3.M2.CondbyVal), edf = df.residual(Exp3.M2.CondbyVal))

#Contrasts
emm <- emmeans(Exp3.M2.SampGbyCondbyVal, ~ Valence_eff:Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp3.ValCondEvalContrast <- contrast(emm, simple = "Evaluated.Group_eff")
Exp3.ValCondEvalContrast_d <- eff_size(emm, sigma = sigma(Exp3.M2.SampGbyCondbyVal), edf = df.residual(Exp3.M2.SampGbyCondbyVal))

emm <- emmeans(Exp3.M2.SampGbyCond, ~ Condition_c_eff:Evaluated.Group_eff, type = "response", lmer.df = "satterthwaite")
Exp3.CondEvalContrast <- contrast(emm, simple = "Evaluated.Group_eff")
Exp3.CondEvalContrast_d <- eff_size(emm, sigma = sigma(Exp3.M2.SampGbyCond), edf = df.residual(Exp3.M2.SampGbyCond))
#####

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
##**Plotting Point-estimates for both with main effects of Condition**
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


#####Print the results in the order they appear in the manuscript
print('RESULTS FROM EXPERIMENT 1')
print('Binomial test for first sample')
print(Exp.3.binom.test)
print(firstSamplePlot3)
print('Binomial test for first sample after positive')
print(Exp.3.binom.test.pos)
print(firstSamplePlotPos)
print('Binomial test for first sample after negative')
print(Exp.3.binom.test.neg)
print(firstSamplePlotNeg)

print('Mixed effects model on sampling behavior')
print('dispersion paramater for poisson')
print(Exp3.M1.disp)
print(Exp3.M1.effects)
print(Exp3.M1.coefficients)
print(moreIngroup3plot)

print('Descriptive statistics for number of ingroup and outgroup samples')
print(Exp3.contrasts.samp)

print('Mixed effects model on ratings')
print(Exp3.M2.effects)
print(Exp3.M2.coefficients)
print(means_cond_Val3Plot)
print(means_cond_Cond3Plot)

print('Contrast and effect size for real group difference by evaluated group')
print(Exp3.ValCondContrast)
print(Exp3.ValCondContrast_d)

print('Contrast and effect size for real group difference by evaluated group by valence')
print(Exp3.ValCondEvalContrast)
print(Exp3.ValCondEvalContrast_d)

print('Contrast and effect size for real group difference by valence')
print(Exp3.CondEvalContrast)
print(Exp3.CondEvalContrast_d)

print('Sampling Model with Pundit as Factor')
print('No Difference')
print(Exp3.M3.effects)



