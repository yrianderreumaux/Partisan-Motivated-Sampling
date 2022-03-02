#Load Data
MetaStudy1 <- read.csv("data/MetaStudy2Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#create long format data
#####
meta2 <- data.frame(rep(MetaStudy1$Participant,2), rep(MetaStudy1$Condition,2),
                   c(MetaStudy1$In.Est, MetaStudy1$Out.Est), factor(rep(c(1,2), each=1445), labels = c("In", "Out")),
                   rep(MetaStudy1$Val, 2), rep(MetaStudy1$group, 2),
                   rep(MetaStudy1$study, 2),rep(MetaStudy1$Pol.Affil, 2), rep(MetaStudy1$firstSample, 2))

names(meta2) <- c("Participant", "Condition", "P.Estimate", "Eval_Group", "Valence", "Group", "Study", "polAffil", "firstSample")
##Making sampled group into character so that it can be effects coded
meta2$Samp_GroupString <- as.character(meta2$Eval_Group)
meta2$GroupString <- as.character(meta2$Group)
meta2$ValenceString <- as.character(meta2$Valence)
#####

#Effects and dummy code variables
#####
#Change conditions to 1 2 3 for clarity
meta2$Condition[meta2$Condition == 1] <- "Worse"
meta2$Condition[meta2$Condition == 2] <- "Same"
meta2$Condition[meta2$Condition == 3] <- "Better"

#Effects + dummy coding, let's call it _c.
meta2$Condition_c[meta2$Condition == "Worse"] <- -1
meta2$Condition_c[meta2$Condition == "Same"] <- 0
meta2$Condition_c[meta2$Condition == "Better"] <- 1
meta2$Condition_c_eff <- factor(meta2$Condition_c)
meta2$Condition_c_dum <- factor(meta2$Condition,
                                levels = c("Worse", "Same", "Better"))

#effects + dummy coding In and Out group with out as thro-away (Samp_Group)
meta2$Samp_GroupB_eff <- meta2$Samp_GroupString
meta2$Samp_GroupB_dum <- meta2$Samp_GroupString
meta2$Samp_GroupB_eff <- as.factor(meta2$Samp_GroupB_eff)
meta2$Samp_GroupB_dum <- as.factor(meta2$Samp_GroupB_dum)
meta2$Samp_GroupB_eff <- factor(meta2$Samp_GroupB_eff,
                                levels = c("In", "Out"))
meta2$Samp_GroupB_dum <- factor(meta2$Samp_GroupB_dum,
                                levels = c("In", "Out"))

#effects + dummy coding group so that Rep is throw-away (Group)
meta2$Group_dum <- as.factor(meta2$Group)
meta2$Group_eff <- as.factor(meta2$Group)

#coding valence
meta2$Valence_eff <- meta2$ValenceString
meta2$Valence_dum <- meta2$ValenceString
meta2$Valence_eff <- as.factor(meta2$Valence_eff)
meta2$Valence_dum <- as.factor(meta2$Valence_dum)
meta2$Study <- as.factor(meta2$Study)

contrasts(meta2$Condition_c_eff) <-contr.sum(3)
contrasts(meta2$Condition_c_dum) <-contr.sum(3)
contrasts(meta2$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(meta2$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(meta2$Condition_c_dum)) = c("Worse", "Same")

contrasts(meta2$Samp_GroupB_eff) <-contr.sum(2)
contrasts(meta2$Samp_GroupB_dum) <-contr.sum(2)
contrasts(meta2$Samp_GroupB_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(meta2$Samp_GroupB_eff)) = c("In.Group")
colnames(contrasts(meta2$Samp_GroupB_dum)) = c("In.Group")

#change the dummy code reference group to DEM
contrasts(meta2$Group_dum) <-contr.treatment(2, base = 2)
contrasts(meta2$Group_eff) <- contr.sum(2)
colnames(contrasts(meta2$Group_eff)) = c("Dem")
colnames(contrasts(meta2$Group_dum)) = c("Dem")

contrasts(meta2$Valence_eff) <-contr.sum(2)
contrasts(meta2$Valence_dum) <-contr.sum(2)
contrasts(meta2$Valence_dum) <- contr.treatment(2, base = 2)
colnames(contrasts(meta2$Valence_eff)) = c("Neg")
colnames(contrasts(meta2$Valence_dum)) = c("Neg")

contrasts(meta2$Study) <-contr.sum(2)
colnames(contrasts(meta2$Study)) = c("Study1")

S1EvalsOutgroup <- meta2[which(meta2$firstSample==0),]
meta2Out <- S1EvalsOutgroup
meta2Out$Condition[meta2Out$Condition=="Better"] <- 3
meta2Out$Condition[meta2Out$Condition=="Same"] <- 2
meta2Out$Condition[meta2Out$Condition=="Worse"] <- 1
#####

#Subset outgroup first participants
#####
Meta2EvalsOutgroup <- meta2[which(meta2$firstSample==0),]
#####

#Eval Model
######
miniOutgroup <- lmer(P.Estimate~1 +(1|Participant), data = Meta2EvalsOutgroup,control = lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4),optimizer ="Nelder_Mead"), REML=F)
Meta2EvalsSampG <- update(miniOutgroup, .~. + Samp_GroupB_dum)
Meta2EvalsValence <- update(Meta2EvalsSampG, .~. + Valence_eff)
Meta2EvalsCond <- update(Meta2EvalsValence, .~. + Condition_c_eff)
Meta2EvalsPolAff <- update(Meta2EvalsCond, .~. + Group_eff)
Meta2EvalsStudy <- update(Meta2EvalsPolAff, .~. + Study)

Meta2EvalsSampGbyVal <- update(Meta2EvalsStudy, .~. + Samp_GroupB_dum:Valence_eff)
Meta2EvalsSampGbyAffil <- update(Meta2EvalsSampGbyVal, .~. + Samp_GroupB_dum:Group_eff)
Meta2EvalsSampGbyCond <- update(Meta2EvalsSampGbyAffil, .~. + Samp_GroupB_dum:Condition_c_eff)
Meta2EvalsValbyCond <- update(Meta2EvalsSampGbyCond, .~. + Valence_eff:Condition_c_eff)
Meta2EvalsValbyAffil <- update(Meta2EvalsValbyCond, .~. + Valence_eff:Group_eff)

Meta2EvalsValbyCondbyValence <- update(Meta2EvalsValbyAffil, .~. + Samp_GroupB_dum:Valence_eff:Condition_c_eff)
Meta2EvalsValbyCondbyAffil <- update(Meta2EvalsValbyCondbyValence, .~. + Samp_GroupB_dum:Valence_eff:Group_eff)
Meta2AffilbyValbyCond <- update(Meta2EvalsValbyAffil, .~. + Group_eff:Valence_eff:Condition_c_eff)
Meta2EvalsFull <- update(Meta2AffilbyValbyCond, .~. + Samp_GroupB_dum*Group_eff*Valence_eff*Condition_c_eff)

dd.meta2 <- update(miniOutgroup,devFunOnly=TRUE) #random intercepts crashed to 0 giving us singularity warnings. 
params.meta2 <- getME(miniOutgroup,"theta") 
grad(dd.meta2,params.meta2)
## values  < 1e-3
eigen(solve(hessian(dd.meta2,params.meta2)),only.values=TRUE)$values #in this case we can ignore singularity warning. 

Meta2.effects <- anova(miniOutgroup, Meta2EvalsSampG, Meta2EvalsValence, Meta2EvalsCond, Meta2EvalsPolAff, Meta2EvalsStudy, Meta2EvalsSampGbyVal, Meta2EvalsSampGbyAffil, Meta2EvalsSampGbyCond, Meta2EvalsValbyCond, Meta2EvalsValbyAffil, Meta2EvalsValbyCondbyValence, Meta2EvalsValbyCondbyAffil, 
       Meta2EvalsFull)
Meta2.coefficients <-summary(Meta2EvalsFull)
#####

#contrasts
#####
#first sample valence effects
emm <- emmeans(Meta2EvalsSampGbyVal, ~ Valence_eff:Samp_GroupB_dum, type = "response", lmer.df = "satterthwaite")
Meta2.ValGroupContrast <- contrast(emm, simple = "Samp_GroupB_dum")
Meta2.ValGroupContrast_d <- eff_size(emm, sigma = sigma(Meta2EvalsSampGbyVal), edf = df.residual(Meta2EvalsSampGbyVal))

#contrasts for condition * evaluated group
emm <- emmeans(Meta2EvalsSampGbyCond, ~ Samp_GroupB_dum:Condition_c_eff, type = "response", lmer.df = "satterthwaite")
Meta2.CondGroupContrast <- contrast(emm, simple = "Samp_GroupB_dum")
Meta2.CondGroupContrast_d <- eff_size(emm, sigma = sigma(Meta2EvalsSampGbyCond), edf = df.residual(Meta2EvalsSampGbyCond))
#####

#Visuals
#####
meta2_valence <- summarySEwithin(data=meta2Out, idvar='Participant', measurevar = 'P.Estimate', betweenvars =  "Valence", withinvars = 'Eval_Group', na.rm = TRUE)
meta2_valencePlot <- ggplot(meta2_valence, aes(x =Valence, y=P.Estimate, fill=Eval_Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimate-se, ymax=P.Estimate+se),
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

meta2_valencePlot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 20,face = "plain", color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("meta2_valencePlot.png")#save fill legend

meta2_Cond <- summarySEwithin(data=meta2Out, idvar='Participant', measurevar = 'P.Estimate', betweenvars =  "Condition", withinvars = 'Eval_Group', na.rm = TRUE)
meta2_CondPlot <- ggplot(meta2_Cond, aes(x =Condition, y=P.Estimate, fill=Eval_Group)) +
  geom_hline(yintercept = 3, linetype="solid") +
  geom_hline(yintercept = 102, linetype="dashed") +
  geom_bar(position=position_dodge(), stat="identity",color = "black") +
  geom_errorbar(aes(ymin=P.Estimate-se, ymax=P.Estimate+se),
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

meta2_CondPlot+ theme(
  plot.title = element_text(color="black", size=18, family = "Times New Roman"),
  axis.title.x = element_text(color="black", size=40,face = "bold", family = "Times New Roman"),
  axis.title.y = element_text(color="black", size=40, face = "bold",family = "Times New Roman"),
  axis.text = element_text(size = 17, face = "plain",color = "black", family = "Times New Roman"),
  panel.grid.major.x = element_blank(),
  panel.border = element_rect(colour = "black", fill=NA, size=1))
#ggsave("meta2_CondPlot.png")#save fill legend
#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FROM Meta 2')
print(Meta2.effects)

print('Contrasts and effect  for valence by eval group')
print(Meta2.ValGroupContrast)
print(Meta2.ValGroupContrast_d)

print('Contrasts and effect sizes for real group difference by evaluated group')
print(Meta2.CondGroupContrast)
print(Meta2.CondGroupContrast_d)

print('Meta 2 Plot')
plot(meta2_valencePlot)
plot(meta2_CondPlot)

