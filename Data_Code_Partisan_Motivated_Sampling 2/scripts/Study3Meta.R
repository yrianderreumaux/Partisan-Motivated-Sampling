#Load Data
meta3 <- read.csv("data/MetaStudy3Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))#load data
meta3$Participant <- as.factor(meta3$Participant)

#effects code
#####
meta3$Condition_c[meta3$Condition == "Worse"] <- -1
meta3$Condition_c[meta3$Condition == "Same"] <- 0
meta3$Condition_c[meta3$Condition == "Better"] <- 1
meta3$Condition_c_eff <- factor(meta3$Condition_c)
meta3$Condition_c_dum <- factor(meta3$Condition,
                                levels = c("Worse", "Same", "Better"))
#better as throw away/reference
contrasts(meta3$Condition_c_eff) <-contr.sum(3)
contrasts(meta3$Condition_c_dum) <- contr.treatment(3, base = 3)
colnames(contrasts(meta3$Condition_c_eff)) = c("Worse", "Same")
colnames(contrasts(meta3$Condition_c_dum)) = c("Worse", "Same")
#group
meta3$group[meta3$group == "dem"] <- 1
meta3$group[meta3$group == "rep"] <- 0
meta3$group_eff <- as.factor(meta3$group)
contrasts(meta3$group_eff) <-contr.sum(2)
colnames(contrasts(meta3$group_eff)) = c("Rep")
#coding valence
meta3$Val_dum <- as.factor(meta3$Val)
meta3$Val_eff <- as.factor(meta3$Val)
contrasts(meta3$Val_eff) <-contr.sum(2)
colnames(contrasts(meta3$Val_eff)) = c("Neg")
#study
meta3$study_eff <- as.factor(meta3$study)
contrasts(meta3$study_eff) <-contr.sum(3)
colnames(contrasts(meta3$study_eff)) = c(1,2)
#####

#Isolate first 15 trials as in manuscript. 
#####
IQR <- IQR(meta3$n_trials)
TrialDescr <- summary(meta3$n_trials)
meta3Final <- meta3[-which(meta3$Trial>=15),] #subset 15 but we lose the first trial 
meta3Final$t <- meta3Final$Trial
meta3Final$t <- meta3Final$t -7 #center
#####

#shift cells
#####
studies1n2 <- meta3Final[which(meta3Final$study==1|meta3Final$study==2),]
study3 <- meta3Final[which(meta3Final$study==3),]

#create function to shift cells. 
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}
#re-order by participants and then trial
studies1n2 <- studies1n2[order(studies1n2$Participant, studies1n2$Trial),]
#run for loop to shift cells so that first sample is counted. 
for (i in unique(studies1n2$Participant)) {
  studies1n2$sampled_group[studies1n2$Participant==i] <- shift(studies1n2$sampled_group[studies1n2$Participant==i], 1)
}
#this was not done in 1a and 1b so that is why we are shifting separately and then bringing them together after
meta3Final_t <- rbind(studies1n2, study3)
#####


#Model with random intercepts for participants and random slope for level 1 predictor (as needed for cross level interaction to avoid inflated type 1 errors). 
#####
Meta3M1InterceptOnly <- glmer(sampled_group~ +(1|Participant)+(0+t|Participant), data = meta3Final, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
TrialM1  <- update(Meta3M1InterceptOnly, .~. +t)
ValenceM1  <- update(TrialM1 , .~. +Val_eff)
ConditionM1  <-update(ValenceM1 , .~. +Condition_c_eff)
AffiliationM1  <- update(ConditionM1 , .~. +group_eff) #control for pol affiliation
StudyM1  <- update(AffiliationM1 , .~. +study_eff) #control for study
ValenceTrialM1  <- update(StudyM1 , .~. +t:Val_eff)
ConditionTrialM1  <- update(ValenceTrialM1 , .~. +t:Condition_c_eff)
ConditionValenceM1  <- update(ConditionTrialM1 , .~. +Condition_c_eff:group_eff)
TrialConditionValenceM1  <- update(ConditionValenceM1 , .~. +t*Condition_c_eff*Val_eff)
#model comparison 
meta3.M1.effects <- anova(Meta3M1InterceptOnly, TrialM1, ValenceM1, ConditionM1, AffiliationM1, StudyM1, ValenceTrialM1, ConditionTrialM1, ConditionValenceM1, TrialConditionValenceM1)
meta3Coefficients <- summary(meta3.M1.effects)

#Model for visuals
Meta3model.vis <- glmer(sampled_group~ t*Val_eff*Condition_c_eff+group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3Final_t, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
plot_model(Meta3model.vis, type = "pred", terms = c("t","Val_eff","Condition_c_eff"))
#####

#Model 2 with difference score 
#####
Meta3.M2 <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff+group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3Final, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
Meta3.M2.coefficients <- summary(Meta3.M2)
#####

#Visuals
#####

meta3Plot <- ggpredict(Meta3model.vis, terms = c("t","Val_eff","Condition_c_eff"))%>% plot()
meta3Plot <- meta3Plot + labs(x = "", y = "") +theme_apa()+
  theme(axis.title.y = element_text(size = rel(1.4)))+ theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
meta3Plot <- meta3Plot + scale_x_continuous(breaks = c( -5, -3,  -1,  1, 3, 5, 7),labels=c())+ scale_y_continuous(limits = c(.3, .7), labels = c())+theme(legend.position="bottom")
meta3Plot <- meta3Plot + theme(axis.text.x = element_text(color="black", size=16), axis.text.y = element_text( color="black", size=16)) +theme(legend.position = "none")

meta3Plot <- meta3Plot   + theme(panel.grid.major =   element_line(colour = "white",size=0.75))
#ggsave("meta3Plot.jpeg", dpi = 300, width = 7, height = 6 )#save fill legend

#####
# meta3Plot <- meta3Plot + scale_x_continuous(breaks = c(-5,  -3, -1, 1, 3, 5),labels=c("2", "4", "6", "8", "10","12"))+ scale_y_continuous(limits = c(.35, .65), labels = c("30%", "40%", "50%", "60%", "70%"))+theme(legend.position="bottom")
# meta3Plot <- meta3Plot + scale_x_continuous(breaks = c(-6, -5, -4 -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"))+ scale_y_continuous(limits = c(.35, .65), labels = c("30%", "40%", "50%", "60%", "70%"))+theme(legend.position="bottom")

#Model without difference score but interacting with political affiliation 
#####
# BaseM2 <- glmer(sampled_group~ +(1|Participant)+(0+t|Participant), data = meta3Final, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# TrialM2 <- update(BaseM2, .~. +t)
# ValenceM2 <- update(TrialM2, .~. +Val_eff)
# ConditionM2 <-update(ValenceM2, .~. +Condition_c_eff)
# AffiliationM2 <- update(ConditionM2, .~. +group_eff)
# StudyM2 <- update(AffiliationM2, .~. +study_eff)
# ValenceTrialM2 <- update(StudyM2, .~. +t:Val_eff)
# ConditionTrialM2 <- update(ValenceTrialM2, .~. +t:Condition_c_eff)
# AffiliationTrialM2 <- update(ConditionTrialM2, .~. +t:group_eff)
# AffiliationValenceM2 <- update(AffiliationTrialM2, .~. +Val_eff:group_eff)
# AffiliationConditionM2 <- update(AffiliationValenceM2, .~. +Condition_c_eff:group_eff)
# ValenceConditionM2 <- update(AffiliationConditionM2, .~. +Val_eff:Condition_c_eff)
# TrialAffiliationValenceM2 <- update(ValenceConditionM2, .~. +t:Val_eff:group_eff)
# TrialAffiliationConditionM2 <- update(TrialAffiliationValenceM2, .~. +t:Condition_c_eff:group_eff)
# TrialValenceConditionM2 <- update(TrialAffiliationConditionM2, .~. +t:Val_eff:Condition_c_eff)
# Full <- update(TrialValenceConditionM2, .~. +t*Val_eff*Condition_c_eff*group_eff)
# meta3.M2effects.noDiffGroup <- anova(BaseM2, TrialM2, ValenceM2, ConditionM2, AffiliationM2, StudyM2, ValenceTrialM2, ConditionTrialM2, AffiliationTrialM2, AffiliationValenceM2, AffiliationConditionM2, ValenceConditionM2, TrialAffiliationValenceM2, TrialAffiliationConditionM2, TrialValenceConditionM2, Full)
#####

#Model that fits every possible interaction. #Just uncomment (by highlighting all and selecting command + shift + c) to run as it will take at least 2 hours to complete. 
# #####
# Base3 <- glmer(sampled_group~ +(1|Participant)+(0+t|Participant), data = meta3Final, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=300000)))
# Trial3 <- update(Base3, .~. +t)
# Valence3 <- update(Trial3, .~. +Val_eff)
# Condition3<-update(Valence3, .~. +Condition_c_eff)
# diffScore3 <- update(Condition3, .~. +difSca)
# Affiliation3 <- update(diffScore3, .~. +group_eff)
# Study3 <- update(Affiliation3, .~. +study_eff)
# ValenceTrial3 <- update(Study3, .~. +t:Val_eff)
# ConditionTrial3 <- update(ValenceTrial3, .~. +t:Condition_c_eff)
# diffScoreTrial3 <- update(ConditionTrial3, .~. +t:difSca)
# StudyTrial3 <- update(diffScoreTrial3, .~. +t:study_eff)
# AffiliationTrial3 <- update(StudyTrial3, .~. +t:group_eff)
# #all 2 way interactions
# diffScoreValence3 <- update(AffiliationTrial3, .~. +difSca:Val_eff)
# diffScoreCondition3 <- update(diffScoreValence3, .~. +difSca:Condition_c_eff)
# diffScoreStudy3 <- update(diffScoreCondition3, .~. +difSca:study_eff)
# diffScoreGroup3 <- update(diffScoreStudy3, .~. +difSca:group_eff)
# ConditionValence3 <- update(diffScoreGroup3, .~. +Condition_c_eff:Val_eff)
# ConditionStudy3 <- update(ConditionValence3, .~. +Condition_c_eff:study_eff)
# ConditionGroup3 <- update(ConditionStudy3, .~. +Condition_c_eff:group_eff)
# GroupCondition3 <- update(ConditionGroup3, .~. +group_eff:Condition_c_eff)
# GroupStudy3 <- update(GroupCondition3, .~. +group_eff:study_eff)
# GroupStudy3 <- update(GroupStudy3, .~. +group_eff:Val_eff)
# ValenceStudy3 <- update(GroupStudy3, .~. +Val_eff:study_eff)
# #all 3-way interactions with trial
# TrialValenceDiffScore3 <- update(ValenceStudy3, .~. +t:difSca:Val_eff)
# TrialConditionDiffScore3 <- update(TrialValenceDiffScore3, .~. +t:difSca:Condition_c_eff)
# TrialStudyDiffScore3 <- update(TrialConditionDiffScore3, .~. +t:difSca:study_eff)
# TrialGroupDiffScore3 <- update(TrialStudyDiffScore3, .~. +t:difSca:group_eff)
# TrialConditionValence3 <- update(TrialGroupDiffScore3, .~. +t:Condition_c_eff:Val_eff)
# TrialConditioStudy3 <- update(TrialConditionValence3, .~. +t:Condition_c_eff:study_eff)
# TrialConditioGroup3 <- update(TrialConditioStudy3, .~. +t:Condition_c_eff:group_eff)
# TrialGroupCondition3 <- update(TrialConditioGroup3, .~. +t:group_eff:Condition_c_eff)
# TrialGroupStudy3 <- update(TrialGroupCondition3, .~. +t:group_eff:study_eff)
# TrialGroupValence3 <- update(TrialGroupStudy3, .~. +t:group_eff:Val_eff)
# TrialValenceStudy3 <- update(TrialGroupValence3, .~. +t:Val_eff:study_eff)
# #all 4-way interactions
# TrialValenceDiffScoreCondition<- update(TrialValenceStudy3, .~. +t:Val_eff:difSca:Condition_c_eff)
# TrialValenceDiffScoreStudy<- update(TrialValenceDiffScoreCondition, .~. +t:Val_eff:difSca:study_eff)
# TrialValenceDiffScoreGroup<- update(TrialValenceDiffScoreStudy, .~. +t:Val_eff:difSca:group_eff)
# TrialConditionValenceGroup<- update(TrialValenceDiffScoreGroup, .~. +t:Condition_c_eff:Val_eff:group_eff)
# TrialConditionValenceStudy<-update(TrialConditionValenceGroup, .~. +t:Condition_c_eff:Val_eff:study_eff)
# #all 5-way interactions
# TrialValenceDiffScoreConditionGroup<- update(TrialConditionValenceStudy, .~. +t:Val_eff:difSca:Condition_c_eff:group_eff)
# TrialValenceDiffScoreConditionStudy<- update(TrialValenceDiffScoreConditionGroup, .~. +t:Val_eff:difSca:Condition_c_eff:study_eff)
# TrialGroupDiffScoreConditionStudy<- update(TrialValenceDiffScoreConditionStudy, .~. +t:group_eff:difSca:Condition_c_eff:study_eff)
# 
# #model with affil and study interacting with all other predictors. 
# Full3 <- update(TrialGroupDiffScoreConditionStudy, .~. +t*Condition_c_eff*Val_eff*study_eff*difSca*group_eff)
# #model comparison
# Meta3effects <- anova(Base3, Trial3, Valence3, Condition3, diffScore3, Affiliation3, Study3, 
#       ValenceTrial3, ConditionTrial3, diffScoreTrial3, StudyTrial3, AffiliationTrial3, 
#       diffScoreValence3, diffScoreCondition3, diffScoreStudy3, diffScoreGroup3, 
#       ConditionValence3,  ConditionStudy3, ConditionGroup3, ValenceStudy3,
#       TrialValenceDiffScore3, TrialConditionDiffScore3, TrialStudyDiffScore3, TrialGroupDiffScore3, 
#       TrialConditionValence3, TrialConditioStudy3, TrialConditioGroup3,
#       TrialGroupCondition3, TrialGroupStudy3, TrialGroupValence3, TrialValenceStudy3,
#       TrialValenceDiffScoreCondition, TrialValenceDiffScoreStudy, TrialValenceDiffScoreGroup, 
#       TrialConditionValenceGroup, TrialConditionValenceStudy, 
#       TrialValenceDiffScoreConditionGroup, TrialValenceDiffScoreConditionStudy, TrialGroupDiffScoreConditionStudy, 
#       Full3)
#####

#supplemental models with all different trial cutoffs to ensure similar direction and effects. Once again, uncomment to run. This will take ~2 hours. 
#####
# meta3_5 <- meta3[-which(meta3$Trial>=5),] #subset 5 
# meta3_5$t <- meta3_5$t -2
# meta3_7 <- meta3[-which(meta3$Trial>=7),] #subset 7
# meta3_7$t <- meta3_7$t -3
# meta3_9 <- meta3[-which(meta3$Trial>=9),] #subset 9
# meta3_9$t <- meta3_9$t -4
# meta3_11 <- meta3[-which(meta3$Trial>=11),] #subset 11
# meta3_11$t <- meta3_11$t -5
# meta3_15 <- meta3[-which(meta3$Trial>=15),] #subset 15
# meta3_15$t <- meta3_15$t -7
# meta3_17 <- meta3[-which(meta3$Trial>=17),] #subset 17
# meta3_17$t <- meta3_17$t -8
# meta3_19 <- meta3[-which(meta3$Trial>=19),] #subset 19
# meta3_19$t <- meta3_19$t -8
# meta3_21 <- meta3[-which(meta3$Trial>=21),] #subset 21
# meta3_21$t <- meta3_21$t -8
# meta3_23 <- meta3[-which(meta3$Trial>=23),] #subset 23
# meta3_23$t <- meta3_23$t -8
# meta3_25 <- meta3[-which(meta3$Trial>=25),] #subset 25
# meta3_25$t <- meta3_25$t -8
# meta3_27 <- meta3[-which(meta3$Trial>=27),] #subset 27
# meta3_27$t <- meta3_27$t -8

# Meta3_5_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_5, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_5_model.effects <- summary(Meta3_5_model)
# meta3_7_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_7, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_7_model.effects <- summary(meta3_7_model)
# meta3_9_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_9, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_9_model.effects <- summary(meta3_9_model)
# Meta3_11_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_11, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_11_model.effects <- summary(Meta3_11_model)
# Meta3_15_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_15, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_15_model.effects <- summary(Meta3_15_model)
# Meta3_17_model <- glmer(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff+(1|Participant)+(0+t|Participant), data = meta3_17, family = 'binomial', control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=200000)))
# Meta3_17_model.effects <- summary(Meta3_17_model)

##model without nesting to avoid model##
# Meta3_19_model <- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3_19, family = 'binomial')
# Meta3_19_model.effects <- summary(Meta3_19_model)
# Meta3_21_model <- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3_21, family = 'binomial')
# Meta3_21_model.effects <- summary(Meta3_21_model)
# Meta3_23_model <- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3_23, family = 'binomial')
# Meta3_23_model.effects <- summary(Meta3_23_model)
# Meta3_25_model <- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3_25, family = 'binomial')
# Meta3_25_model.effects <- summary(Meta3_25_model)
# Meta3_27_model <- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3_27, family = 'binomial')
# Meta3_27_model.effects <- summary(Meta3_27_model)

##Model with all trials without nested structure ##
# Meta3_full<- glm(sampled_group~ t*Val_eff*difSca*Condition_c_eff +group_eff +study_eff, data = meta3, family = 'binomial')
#####


#####Print the results in the order they appear in the manuscript
print('RESULTS FROM Meta 3')
print('Model without difference score')
print(meta3.M1.effects)
print(meta3Plot)
print(meta3Coefficients)

print('RESULTS FROM Meta 3 with difference score')
print(Meta3.M2.coefficients)


print('Uncomment and run lines 129-246 for supplemental models (Warning, they take a few hours to run!)')
# print(Meta3_5_model.effects)
# print(Meta3_7_model.effects)
# print(Meta3_9_model.effects)
# print(Meta3_11_model.effects)
# print(Meta3_15_model.effects)
# print(Meta3_17_model.effects)
# print(Meta3_19_model.effects)
# print(Meta3_21_model.effects)
# print(Meta3_23_model.effects)
# print(Meta3_25_model.effects)
# print(Meta3_27_model.effects)

