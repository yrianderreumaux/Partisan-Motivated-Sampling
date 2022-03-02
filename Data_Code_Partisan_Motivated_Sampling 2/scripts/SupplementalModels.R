#Reviewer requested analyses
df <- read.csv("data/suppDF.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))#load data

#warm glow models. Do participants simply rate the group they see more higher?
#####
df$diffIngroup <- df$mean_Ingroup - df$In.Est #difference between what participants actually saw for ingroup and what they estimated for ingroup
df$diffOutgroup <- df$mean_Outgroup - df$Out.Est #difference between what participants actually saw for outgroup and what they estimated for outgroup

Supp1 <- lm(scale(diffIngroup)~scale(in_samples)*Condition + scale(out_samples), data = df) #predicting in bias by # ingroup samples * condition controlling for # outgroup samples. 
Supp2 <- lm(scale(diffOutgroup)~scale(out_samples)*Condition + scale(in_samples), data = df) #vice versa
#####

#individual differences predicting first choice
#####
#SDO
SDO_model <- glm(firstSample~scale(SDO)+study, data = df, family=binomial(link='logit'))
#Honesty Humility
HH_model <- glm(firstSample~scale(HH)+study, data = df, family=binomial(link='logit'))
#Extroversion
EX_model <- glm(firstSample~scale(EX)+study, data = df, family=binomial(link='logit'))
#Extroversion
BFI_Open_model <- glm(firstSample~scale(BFI_Open)+study, data = df, family=binomial(link='logit'))
#####

#individual differences predicting # of trials
#####
#SDO
SDO_modelTrial <- glm(n_trials~scale(SDO)+study, family = "poisson", data = df)
#HH
HH_modelTrial <- glm(n_trials~scale(HH)+study,  family = "poisson", data = df)
#EX
EX_modelTrial <- glm(n_trials~scale(EX)+study,  family = "poisson", data = df)
#BFI
BFI_modelTrial <- glm(n_trials~scale(BFI_Open)+study,  family = "poisson", data = df)
#####

#individual differences predicting eval bias
#####
df$diffScore <- df$In.Est -df$Out.Est
#SDO
SDO_modelEval <- lm(scale(diffScore)~scale(SDO)*as.factor(Condition)+study, data = df)
#HH
HH_modelEval <- lm(scale(diffScore)~scale(HH)*as.factor(Condition)+study, data = df)
#EX
Ex_modelEval <- lm(scale(diffScore)~scale(EX)*as.factor(Condition)+study, data = df)
#BFI
BFI_modelEval <- lm(scale(diffScore)~scale(BFI_Open)*as.factor(Condition)+study,  data = df)
#####

print('print supplemental models')
print(summary(Supp1))
print(summary(Supp2))
print(summary(SDO_model))
print(summary(HH_model))
print(summary(EX_model))

print(summary(BFI_Open_model))
print(summary(SDO_modelTrial))
print(summary(HH_modelTrial))
print(summary(EX_modelTrial))
print(summary(BFI_modelTrial))

print(summary(SDO_modelEval))
print(summary(HH_modelEval))
print(summary(Ex_modelEval))
print(summary(BFI_modelEval))
