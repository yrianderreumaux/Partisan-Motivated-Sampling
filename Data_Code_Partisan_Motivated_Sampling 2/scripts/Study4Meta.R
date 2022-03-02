#Load data
Survdf <- read.csv("data/MetaStudy4Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))

#calculate ingroup and outgroup variance. 
#####
InSamplesDF <- Survdf[which(Survdf$sampled_group==1),]
OutSamplesDF <- Survdf[which(Survdf$sampled_group==0),]
ingroupDescriptives <- InSamplesDF %>% # the names of the new data frame and the data frame to be summarised
  group_by(Participant) %>%   # the grouping variable
  dplyr::summarise(meanIngroup = mean(data, na.rm = T),
                   sdIngroup = sd(data, na.rm = T),# calculates the mean of each group
                   n_SampleIngroup = n(),  # calculates the sample size per group
                   SEIngroup = sd(data, na.rm = T)/sqrt(n())) # calculates the standard error of each group
outgroupDescriptives <- OutSamplesDF %>% # the names of the new data frame and the data frame to be summarised
  group_by(Participant) %>%   # the grouping variable
  dplyr::summarise(meanOutgroup = mean(data, na.rm = T),
                   sdOutgroup = sd(data, na.rm = T),# calculates the mean of each group
                   n_SampleOutgroup = n(),  # calculates the sample size per group
                   SEOutgroup = sd(data, na.rm = T)/sqrt(n())) # calculates the standard error of each group

Survdf <- merge(Survdf, ingroupDescriptives, by = c("Participant"), all.x=T)
Survdf <- merge(Survdf, outgroupDescriptives, by = c("Participant"), all.x=T)
#####

#disag data
#####
Survdf_Dis <- Survdf[!duplicated(Survdf$Participant), ] #remove all duplicate ids so that each subject has 1 column
#####

#t.test for in vs. out variability
#####
varTest <- t.test(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, paired = T, alternative = "two.sided") #yes, people do have more variable ingroup experiences. 
varTestLevene <- leveneTest(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup) #to ensure we are not violating assumptions
varTestCohenD <- effsize::cohen.d(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, na.rm =T)
#nonparametric
varTestWilcox <- wilcox.test(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, alternative = "two.sided") #here is results of nonparametric in case people want to see it. 
#####

#Variability multiple regression model
#####
VarBiasModel <- lm(scale(diff_score)~scale(sdIngroup)*Condition+scale(sdOutgroup)*Condition+ study_eff, data = Survdf_Dis)
VarBiasModelCI <- confint(VarBiasModel, level=0.95)
#####

#for loop to show how I created event variable for the survival model. 
#####
# Survdf$event <- 0
# Survdf <- Survdf[order(Survdf$Participant, Survdf$Trial),] #order by P and then trial
# Survdf$sampled_group[which(Survdf$Trial==49)] <- NA #can't get event if they sampled all 50 times, so remove the last two
# Survdf$sampled_group[which(Survdf$Trial==48)] <- NA
# #iterate through Ps and create an event the trial after they stop sampling
# for (Participant in unique(Survdf$Participant)) {
#   subDf <- Survdf[Survdf$Participant == Participant,] #isolate each participant
#   event <-  grep(TRUE, is.na(subDf$sampled_group))[1] #identify index of last sample that is not NA
#   event <-   event -1
#   subDf$event[event] <- 1 #make that index a 1 under variable event. Event being they quit sampling
#   subDf$event[(event+1):nrow(subDf)] <- NA #make all other rows NAs
#   Survdf[Survdf$Participant == Participant,] <- subDf #append each participant back to original df.
#   print(Participant) #to monitor bugs
# }
# Survdf <- Survdf[!is.na(Survdf$event),] #remove all NA rows
#####

#survival model
#####
Meta4InVar <- coxme(Surv(Trial, event) ~scale(sdIngroup)*Condition_c_dum+study_eff + (1|Participant), data = Survdf)
Meta4OutVar <- coxme(Surv(Trial, event) ~scale(sdOutgroup)*Condition_c_dum+study_eff + (1|Participant), data = Survdf)
#####

#Visuals
#####
#Median Split Variability for figure 7
Survdf$inVarDich[Survdf$sdIngroup > median(Survdf$sdIngroup, na.rm = T) ] <- "high"
Survdf$inVarDich[Survdf$sdIngroup < median(Survdf$sdIngroup, na.rm = T) ] <- "low"

BetterSurvDF <- Survdf[which(Survdf$Condition=="Better"),]
BetterM <- survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = BetterSurvDF)
BetterSurv <- ggsurvplot(fit = survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = BetterSurvDF), linetype = c("strata"),conf.int = TRUE,conf.int.style = "ribbon",
                         xlab = "", 
                         ylab = "",palette = c("chocolate2", "cyan4"), legend.title = "Variability",
                         legend.labs = c("High", "Low"))
BetterSurv %++% theme(axis.text.y = element_blank(),axis.text.x = element_blank())
#ggsave("BetterSurv.jpg", dpi = 300 )

SameSurvDF <- Survdf[which(Survdf$Condition=="Same"),]
SameM <- survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = SameSurvDF)
SameSurv <-ggsurvplot(fit = survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = SameSurvDF), conf.int = TRUE,  linetype = c("strata"),conf.int.style = "ribbon",conf.int.alpha = .15,
                      xlab = "", 
                      ylab = "", palette = c("chocolate2", "cyan4"), legend.title = "Variability",
                      legend.labs = c("High", "Low"))
SameSurv %++% theme(axis.text.y = element_blank(),axis.text.x = element_blank()) 
#ggsave("SameSurv.jpg", dpi = 300 )

WorseSurvDF <- Survdf[which(Survdf$Condition=="Worse"),]
WorseM <- survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = WorseSurvDF)
WorseSurv <-ggsurvplot(fit = survfit(Surv(Trial, event) ~ inVarDich +cluster(Participant), data = WorseSurvDF), linetype = c("strata"),conf.int = TRUE,conf.int.style = "ribbon",conf.int.alpha = .15,
                       xlab = "", 
                       ylab = "", palette = c("chocolate2", "cyan4"), legend.title = "Variability",
                       legend.labs = c("High", "Low"),panel.border = element_rect(colour = "black", fill=NA, size=.5))
WorseSurv %++% theme(axis.text.y = element_blank(),axis.text.x = element_blank()) 
#ggsave("WorseSurv.jpg", dpi = 300 )
#####

#####Print the results in the order they appear in the manuscript
print('Results for Meta 4')
print('More Variable Ingroup Experiences')
print(varTest)
print(varTestLevene)
print(varTestCohenD)

print('Ingroup variance predicts more biased evals')
print(VarBiasModel)
print(VarBiasModelCI)

print('Survival Analysis')
print('Ingroup variance predicts more biased evals')
print(Meta4InVar)

print('Outgroup variance does not predict more biased evals')
print(Meta4OutVar)

print('Survival Plot')
print(BetterSurv)
print(SameSurv)
print(WorseSurv)
