#Load Data Experiment 1a
#####
Experiment1aIndDiff <- read.csv("data/Experiment1aIndDiff.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
#isolate numerical columns, e.g., removing PID variable. 
Experiment1aIndDiff_num <- Experiment1aIndDiff[,c(8:13, 15: 28)]
Experiment1aIndDiff_num$diff_score <- Experiment1aIndDiff_num$In.Est - Experiment1aIndDiff_num$Out.Est #create diff score variable. 
#descriptives of individual differences, demographics and number of samples
Exp1.polAffil <- hist(Experiment1aIndDiff_num$Pol.Affil)
Exp1.education <- hist(Experiment1aIndDiff_num$Education)
Exp1.income <- hist(Experiment1aIndDiff_num$Income)
Exp1.SDO <- hist(Experiment1aIndDiff_num$SDO) #social dominance
Exp1.HH <- hist(Experiment1aIndDiff_num$HH) #Honesty humility 
Exp1.EX <- hist(Experiment1aIndDiff_num$EX) #extraversion (BFI-2)
Exp1.BFI_Open <- hist(Experiment1aIndDiff_num$BFI_Open) #open (BFI-2)
Exp1.CSE_Mem <- hist(Experiment1aIndDiff_num$SE_Mem) #CSE membership 
Exp1.SE_Importance <- hist(Experiment1aIndDiff_num$SE_Importance) #CSE importance 
Exp1.SE_Private <- hist(Experiment1aIndDiff_num$SE_Private) #CSE private
Exp1.SE_Public <- hist(Experiment1aIndDiff_num$SE_Public) #CSE public
Exp1.N_Trials <- hist(Experiment1aIndDiff_num$n_trials) #number of trials
Exp1.age <- hist(Experiment1aIndDiff_num$Age)
Exp1.age.descr <- describe(Experiment1aIndDiff_num$Age)
Exp1.gender <- table(Experiment1aIndDiff_num$Gender)
#correlation matrix Study1
Experiment1a.Cor <- psych::corr.test(Experiment1aIndDiff_num)
#####

#Load Experiment 1b
#####
Experiment1bIndDiff <- read.csv("data/Experiment1bIndDiff.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Experiment1bIndDiff <- Experiment1bIndDiff[,-c(1:2)] #remove superfluous columns
#isolate numerical columns, e.g., removing PID variable. 
Experiment1bIndDiff_num <- Experiment1bIndDiff[,c(7:11, 14:23, 25:29)]
Experiment1bIndDiff_num$diff_score <- Experiment1bIndDiff_num$In.Est - Experiment1bIndDiff_num$Out.Est #create diff score variable. 
#descriptives of individual differences, demographics and number of samples
Exp1b.polAffil <- hist(Experiment1bIndDiff_num$Pol.Affil)
Exp1b.education <- hist(Experiment1bIndDiff_num$education)
Exp1b.income <- hist(Experiment1bIndDiff_num$income)
Exp1b.SDO <- hist(Experiment1bIndDiff_num$SDO) #social dominance
Exp1b.HH <- hist(Experiment1bIndDiff_num$HH) #Honesty humility 
Exp1b.EX <- hist(Experiment1bIndDiff_num$EX) #extraversion (BFI-2)
Exp1b.BFI_Open <- hist(Experiment1bIndDiff_num$BFI_Open) #open (BFI-2)
Exp1b.CSE_Mem <- hist(Experiment1bIndDiff_num$SE_Mem) #CSE membership 
Exp1b.SE_Importance <- hist(Experiment1bIndDiff_num$SE_Importance) #CSE importance 
Exp1b.SE_Private <- hist(Experiment1bIndDiff_num$SE_Private) #CSE private
Exp1b.SE_Public <- hist(Experiment1bIndDiff_num$SE_Public) #CSE public
Exp1b.N_Trials <- hist(Experiment1bIndDiff_num$n_trials) #number of trials
Exp1b.age <- hist(Experiment1bIndDiff_num$age)
Exp1b.age.descr <- describe(Experiment1bIndDiff_num$age)
Exp1b.gender <- table(Experiment1bIndDiff_num$gender)
Exp1b.diffscore <- table(Experiment1bIndDiff_num$diff_score)
#correlation Matrix Study2
Experiment1b.Cor <- psych::corr.test(Experiment1bIndDiff_num)
#####

#Load Experiment 2
#####
Experiment2IndDiff <- read.csv("data/Experiment2IndDiff.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data
Experiment2IndDiff <- Experiment2IndDiff[,-c(1:2)] #remove superfluous columns
#isolate numerical columns, e.g., removing PID variable. 
Experiment2IndDiff_num <- Experiment2IndDiff[,c(6:12, 19:23, 25:29)]
Experiment2IndDiff_num$diff_score <- Experiment2IndDiff_num$In.Est - Experiment2IndDiff_num$Out.Est #create diff score variable.
#descriptives of individual differences, demographics and number of samples
Exp2.polAffil <- hist(Experiment2IndDiff$Pol.Affil)
Exp2.education <- hist(Experiment2IndDiff$education)
Exp2.income <- hist(Experiment2IndDiff$income)
Exp2.HH <- hist(Experiment2IndDiff$HH) #Honesty humility 
Exp2.EX <- hist(Experiment2IndDiff$EX) #extraversion (BFI-2)
Exp2.N_Trials <- hist(Experiment2IndDiff$n_trials) #number of trials
Exp2.age <- hist(Experiment2IndDiff$age)
Exp2.diff.score <- hist(Experiment2IndDiff_num$diff_score)
Exp2.age.descr <- describe(Experiment2IndDiff$age)
Exp2.gender <- table(Experiment2IndDiff$gender)
#correlation Matrix Study2
Experiment2.Cor <- psych::corr.test(Study2IndDiff_num)
#####

#Load meta 1 data
#####
Meta1 <- read.csv("data/MetaStudy1Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) #load data meta 1
Meta1_num <- Meta1[,c(5:10, 12:24, 26, 27)]
#descriptives of individual differences, demographics and number of samples
Meta1.polAffil <- hist(Meta1$Pol.Affil) #add pol affil
Meta1.education <- hist(Meta1$education)
Meta1.income <- hist(Meta1$income)
Meta1.HH <- hist(Meta1$HH) #Honesty humility 
Meta1.EX <- hist(Meta1$EX) #extraversion (BFI-2)
Meta1.N_Trials <- hist(Meta1$n_trials) #number of trials
Meta1.age <- hist(Meta1$age)
Meta1.age.descr <- describe(Meta1$age)
Meta1.gender <- table(Meta1$gender)
Meta1.diffscore <- table(Meta1$diff_score)
#correlation Matrix meta1
Meta1.cor <- psych::corr.test(Meta1_num)
#####

#Load meta 2 data
#####
Meta2 <- read.csv("data/MetaStudy2Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) 
Meta2OutFirst <- Meta2[which(Meta2$firstSample==0),]
Meta2OutFirst_num <- Meta2OutFirst[,c(5:10, 12:24, 26, 27)]
#descriptives of individual differences, demographics and number of samples
Meta2OutFirst.polAffil <- hist(Meta2OutFirst$Pol.Affil)
Meta2OutFirst.education <- hist(Meta2OutFirst$education)
Meta2OutFirst.income <- hist(Meta2OutFirst$income)
Meta2OutFirst.SDO <- hist(Meta2OutFirst$SDO)
Meta2OutFirst.BFI_Open <- hist(Meta2OutFirst$BFI_Open)
Meta2OutFirst.HH <- hist(Meta2OutFirst$HH) #Honesty humility 
Meta2OutFirst.EX <- hist(Meta2OutFirst$EX) #extraversion (BFI-2)
Meta2OutFirst.CSE_Mem <- hist(Meta2OutFirst$SE_Mem) #CSE membership 
Meta2OutFirst.SE_Importance <- hist(Meta2OutFirst$SE_Importance) #CSE importance 
Meta2OutFirst.SE_Private <- hist(Meta2OutFirst$SE_Private) #CSE private
Meta2OutFirst.SE_Public <- hist(Meta2OutFirst$SE_Public) #CSE public
Meta2OutFirst.N_Trials <- hist(Meta2OutFirst$n_trials) #number of trials
Meta2OutFirst.age <- hist(Meta2OutFirst$age)
Meta2OutFirst.age.descr <- describe(Meta2OutFirst$age)
Meta2OutFirst.gender <- table(Meta2OutFirst$gender)
Meta2OutFirst.diffscore <- table(Meta2$diff_score)
#correlation Matrix meta2
Meta2.study <- psych::corr.test(Meta2OutFirst_num)
#####

#Load meta 3 data
#####
Meta3IndDiff <- read.csv("data/MetaStudy3Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA")) 
#descriptives of individual differences, demographics and number of samples
Meta3.polAffil <- hist(Meta3IndDiff$Pol.Affil)
Meta3.education <- hist(Meta3IndDiff$education)
Meta3.N_Trials <- hist(Meta3IndDiff$n_trials) #number of trials
Meta3.age <- hist(Meta3IndDiff$age)
Meta3.age.desc <- describe(Meta3IndDiff$age)
Meta3.gender <- table(Meta3IndDiff$gender)
Meta3.diffscore <- hist(Meta3IndDiff$diff_score)
#####

#####Print the results in the order they appear in the manuscript
print('Descriptives for Experiment 1a')
plot(Exp1a.polAffil)
plot(Exp1a.education)
plot(Exp1a.income)
plot(Exp1a.SDO)
plot(Exp1a.HH)
plot(Exp1a.EX)
plot(Exp1a.BFI_Open)
plot(Exp1a.CSE_Mem)
plot(Exp1a.SE_Importance)
plot(Exp1a.SE_Private)
plot(Exp1a.SE_Public)
plot(Exp1a.N_Trials)
plot(Exp1a.age)
print(Exp1a.age.descr)
print(Exp1a.gender)
print('Experiment 1a Correlation Matrix')
print(Experiment1a.Cor)

print('Descriptives for Experiment 1b')
plot(Exp1b.polAffil)
plot(Exp1b.education)
plot(Exp1b.income)
plot(Exp1b.SDO)
plot(Exp1b.HH)
plot(Exp1b.EX)
plot(Exp1b.BFI_Open)
plot(Exp1b.CSE_Mem)
plot(Exp1b.SE_Importance)
plot(Exp1b.SE_Private)
plot(Exp1b.SE_Public)
plot(Exp1b.N_Trials)
plot(Exp1b.age)
print(Exp1b.age.descr)
print(Exp1b.gender)
print('Experiment 1b Correlation Matrix')
print(Experiment1b.Cor)

print('Descriptives for Study 2')
plot(Exp2.polAffil)
plot(Exp2.education)
plot(Exp2.income)
plot(Exp2.HH)
plot(Exp2.EX)
plot(Exp2.N_Trials)
plot(Exp2.age)
plot(Exp2.diff.score)
print(Exp2.age.descr)
print(Exp2.gender)
print('Experiment 2 Correlation Matrix')
print(Experiment2.Cor)

print('Descriptives for Meta 1')
plot(Meta1.polAffil)
plot(Meta1.education)
plot(Meta1.income)
plot(Meta1.HH)
plot(Meta1.EX)
plot(Meta1.N_Trials)
plot(Meta1.age)
print(Meta1.age.descr)
print(Meta1.gender)
print('Study 2 Correlation Matrix')
print(Meta1.cor)

print('Descriptives for Meta 2')
plot(Meta2OutFirst.polAffil)
plot(Meta2OutFirst.education)
plot(Meta2OutFirst.income)
plot(Meta2OutFirst.SDO)
plot(Meta2OutFirst.HH)
plot(Meta2OutFirst.EX)
plot(Meta2OutFirst.BFI_Open)
plot(Meta2OutFirst.CSE_Mem)
plot(Meta2OutFirst.SE_Importance)
plot(Meta2OutFirst.SE_Private)
plot(Meta2OutFirst.SE_Public)
plot(Meta2OutFirst.N_Trials)
plot(Meta2OutFirst.age)
plot(Meta2OutFirst.diffscore)
print(Meta2OutFirst.age.descr)
print(Meta2OutFirst.gender)
print('Study 2 Correlation Matrix')
print(Meta2.study)

print('Descriptives for Meta 3')
plot(Meta3.polAffil)
plot(Meta3.education)
plot(Meta3.N_Trials)
plot(Meta3.age)
plot(Meta3.diffscore)
print(Meta3.age.desc)
print(Meta3.gender)

