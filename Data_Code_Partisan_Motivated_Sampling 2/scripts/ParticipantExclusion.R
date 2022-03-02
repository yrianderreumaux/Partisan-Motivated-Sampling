#Experiment 1a
#####
Experiment1aexclus <- read.csv("data/Experiment1aDataExclusion.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
Experiment1aexclus <- Experiment1aexclus[,-c(4:8)]
#raw data had N = 599
Experiment1AffilNeither <- Experiment1aexclus[which(Experiment1aexclus$PolAffil=="Neither liberal nor conservative\n\n4"),] #subset participants who do not identify as either conservative or liberal (i.e. neither)
Experiment1AffilNeither <- length(unique(Experiment1AffilNeither$Participant))
#55 identified as neutral 
Experiment1aexclus <- Experiment1aexclus[-which(Experiment1aexclus$PolAffil=="Neither liberal nor conservative\n\n4"),] #remove participants who identify as neither
Experiment1Outliers <- Experiment1aexclus[which(Experiment1aexclus$Att>2),] #isolate participants who fail 3 or more attention checks. 
Experiment1Outliers <- length(unique(Experiment1Outliers$Participant))
#4 outliers
Experiment1aexclus <- Experiment1aexclus[-which(Experiment1aexclus$Att>2),] #subset participants who do not identify as either conservative or liberal (i.e. neither)
Experiment1length <- length(unique(Experiment1aexclus$Participant))
#Total Sample for Experiment 1 after exclusion = 540
#####

#Experiment1b
#####
Experiment1bexclus <- read.csv("data/Experiment1bDataExclusion.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#raw data had N = 974
Experiment2AffilNeither<- Experiment1bexclus[which(Experiment1bexclus$group=="Neither"),] #isolate participants who identify as neither conservative nor liberal
Experiment2AffilNeither <- length(unique(Experiment2AffilNeither$Participant))
#60 identified as neutral 
Experiment1bexclus <- Experiment1bexclus[-which(Experiment1bexclus$group=="Neither"),] #remove the 60 participants who identify as neither
Experiment2Outliers <- Experiment1bexclus[which(Experiment1bexclus$Att>2),] #isolate participants who fail 3 or more attention checks. 
Experiment2Outliers <- length(unique(Experiment2Outliers$Participant))
#9 outliers
Experiment1bexclus <- Experiment1bexclus[-which(Experiment1bexclus$Att>2),] #remove the 60 participants who identify as neither
Experiment2length <-length(unique(Experiment1bexclus$Participant))
#Total Sample for Experiment 1 after exclusion = 905
#####

#Experiment2
#####
Experiment2exclus <- read.csv("data/Experiment2DataExclusion.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))
#raw data had N = 1076
Experiment3AffilNeither<- Experiment2exclus[which(Experiment2exclus$Pol.Affil==4),] #isolate participants who identify as neither conservative nor liberal
Experiment3AffilNeither <- length(unique(Experiment3AffilNeither$Participant))
#79 identified as neutral 
Experiment2exclus <- Experiment2exclus[-which(Experiment2exclus$Pol.Affil==4),] #remove the 60 participants who identify as neither
Experiment3Outliers <- Experiment2exclus[which(Experiment2exclus$Att>2),] #isolate participants who fail 3 or more attention checks. 
Experiment3Outliers <- length(unique(Experiment3Outliers$Participant))
#11 outliers
Experiment2exclus <- Experiment2exclus[-which(Experiment2exclus$Att>2),] #remove the 60 participants who identify as neither
Experiment3length <- length(unique(Experiment2exclus$Participant))
#Total Sample for Experiment 1 after exclusion = 986
#####

print("Participants Excluded for +2 attention check failures or identifying as neither liberal nor conservative")
print("Experiment1")
print(Experiment1AffilNeither)
print("Participants who identify as neither cons or lib")
print(Experiment1Outliers)
print("Outliers")
print(Experiment1length)
print("Final N")

print("Experiment2")
print(Experiment2AffilNeither)
print("Participants who identify as neither cons or lib")
print("+ 2 participants who did not specify any affiliation")
print(Experiment2Outliers)
print("Outliers")
print(Experiment2length)
print("Final N")

print("Experiment3")
print(Experiment3AffilNeither)
print("Participants who identify as neither cons or lib")
print(Experiment3Outliers)
print("Outliers")
print(Experiment3length)
print("Final N")

