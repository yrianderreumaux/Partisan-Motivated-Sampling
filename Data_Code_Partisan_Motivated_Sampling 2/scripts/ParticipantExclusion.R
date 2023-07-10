# Experiment 1a
Experiment1aexclus <- read.csv("data/Experiment1aDataExclusion.csv", header = T, stringsAsFactors = FALSE, na.strings = c("", "NA"))
Experiment1aexclus <- Experiment1aexclus[, -c(4:8)]
Experiment1AffilNeither <- length(unique(Experiment1aexclus$Participant[Experiment1aexclus$PolAffil == "Neither liberal nor conservative\n\n4"]))
Experiment1aexclus <- Experiment1aexclus[!(Experiment1aexclus$PolAffil == "Neither liberal nor conservative\n\n4"),]
Experiment1Outliers <- length(unique(Experiment1aexclus$Participant[Experiment1aexclus$Att > 2]))
Experiment1aexclus <- Experiment1aexclus[!(Experiment1aexclus$Att > 2),]
Experiment1length <- length(unique(Experiment1aexclus$Participant))

# Experiment 1b
Experiment1bexclus <- read.csv("data/Experiment1bDataExclusion.csv", header = T, stringsAsFactors = FALSE, na.strings = c("", "NA"))
Experiment2AffilNeither <- length(unique(Experiment1bexclus$Participant[Experiment1bexclus$group == "Neither"]))
Experiment1bexclus <- Experiment1bexclus[!(Experiment1bexclus$group == "Neither"),]
Experiment2Outliers <- length(unique(Experiment1bexclus$Participant[Experiment1bexclus$Att > 2]))
Experiment1bexclus <- Experiment1bexclus[!(Experiment1bexclus$Att > 2),]
Experiment2length <- length(unique(Experiment1bexclus$Participant))

# Experiment 2
Experiment2exclus <- read.csv("data/Experiment2DataExclusion.csv", header = T, stringsAsFactors = FALSE, na.strings = c("", "NA"))
Experiment3AffilNeither <- length(unique(Experiment2exclus$Participant[Experiment2exclus$Pol.Affil == 4]))
Experiment2exclus <- Experiment2exclus[!(Experiment2exclus$Pol.Affil == 4),]
Experiment3Outliers <- length(unique(Experiment2exclus$Participant[Experiment2exclus$Att > 2]))
Experiment2exclus <- Experiment2exclus[!(Experiment2exclus$Att > 2),]
Experiment3length <- length(unique(Experiment2exclus$Participant))

# Print the results
cat("Participants Excluded for +2 attention check failures or identifying as neither liberal nor conservative\n")
cat("Experiment 1:\n")
cat("Participants who identify as neither conservative nor liberal: ", Experiment1AffilNeither, "\n")
cat("Outliers: ", Experiment1Outliers, "\n")
cat("Final N: ", Experiment1length, "\n\n")

cat("Experiment 2:\n")
cat("Participants who identify as neither conservative nor liberal: ", Experiment2AffilNeither, "\n")
cat("Participants who did not specify any affiliation: ", Experiment2Outliers, "\n")
cat("Outliers: ", Experiment2length, "\n")
cat("Final N: ", Experiment2length, "\n\n")

cat("Experiment 3:\n")
cat("Participants who identify as neither conservative nor liberal: ", Experiment3AffilNeither, "\n")
cat("Outliers: ", Experiment3Outliers, "\n")
cat("Final N: ", Experiment3length, "\n")
