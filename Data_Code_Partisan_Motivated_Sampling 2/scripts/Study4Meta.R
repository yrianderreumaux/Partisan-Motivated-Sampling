# Load data
Survdf <- read.csv("data/MetaStudy4Data.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

Survdf <- Survdf %>%
  group_by(Participant) %>%
  mutate(
    meanGroup = mean(data, na.rm = TRUE),
    sdGroup = sd(data, na.rm = TRUE),
    n_SampleGroup = n(),
    SEGroup = sd(data, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup() %>%
  pivot_wider(
    names_from = sampled_group,
    values_from = c(meanGroup, sdGroup, n_SampleGroup, SEGroup),
    names_prefix = c("meanIngroup_", "meanOutgroup_", "sdIngroup_", "sdOutgroup_", "n_SampleIngroup_", "n_SampleOutgroup_", "SEIngroup_", "SEOutgroup_")
  )

Survdf <- left_join(Survdf, ingroupDescriptives, by = "Participant")
Survdf <- left_join(Survdf, outgroupDescriptives, by = "Participant")
# Disaggregate data
Survdf_Dis <- Survdf %>%
  distinct(Participant, .keep_all = TRUE) # Remove all duplicate ids so that each subject has 1 column

# T-test for in vs. out variability
varTest <- t.test(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, paired = TRUE, alternative = "two.sided")
varTestLevene <- leveneTest(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup)
varTestCohenD <- effsize::cohen.d(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, na.rm = TRUE)
# Nonparametric
varTestWilcox <- wilcox.test(Survdf_Dis$sdIngroup, Survdf_Dis$sdOutgroup, alternative = "two.sided")

# Variability multiple regression model
VarBiasModel <- lm(scale(diff_score) ~ scale(sdIngroup) * Condition + scale(sdOutgroup) * Condition + study_eff, data = Survdf_Dis)
VarBiasModelCI <- confint(VarBiasModel, level = 0.95)

# Survival model
Meta4InVar <- coxme(Surv(Trial, event) ~ scale(sdIngroup) * Condition_c_dum + study_eff + (1 | Participant), data = Survdf)
Meta4OutVar <- coxme(Surv(Trial, event) ~ scale(sdOutgroup) * Condition_c_dum + study_eff + (1 | Participant), data = Survdf)

# Median Split Variability for figure 7
Survdf$inVarDich <- ifelse(Survdf$sdIngroup > median(Survdf$sdIngroup, na.rm = TRUE), "high", "low")

BetterSurvDF <- subset(Survdf, Condition == "Better")
BetterM <- survfit(Surv(Trial, event) ~ inVarDich + cluster(Participant), data = BetterSurvDF)
BetterSurv <- ggsurvplot(fit = BetterM, linetype = "strata", conf.int = TRUE, conf.int.style = "ribbon",
                         xlab = "", ylab = "", palette = c("chocolate2", "cyan4"),
                         legend.title = "Variability", legend.labs = c("High", "Low")) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

SameSurvDF <- subset(Survdf, Condition == "Same")
SameM <- survfit(Surv(Trial, event) ~ inVarDich + cluster(Participant), data = SameSurvDF)
SameSurv <- ggsurvplot(fit = SameM, linetype = "strata", conf.int = TRUE, conf.int.style = "ribbon", conf.int.alpha = 0.15,
                       xlab = "", ylab = "", palette = c("chocolate2", "cyan4"),
                       legend.title = "Variability", legend.labs = c("High", "Low")) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

WorseSurvDF <- subset(Survdf, Condition == "Worse")
WorseM <- survfit(Surv(Trial, event) ~ inVarDich + cluster(Participant), data = WorseSurvDF)
WorseSurv <- ggsurvplot(fit = WorseM, linetype = "strata", conf.int = TRUE, conf.int.style = "ribbon", conf.int.alpha = 0.15,
                        xlab = "", ylab = "", palette = c("chocolate2", "cyan4"),
                        legend.title = "Variability", legend.labs = c("High", "Low"),
                        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

# Print the results in the order they appear in the manuscript
cat("Results for Meta 4\n")
cat("More Variable Ingroup Experiences\n")
print(varTest)
print(varTestLevene)
print(varTestCohenD)

cat("Ingroup variance predicts more biased evals\n")
print(VarBiasModel)
print(VarBiasModelCI)

cat("Survival Analysis\n")
cat("Ingroup variance predicts more biased evals\n")
print(Meta4InVar)

cat("Outgroup variance does not predict more biased evals\n")
print(Meta4OutVar)

cat("Survival Plot\n")
print(BetterSurv)
print(SameSurv)
print(WorseSurv)
