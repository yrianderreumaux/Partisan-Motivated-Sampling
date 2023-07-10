library(dplyr)

# Load Data
meta3 <- read.csv("data/MetaStudy3Data.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
meta3$Participant <- as.factor(meta3$Participant)

# Effects code
meta3 <- meta3 %>%
  mutate(
    Condition_c = case_when(
      Condition == "Worse" ~ -1,
      Condition == "Same" ~ 0,
      Condition == "Better" ~ 1
    ),
    Condition_c_eff = as.factor(Condition_c),
    Condition_c_dum = as.factor(Condition)
  )

# Better as throw away/reference
meta3 <- meta3 %>%
  mutate(
    Condition_c_eff = relevel(Condition_c_eff, ref = "Same"),
    Condition_c_dum = relevel(Condition_c_dum, ref = "Same")
  )

# Group
meta3 <- meta3 %>%
  mutate(
    group = ifelse(group == "dem", 1, 0),
    group_eff = as.factor(group)
  ) %>%
  mutate(group_eff = relevel(group_eff, ref = "0"))

# Coding valence
meta3 <- meta3 %>%
  mutate(
    Val_dum = as.factor(Val),
    Val_eff = as.factor(Val)
  ) %>%
  mutate(Val_eff = relevel(Val_eff, ref = "0"))

# Study
meta3 <- meta3 %>%
  mutate(
    study_eff = as.factor(study)
  ) %>%
  mutate(study_eff = relevel(study_eff, ref = "1"))

# Isolate first 15 trials as in manuscript
IQR <- IQR(meta3$n_trials)
TrialDescr <- summary(meta3$n_trials)
meta3Final <- meta3[-which(meta3$Trial >= 15),]
meta3Final$t <- meta3Final$Trial
meta3Final$t <- meta3Final$t - 7

# Shift cells
studies1n2 <- meta3Final[which(meta3Final$study == 1 | meta3Final$study == 2),]
study3 <- meta3Final[which(meta3Final$study == 3),]

# Create function to shift cells
shift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}

# Re-order by participants and then trial
studies1n2 <- studies1n2 %>% arrange(Participant, Trial)

# Run for loop to shift cells so that the first sample is counted
for (i in unique(studies1n2$Participant)) {
  studies1n2$sampled_group[studies1n2$Participant == i] <- shift(studies1n2$sampled_group[studies1n2$Participant == i], 1)
}

# Combine datasets
meta3Final_t <- bind_rows(studies1n2, study3)

# Model with random intercepts for participants and random slope for level 1 predictor
Meta3M1InterceptOnly <- glmer(sampled_group ~ (1 | Participant) + (0 + t | Participant), data = meta3Final, family = 'binomial', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))
TrialM1 <- update(Meta3M1InterceptOnly, . ~ . + t)
ValenceM1 <- update(TrialM1, . ~ . + Val_eff)
ConditionM1 <- update(ValenceM1, . ~ . + Condition_c_eff)
AffiliationM1 <- update(ConditionM1, . ~ . + group_eff)
StudyM1 <- update(AffiliationM1, . ~ . + study_eff)
ValenceTrialM1 <- update(StudyM1, . ~ . + t:Val_eff)
ConditionTrialM1 <- update(ValenceTrialM1, . ~ . + t:Condition_c_eff)
ConditionValenceM1 <- update(ConditionTrialM1, . ~ . + Condition_c_eff:group_eff)
TrialConditionValenceM1 <- update(ConditionValenceM1, . ~ . + t * Condition_c_eff * Val_eff)

# Model comparison
meta3.M1.effects <- anova(Meta3M1InterceptOnly, TrialM1, ValenceM1, ConditionM1, AffiliationM1, StudyM1, ValenceTrialM1, ConditionTrialM1, ConditionValenceM1, TrialConditionValenceM1)
meta3Coefficients <- summary(meta3.M1.effects)

# Model for visuals
Meta3model.vis <- glmer(sampled_group ~ t * Val_eff * Condition_c_eff + group_eff + study_eff + (1 | Participant) + (0 + t | Participant), data = meta3Final_t, family = 'binomial', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))
plot_model(Meta3model.vis, type = "pred", terms = c("t", "Val_eff", "Condition_c_eff"))

# Model 2 with difference score
Meta3.M2 <- glmer(sampled_group ~ t * Val_eff * difSca * Condition_c_eff + group_eff + study_eff + (1 | Participant) + (0 + t | Participant), data = meta3Final, family = 'binomial', control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))
Meta3.M2.coefficients <- summary(Meta3.M2)

# Print the results in the order they appear in the manuscript
cat("RESULTS FROM Meta 3\n")
cat("Model without difference score\n")
print(meta3.M1.effects)
cat("Coefficients:\n")
print(meta3Coefficients)

cat("RESULTS FROM Meta 3 with difference score\n")
print(Meta3.M2.coefficients)
