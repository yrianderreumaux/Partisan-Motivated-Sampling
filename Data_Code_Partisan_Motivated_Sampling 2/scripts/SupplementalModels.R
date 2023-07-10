# Load data
df <- read.csv("data/suppDF.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Warm glow models. Do participants simply rate the group they see more highly?
df <- df %>%
  mutate(diffIngroup = mean_Ingroup - In.Est,
         diffOutgroup = mean_Outgroup - Out.Est)

Supp1 <- lm(scale(diffIngroup) ~ scale(in_samples) * Condition + scale(out_samples), data = df)
Supp2 <- lm(scale(diffOutgroup) ~ scale(out_samples) * Condition + scale(in_samples), data = df)

# Individual differences predicting first choice
SDO_model <- glm(firstSample ~ scale(SDO) + study, data = df, family = binomial(link = 'logit'))
HH_model <- glm(firstSample ~ scale(HH) + study, data = df, family = binomial(link = 'logit'))
EX_model <- glm(firstSample ~ scale(EX) + study, data = df, family = binomial(link = 'logit'))
BFI_Open_model <- glm(firstSample ~ scale(BFI_Open) + study, data = df, family = binomial(link = 'logit'))

# Individual differences predicting # of trials
SDO_modelTrial <- glm(n_trials ~ scale(SDO) + study, family = "poisson", data = df)
HH_modelTrial <- glm(n_trials ~ scale(HH) + study, family = "poisson", data = df)
EX_modelTrial <- glm(n_trials ~ scale(EX) + study, family = "poisson", data = df)
BFI_modelTrial <- glm(n_trials ~ scale(BFI_Open) + study, family = "poisson", data = df)

# Individual differences predicting eval bias
df <- df %>%
  mutate(diffScore = In.Est - Out.Est)

SDO_modelEval <- lm(scale(diffScore) ~ scale(SDO) * as.factor(Condition) + study, data = df)
HH_modelEval <- lm(scale(diffScore) ~ scale(HH) * as.factor(Condition) + study, data = df)
Ex_modelEval <- lm(scale(diffScore) ~ scale(EX) * as.factor(Condition) + study, data = df)
BFI_modelEval <- lm(scale(diffScore) ~ scale(BFI_Open) * as.factor(Condition) + study, data = df)

# Print supplemental models
cat("Supplemental Models:\n")
cat("Supp1\n")
print(tidy(summary(Supp1)))
cat("\nSupp2\n")
print(tidy(summary(Supp2)))
cat("\nSDO_model\n")
print(tidy(summary(SDO_model)))
cat("\nHH_model\n")
print(tidy(summary(HH_model)))
cat("\nEX_model\n")
print(tidy(summary(EX_model)))
cat("\nBFI_Open_model\n")
print(tidy(summary(BFI_Open_model)))
cat("\nSDO_modelTrial\n")
print(tidy(summary(SDO_modelTrial)))
cat("\nHH_modelTrial\n")
print(tidy(summary(HH_modelTrial)))
cat("\nEX_modelTrial\n")
print(tidy(summary(EX_modelTrial)))
cat("\nBFI_modelTrial\n")
print(tidy(summary(BFI_modelTrial)))
cat("\nSDO_modelEval\n")
print(tidy(summary(SDO_modelEval)))
cat("\nHH_modelEval\n")
print(tidy(summary(HH_modelEval)))
cat("\nEx_modelEval\n")
print(tidy(summary(Ex_modelEval)))
cat("\nBFI_modelEval\n")
print(tidy(summary(BFI_modelEval)))
