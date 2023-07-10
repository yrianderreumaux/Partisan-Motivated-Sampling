# Load Data
MetaStudy1 <- read.csv("data/MetaStudy1Data.csv", header = T, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Effects code
MetaStudy1$Pol.AffilDUMMY <- as.factor(MetaStudy1$Pol.Affil)
contrasts(MetaStudy1$Pol.AffilDUMMY) <- contr.treatment(3, base = 2)

MetaStudy1$study_eff <- as.factor(MetaStudy1$study)
contrasts(MetaStudy1$study_eff) <- contr.sum(2)

# Logistic regression models
politicalAffil <- glm(firstSample ~ Pol.AffilDUMMY + study_eff, data = MetaStudy1, family = binomial(link = 'logit'))
summary(politicalAffil)
politicalAffil.OR <- exp(cbind(coef(politicalAffil), confint(politicalAffil))) # Calculate odds ratio

CSEModel <- glm(firstSample ~ fullCSE + study_eff, data = MetaStudy1, family = binomial(link = 'logit'))
summary(CSEModel)
CSEModel.OR <- exp(cbind(coef(CSEModel), confint(CSEModel)))

CSEFacets <- glm(firstSample ~ scale(SE_Public) + scale(SE_Importance) + scale(SE_Mem) + scale(SE_Private) + study_eff, data = MetaStudy1, family = binomial(link = 'logit'))
summary(CSEFacets)
CSEFacets.OR <- exp(cbind(coef(CSEFacets), confint(CSEFacets)))

CSEPublic <- glm(firstSample ~ scale(SE_Public) + study_eff, data = MetaStudy1, family = binomial(link = 'logit'))
summary(CSEPublic)
CSEPublic.OR <- exp(coefficients(CSEPublic))

CSE_Strength <- glm(firstSample ~ scale(SE_Public) + Pol.AffilDUMMY + study_eff, data = MetaStudy1, family = binomial(link = 'logit'))
summary(CSE_Strength)
CSE_Strength.OR <- exp(coefficients(CSE_Strength))

# Visuals
AffilPlot <- ggpredict(politicalAffil, terms = c("Pol.AffilDUMMY")) %>% plot() +
  labs(x = "Political Affiliation", y = "Probability of First Sample") +
  theme(axis.title.x = element_text(size = rel(1.4))) +
  theme(axis.title.y = element_text(size = rel(1.4))) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.title = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = .5)) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Very", "Somewhat", "Closer")) +
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

CSEPlot <- ggpredict(CSEPublic, terms = c("SE_Public[4,5,6]")) %>% plot() +
  labs(x = "CSE [Public]", y = "Probability of Sampling from the Ingroup") +
  theme(axis.title.x = element_text(size = rel(1.4))) +
  theme(axis.title.y = element_text(size = rel(1.4))) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.title = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = .5)) +
  scale_x_continuous(breaks = c(6, 5, 4), labels = c("-1 SD", "Mean", "+1 SD")) +
  theme(axis.text.x = element_text(color = "black", size = 12), axis.text.y = element_text(color = "black", size = 12))

# Print the results
cat("RESULTS FROM Meta 1\n")

cat("Political Affiliation Model:\n")
print(politicalAffil)
print(politicalAffil.OR)

cat("CSE Model:\n")
print(CSEModel)
print(CSEModel.OR)

cat("CSE all Facets:\n")
print(CSEFacets)
print(CSEFacets.OR)

cat("CSE Public Facet Model:\n")
print(CSEPublic)
print(CSEPublic.OR)

print(AffilPlot)
print(CSEPlot)