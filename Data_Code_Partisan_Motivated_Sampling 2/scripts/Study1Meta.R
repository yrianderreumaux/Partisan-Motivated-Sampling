#Load Data
MetaStudy1 <- read.csv("data/MetaStudy1Data.csv", header=T, stringsAsFactors = FALSE, na.strings=c("","NA"))

#effects code
#####
MetaStudy1$Pol.AffilDUMMY <- as.factor(MetaStudy1$Pol.Affil)
contrasts(MetaStudy1$Pol.AffilDUMMY) <- contr.treatment(3, base = 2)

MetaStudy1$study_eff <- as.factor(MetaStudy1$study)
contrasts(MetaStudy1$study_eff) <- contr.sum(2)
#####

#Logistic regression models
#####
politicalAffil <- glm(firstSample~Pol.AffilDUMMY+study_eff, data = MetaStudy1, family=binomial(link='logit'))
summary(politicalAffil)
politicalAffil.OR <- exp(cbind(coef(politicalAffil), confint(politicalAffil)))#calc odds ratio

CSEModel <- glm(firstSample~fullCSE+study_eff, data = MetaStudy1, family=binomial(link='logit'))
summary(CSEModel)
CSEModel.OR <-exp(cbind(coef(CSEModel), confint(CSEModel)))

CSEFacets <- glm(firstSample~scale(SE_Public)+scale(SE_Importance)+scale(SE_Mem)+scale(SE_Private)+study_eff, data = MetaStudy1, family=binomial(link='logit'))
summary(CSEFacets)
CSEFacets.OR <-exp(cbind(coef(CSEFacets), confint(CSEFacets)))

CSEPublic <- glm(firstSample~scale(SE_Public)+study_eff, data = MetaStudy1, family=binomial(link='logit'))
summary(CSEPublic)
CSEPublic.OR <-exp(coefficients(CSEPublic))

CSE_Strength <- glm(firstSample~scale(SE_Public)+Pol.AffilDUMMY+study_eff, data = MetaStudy1, family=binomial(link='logit'))
summary(CSE_Strength)
CSE_Strength.OR <-exp(coefficients(CSE_Strength))
#####

#Visuals
#####
AffilPlot <- ggpredict(politicalAffil, terms = c("Pol.AffilDUMMY"))%>% plot()
AffilPlot <- AffilPlot  + labs(x = "Poltical Affiliation", y = "Probability of First Sample")+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+ theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
AffilPlot<- AffilPlot + scale_x_continuous(breaks = c(1,2,3),labels=c("Very", "Somewhat","Closer"))
AffilPlot<- AffilPlot + theme(axis.text.x = element_text(color="black", size=12), axis.text.y = element_text( color="black", size=12))
#ggsave("AffilPlot.png", width = 4, height = 4, dpi = 700)


CSEPlot <- ggpredict(CSEPublic, terms = c("SE_Public[4,5,6]"))%>% plot()
CSEPlot <- CSEPlot  + labs(x = "CSE [Public]", y = "Probability of Sampling from the Ingroup")+
  theme(axis.title.x = element_text(size = rel(1.4)))+
  theme(axis.title.y = element_text(size = rel(1.4)))+ theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  theme(plot.title = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=.5))
CSEPlot <- CSEPlot + scale_x_continuous(breaks = c(6,5,4),labels=c("-1 SD", "Mean","+1 SD"))
CSEPlot <- CSEPlot + theme(axis.text.x = element_text(color="black", size=12), axis.text.y = element_text( color="black", size=12)) 
#ggsave("CSEPlot.png", width = 4, height = 4, dpi = 700)

#####

#####Print the results in the order they appear in the manuscript
print('RESULTS FROM Meta 1')
print('Political Affilitaion Model')
print(politicalAffil)
print(politicalAffil.OR)

print('CSE Model')
print(CSEModel)
print(CSEModel.OR)

print('CSE all Facets')
print(CSEFacets)
print(CSEFacets.OR)

print('CSE Public Facet Model')
print(CSEPublic)
print(CSEPublic.OR)

print(AffilPlot)
print(CSEPlot)


