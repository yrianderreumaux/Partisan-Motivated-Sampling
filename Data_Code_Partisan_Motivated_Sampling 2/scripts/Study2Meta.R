# Load Data
MetaStudy1 <- read.csv("data/MetaStudy2Data.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Create long format data
meta2 <- MetaStudy1 %>%
  mutate_at(vars(In.Est, Out.Est), list(P.Estimate = .)) %>%
  select(Participant, Condition, P.Estimate, Eval_Group = c(In.Est, Out.Est),
         Valence, Group, Study, polAffil, firstSample) %>%
  mutate(
    Samp_GroupB_eff = Eval_Group,
    Samp_GroupB_dum = Eval_Group,
    Condition_c_eff = case_when(
      Condition == 1 ~ -1,
      Condition == 2 ~ 0,
      Condition == 3 ~ 1
    ),
    Condition_c_dum = as.factor(Condition),
    Valence_eff = as.factor(Valence),
    Valence_dum = as.factor(Valence),
    Group_eff = as.factor(Group),
    Group_dum = as.factor(Group),
    Study = as.factor(Study)
  ) %>%
  dummy_cols(select_columns = c("Condition_c_dum", "Samp_GroupB_dum", "Group_dum", "Valence_dum"), remove_selected_columns = TRUE)

# Subset outgroup first participants
Meta2EvalsOutgroup <- meta2[meta2$firstSample == 0, ]

# Eval Model
miniOutgroup <- lmer(P.Estimate ~ 1 + (1 | Participant), data = Meta2EvalsOutgroup,
                     control = lmerControl(check.conv.singular = .makeCC(action = "ignore", tol = 1e-4), optimizer = "Nelder_Mead"), REML = FALSE)
Meta2EvalsSampG <- update(miniOutgroup, . ~ . + Samp_GroupB_dum)
Meta2EvalsValence <- update(Meta2EvalsSampG, . ~ . + Valence_eff)
Meta2EvalsCond <- update(Meta2EvalsValence, . ~ . + Condition_c_eff)
Meta2EvalsPolAff <- update(Meta2EvalsCond, . ~ . + Group_eff)
Meta2EvalsStudy <- update(Meta2EvalsPolAff, . ~ . + Study)

Meta2EvalsSampGbyVal <- update(Meta2EvalsStudy, . ~ . + Samp_GroupB_dum:Valence_eff)
Meta2EvalsSampGbyAffil <- update(Meta2EvalsSampGbyVal, . ~ . + Samp_GroupB_dum:Group_eff)
Meta2EvalsSampGbyCond <- update(Meta2EvalsSampGbyAffil, . ~ . + Samp_GroupB_dum:Condition_c_eff)
Meta2EvalsValbyCond <- update(Meta2EvalsSampGbyCond, . ~ . + Valence_eff:Condition_c_eff)
Meta2EvalsValbyAffil <- update(Meta2EvalsValbyCond, . ~ . + Valence_eff:Group_eff)

Meta2EvalsValbyCondbyValence <- update(Meta2EvalsValbyAffil, . ~ . + Samp_GroupB_dum:Valence_eff:Condition_c_eff)
Meta2EvalsValbyCondbyAffil <- update(Meta2EvalsValbyCondbyValence, . ~ . + Samp_GroupB_dum:Valence_eff:Group_eff)
Meta2AffilbyValbyCond <- update(Meta2EvalsValbyAffil, . ~ . + Group_eff:Valence_eff)

# Eval Model effects
Meta2.effects <- anova(
  miniOutgroup, Meta2EvalsSampG, Meta2EvalsValence, Meta2EvalsCond,
  Meta2EvalsPolAff, Meta2EvalsStudy, Meta2EvalsSampGbyVal,
  Meta2EvalsSampGbyAffil, Meta2EvalsSampGbyCond, Meta2EvalsValbyCond,
  Meta2EvalsValbyAffil, Meta2EvalsValbyCondbyValence,
  Meta2EvalsValbyCondbyAffil, Meta2AffilbyValbyCond
)

# Contrasts
emm_meta2 <- emmeans(Meta2EvalsOutgroup, ~ Valence_eff:Samp_GroupB_dum)
Meta2.ValGroupContrast <- contrast(emm_meta2, simple = "Samp_GroupB_dum")
Meta2.ValGroupContrast_d <- eff_size(emm_meta2, sigma = sigma(Meta2EvalsOutgroup), edf = df.residual(Meta2EvalsOutgroup))

emm_meta2_cond <- emmeans(Meta2EvalsOutgroup, ~ Samp_GroupB_dum:Condition_c_eff)
Meta2.CondGroupContrast <- contrast(emm_meta2_cond, simple = "Samp_GroupB_dum")
Meta2.CondGroupContrast_d <- eff_size(emm_meta2_cond, sigma = sigma(Meta2EvalsOutgroup), edf = df.residual(Meta2EvalsOutgroup))

# Visuals
meta2_valencePlot <- ggplot(Meta2EvalsOutgroup, aes(x = Valence, y = P.Estimate, fill = Eval_Group)) +
  geom_hline(yintercept = 3, linetype = "solid") +
  geom_hline(yintercept = 102, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = P.Estimate - se, ymax = P.Estimate + se),
                width = 0.2,
                position = position_dodge(0.9)) +
  xlab("") +
  scale_x_discrete(labels = c("Negative", "Positive")) +
  ylab("") +
  scale_fill_manual(
    name = "Evaluated Group",
    values = c("In" = "grey", "Out" = "white"),
    labels = c("Ingroup", "Outgroup")
  ) +
  scale_y_continuous(limits = c(0, 80), breaks = c(50, 60, 70)) +
  theme_bw() +
  theme(
    legend.justification = c(0.05, 0.98),
    legend.position = c(2, 0.90),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(color = "black", size = 18, family = "Times New Roman"),
    axis.title.x = element_text(color = "black", size = 40, face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(color = "black", size = 40, face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 20, face = "plain", color = "black", family = "Times New Roman"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) +
  coord_cartesian(ylim = c(45, 75))

meta2_CondPlot <- ggplot(Meta2EvalsOutgroup, aes(x = Condition, y = P.Estimate, fill = Eval_Group)) +
  geom_hline(yintercept = 3, linetype = "solid") +
  geom_hline(yintercept = 102, linetype = "dashed") +
  geom_bar(position = position_dodge(), stat = "identity", color = "black") +
  geom_errorbar(aes(ymin = P.Estimate - se, ymax = P.Estimate + se),
                width = 0.2,
                position = position_dodge(0.9)) +
  xlab("") +
  scale_x_discrete(labels = c("Ingroup Worse", "No Difference", "Ingroup Better")) +
  ylab("") +
  scale_fill_manual(
    name = "Evaluated Group",
    values = c("In" = "grey", "Out" = "white"),
    labels = c("Ingroup", "Outgroup")
  ) +
  scale_y_continuous(limits = c(0, 80), breaks = c(50, 60, 70)) +
  theme_bw() +
  theme_linedraw() +
  theme(
    legend.justification = c(0.05, 0.98),
    legend.position = c(0.05, 0.98),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(color = "black", size = 18, family = "Times New Roman"),
    axis.title.x = element_text(color = "black", size = 40, face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(color = "black", size = 40, face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 17, face = "plain", color = "black", family = "Times New Roman"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1)
  ) +
  coord_cartesian(ylim = c(45, 75))

# Print the results
cat("RESULTS FROM Meta 2:\n")
print(Meta2.effects)

cat("\nContrasts and effect for valence by evaluated group:\n")
print(Meta2.ValGroupContrast)
print(Meta2.ValGroupContrast_d)

cat("\nContrasts and effect sizes for real group difference by evaluated group:\n")
print(Meta2.CondGroupContrast)
print(Meta2.CondGroupContrast_d)

# Plots
print("Meta 2 Valence Plot:")
print(meta2_valencePlot)

print("Meta 2 Condition Plot:")
print(meta2_CondPlot)
