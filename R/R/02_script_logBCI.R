library(nlme)
library(emmeans)
library(MuMIn)
library(multcomp)
library(ggplot2)
library(forcats)

datam<-read.csv(file='input/body_condition.csv', h=T)

#prepare data for violin plot of BCI per sex and locality
datam$loc <- fct_relevel(datam$loc, "k", "b", "p")
quartilesf <- datam[datam$sex == "f", ]
quartilesm <- datam[datam$sex == "m", ]
means <- aggregate(BCI ~ sex+loc, datam, mean)
meansf <- means[means$sex == "f", ]
meansm <- means[means$sex == "m", ]

BCIplot<-ggplot(datam, aes(x = loc, y = BCI, fill = sex)) +
  geom_violin(color = NA) +
  geom_boxplot(position = position_nudge(x = 0.225), width = 0.1, data = quartilesm, fill = "white", color = "lightblue") +  # Add boxplot for quartiles
  geom_boxplot(position = position_nudge(x = -0.225), width = 0.1, data = quartilesf, fill = "white", color = "lightgreen") +  # Add boxplot for quartiles
  geom_point(position = position_nudge(x = 0.225), data = meansm, aes(y = BCI), shape = 3, size = 1, alpha = 0.7,  color = "red") +
  geom_point(position = position_nudge(x = -0.225), data = meansf, aes(y = BCI), shape = 3, size = 1, alpha = 0.7, color = "red") +
  scale_fill_manual(values = c("m" = "lightblue", "f" = "lightgreen") , labels = c("Male", "Female"))+
  scale_x_discrete(labels = c("Konjsko", "Beaches", "Plateau")) +
  theme_minimal() +
  labs(
    x = "Locality",
    y = "Body Condition Index",
    fill = "sex",) +
  theme(text = element_text(size = 10), axis.title = element_text(size=12), plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

library(Cairo)
ggsave("figures/BCI_plot.png", plot = BCIplot, width = 6, height = 4, dpi = 300, type = "cairo")
ggsave("figures/BCI_plot.pdf", plot = BCIplot, width = 6, height = 4)

datam$BCI_log <- log(datam$BCI)
global_model <- lme(log_BCI ~ loc * sex + loc * season + as.factor(year),
                    random = ~1 | ind,
                    data = datam,
                    weights = varIdent(form = ~1 | loc),
                    method = "ML")

# --- Perform automated model selection with dredge ---

options(na.action = "na.fail") # This is required for dredge()
model_selection <- dredge(global_model, extra = "R^2", fixed = "~1 | ind")
print(model_selection)
write.csv(as.data.frame(model_selection), "output/BCIvariation_model_selection_table.csv", row.names = FALSE)

# --- Get the best-fitting model ---
best_model <- get.models(model_selection, subset = 1)[[1]]
print(best_model)

# --- Fit the chosen best-fitting model with REML ---
best_model <- lme(log_BCI ~ loc * sex  + as.factor(year) + season,
                  random = ~1 | ind,
                  data = datam,
                  na.action = na.omit,
                  weights = varIdent(form = ~1 | loc),
                  method = "REML")

# --- Diagnostics for Model Assumptions ---
# Plot 1: Normality of Residuals (Q-Q Plot)
qqnorm(best_model$residuals)
qqline(best_model$residuals)

# Plot 2: Homoscedasticity (Residuals vs. Fitted Plot)
plot(best_model)


# --- Extract model results.
summary(best_model)
model_summary_df <- as.data.frame(summary(best_model)$tTable)
model_summary_df$term <- rownames(model_summary_df)
write.csv(model_summary_df, "output/BCI_best_model_summary.csv")

# After extracting the summary fit the best-fitting model with a new cohort variable that now represents all of the 'loc:sex' interactions
# in order to perform post-hoc tests on a single factor as 'glht' cannot directly handle an interaction term.
datam$cohort <- interaction(datam$loc, datam$sex, sep = " ", lex.ord = TRUE)
best_model_cohort <- lme(log_BCI ~ cohort + season + as.factor(year),
                  random = ~1 | ind,
                  data = datam,
                  na.action = na.omit,
                  weights = varIdent(form = ~1 | loc),
                  method = "REML")
posthoc_comparisons <- glht(best_model_cohort, mcp(cohort = "Tukey"))
summary(posthoc_comparisons)
confint(posthoc_comparisons)

em_model <- emmeans(best_model_cohort, "cohort")
posthoc_table <- pairs(em_model, adjust = "tukey")
posthoc_df <- as.data.frame(posthoc_table)
write.csv(posthoc_df, "output/cohort_BCI_posthocComparisons.csv", row.names = FALSE)

