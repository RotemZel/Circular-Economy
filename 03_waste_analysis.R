###############################################################################
##  03_waste_analysis.R
##
##  By-product exchange among micro-businesses: the regression analysis.
##
##  0. settings and packages
##  1. load the analysis table
##  2. bivariate screen: what goes with transfer, one variable at a time
##  3. material streams: which streams carry the association
##  4. logistic models of transfer (behaviour) and willingness (intention)
##  5. multilevel model: does the town explain anything?
##  6. intention-behaviour gap: willing but not doing
##  7. receiving side: willingness to take material in (rare outcome)
##  8. export the tables and the figure used in the paper
###############################################################################

################################################################################
#-------------------------- 0. settings and packages ---------------------------
################################################################################

library(broom)             # tidy() turns any model into a data frame
library(sandwich)          # HC1 robust standard errors
library(lmtest)            # coeftest() applies them
library(pROC)              # AUC
library(performance)       # McFadden R2, ICC, collinearity
library(marginaleffects)   # average marginal effects
library(caret)             # cross-validation
library(logistf)           # Firth penalised logit, for rare outcomes
library(lme4)              # multilevel models
library(blme)              # the same, with a weak prior (only 3 towns)
library(broom.mixed)       # tidy() for multilevel models
library(VGAM)              # bivariate probit
library(nnet)              # multinomial logit
library(mice)              # multiple imputation
library(ResourceSelection) # Hosmer-Lemeshow goodness of fit
library(psych)             # Cohen's kappa
library(ggthemes)          # colourblind-safe palette for the figure
library(modelsummary)      # writes the LaTeX tables
options(modelsummary_factory_latex = "kableExtra",
        modelsummary_format_numeric_latex = "plain")
library(here)              # project-relative file paths
library(tidyverse)         # LOADED LAST ON PURPOSE: mice and VGAM both define
# their own filter() and select(), which would otherwise hide the dplyr ones
# and make the pipelines below fail. Loading tidyverse last puts dplyr's
# versions on top. Check the startup messages: the masking warnings should
# name mice and VGAM, not dplyr.

seed <- 20250724
set.seed(seed)

dir.create(here("outputs"), showWarnings = FALSE)

################################################################################
#--------------------------- 1. load the analysis table ------------------------
################################################################################

if (!file.exists(here("inputs", "analysis_df.rds"))) {
  stop("inputs/analysis_df.rds not found under:\n  ", here(),
       "\nRun 01_make_waste_tidy.R and 02_build_analysis_frame.R first. If the",
       " folder above is not the project folder, run",
       " here::set_here(\"<full path to the analysis folder>\") once and",
       " restart R.")
}

businesses <- readRDS(here("inputs", "analysis_df.rds"))

print(paste("businesses surveyed:", nrow(businesses)))
glimpse(businesses)   # look at the columns before doing anything with them

# the four outcomes, and how many businesses answered each
businesses %>%
  summarise(across(c(transfer, willing_tr, receive, willing_rc),
                   list(yes      = ~ sum(.x, na.rm = TRUE),
                        answered = ~ sum(!is.na(.x))))) %>%
  print()

################################################################################
#--------- 2. bivariate screen: one variable at a time against transfer --------
################################################################################
# Fisher's exact test for the categorical candidates and a Wilcoxon rank-sum
# test for the continuous ones, then one Benjamini-Hochberg correction across
# the whole family

set.seed(seed)   # the Fisher p-values below are simulated, so they need a seed

screen_categorical <- businesses %>%
  select(settlement, sector, micro, owner, female, tertiary, pays, problem,
         reduces, epr, knows, organic, fibre, plastic, oils, durable, volume,
         transfer) %>%
  mutate(across(everything(), as.character)) %>%   # one common type to stack on
  pivot_longer(-transfer, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value), !is.na(transfer)) %>%
  group_by(variable) %>%
  summarise(test  = "Fisher exact",
            n     = n(),
            p_raw = fisher.test(table(value, transfer),
                                simulate.p.value = TRUE, B = 20000)$p.value,
            .groups = "drop")

screen_continuous <- businesses %>%
  select(firm_age, capacity, resp_age, n_streams, transfer) %>%
  pivot_longer(-transfer, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value), !is.na(transfer)) %>%
  group_by(variable) %>%
  summarise(test  = "Wilcoxon rank-sum",
            n     = n(),
            p_raw = wilcox.test(value ~ transfer)$p.value,
            .groups = "drop")

screen <- bind_rows(screen_categorical, screen_continuous) %>%
  mutate(p_fdr = p.adjust(p_raw, method = "BH")) %>%
  arrange(p_fdr)

print(screen, n = Inf)

screen %>%
  mutate(across(c(p_raw, p_fdr), ~ formatC(.x, format = "f", digits = 3))) %>%
  knitr::kable(format = "latex", booktabs = TRUE,
               caption = paste("Bivariate screen of candidate predictors of",
                               "by-product transfer, with false-discovery-rate",
                               "correction."),
               label   = "screen") %>%   # writes \label{tab:screen} in the caption
  writeLines(here("outputs", "tab_screen_raw.tex"))

################################################################################
#--------------- 3. material streams: which ones go with transfer --------------
################################################################################

streams <- businesses %>%
  pivot_longer(c(organic, oils, durable, plastic, fibre),
               names_to = "stream", values_to = "has") %>%
  group_by(stream) %>%
  summarise(n      = sum(has == 1),
            pct    = 100 * mean(has == 1),
            tr_yes = 100 * mean(transfer[has == 1], na.rm = TRUE),
            tr_no  = 100 * mean(transfer[has == 0], na.rm = TRUE),
            gap    = tr_yes - tr_no,
            p_raw  = fisher.test(table(has, transfer))$p.value,
            .groups = "drop") %>%
  mutate(p_fdr = p.adjust(p_raw, method = "BH")) %>%
  arrange(desc(gap))

print(streams, digits = 3)

################################################################################
#------------------- 4. logistic models: behaviour and intention ---------------
################################################################################
# Two outcomes:
#   transfer   = the business currently passes a by-product on (behaviour)
#   willing_tr = the business says it would be willing to (intention)
# Two specifications:
#   parsimonious = waste character and local knowledge only
#   full         = adds waste oils, town, size, age, sex, storage capacity

models <- list(
  
  "transfer parsimonious" = glm(
    transfer ~ sector + volume_n + organic + knows,
    data = businesses, family = binomial("logit")),
  
  "transfer full" = glm(
    transfer ~ sector + volume_n + organic + oils + knows +
      settlement + micro + firm_age_ln + female + capacity_ln,
    data = businesses, family = binomial("logit")),
  
  "willing_tr parsimonious" = glm(
    willing_tr ~ sector + volume_n + organic + knows,
    data = businesses, family = binomial("logit")),
  
  "willing_tr full" = glm(
    willing_tr ~ sector + volume_n + organic + oils + knows +
      settlement + micro + firm_age_ln + female + capacity_ln,
    data = businesses, family = binomial("logit"))
)

main_names <- names(models)   # section 5 adds the multilevel model to this list

coefficients <- data.frame()   # one row per term per model (HC1 robust)
fit_stats    <- data.frame()   # one row per model (n, events, AIC, R2, AUC)

for (this_model in main_names) {
  
  fitted_model <- models[[this_model]]
  
  # the outcome column actually used by this model, after missing rows dropped
  outcome_used <- model.response(model.frame(fitted_model))
  
  coefficients <- rbind(
    coefficients,
    tidy(coeftest(fitted_model, vcov. = vcovHC(fitted_model, "HC1"))) %>%
      mutate(model = this_model, odds_ratio = exp(estimate)))
  
  fit_stats <- rbind(
    fit_stats,
    data.frame(model  = this_model,
               n      = nobs(fitted_model),
               events = sum(outcome_used),
               aic    = AIC(fitted_model),
               r2     = r2_mcfadden(fitted_model)$R2,
               auc    = as.numeric(auc(roc(outcome_used, fitted(fitted_model),
                                           quiet = TRUE)))))
}

print(coefficients, digits = 3)
print(fit_stats, digits = 3)

#-------------------------------------------------------------------------------
# 4.1. does adding the other three streams change anything?
#-------------------------------------------------------------------------------

for (this_model in main_names) {
  
  as_reported  <- models[[this_model]]
  with_streams <- update(as_reported, . ~ . + fibre + plastic + durable)
  
  print(paste("---", this_model, "---"))
  print(anova(as_reported, with_streams, test = "LRT"))
  print(AIC(as_reported, with_streams))
}

#-------------------------------------------------------------------------------
# 4.2. diagnostics and robustness, transfer models
#-------------------------------------------------------------------------------

# average marginal effects: the change in probability, not in log-odds
print(avg_slopes(models[["transfer parsimonious"]], vcov = "HC1"), digits = 3)

# goodness of fit and collinearity
print(hoslem.test(model.response(model.frame(models[["transfer parsimonious"]])),
                  fitted(models[["transfer parsimonious"]]), g = 10))
print(check_collinearity(models[["transfer full"]]))

# link test for functional form
for (m in c("transfer parsimonious", "willing_tr parsimonious")) {
  lp <- predict(models[[m]])
  y  <- model.response(model.frame(models[[m]]))
  cat("\nLink test,", m, "\n")
  print(summary(glm(y ~ lp + I(lp^2), family = binomial))$coefficients)
}

# cross-validated AUC
set.seed(seed)
print(train(
  factor(transfer, levels = c(0, 1), labels = c("no", "yes")) ~
    sector + volume_n + organic + knows,
  data      = businesses %>%
    select(transfer, sector, volume_n, organic, knows) %>% drop_na(),
  method    = "glm",
  family    = binomial,
  metric    = "ROC",
  trControl = trainControl(method = "cv", number = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)))

# robustness 1: Firth penalised likelihood, less biased in small samples
print(logistf(transfer ~ sector + volume_n + organic + knows, data = businesses))

# robustness 2: multiple imputation, so that all 200 businesses contribute
print(summary(pool(with(
  mice(businesses %>% select(transfer, willing_tr, sector, volume_n, organic,
                             oils, knows, settlement, micro, firm_age_ln,
                             female, capacity_ln),
       m = 30, printFlag = FALSE, seed = seed),
  glm(transfer ~ sector + volume_n + organic + oils + knows + settlement +
        micro + firm_age_ln + female + capacity_ln, family = binomial))),
  conf.int = TRUE), digits = 3)

################################################################################
#--------------- 5. multilevel model: does the town explain anything? ----------
################################################################################

models[["multilevel"]] <- bglmer(
  transfer ~ volume_n + organic + oils + knows + (1 | settlement),
  data = businesses, family = binomial,
  cov.prior = gamma(shape = 2.5, rate = 0),
  control = glmerControl(optimizer = "bobyqa"))

print(tidy(models[["multilevel"]], effects = "fixed", conf.int = TRUE), n = Inf)
print(icc(models[["multilevel"]]))    # from the performance package
print(VarCorr(models[["multilevel"]]))

# the same model without the prior, to show the variance really is at zero
print(VarCorr(glmer(transfer ~ volume_n + organic + oils + knows + (1 | settlement),
                    data = businesses, family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))))

# is the clustering really about the town, or about the sector?
print(VarCorr(bglmer(transfer ~ volume_n + organic + oils + knows +
                       (1 | settlement) + (1 | sector),
                     data = businesses, family = binomial,
                     cov.prior = gamma(shape = 2.5, rate = 0),
                     control = glmerControl(optimizer = "bobyqa"))))

################################################################################
#--------------- 6. intention-behaviour gap: willing but not doing -------------
################################################################################

gap_table <- table(transfer = businesses$transfer, willing = businesses$willing_tr)
print(gap_table)

# McNemar asks whether the two off-diagonal cells are equally common: are there
# as many "doing but unwilling" businesses as "willing but not doing" ones?
print(mcnemar.test(gap_table, correct = FALSE))
print(binom.test(gap_table[1, 2], gap_table[1, 2] + gap_table[2, 1]))

# Cohen's kappa: agreement between intention and behaviour, beyond chance
print(cohen.kappa(gap_table))

# Bivariate probit
biprobit <- vglm(
  cbind(transfer, willing_tr) ~ sector + volume_n + organic + knows +
    settlement + micro,
  binom2.rho(zero = 3),
  data = businesses %>%
    select(transfer, willing_tr, sector, volume_n, organic, knows,
           settlement, micro) %>% drop_na())

print(summary(biprobit))

# VGAM reports that correlation on the rhobit scale
rhobit_value <- coef(biprobit)[grep(":3$", names(coef(biprobit)))]
print(c(rhobit_scale = rhobit_value,
        correlation  = (exp(rhobit_value) - 1) / (exp(rhobit_value) + 1)))

# The four-state typology (established / reluctant / latent / detached), to see
# what marks out the businesses that are willing but have not acted.
print(table(businesses$typology))
print(tidy(multinom(typology ~ sector + volume_n + organic + knows +
                      settlement + micro,
                    data = businesses, trace = FALSE)) %>%
        mutate(relative_risk = exp(estimate)) %>%
        select(y.level, term, relative_risk, p.value), n = Inf)

################################################################################
#------------- 7. receiving side: willingness to take material in --------------
################################################################################
# Only about 5% are willing to receive, so ordinary logit would be unreliable.
# Firth penalisation is designed for exactly this situation.

print(paste("willing to receive:", sum(businesses$willing_rc, na.rm = TRUE),
            "| actually receiving:", sum(businesses$receive, na.rm = TRUE)))

print(logistf(willing_rc ~ sector + micro + knows + volume_n, data = businesses))

# is willingness to receive related to already supplying?
print(fisher.test(table(businesses$transfer, businesses$willing_rc)))

################################################################################
#------------------------ 8. export tables and figure --------------------------
################################################################################
options(modelsummary_factory_latex = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

term_labels <- c(
  "(Intercept)"         = "Constant",
  "sectorFood"          = "Food-related",
  "sectorManufacturing" = "Manufacturing and materials",
  "sectorServices"      = "Vehicle and personal services",
  "volume_n"            = "Weekly waste volume (band, 1--3)",
  "organic"             = "Organic stream present",
  "oils"                = "Waste oil stream present",
  "knows"               = "Aware of other exchanging businesses",
  "settlementB"         = "Settlement B",
  "settlementC"         = "Settlement C",
  "micro"               = "Micro-business (4 or fewer employees)",
  "firm_age_ln"         = "Firm age (log years)",
  "female"              = "Respondent female",
  "capacity_ln"         = "Storage capacity (log litres)")

# Stars are written in plain text because the export escapes maths inside
# notes; the hand-tidied copies in tabs/ set them properly ($^{*}p<0.1$ etc.).
table_note <- paste("Heteroscedasticity-consistent (HC1) standard errors in parentheses.",
                    "Coefficients are on the log-odds scale.",
                    "* p<0.1; ** p<0.05; *** p<0.01.")

table_titles <- c(
  transfer   = "\\label{tab:logit-transfer}Logistic regression of current transfer of by-products to another business.",
  willing_tr = "\\label{tab:logit-willing}Logistic regression of stated willingness to supply by-products.")

table_files <- c(transfer   = here("outputs", "tab_model1.tex"),
                 willing_tr = here("outputs", "tab_model2.tex"))

# one table per outcome, parsimonious and full side by side
for (outcome in c("transfer", "willing_tr")) {
  
  these_models <- models[paste(outcome, c("parsimonious", "full"))]
  names(these_models) <- c("Parsimonious", "Full")
  
  these_stats <- fit_stats %>%
    filter(model %in% paste(outcome, c("parsimonious", "full"))) %>%
    arrange(match(model, paste(outcome, c("parsimonious", "full"))))
  
  extra_rows <- data.frame(
    term         = c("Events", "McFadden pseudo R2", "Area under ROC curve"),
    Parsimonious = c(as.character(these_stats$events[1]),
                     sprintf("%.3f", these_stats$r2[1]),
                     sprintf("%.3f", these_stats$auc[1])),
    Full         = c(as.character(these_stats$events[2]),
                     sprintf("%.3f", these_stats$r2[2]),
                     sprintf("%.3f", these_stats$auc[2])))
  
  modelsummary(these_models,
               vcov      = "HC1",
               estimate  = "{estimate}{stars} ({std.error})",
               statistic = NULL,
               coef_map  = term_labels,
               stars    = c("*" = .1, "**" = .05, "***" = .01),
               gof_map  = c("nobs", "aic"),
               add_rows = extra_rows,
               # [[ ]] drops the vector name: modelsummary silently drops
               # the whole caption (and with it the \label) when the title
               # is a named vector element
               title    = table_titles[[outcome]],
               notes    = table_note,
               escape   = FALSE,   # keep the LaTeX in the note (math) intact
               output   = table_files[[outcome]])
}

# multilevel table
modelsummary(list(Multilevel = models[["multilevel"]]),
             estimate  = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             coef_map = term_labels,
             stars    = c("*" = .1, "**" = .05, "***" = .01),
             gof_map  = c("nobs"),
             add_rows = data.frame(
               term = c("Settlement variance", "Intra-class correlation",
                        "Groups (settlements)"),
               Multilevel = c(
                 sprintf("%.3f", as.numeric(VarCorr(models[["multilevel"]])$settlement)),
                 sprintf("%.3f", icc(models[["multilevel"]])$ICC_adjusted),
                 as.character(ngrps(models[["multilevel"]])))),
             title = paste("\\label{tab:multilevel}Multilevel (random-intercept) logistic regression of",
                           "by-product transfer, with settlements as the grouping level."),
             notes = paste("Random-intercept logistic model estimated with a",
                           "weakly-informative prior on the between-settlement",
                           "standard deviation (Chung et al. 2013) to accommodate the",
                           "small number of clusters. Intra-class correlation on the",
                           "latent scale.",
                           "* p<0.1; ** p<0.05; *** p<0.01."),
             escape = FALSE,   # keep the math in the note intact
             output = here("outputs", "tab_multilevel.tex"))

#-------------------------------------------------------------------------------
# 8.1. figure: the raw data by sector, with the model's odds ratios on top
#-------------------------------------------------------------------------------
plot_data <- businesses %>%
  select(sector, transfer, willing_tr) %>%
  pivot_longer(-sector, names_to = "outcome", values_to = "y") %>%
  filter(!is.na(y)) %>%
  mutate(outcome = ifelse(outcome == "transfer",
                          "Transfer (behaviour)", "Willingness (intention)"))

# observed proportion and its 95% confidence interval, per sector and outcome
plot_proportions <- plot_data %>%
  group_by(sector, outcome) %>%
  summarise(yes = sum(y), n = n(), .groups = "drop") %>%
  rowwise() %>%
  mutate(proportion = yes / n,
         lo = prop.test(yes, n)$conf.int[1],
         hi = prop.test(yes, n)$conf.int[2]) %>%
  ungroup()

plot_labels <- coefficients %>%
  filter(model %in% c("transfer parsimonious", "willing_tr parsimonious"),
         str_starts(term, "sector")) %>%
  mutate(sector  = str_remove(term, "^sector"),
         outcome = ifelse(model == "transfer parsimonious",
                          "Transfer (behaviour)", "Willingness (intention)"),
         stars   = case_when(p.value < .01 ~ "***",
                             p.value < .05 ~ "**",
                             p.value < .1  ~ "*",
                             TRUE          ~ ""),
         label   = sprintf("OR = %.2f%s", odds_ratio, stars)) %>%
  select(sector, outcome, label) %>%
  # Retail is the reference category, so it has no odds ratio of its own
  bind_rows(data.frame(sector  = "Retail",
                       outcome = c("Transfer (behaviour)", "Willingness (intention)"),
                       label   = "(reference)")) %>%
  mutate(sector = factor(sector, levels = levels(businesses$sector)))

figure <- ggplot(plot_data, aes(x = sector, y = y, colour = sector)) +
  geom_jitter(width = 0.22, height = 0.06, alpha = 0.35, size = 1.6) +
  geom_errorbar(data = plot_proportions,
                aes(y = proportion, ymin = lo, ymax = hi),
                width = 0.15, linewidth = 0.7) +
  geom_point(data = plot_proportions, aes(y = proportion),
             size = 3.2, shape = 18) +
  geom_text(data = plot_labels, aes(y = 1.10, label = label),
            colour = "grey15", size = 3.1, fontface = "bold") +
  facet_wrap(~ outcome) +
  scale_colour_colorblind() +
  # stagger the sector labels over two rows so they cannot collide at 7 in
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("No (0%)", "25%", "50%", "75%", "Yes (100%)"),
                     limits = c(-0.12, 1.18), expand = c(0, 0)) +
  # no title or subtitle on the figure itself: the journal wants the caption
  # in the manuscript, not on the artwork
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(legend.position    = "none",
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text         = element_text(face = "bold"))

print(figure)

ggsave(here("outputs", "fig_coefficients.pdf"),  figure, width = 7, height = 4.4)
ggsave(here("outputs", "fig_coefficients.tiff"), figure, width = 7, height = 4.4, dpi = 300)

# save the data and the result tables used throughout
save(businesses, models, coefficients, fit_stats, screen, streams,
     file = here("outputs", "results_objects.RData"))

print("done")
