# 03_waste_analysis.R
#
# Runs the regression analysis of by-product exchange: bivariate tests of each
# variable against transfer, transfer rates by material group, logistic models
# of transfer (behaviour) and willingness to transfer (intention) with their
# diagnostics and robustness checks, the intention-behaviour gap and the
# receiving side. Prints each test and model result to the console. The seed,
# the test and model options (Fisher simulation and replicates,
# multiple-testing correction, McNemar continuity correction, Hosmer-Lemeshow
# groups, folds, imputations and the constant parameter of the bivariate
# probit) and the significance cut-offs are in settings.
#
# Reads:  inputs/analysis_df.rds (written by 02_build_analysis_frame.R)
# Writes: outputs/tab_screen_raw.tex
#         outputs/tab_model1.tex
#         outputs/tab_model2.tex
#         outputs/fig_coefficients.pdf
#         outputs/fig_coefficients.tiff
#         outputs/results_objects.RData (businesses, models, screen and
#         material_groups)

settings <- list(
  # seed of the Fisher simulation, the cross-validation folds, the imputations
  # and the jitter of the points in the figure
  seed              = 20250724,
  # simulate the Fisher p-values of tables larger than 2 x 2 (FALSE = exact)
  fisher_simulate   = TRUE,
  # Monte Carlo replicates of the simulated Fisher p-values
  fisher_replicates = 20000,
  # multiple-testing correction of the bivariate-test and material-group p-values
  fdr_method        = "BH",
  # continuity correction in McNemar's test of the intention-behaviour gap
  mcnemar_correct   = FALSE,
  # groups in the Hosmer-Lemeshow test
  hl_groups         = 10,
  # folds in the cross-validated AUC
  cv_folds          = 10,
  # imputed data sets in the multiple imputation
  imputations       = 30,
  # parameter of the bivariate probit held constant across businesses ("rho":
  # one error correlation for all businesses)
  biprobit_constant = "rho",
  # p-value cut-offs of the significance stars in the tables and the figure
  stars             = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
)

# 1. set the table format and load the analysis table ----

# modelsummary writes booktabs LaTeX tables with plain numbers
options(modelsummary_factory_latex        = "kableExtra",
        modelsummary_format_numeric_latex = "plain")

businesses <- readRDS(here::here("inputs", "analysis_df.rds"))

# 2. count the four outcomes ----
# The answers to each outcome: no (0), yes (1) and not answered (NA).

print(table(transfer   = businesses$transfer,   useNA = "ifany"))
print(table(willing_tr = businesses$willing_tr, useNA = "ifany"))
print(table(receive    = businesses$receive,    useNA = "ifany"))
print(table(willing_rc = businesses$willing_rc, useNA = "ifany"))

# 3. test each variable against transfer ----
# Fisher's exact test for the categorical variables and a Wilcoxon rank-sum
# test for the continuous ones, then one multiple-testing correction across all
# of them, with the method in settings. The variables of each kind are stacked
# in one column, with one row per business and variable (the categorical
# variables as text, so that they fit in one column), and each variable is
# tested on the businesses that answered it. The Fisher p-values of tables
# larger than 2 x 2 are simulated when settings say so, so the seed is set
# first.

set.seed(settings$seed)

screen <- dplyr::bind_rows(
  businesses |>
    dplyr::select(settlement, sector, micro, owner, female, tertiary, pays,
                  problem, reduces, epr, knows, organic, fibre, plastic, oils,
                  durable, volume_n, transfer) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::pivot_longer(-transfer, names_to = "variable", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(transfer)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(test    = "Fisher exact",
                     n       = dplyr::n(),
                     p_raw   = fisher.test(table(value, transfer),
                                           simulate.p.value = settings$fisher_simulate,
                                           B = settings$fisher_replicates)$p.value,
                     .groups = "drop"),
  businesses |>
    dplyr::select(firm_age, capacity, resp_age, n_groups, transfer) |>
    tidyr::pivot_longer(-transfer, names_to = "variable", values_to = "value") |>
    dplyr::filter(!is.na(value), !is.na(transfer)) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(test    = "Wilcoxon rank-sum",
                     n       = dplyr::n(),
                     p_raw   = wilcox.test(value ~ transfer)$p.value,
                     .groups = "drop")) |>
  dplyr::mutate(p_fdr = p.adjust(p_raw, method = settings$fdr_method)) |>
  dplyr::arrange(p_fdr)

print(screen, n = Inf)

# write the tests for Appendix A, with the p-values rounded to three decimals
screen |>
  dplyr::mutate(p_raw = format(round(p_raw, 3), nsmall = 3),
                p_fdr = format(round(p_fdr, 3), nsmall = 3)) |>
  knitr::kable(format = "latex", booktabs = TRUE,
               caption = paste("Bivariate tests of association between the",
                               "variables and by-product transfer, with",
                               "false-discovery-rate correction."),
               label   = "screen") |>
  writeLines(here::here("outputs", "tab_screen_raw.tex"))

# 4. compare transfer rates by material group ----
# One row per material group: how many businesses generate it (n, pct), the
# percentage transferring among those that do and those that do not, the gap
# between the two in percentage points, and the group's Fisher exact p-value
# from section 3, corrected across the five groups with the method in settings.

material_groups <- businesses |>
  tidyr::pivot_longer(c(organic, oils, durable, plastic, fibre),
                      names_to = "group", values_to = "has") |>
  dplyr::group_by(group) |>
  dplyr::summarise(n       = sum(has == 1),
                   pct     = 100 * mean(has == 1),
                   tr_yes  = 100 * mean(transfer[has == 1], na.rm = TRUE),
                   tr_no   = 100 * mean(transfer[has == 0], na.rm = TRUE),
                   gap     = tr_yes - tr_no,
                   .groups = "drop") |>
  dplyr::left_join(dplyr::select(screen, group = variable, p_raw), by = "group") |>
  dplyr::mutate(p_fdr = p.adjust(p_raw, method = settings$fdr_method)) |>
  dplyr::arrange(dplyr::desc(gap))

print(material_groups)

# 5. fit the logistic models of transfer and willingness ----
# Two outcomes: transfer (the business currently passes a by-product on, the
# behaviour) and willing_tr (it says it would be willing to, the intention).
# Two specifications: parsimonious (sector, waste character and local
# knowledge) and full (adds waste oils, town, size, firm age, respondent sex
# and storage capacity).

models <- list(
  transfer_parsimonious = glm(transfer ~ sector + volume_n + organic + knows,
                              family = binomial, data = businesses),
  transfer_full         = glm(transfer ~ sector + volume_n + organic + oils + knows +
                                settlement + micro + firm_age_ln + female + capacity_ln,
                              family = binomial, data = businesses),
  willing_parsimonious  = glm(willing_tr ~ sector + volume_n + organic + knows,
                              family = binomial, data = businesses),
  willing_full          = glm(willing_tr ~ sector + volume_n + organic + oils + knows +
                                settlement + micro + firm_age_ln + female + capacity_ln,
                              family = binomial, data = businesses))

# the coefficients of each model with their standard errors and p-values, and
# the same coefficients as odds ratios
print(summary(models$transfer_parsimonious))
print(exp(coef(models$transfer_parsimonious)))
print(summary(models$transfer_full))
print(exp(coef(models$transfer_full)))
print(summary(models$willing_parsimonious))
print(exp(coef(models$willing_parsimonious)))
print(summary(models$willing_full))
print(exp(coef(models$willing_full)))

# the fit statistics of the four models: observations, events, AIC, McFadden
# pseudo-R2 and the area under the ROC curve
fit_statistics <- data.frame(
  n      = c(nobs(models$transfer_parsimonious), nobs(models$transfer_full),
             nobs(models$willing_parsimonious),  nobs(models$willing_full)),
  events = c(sum(models$transfer_parsimonious$y), sum(models$transfer_full$y),
             sum(models$willing_parsimonious$y),  sum(models$willing_full$y)),
  aic    = c(AIC(models$transfer_parsimonious), AIC(models$transfer_full),
             AIC(models$willing_parsimonious),  AIC(models$willing_full)),
  r2     = c(performance::r2_mcfadden(models$transfer_parsimonious)$R2,
             performance::r2_mcfadden(models$transfer_full)$R2,
             performance::r2_mcfadden(models$willing_parsimonious)$R2,
             performance::r2_mcfadden(models$willing_full)$R2),
  auc    = c(pROC::auc(models$transfer_parsimonious$y, fitted(models$transfer_parsimonious),
                       quiet = TRUE),
             pROC::auc(models$transfer_full$y, fitted(models$transfer_full), quiet = TRUE),
             pROC::auc(models$willing_parsimonious$y, fitted(models$willing_parsimonious),
                       quiet = TRUE),
             pROC::auc(models$willing_full$y, fitted(models$willing_full), quiet = TRUE)),
  row.names = c("transfer_parsimonious", "transfer_full",
                "willing_parsimonious", "willing_full"))

print(fit_statistics, digits = 3)

# 6. test whether fibre, plastic and durable improve the models, and whether town does ----
# Each model is refitted with fibre, plastic and durable added. For each model,
# the likelihood-ratio test of the three groups, the AIC of both versions and
# the coefficients of the refitted model are printed. A large p-value and a
# higher AIC mean the three groups are not worth keeping. Each full model is
# then refitted without town, and the likelihood-ratio test of the two town
# effects is printed.

models_with_groups <- list(
  transfer_parsimonious = update(models$transfer_parsimonious, . ~ . + fibre + plastic + durable),
  transfer_full         = update(models$transfer_full,         . ~ . + fibre + plastic + durable),
  willing_parsimonious  = update(models$willing_parsimonious,  . ~ . + fibre + plastic + durable),
  willing_full          = update(models$willing_full,          . ~ . + fibre + plastic + durable))

print(anova(models$transfer_parsimonious, models_with_groups$transfer_parsimonious, test = "LRT"))
print(anova(models$transfer_full,         models_with_groups$transfer_full,         test = "LRT"))
print(anova(models$willing_parsimonious,  models_with_groups$willing_parsimonious,  test = "LRT"))
print(anova(models$willing_full,          models_with_groups$willing_full,          test = "LRT"))

print(AIC(models$transfer_parsimonious, models_with_groups$transfer_parsimonious))
print(AIC(models$transfer_full,         models_with_groups$transfer_full))
print(AIC(models$willing_parsimonious,  models_with_groups$willing_parsimonious))
print(AIC(models$willing_full,          models_with_groups$willing_full))

print(summary(models_with_groups$transfer_parsimonious))
print(summary(models_with_groups$transfer_full))
print(summary(models_with_groups$willing_parsimonious))
print(summary(models_with_groups$willing_full))

print(anova(update(models$transfer_full, . ~ . - settlement), models$transfer_full, test = "LRT"))
print(anova(update(models$willing_full,  . ~ . - settlement), models$willing_full,  test = "LRT"))

# 7. check the models: marginal effects, fit, link test, cross-validated AUC ----

# average marginal effects of the parsimonious transfer model: the change in
# probability, not in log-odds
print(marginaleffects::avg_slopes(models$transfer_parsimonious), digits = 3)

# Hosmer-Lemeshow goodness of fit of the parsimonious transfer model
print(ResourceSelection::hoslem.test(models$transfer_parsimonious$y,
                                     fitted(models$transfer_parsimonious),
                                     g = settings$hl_groups))

# link test of functional form for the two parsimonious models: the model's
# outcome regressed on its linear predictor and the square of it; a small
# p-value on the squared term points to a misspecified model
glm(y ~ lp + I(lp^2), family = binomial,
    data = data.frame(y  = models$transfer_parsimonious$y,
                      lp = predict(models$transfer_parsimonious))) |>
  summary() |>
  coef() |>
  print()

glm(y ~ lp + I(lp^2), family = binomial,
    data = data.frame(y  = models$willing_parsimonious$y,
                      lp = predict(models$willing_parsimonious))) |>
  summary() |>
  coef() |>
  print()

# cross-validated AUC of the parsimonious transfer model on the businesses that
# answered all of its variables, with the outcome labelled no and yes as the
# caret package requires; the seed is set first because the folds are drawn at
# random
set.seed(settings$seed)
caret::train(factor(transfer, levels = c(0, 1), labels = c("no", "yes")) ~
               sector + volume_n + organic + knows,
             data      = tidyr::drop_na(businesses, transfer, sector, volume_n, organic, knows),
             method    = "glm",
             family    = binomial,
             metric    = "ROC",
             trControl = caret::trainControl(method = "cv", number = settings$cv_folds,
                                             classProbs = TRUE,
                                             summaryFunction = caret::twoClassSummary)) |>
  print()

# 8. refit the models with Firth's penalty and with imputed data ----

# the two transfer models refitted with Firth penalised likelihood: each
# coefficient with its profile-likelihood confidence interval and p-value
logistf::logistf(formula(models$transfer_parsimonious), data = businesses) |>
  summary()
logistf::logistf(formula(models$transfer_full), data = businesses) |>
  summary()

# the four models pooled over multiply imputed data sets, so that all 200
# businesses contribute: the data sets are imputed once, each model is fitted to
# every completed data set, and the fits are pooled with Rubin's rules; the
# pooled estimates are printed with their confidence intervals
imputed <- businesses |>
  dplyr::select(transfer, willing_tr, sector, volume_n, organic, oils, knows,
                settlement, micro, firm_age_ln, female, capacity_ln) |>
  mice::mice(m = settings$imputations, printFlag = FALSE, seed = settings$seed)

with(imputed, glm(transfer ~ sector + volume_n + organic + knows, family = binomial)) |>
  mice::pool() |>
  summary(conf.int = TRUE) |>
  print(digits = 3)

with(imputed, glm(transfer ~ sector + volume_n + organic + oils + knows +
                    settlement + micro + firm_age_ln + female + capacity_ln,
                  family = binomial)) |>
  mice::pool() |>
  summary(conf.int = TRUE) |>
  print(digits = 3)

with(imputed, glm(willing_tr ~ sector + volume_n + organic + knows, family = binomial)) |>
  mice::pool() |>
  summary(conf.int = TRUE) |>
  print(digits = 3)

with(imputed, glm(willing_tr ~ sector + volume_n + organic + oils + knows +
                    settlement + micro + firm_age_ln + female + capacity_ln,
                  family = binomial)) |>
  mice::pool() |>
  summary(conf.int = TRUE) |>
  print(digits = 3)

# 9. compare intention with behaviour: willing but not doing ----

gap_table <- table(transfer = businesses$transfer, willing = businesses$willing_tr)
print(gap_table)

# McNemar asks whether the two off-diagonal cells are equally common: are there
# as many "doing but unwilling" businesses as "willing but not doing" ones?
print(mcnemar.test(gap_table, correct = settings$mcnemar_correct))

# bivariate probit on the businesses that answered all of its variables: fits
# both outcomes together and estimates the correlation between their errors,
# with the parameter held constant across businesses as set in settings
biprobit <- VGAM::vglm(cbind(transfer, willing_tr) ~ sector + volume_n + organic + knows +
                         settlement + micro,
                       VGAM::binom2.rho(zero = settings$biprobit_constant),
                       data = tidyr::drop_na(businesses, transfer, willing_tr, sector,
                                             volume_n, organic, knows, settlement, micro))

print(summary(biprobit))

# the error correlation between the two outcomes: VGAM reports it on its own
# rhobit scale, and rhobitlink converts it back to a correlation
print(VGAM::rhobitlink(coef(biprobit)[["(Intercept):3"]], inverse = TRUE))

# a multinomial logit of the four-state typology on the predictors of the
# bivariate probit, printed as the relative risk of each state against the
# reference state set in script 02; the four states are the four cells of
# gap_table (established: transfers and willing; reluctant: transfers but not
# willing; latent: willing but not transferring; detached: neither)
nnet::multinom(typology ~ sector + volume_n + organic + knows + settlement + micro,
               data = businesses, trace = FALSE) |>
  broom::tidy() |>
  dplyr::mutate(relative_risk = exp(estimate)) |>
  dplyr::select(y.level, term, relative_risk, p.value) |>
  print(n = Inf)

# 10. model willingness to take material in ----

# the Firth-penalised model of willingness to receive, with profile-likelihood
# p-values
logistf::logistf(willing_rc ~ sector + micro + knows + volume_n, data = businesses) |>
  summary()

# is willingness to receive related to already supplying?
print(fisher.test(table(businesses$transfer, businesses$willing_rc)))

# the reasons coded in script 01 for not being willing to take material in,
# counted by willingness to receive
print(table(reason = businesses$refusal_reason, willing_rc = businesses$willing_rc,
            useNA = "ifany"))

# 11. write the regression tables ----
# modelsummary writes each table from the two models of an outcome, with the
# estimate and its standard error on one line, and the observations and the AIC
# below the coefficients. The events, McFadden pseudo-R2 and area under the ROC
# curve from section 5 are added below these, rounded as the tables show them.
# Each title starts with the table's \label, and each note ends with the
# meaning of the stars.

# the formatting shared by the two regression tables: the labels of the model
# terms in the order the tables show them, the fit statistics taken from the
# models and the note, which ends with the meaning of the stars
table_format <- list(
  term_labels = c(
    "(Intercept)"         = "Constant",
    "sectorFood"          = "Food sector",
    "sectorManufacturing" = "Manufacturing sector",
    "sectorServices"      = "Services sector",
    "volume_n"            = "Weekly waste volume (band, 1--3)",
    "organic"             = "Organic residues",
    "oils"                = "Waste oils",
    "knows"               = "Awareness of local exchange",
    "settlementB"         = "Town B",
    "settlementC"         = "Town C",
    "micro"               = "Micro-business (4 or fewer employees)",
    "firm_age_ln"         = "Firm age (log years)",
    "female"              = "Respondent sex (female)",
    "capacity_ln"         = "Storage capacity (log litres)"),
  logit_gof   = data.frame(raw   = c("nobs", "aic"),
                           clean = c("Observations", "AIC"),
                           fmt   = c(0, 1)),
  logit_note  = paste0("Standard errors in parentheses. ",
                       "Coefficients are on the log-odds scale. ",
                       "AIC: Akaike information criterion; ROC: receiver operating ",
                       "characteristic. Reference categories: retail sector and town A. ",
                       paste0(names(settings$stars), " p<", settings$stars, collapse = "; "),
                       "."))

# the transfer models: the parsimonious and the full model side by side, with
# the coefficients and, below them, the observations, AIC, events, McFadden R2
# and AUC
modelsummary::modelsummary(
  list(Parsimonious = models$transfer_parsimonious,
       Full         = models$transfer_full),
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map  = table_format$term_labels,
  stars     = settings$stars,
  gof_map   = table_format$logit_gof,
  add_rows  = data.frame(
    term         = c("Events", "McFadden pseudo-$R^2$", "Area under ROC curve"),
    Parsimonious = c(format(fit_statistics["transfer_parsimonious", "events"]),
                     format(round(fit_statistics["transfer_parsimonious", "r2"], 3), nsmall = 3),
                     format(round(fit_statistics["transfer_parsimonious", "auc"], 3), nsmall = 3)),
    Full         = c(format(fit_statistics["transfer_full", "events"]),
                     format(round(fit_statistics["transfer_full", "r2"], 3), nsmall = 3),
                     format(round(fit_statistics["transfer_full", "auc"], 3), nsmall = 3))),
  title     = paste("\\label{tab:logit-transfer}Logistic regression results for current",
                    "transfer of by-products: parsimonious and full specifications."),
  notes     = table_format$logit_note,
  escape    = FALSE,
  output    = here::here("outputs", "tab_model1.tex"))

# the same table for the willingness models
modelsummary::modelsummary(
  list(Parsimonious = models$willing_parsimonious,
       Full         = models$willing_full),
  estimate  = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_map  = table_format$term_labels,
  stars     = settings$stars,
  gof_map   = table_format$logit_gof,
  add_rows  = data.frame(
    term         = c("Events", "McFadden pseudo-$R^2$", "Area under ROC curve"),
    Parsimonious = c(format(fit_statistics["willing_parsimonious", "events"]),
                     format(round(fit_statistics["willing_parsimonious", "r2"], 3), nsmall = 3),
                     format(round(fit_statistics["willing_parsimonious", "auc"], 3), nsmall = 3)),
    Full         = c(format(fit_statistics["willing_full", "events"]),
                     format(round(fit_statistics["willing_full", "r2"], 3), nsmall = 3),
                     format(round(fit_statistics["willing_full", "auc"], 3), nsmall = 3))),
  title     = paste("\\label{tab:logit-willing}Logistic regression results for stated",
                    "willingness to supply by-products: parsimonious and full specifications."),
  notes     = table_format$logit_note,
  escape    = FALSE,
  output    = here::here("outputs", "tab_model2.tex"))

# 12. draw the figure: the raw data by sector, with the odds ratios on top ----

# the odds ratio of each sector in the two parsimonious models, with its
# significance stars, as a label
figure_labels <- dplyr::bind_rows(
  transfer   = broom::tidy(models$transfer_parsimonious, exponentiate = TRUE),
  willing_tr = broom::tidy(models$willing_parsimonious, exponentiate = TRUE),
  .id = "outcome") |>
  dplyr::filter(stringr::str_detect(term, "sector")) |>
  dplyr::mutate(sector = factor(stringr::str_remove(term, "sector"),
                                levels = levels(businesses$sector)),
                stars  = dplyr::case_when(p.value < settings$stars[["***"]] ~ "***",
                                          p.value < settings$stars[["**"]]  ~ "**",
                                          p.value < settings$stars[["*"]]   ~ "*",
                                          TRUE                              ~ ""),
                label  = paste0("OR = ", format(round(estimate, 2), nsmall = 2), stars)) |>
  dplyr::select(sector, outcome, label)

# one point per business and outcome
figure_points <- businesses |>
  dplyr::select(sector, transfer, willing_tr) |>
  tidyr::pivot_longer(-sector, names_to = "outcome", values_to = "y") |>
  dplyr::filter(!is.na(y))

# one row per sector and outcome: the observed proportion with its 95%
# confidence interval from prop.test, and the odds-ratio label; the reference
# sector has no odds ratio and is labelled as the reference
figure_proportions <- figure_points |>
  dplyr::group_by(sector, outcome) |>
  dplyr::summarise(proportion = sum(y) / dplyr::n(),
                   lo         = prop.test(sum(y), dplyr::n())$conf.int[1],
                   hi         = prop.test(sum(y), dplyr::n())$conf.int[2],
                   .groups    = "drop") |>
  dplyr::left_join(figure_labels, by = c("sector", "outcome")) |>
  tidyr::replace_na(list(label = "(reference)"))

# the points, the observed proportions with their intervals, and the odds-ratio
# labels
figure <- ggplot2::ggplot(figure_points, ggplot2::aes(x = sector, y = y, colour = sector)) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.22, height = 0.06,
                                                           seed = settings$seed),
                       alpha = 0.35, size = 1.6) +
  ggplot2::geom_errorbar(data = figure_proportions,
                         ggplot2::aes(y = proportion, ymin = lo, ymax = hi),
                         width = 0.15, linewidth = 0.7) +
  ggplot2::geom_point(data = figure_proportions, ggplot2::aes(y = proportion),
                      size = 3.2, shape = 18) +
  ggplot2::geom_text(data = figure_proportions,
                     ggplot2::aes(y = 1.10, label = label),
                     colour = "grey15", size = 3.1, fontface = "bold") +
  ggplot2::facet_wrap(~ outcome,
                      labeller = ggplot2::as_labeller(c(transfer   = "Transfer",
                                                        willing_tr = "Willingness to supply"))) +
  # one colour per sector, from a colour-blind-safe palette
  ggplot2::scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73")) +
  # sector labels on two staggered rows
  ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2)) +
  ggplot2::scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                              labels = c("No (0%)", "25%", "50%", "75%", "Yes (100%)"),
                              limits = c(-0.12, 1.18), expand = c(0, 0)) +
  # no axis titles
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(legend.position    = "none",
                 panel.grid.minor   = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(),
                 strip.text         = ggplot2::element_text(face = "bold"))

ggplot2::ggsave(here::here("outputs", "fig_coefficients.pdf"), figure,
                width = 7, height = 4.4)
ggplot2::ggsave(here::here("outputs", "fig_coefficients.tiff"), figure,
                width = 7, height = 4.4, dpi = 300)

# 13. save the data, the models and the result tables ----

save(businesses, models, screen, material_groups,
     file = here::here("outputs", "results_objects.RData"))
