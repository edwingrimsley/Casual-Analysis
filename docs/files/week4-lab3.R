# Lab 3: Randomized Experiments and Balance Tables
# Week 4 | QMSS 75000 / SOC 81900
# Companion to the revised Lab 3 handout. Base R only.
# Run one section at a time in RStudio. Internet access is needed for setup.
# This script contains worked code for Q1-Q9.

# Setup ------------------------------------------------------------------
base_url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/"
women <- read.csv(paste0(base_url, "PREDICTION/women.csv"),
                  stringsAsFactors = FALSE)
social <- read.csv(paste0(base_url, "CAUSALITY/social.csv"),
                   stringsAsFactors = FALSE)
str(women)
str(social)

# Check that the variables used below are present and complete.
stopifnot(all(c("GP", "village", "reserved", "female", "irrigation",
                "water") %in% names(women)))
stopifnot(all(c("sex", "yearofbirth", "primary2004", "messages",
                "primary2006", "hhsize") %in% names(social)))
stopifnot(!anyNA(women), !anyNA(social))

# Exercise 1: Female politicians and policy outcomes ----------------------
# Chattopadhyay and Duflo (2004).
# Assignment: council (GP). Each row in women: one village.

# Q1: Implementation check using one row per council ----------------------
stopifnot(all(table(women$GP) == 2))
gp <- aggregate(cbind(reserved, female, irrigation, water) ~ GP,
                data = women, FUN = mean)
stopifnot(all(gp$reserved %in% c(0, 1)),
          all(gp$female %in% c(0, 1)))

cat("\nQ1: Number of councils in each assignment group\n")
print(table(gp$reserved))
cat("\nShare with female heads, by reservation status\n")
print(tapply(gp$female, gp$reserved, mean))
print(with(gp, table(reserved, female)))
# Expected counts: 54 reserved councils and 107 unreserved councils.
# All 54 reserved councils have female heads; 8 unreserved councils do too.
# A table of women$reserved and women$female counts villages, not councils.

# Q2: Village-level differences in means ----------------------------------
# Sign convention throughout: reserved minus unreserved.
outcomes <- c("irrigation", "water")
village_effects <- sapply(outcomes, function(v) {
  mean(women[women$reserved == 1, v]) -
    mean(women[women$reserved == 0, v])
})
cat("\nQ2: Effect of reservation, facilities per village\n")
print(village_effects)
# Expected: irrigation about -0.3693; water about +9.2524.

# Q3: Welch tests at the village level ------------------------------------
# SOFTWARE DEMONSTRATION ONLY: these tests ignore council clustering.
# The formula interface orders groups as 0 then 1. Its contrast is therefore
# unreserved minus reserved, the reverse of our Q2 convention.
for (v in outcomes) {
  cat("\nQ3: Village-level Welch demonstration for", v, "\n")
  print(t.test(reformulate("reserved", response = v), data = women))
}

# Q4: OLS and equal-variance tests at the village level --------------------
# In unadjusted OLS with an intercept and binary treatment, the treatment
# coefficient equals the treated-minus-control difference in means.
# Default OLS SEs and the equal-variance t-test use the same pooled variance.
# Their two-sided p-values agree here; neither accounts for council clusters.
for (v in outcomes) {
  form <- reformulate("reserved", response = v)
  model <- lm(form, data = women)
  cat("\nQ4: Village-level OLS demonstration for", v, "\n")
  print(summary(model)$coefficients)
  print(t.test(form, data = women, var.equal = TRUE))
  stopifnot(isTRUE(all.equal(unname(coef(model)["reserved"]),
                            unname(village_effects[v]))))
}

# Q4b: Approximate inference using council means --------------------------
# Use these results for the Exercise 1 checkpoint.
# Equal weighting of council means preserves Q2's point estimates because
# every council contributes exactly two villages. Welch allows unequal
# outcome variances across arms. This comparison does not reconstruct any
# additional blocking or stratification in the original assignment design.
council_results <- do.call(rbind, lapply(outcomes, function(v) {
  treated <- gp[gp$reserved == 1, v]
  control <- gp[gp$reserved == 0, v]
  test <- t.test(treated, control)
  data.frame(outcome = v,
             effect = mean(treated) - mean(control),
             ci_lower = unname(test$conf.int[1]),
             ci_upper = unname(test$conf.int[2]),
             p_value = test$p.value)
}))
cat("\nQ4b: Council-level Welch results, reserved minus unreserved\n")
print(council_results, row.names = FALSE)
stopifnot(isTRUE(all.equal(council_results$effect,
                          unname(village_effects))))
# Expected two-sided p-values: irrigation about .730; water about .072.
# Interpret the confidence intervals and effect sizes. A p-value above .05
# does not establish that the treatment has no effect.

# Q5: Assignment versus actual female leadership --------------------------
# Q2 and Q4b estimate effects of reservation (intention to treat).
# Actual female leadership is not randomly assigned in unreserved councils.
# Comparing councils by actual leader gender can reintroduce selection bias.
leadership_rates <- tapply(gp$female, gp$reserved, mean)
first_stage <- unname(leadership_rates["1"] - leadership_rates["0"])
cat("\nQ5: Effect of reservation on female-leadership probability\n")
print(first_stage)
# Dividing an outcome ITT by this first stage requires further assumptions
# for a complier-effect interpretation: exclusion, monotonicity, relevance,
# random assignment, and appropriate consistency/no-interference conditions.
# Exclusion is substantive: could reservation affect spending through
# channels other than the head's gender? Randomization alone cannot answer.

# Exercise 2: Social pressure and voter turnout ---------------------------
# Gerber, Green, and Larimer (2008).
# Assignment: household. Each row in social: one registered voter.
# This teaching file has hhsize but no household ID. Household size and row
# order cannot recover household clusters. We report descriptive balance
# and point estimates, not voter-level p-values or standard errors.

# Q6: Arm sizes, turnout, and effects relative to control ------------------
cat("\nQ6: Number of voters by arm\n")
print(table(social$messages))
turnout <- tapply(social$primary2006, social$messages, mean)
turnout_effects <- turnout - turnout["Control"]
print(data.frame(arm = names(turnout),
                 turnout_share = as.numeric(turnout),
                 effect_percentage_points = 100 * as.numeric(turnout_effects)),
      row.names = FALSE)
# Neighbors versus control is approximately +8.13 percentage points.

# Q7: Descriptive balance -------------------------------------------------
stopifnot(all(social$sex %in% c("female", "male")))
social$female <- as.numeric(social$sex == "female")
social$age <- 2006 - social$yearofbirth
covs <- c("female", "age", "primary2004", "hhsize")
arms <- c("Civic Duty", "Hawthorne", "Neighbors")

bal <- aggregate(social[, covs],
                 by = list(arm = social$messages), FUN = mean)
bal_display <- bal
bal_display[, covs] <- round(bal_display[, covs], 3)
cat("\nQ7: Baseline means by arm\n")
print(bal_display, row.names = FALSE)

# SMD denominator: square root of the average of the two sample variances.
# This is descriptive, not a hypothesis test or proof of randomization.
balance_differences <- do.call(rbind, lapply(covs, function(v) {
  x0 <- social[[v]][social$messages == "Control"]
  do.call(rbind, lapply(arms, function(g) {
    x1 <- social[[v]][social$messages == g]
    gap <- mean(x1) - mean(x0)
    sd_pool <- sqrt((var(x1) + var(x0)) / 2)
    data.frame(covariate = v, arm = g, difference = gap,
               SMD = if (sd_pool > 0) gap / sd_pool else NA_real_)
  }))
}))
balance_display <- balance_differences
balance_display[, c("difference", "SMD")] <-
  round(balance_display[, c("difference", "SMD")], 4)
print(balance_display, row.names = FALSE)

# Discussion: measured balance cannot prove hidden covariates are balanced.
# Protection against unmeasured confounding comes from the assignment rule.
# If 12 valid tests each have a 5% false-positive rate and all nulls are true:
expected_false_positives <- 12 * 0.05
print(expected_false_positives)  # 0.6; not the probability of at least one.
# Independence is not required for this expected count.
# We omit voter-level t-tests and the default joint F-test because they
# ignore household assignment. A joint test cannot assess hidden covariates.

# Q8: Point estimates with and without baseline covariates -----------------
social$messages <- relevel(factor(social$messages), ref = "Control")
m1 <- lm(primary2006 ~ messages, data = social)
m2 <- lm(primary2006 ~ messages + female + age + primary2004 + hhsize,
         data = social)
treatment_terms <- grep("^messages", names(coef(m1)), value = TRUE)
effect_comparison <- cbind(unadjusted = coef(m1)[treatment_terms],
                           adjusted = coef(m2)[treatment_terms])
cat("\nQ8: Treatment effects in percentage points\n")
print(round(100 * effect_comparison, 3))
# Small coefficient changes are consistent with small observed imbalances.
# Baseline predictors can improve precision, but the default model SEs do
# not account for households. We therefore do not report them here.

# Unadjusted comparisons between treatment packages (percentage points).
package_contrasts <- c(
  Hawthorne_minus_Civic_Duty = unname(turnout["Hawthorne"] -
                                      turnout["Civic Duty"]),
  Neighbors_minus_Hawthorne = unname(turnout["Neighbors"] -
                                     turnout["Hawthorne"])
)
print(100 * package_contrasts)
# Discuss what changes between mailings. A contrast does not automatically
# identify a single psychological mechanism. Generalizing to a presidential
# election requires substantive reasoning beyond random assignment.

# Optional Q9: Fisher's exact randomization test ---------------------------
# Instructor-created example following Rosenbaum (2023), chapter 2,
# pp. 37-41. Exactly two of four participants are randomly assigned treatment.
# Under the sharp null, every person's outcome is unchanged by treatment.
y <- c(A = 8, B = 6, C = 4, D = 2)
assignments <- combn(seq_along(y), 2)
stat <- function(treated) mean(y[treated]) - mean(y[-treated])
null_stats <- apply(assignments, 2, stat)
observed <- stat(c(1, 2))  # A and B were treated.

randomization_table <- data.frame(
  treated = apply(assignments, 2, function(i) {
    paste(names(y)[i], collapse = ", ")
  }),
  difference = null_stats
)
cat("\nQ9: All six equally likely assignments under the sharp null\n")
print(randomization_table, row.names = FALSE)
exact_p <- mean(abs(null_stats) >= abs(observed))
cat("Observed difference:", observed, "\n")
cat("Exact two-sided p-value:", exact_p, "\n")
stopifnot(observed == 4, isTRUE(all.equal(exact_p, 1 / 3)))

# Two of six assignments produce an absolute difference of at least 4.
# The p-value is the probability of a result at least this extreme under
# the sharp null and this assignment design. It is NOT P(null is true).
# A zero ATE is weaker than the sharp null: individual effects may cancel.
# In cluster experiments, reassign entire clusters and preserve the actual
# blocking and allocation rules. Never shuffle individual rows arbitrarily.
