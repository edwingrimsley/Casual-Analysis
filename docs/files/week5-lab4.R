###############################################################################
# Lab 4: Ban the Box, Criminal Records, and Racial Discrimination
# Replicating and extending Agan & Starr (2018, QJE)
#
# Causal Analysis — CUNY Graduate Center, QMSS
#
# What this script does
#   Part 1  Sets up the data and checks that the randomization worked
#   Part 2  Replicates Tables I–III (descriptives and main effects)
#   Part 3  Replicates Figure I and Table IV (difference-in-differences)
#   Part 4  Replicates Figure II and Table V (triple differences)
#   Part 5  Replicates Table VII (a placebo characteristic: GED)
#   Part 6  New questions the paper doesn't ask
#
# Data: AganStarrQJEData.dta (the authors' replication file). Put it in the
# same folder as this script, or change DATA_PATH below.
#
# Translating from the authors' Stata code:
#   Stata:  reg y x1 x2 i.center if <condition>, cl(chain_id)
#   R:      lm_robust(y ~ x1 + x2 + factor(center),
#                     data = filter(df, <condition>),
#                     clusters = chain_id, se_type = "stata")
#   se_type = "stata" uses the same small-sample correction as Stata's
#   cl(), so the standard errors match the published tables exactly.
#
#   If you prefer fixest, the equivalent is:
#   feols(y ~ x1 + x2 | center, data = ..., cluster = ~chain_id)
###############################################################################


## ---- setup ----
# install.packages(c("haven", "dplyr", "tidyr", "ggplot2", "estimatr"))
library(haven)     # read Stata .dta files
library(dplyr)     # data manipulation
library(tidyr)     # reshaping
library(ggplot2)   # figures
library(estimatr)  # lm_robust(): OLS with clustered standard errors

DATA_PATH <- "AganStarrQJEData.dta"

# A small helper: print just the coefficients we care about,
# with clustered SEs and p-values, rounded to 3 decimals.
show <- function(model, terms) {
  out <- data.frame(
    term = terms,
    estimate = coef(model)[terms],
    std.error = model$std.error[terms],
    p.value = model$p.value[terms],
    row.names = NULL
  )
  out[, -1] <- round(out[, -1], 3)
  out$N <- nobs(model)
  out
}


###############################################################################
# PART 1. THE DATA AND THE DESIGN
###############################################################################

## ---- load ----
raw <- read_dta(DATA_PATH)

# The .dta file carries Stata value labels. zap_labels() turns those
# columns into plain numbers so they behave normally in R.
raw <- zap_labels(raw)

dim(raw)   # 14,813 applications, 62 variables

# Each row is ONE job application sent by ONE fictitious applicant to ONE store.
# Look at the variable labels the authors attached:
labels <- sapply(read_dta(DATA_PATH, n_max = 1),
                 function(x) if (is.null(attr(x, "label"))) "" else attr(x, "label"))
head(data.frame(variable = names(labels), label = labels, row.names = NULL), 25)

## ---- samples ----
# remover is the key treatment variable, coded at the store level:
#    1 = store had the box before BTB and removed it after ("box remover")
#    0 = store's application did not change (mostly never had a box)
#   -1 = store ADDED the box after BTB (an administrative oddity; excluded)
table(raw$remover)

# The main analysis sample drops the -1 stores: 14,637 applications
df <- filter(raw, remover != -1)
nrow(df)

# The "balanced" sample keeps only stores that got all four applications:
# one Black/white pair before BTB and one pair after.
bal <- filter(df, balanced == 1)
nrow(bal)    # 11,188

# Key variables we will use throughout
df |>
  select(response, white, crime, ged, empgap, crimbox, pre, post,
         remover, nj, chain_id, storeid, center) |>
  summary()

## ---- randomization-check ----
# Race, criminal record, GED, and employment gap were RANDOMLY assigned to
# applicants. If the randomization worked, these traits should be unrelated
# to each other and to the store characteristics.

# (a) Is "white" balanced across the other randomized traits?
df |>
  group_by(white) |>
  summarise(n = n(),
            crime = mean(crime),
            ged = mean(ged),
            empgap = mean(empgap),
            nj = mean(nj),
            pre = mean(pre))

# (b) A joint test: regress race on everything else. If randomization worked,
#     nothing should predict which name the applicant got.
balance_reg <- lm_robust(white ~ crime + ged + empgap + crimbox + pre + nj,
                         data = df, clusters = chain_id, se_type = "stata")
summary(balance_reg)

# (c) Note what is NOT randomized: whether the store has the box.
#     Stores chose that themselves (or their headquarters did).


###############################################################################
# PART 2. DESCRIPTIVES AND MAIN EFFECTS (Tables I–III)
###############################################################################

## ---- table1 ----
# TABLE I: means by period (pre-BTB, post-BTB, combined)
table1_vars <- c("white", "crime", "ged", "empgap", "crimbox",
                 "response", "interview")

table1 <- bind_rows(
  df |> filter(pre == 1) |> summarise(across(all_of(table1_vars), mean), N = n()) |> mutate(period = "Pre-BTB"),
  df |> filter(pre == 0) |> summarise(across(all_of(table1_vars), mean), N = n()) |> mutate(period = "Post-BTB"),
  df |>                     summarise(across(all_of(table1_vars), mean), N = n()) |> mutate(period = "Combined")
)
table1 |> relocate(period) |> mutate(across(where(is.double), ~ round(.x, 3)))

# Callback rates by applicant characteristic and period
df |>
  group_by(period = ifelse(pre == 1, "Pre-BTB", "Post-BTB")) |>
  summarise(Black = mean(response[white == 0]),
            White = mean(response[white == 1]),
            GED   = mean(response[ged == 1]),
            HSD   = mean(response[ged == 0]),
            EmpGap   = mean(response[empgap == 1]),
            NoEmpGap = mean(response[empgap == 0])) |>
  mutate(across(where(is.double), ~ round(.x, 3)))

## ---- table2 ----
# TABLE II: callback rates by conviction status, pre-BTB box stores only.
# This is the only place where employers could actually SEE the record.
box_pre <- filter(df, crimbox == 1, pre == 1)

summ_cb <- function(d, label) {
  d |> summarise(group = label,
                 callback = mean(response),
                 callback_black = mean(response[white == 0]),
                 callback_white = mean(response[white == 1]),
                 N = n())
}

bind_rows(
  summ_cb(filter(box_pre, crime == 0),         "No crime"),
  summ_cb(filter(box_pre, crime == 1),         "Crime"),
  summ_cb(filter(box_pre, propertycrime == 1), "Property"),
  summ_cb(filter(box_pre, drugcrime == 1),     "Drug"),
  summ_cb(box_pre,                             "Combined")
) |> mutate(across(where(is.double), ~ round(.x, 3)))

## ---- table3 ----
# TABLE III: linear probability models of callback on applicant traits.
# Fixed effects: geographic center + chain (small chains grouped: cogroup_comb).
# SEs clustered by chain.

# Column 1: all applications
t3_c1 <- lm_robust(response ~ white + crime + ged + empgap + pre +
                     factor(center) + factor(cogroup_comb),
                   data = df, clusters = chain_id, se_type = "stata")

# Column 2: box applications only (the employer can see the record)
t3_c2 <- lm_robust(response ~ white + crime + ged + empgap +
                     factor(center) + factor(cogroup_comb),
                   data = filter(df, crimbox == 1),
                   clusters = chain_id, se_type = "stata")

# Column 3: split conviction into drug vs. property
t3_c3 <- lm_robust(response ~ white + drugcrime + propertycrime + ged + empgap +
                     factor(center) + factor(cogroup_comb),
                   data = filter(df, crimbox == 1),
                   clusters = chain_id, se_type = "stata")

show(t3_c1, c("white", "crime", "ged", "empgap", "pre"))
show(t3_c2, c("white", "crime", "ged", "empgap"))
show(t3_c3, c("white", "drugcrime", "propertycrime", "ged", "empgap"))
# Published: col 1 white = 0.024 (0.006), crime = -0.014 (0.005)
#            col 2 white = -0.001 (0.009), crime = -0.052 (0.012)

## ---- table3-nocontrols ----
# Because traits were randomized, controls should barely move the estimates.
# Check: drop every control and fixed effect from column 1.
t3_bare <- lm_robust(response ~ white, data = df,
                     clusters = chain_id, se_type = "stata")
show(t3_bare, "white")


###############################################################################
# PART 3. THE BOX AND RACIAL DISCRIMINATION: DIFF-IN-DIFF (Fig. I, Table IV)
###############################################################################

## ---- figure1 ----
# FIGURE I: pre-BTB callback rates by race, box, and crime.
# At no-box stores the employer can't see the record, so crime status is
# pooled into one bar.
fig1_data <- df |>
  filter(pre == 1) |>
  mutate(race = ifelse(white == 1, "White", "Black"),
         box  = ifelse(crimbox == 1, "Box", "No Box"),
         record = case_when(crimbox == 0 ~ "Record not visible",
                            crime == 1   ~ "Crime",
                            TRUE         ~ "No crime")) |>
  group_by(race, box, record) |>
  summarise(callback = mean(response), n = n(), .groups = "drop")

fig1_data

ggplot(fig1_data, aes(x = box, y = callback, fill = record)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = "black") +
  geom_text(aes(label = sprintf("%.3f", callback)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3) +
  facet_wrap(~ race) +
  scale_fill_manual(values = c("Crime" = "black", "No crime" = "white",
                               "Record not visible" = "grey60")) +
  labs(title = "Figure I: Callback rates before Ban the Box",
       x = NULL, y = "Callback rate", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- table4-col1 ----
# TABLE IV, column 1: CROSS-SECTIONAL diff-in-diff, pre-BTB period only.
#   Callback = a + b1*Box + b2*White + b3*(Box x White) + controls
# b3 asks: is the white advantage smaller where employers can see records?
t4_c1 <- lm_robust(response ~ box_white + white + crimbox + ged + empgap +
                     factor(center),
                   data = filter(df, post == 0),
                   clusters = chain_id, se_type = "stata")
show(t4_c1, c("box_white", "white", "crimbox"))
# Published: box_white = -0.030 (0.015), white = 0.032 (0.012)

## ---- tableA3 ----
# The worry with column 1: box and no-box stores may differ in other ways.
# Online Appendix A3 compares them on observables (one row per store).
stores_pre <- df |>
  filter(pre == 1) |>
  distinct(storeid, .keep_all = TRUE)

stores_pre |>
  group_by(crimbox) |>
  summarise(stores = n(),
            pct_white_nbhd = mean(percwhite * 100, na.rm = TRUE),
            pct_black_nbhd = mean(percblack * 100, na.rm = TRUE),
            crime_rate = mean(tot_crime_rate, na.rm = TRUE),
            avg_employees = mean(avg_num_employees, na.rm = TRUE),
            retail = mean(retail)) |>
  mutate(across(where(is.double), ~ round(.x, 2)))

## ---- table4-temporal ----
# TABLE IV, columns 2–4: TEMPORAL diff-in-diff among box-remover stores.
# Now the variation in "Box" comes from the law, not from employer choice.
# Among removers, crimbox == 1 is the same thing as pre == 1.

# Column 2: balanced box removers (same stores before and after)
t4_c2 <- lm_robust(response ~ box_white + white + crimbox + ged + empgap,
                   data = filter(df, remover == 1, balanced == 1),
                   clusters = chain_id, se_type = "stata")

# Column 3: all box removers, with center fixed effects
t4_c3 <- lm_robust(response ~ box_white + white + crimbox + ged + empgap +
                     factor(center),
                   data = filter(df, remover == 1),
                   clusters = chain_id, se_type = "stata")

# Column 4: add chain FE interacted with white and with post.
# (The white and box main effects are no longer interpretable here.)
t4_c4 <- lm_robust(response ~ box_white + white + crimbox + ged + empgap +
                     factor(center) + factor(cogroup_njnyc) +
                     factor(white_cogroup_njnyc) + factor(post_cogroup_njnyc),
                   data = filter(df, remover == 1),
                   clusters = chain_id, se_type = "stata")

show(t4_c2, c("box_white", "white", "crimbox"))
show(t4_c3, c("box_white", "white", "crimbox"))
show(t4_c4, "box_white")
# Published box_white: -0.036 (0.014), -0.033 (0.014), -0.027 (0.013)

## ---- table4-col5 ----
# TABLE IV, column 5: the same pre/post comparison at stores whose
# applications did NOT change. If the race gap moved here too, something
# other than BTB would be going on.
t4_c5 <- lm_robust(response ~ pre_white + white + pre + ged + empgap,
                   data = filter(df, remover == 0, balanced == 1),
                   clusters = chain_id, se_type = "stata")
show(t4_c5, c("pre_white", "white", "pre"))
# Published: pre_white = 0.002 (0.014)


###############################################################################
# PART 4. TRIPLE DIFFERENCES (Figure II, Table V)
###############################################################################

## ---- figure2 ----
# FIGURE II: box removers, balanced sample, before and after BTB.
# After BTB the record is invisible, so post-period bars pool crime status.
fig2_data <- df |>
  filter(remover == 1, balanced == 1) |>
  mutate(race = ifelse(white == 1, "White", "Black"),
         period = factor(ifelse(post == 1, "Post", "Pre"), levels = c("Pre", "Post")),
         record = case_when(post == 1  ~ "Record not visible",
                            crime == 1 ~ "Crime",
                            TRUE       ~ "No crime")) |>
  group_by(race, period, record) |>
  summarise(callback = mean(response), n = n(), .groups = "drop")

fig2_data

ggplot(fig2_data, aes(x = period, y = callback, fill = record)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, colour = "black") +
  geom_text(aes(label = sprintf("%.3f", callback)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3) +
  facet_wrap(~ race) +
  scale_fill_manual(values = c("Crime" = "black", "No crime" = "white",
                               "Record not visible" = "grey60")) +
  labs(title = "Figure II: Box removers before and after Ban the Box",
       x = NULL, y = "Callback rate", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

## ---- table5 ----
# TABLE V: triple differences.
#   Callback = ... + b7 * (BoxRemover x Post x White)
# b7 = change in the white advantage after BTB at box removers,
#      MINUS the change in the white advantage at all other stores.

# Column 1: balanced sample (the authors' preferred specification)
t5_c1 <- lm_robust(response ~ post_remover_white + post_white + post_remover +
                     remover_white + remover + white + post + ged + empgap,
                   data = bal, clusters = chain_id, se_type = "stata")

# Column 2: full sample, with center fixed effects
t5_c2 <- lm_robust(response ~ post_remover_white + post_white + post_remover +
                     remover_white + remover + white + post + ged + empgap +
                     factor(center),
                   data = df, clusters = chain_id, se_type = "stata")

# Column 3: chain FE interacted with white and post
t5_c3 <- lm_robust(response ~ post_remover_white + post_white + white + post +
                     ged + empgap + factor(center) + factor(cogroup_njnyc) +
                     factor(white_cogroup_njnyc) + factor(post_cogroup_njnyc),
                   data = df, clusters = chain_id, se_type = "stata")

show(t5_c1, c("post_remover_white", "post_white", "post_remover",
              "remover_white", "remover", "white", "post"))
show(t5_c2, "post_remover_white")
show(t5_c3, "post_remover_white")
# Published: 0.039 (0.020), 0.040 (0.018), 0.035 (0.018)

## ---- table5-cells ----
# Where does 0.039 come from? Build it by hand from eight cell means.
cells <- bal |>
  group_by(remover, post, white) |>
  summarise(cb = mean(response), .groups = "drop")
cells

gap <- function(r, p) {
  cells$cb[cells$remover == r & cells$post == p & cells$white == 1] -
    cells$cb[cells$remover == r & cells$post == p & cells$white == 0]
}

did_removers <- gap(1, 1) - gap(1, 0)   # change in race gap at box removers
did_others   <- gap(0, 1) - gap(0, 0)   # change in race gap everywhere else
c(gap_removers_pre  = gap(1, 0),
  gap_removers_post = gap(1, 1),
  did_removers = did_removers,
  did_others   = did_others,
  triple_diff  = did_removers - did_others) |> round(3)

## ---- pairs ----
# The audit design sends ONE Black and ONE white applicant to each store in
# each period. So we can compute the race gap WITHIN each pair and run an
# ordinary diff-in-diff on those gaps. The triple difference becomes a double
# difference, and every store-period acts as its own control.
pairs <- bal |>
  group_by(storeid, chain_id, remover, post) |>
  summarise(pair_gap = response[white == 1][1] - response[white == 0][1],
            .groups = "drop")

nrow(pairs)            # one row per store-period
table(pairs$pair_gap)  # -1: only Black called; 0: same; 1: only white called

pair_did <- lm_robust(pair_gap ~ post * remover, data = pairs,
                      clusters = chain_id, se_type = "stata")
show(pair_did, c("(Intercept)", "post", "remover", "post:remover"))


###############################################################################
# PART 5. A PLACEBO CHARACTERISTIC: GED (Table VII)
###############################################################################

## ---- table7 ----
# A GED is a stronger real-world predictor of a criminal record than race.
# If employers were "rationally" substituting toward proxies after BTB, they
# should penalize GEDs more after the box is gone. Table V with GED in place
# of White:
t7_c1 <- lm_robust(response ~ post_remover_ged + post_ged + post_remover +
                     remover_ged + remover + ged + post + empgap + white,
                   data = bal, clusters = chain_id, se_type = "stata")
show(t7_c1, c("post_remover_ged", "post_ged", "ged"))
# Published: post_remover_ged = -0.012 (0.025)


###############################################################################
# PART 6. NEW QUESTIONS
###############################################################################

## ---- ext-clustering ----
# EXTENSION A1: How much does the choice of standard error matter?
# Same point estimate (Table V col 1), five ways of computing uncertainty.
f_t5 <- response ~ post_remover_white + post_white + post_remover +
  remover_white + remover + white + post + ged + empgap

se_compare <- bind_rows(
  show(lm_robust(f_t5, data = bal, se_type = "classical"),   "post_remover_white") |> mutate(se = "Classical (iid)"),
  show(lm_robust(f_t5, data = bal, se_type = "HC1"),         "post_remover_white") |> mutate(se = "Robust (HC1)"),
  show(lm_robust(f_t5, data = bal, clusters = storeid,  se_type = "stata"), "post_remover_white") |> mutate(se = "Cluster: store"),
  show(lm_robust(f_t5, data = bal, clusters = center,   se_type = "stata"), "post_remover_white") |> mutate(se = "Cluster: center"),
  show(lm_robust(f_t5, data = bal, clusters = chain_id, se_type = "stata"), "post_remover_white") |> mutate(se = "Cluster: chain (paper)")
)
se_compare |> select(se, estimate, std.error, p.value)

# How many clusters does each choice give us?
c(stores = n_distinct(bal$storeid),
  centers = n_distinct(bal$center),
  chains = n_distinct(bal$chain_id))

## ---- ext-ri ----
# EXTENSION A2: Randomization inference.
# Treatment (removing the box) was decided at chain headquarters, not by
# store. Ask: if box-remover status had been handed out to a random set of
# chain-by-state groups instead, how often would we see a triple difference
# as large as 0.039?
# We use the pair-gap data, so each permutation is a fast diff-in-diff.

pairs <- pairs |>
  left_join(distinct(bal, storeid, nj), by = "storeid") |>
  mutate(block = paste(chain_id, nj, remover))   # chain x state x status

blocks <- distinct(pairs, block, remover)
table(blocks$remover)   # 75 treated blocks out of 290

ri_stat <- function(dat) {
  m <- lm(pair_gap ~ post * remover, data = dat)
  unname(coef(m)["post:remover"])
}
observed <- ri_stat(pairs)

set.seed(504)
n_perm <- 1000
perm_stats <- replicate(n_perm, {
  shuffled <- blocks |> mutate(remover = sample(remover))
  pairs |>
    select(-remover) |>
    left_join(shuffled, by = "block") |>
    ri_stat()
})

ri_p <- mean(abs(perm_stats) >= abs(observed))
c(observed = round(observed, 3), ri_p_value = ri_p)

ggplot(data.frame(stat = perm_stats), aes(x = stat)) +
  geom_histogram(bins = 40, fill = "grey70", colour = "white") +
  geom_vline(xintercept = observed, colour = "firebrick", linewidth = 1) +
  labs(title = "Randomization inference for the triple difference",
       subtitle = "Grey: estimates under 1,000 random reassignments of box-remover status. Red: actual estimate.",
       x = "Placebo triple-difference estimate", y = "Count") +
  theme_minimal()

## ---- ext-winners-losers ----
# EXTENSION B1: Who wins and who loses when the box goes away?
# Statistical discrimination predicts that Black applicants WITHOUT records
# lose (they can no longer prove they're clean) and white applicants WITH
# records gain (they're assumed clean). Look within each race at box removers:
# how did BTB change callbacks for applicants with vs. without records?
#
# Careful: after BTB the employer can't see the record, so the right
# comparison is each PRE-period cell (e.g. Black, no record, box visible)
# versus the POOLED post-period rate for that race. This mirrors Figure II.
removers_bal <- filter(bal, remover == 1)

winners_losers <- expand.grid(w = c(0, 1), cr = c(0, 1)) |>
  rowwise() |>
  reframe({
    d_wc <- filter(removers_bal, white == w, post == 1 | crime == cr)
    m <- lm_robust(response ~ post + ged + empgap, data = d_wc,
                   clusters = chain_id, se_type = "stata")
    show(m, "post") |>
      mutate(race = ifelse(w == 1, "White", "Black"),
             record = ifelse(cr == 1, "Has record", "No record"),
             pre_rate = mean(d_wc$response[d_wc$post == 0]),
             post_rate = mean(d_wc$response[d_wc$post == 1]))
  }) |>
  select(race, record, pre_rate, post_rate, change = estimate, std.error, p.value) |>
  mutate(across(where(is.double), ~ round(.x, 3)))
winners_losers

## ---- ext-placebo-puzzle ----
# EXTENSION B2: A placebo test. After BTB, box removers can't see the record.
# So the randomized "crime" variable should have NO effect on callbacks there.
placebo_removers_post <- lm_robust(response ~ crime + white + ged + empgap,
                                   data = filter(bal, remover == 1, post == 1),
                                   clusters = chain_id, se_type = "stata")
show(placebo_removers_post, c("crime", "white"))

# Now the same test in ALL applications where the employer couldn't see records
placebo_all_nobox <- lm_robust(response ~ crime + white + ged + empgap +
                                 factor(center) + factor(cogroup_comb),
                               data = filter(df, crimbox == 0),
                               clusters = chain_id, se_type = "stata")
show(placebo_all_nobox, c("crime", "white"))

## ---- ext-place ----
# EXTENSION C: Does discrimination depend on place?

# C1. New Jersey vs. New York City (Online Appendix A5/A6)
by_state <- bind_rows(lapply(c(1, 0), function(j) {
  m <- lm_robust(response ~ white + crime + ged + empgap + pre +
                   factor(center) + factor(cogroup_comb),
                 data = filter(df, nj == j),
                 clusters = chain_id, se_type = "stata")
  show(m, "white") |>
    mutate(place = ifelse(j == 1, "New Jersey", "New York City"),
           black_callback = mean(filter(df, nj == j, white == 0)$response))
}))
by_state |> mutate(pct_white_advantage = round(100 * estimate / black_callback))

# C2. The racial composition of the store's neighborhood (census block group)
df_nbhd <- df |>
  filter(!is.na(percwhite)) |>
  mutate(pct_white_nbhd = percwhite * 100,
         nbhd_quartile = ntile(pct_white_nbhd, 4))

df_nbhd |>
  group_by(nbhd_quartile) |>
  summarise(pct_white_range = paste0(round(min(pct_white_nbhd)), "–",
                                     round(max(pct_white_nbhd)), "%"),
            callback_black = mean(response[white == 0]),
            callback_white = mean(response[white == 1]),
            race_gap = callback_white - callback_black,
            N = n()) |>
  mutate(across(where(is.double), ~ round(.x, 3)))

nbhd_reg <- lm_robust(response ~ white * pct_white_nbhd + crime + ged + empgap +
                        pre + factor(center) + factor(cogroup_comb),
                      data = df_nbhd, clusters = chain_id, se_type = "stata")
show(nbhd_reg, c("white", "pct_white_nbhd", "white:pct_white_nbhd"))

## ---- ext-race-record ----
# EXTENSION D: Is a criminal record more damaging for Black applicants?
# Pager (2003) found the record penalty was larger for Black testers.
# Test the race x record interaction where records are visible.
race_record <- lm_robust(response ~ white * crime + ged + empgap +
                           factor(center) + factor(cogroup_comb),
                         data = filter(df, crimbox == 1),
                         clusters = chain_id, se_type = "stata")
show(race_record, c("white", "crime", "white:crime"))

## ---- ext-logit ----
# EXTENSION E: Does the linear probability model matter?
# Re-estimate the main effects (Table III col 1, no fixed effects for speed)
# as a logit and compare the average marginal effect of "white".
lpm <- lm_robust(response ~ white + crime + ged + empgap + pre,
                 data = df, clusters = chain_id, se_type = "stata")
logit <- glm(response ~ white + crime + ged + empgap + pre,
             data = df, family = binomial)

# Average marginal effect of white: predicted P(callback) if everyone were
# white minus if everyone were Black, averaged over the sample.
ame_white <- mean(predict(logit, newdata = mutate(df, white = 1), type = "response") -
                  predict(logit, newdata = mutate(df, white = 0), type = "response"))
c(LPM = round(unname(coef(lpm)["white"]), 4),
  Logit_AME = round(ame_white, 4),
  Logit_odds_ratio = round(exp(unname(coef(logit)["white"])), 3))

## ---- yourturn ----
###############################################################################
# YOUR TURN: Table VI robustness checks (balanced sample)
###############################################################################
# Using the Table IV col 2 (temporal DiD) and Table V col 1 (triple
# difference) specifications as templates, re-estimate each with:
#
#   (a) interview instead of response as the outcome
#   (b) RA-error observations dropped              hint: raerror != 1
#   (c) New Jersey only, then New York City only   hint: nj == 1 / nj == 0
#   (d) Retail x Post x White controls added (Table V only)
#       hint: + retail + retail_post + retail_white + retail_post_white
#
# Compare each estimate with the main one. Which checks change the point
# estimate, and which only change the precision?

# your code here

## ---- end ----
