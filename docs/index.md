---
title: Home
---

# QMSS 75000 / SOC 81900: Causal Analysis for Public Policy and Social Science

Fall '26, CUNY Graduate Center

---

## Course Overview

This course trains students to design, critique, and execute rigorous causal analyses for public policy and social science research. Causal inference is treated as a design problem, not a statistical trick — regression, matching, weighting, difference-in-differences, regression discontinuity, instrumental variables, experiments, and sensitivity analysis are useful only when tied to a clear estimand, a plausible identification strategy, and transparent evidence about threats to validity.

Students will work primarily in R, replicating published estimates, diagnosing assumptions, and building toward an original causal research paper. In this course we will answer questions including:

- What does it mean for a claim to be causal, and why is a correlation not enough?
- How do experiments, matching, fixed effects, difference-in-differences, and instrumental variables each solve the identification problem differently?
- How credible is a given research design, and what would make a skeptical reader believe it?
- How do you translate a substantive policy question into an estimand, a design, and an analysis plan?

## Instructor

Edwin Grimsley, Ph.D.
Office Hours: details on Brightspace
E-mail: edwin.grimsley@baruch.cuny.edu

## Prerequisites

Working knowledge of R and at least one graduate-level course in multivariate regression.

## Time

Thursdays, 6:30 PM – 8:30 PM
GradCenter 6418

## Final Paper

Due Week 14 — a causal-inference research paper applying course methods to an empirical policy or social science question, in lieu of a final exam. Presented in class.

## Grade Breakdown

25% Labs (in-class applied exercises, one per week)
45% Homework (7 coding problem sets + 2 design memos)
5% Final Paper Presentation
25% Final Paper

## Late Submission Policy

Because many assignments feed directly into seminar discussion and peer review, late work should be discussed with the instructor before the deadline whenever possible. Genuine constraints will be accommodated, but silent late submissions may be penalized.

## Academic Integrity

The course follows CUNY academic integrity policies. Proper attribution is required for data, code, published research, and AI-assisted work. Do not submit fabricated results, unverifiable citations, or code you cannot explain.

## Course Materials

**Required (ZTC — Zero Textbook Cost, all free)**

1. Green, Donald P. (2022). *Social Science Experiments: A Hands-on Introduction*. Cambridge University Press.
2. Rosenbaum, Paul R. (2023). *Causal Inference*. The MIT Press.
3. Huntington-Klein, Nick (2025). *The Effect: An Introduction to Research Design and Causality* — [free online](https://theeffectbook.net)

**Software:** R and RStudio (primary). No prior R experience required.

Slides, labs, and homework are posted in the schedule below as they are released.

## Key Readings

Links go to the published version (use your CUNY library login if prompted) and, where one exists, a free version.

**Week 4 — Randomized Controlled Trials**

- Chattopadhyay, R. & Duflo, E. (2004). Women as Policy Makers: Evidence from a Randomized Policy Experiment in India. *Econometrica*, 72(5), 1409–1443. [Published](https://doi.org/10.1111/j.1468-0262.2004.00539.x) — *Lab 3*
- Gerber, A. S., Green, D. P. & Larimer, C. W. (2008). Social Pressure and Voter Turnout: Evidence from a Large-Scale Field Experiment. *American Political Science Review*, 102(1), 33–48. [Published](https://doi.org/10.1017/S000305540808009X) — *Lab 3*
- Bertrand, M. & Mullainathan, S. (2004). Are Emily and Greg More Employable than Lakisha and Jamal? A Field Experiment on Labor Market Discrimination. *American Economic Review*, 94(4), 991–1013. [Published](https://doi.org/10.1257/0002828042002561) · [Free working paper](https://dspace.mit.edu/handle/1721.1/63261) — *Homework 3*
- Pager, D. (2003). The Mark of a Criminal Record. *American Journal of Sociology*, 108(5), 937–975. [Published](https://doi.org/10.1086/374403) · [Author PDF](https://scholar.harvard.edu/files/pager/files/pager_ajs.pdf) — *Homework 3 framing*
- *Optional:* Pager, D., Western, B. & Bonikowski, B. (2009). Discrimination in a Low-Wage Labor Market: A Field Experiment. *American Sociological Review*, 74(5), 777–799. [Free version](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2915472)

**Week 5 — Read before class on October 1**

- Agan, A. & Starr, S. (2018). Ban the Box, Criminal Records, and Racial Discrimination: A Field Experiment. *Quarterly Journal of Economics*, 133(1), 191–235. [Published](https://doi.org/10.1093/qje/qjx028) — *Lab 4 replication*
  - As you read: What did the researchers randomize, and what did they not? Which comparisons in the paper are experimental, and which are before-and-after?

## Schedule (draft — subject to change)

[Full Syllabus](files/CUNY-GC-causal-analysis-syllabus-merged.docx)

| Description | Week | Slides | Labs | Homework |
|---|---|---|---|---|
| Regression/R Refresher | 1 — Sept 3 | [Slides](files/intro-to-causal-thinking.pptx) | [R Refresher (warm-up)](files/r-refresher-warmup.docx) | — |
| Stratification, Regression, and Causal Adjustment | 2 — Sept 10 | [Slides](files/week2-stratification-controls.pptx) | [Lab 1](files/lab1-confounders-vs-colliders.docx) | [Homework 1 (PS1)](files/homework1-problemset1-sensitivity-analysis.docx) |
| Potential Outcomes and DAGs | 3 — Sept 17 | — | [Lab 2](files/lab2-dags-and-ate-preview-revised.docx) | [Homework 2 (PS2)](files/homework2-problemset2-ate-att-atu.docx) |
| Randomized Controlled Trials | 4 — Sept 24 | [Slides](files/week4-rcts-slides.pptx) | [Lab 3](files/lab3-rct-experiments-and-balance-table.docx) · [R script](files/week4-lab3.R) | [Homework 3 (PS3)](files/homework3-problemset3-race-callbacks.docx) · [R script](files/week4-homework3.R) |
| Matching and Propensity Scores | 5 — Oct 1 | — | [Lab 4](files/week5-matching-lab-hw.docx) | Problem Set 4 |
| Weighting, Bad Controls, Post-Treatment Bias | 6 — Oct 8 | — | [Lab 5](files/week6-weighting-lab-hw.docx) | Design Memo |
| Fixed Effects | 7 — Oct 15 | — | [Lab 6](files/week7-fixed-effects-lab-hw.docx) | Problem Set 5 |
| DiD I: Replicating Legewie & Fagan | 8 — Oct 22 | — | [Lab 7](files/week8-did-legewie-fagan-lab-hw.docx) | Design Memo |
| DiD II: Event Studies and Heterogeneity | 9 — Oct 29 | — | [Lab 8](files/week9-event-studies-lab-hw.docx) | Problem Set 6 |
| Boundary Designs / Proximity Policing | 10 — Nov 5 | — | [Lab 9](files/week10-boundary-nycha-nypd-lab-hw.docx) | — |
| Research Question Workshop | 11 — Nov 12 | — | [Lab 10](files/week11-research-questions-workshop-guide.docx) | Final Paper Outline |
| IV, RD, Sensitivity | 12 — Nov 19 | — | [Lab 11](files/week12-iv-rd-sensitivity-lab-hw.docx) | Problem Set 7 |
| *No class (Thanksgiving)* | — Nov 26 | — | — | — |
| External Validity and Paper Workshop | 13 — Dec 3 | — | [Lab 12](files/week13-external-validity-workshop-guide.docx) | — |
| Final Paper Presentations | 14 — Dec 10 | [Guidelines & Rubric](files/final-presentation-guidelines-rubric.docx) | — | Final Paper Due |

**Note:** Term runs September 3 – December 10, 2026. Class meets Thursdays. Materials appear in the table as they are finalized.
