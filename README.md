# Alp Synthetic Health Data Generator

**Generate realistic synthetic health/patient data, compare generative models side by side, and measure the trade-off between fidelity and privacy — in an interactive R/Shiny app.**

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
![Language: R](https://img.shields.io/badge/Language-R-276DC3.svg)
![Built with: Shiny](https://img.shields.io/badge/Built%20with-Shiny-447099.svg)

This is a teaching and quality-check tool for synthetic data. You give it a dataset (or use the built-in synthetic sample), it learns the patterns with two generative models of your choice, and it grades the result on two axes that usually pull against each other: **how realistic** the synthetic data is, and **how private** it is. A built-in fidelity sweep shows that tension as a curve, so picking an operating point becomes a deliberate choice rather than a guess.

---

## Try it / run it

**Live demo (sample data only):** _add your shinyapps.io URL here after deploying_

> ⚠️ **The online demo runs on built-in synthetic sample data only and disables file upload.** It is hosted on a shared public server and is **not** a private or secure environment. **Do not upload or enter real or patient data into the demo.** To work with your own data, download the app and run it on your own machine (below), where your data never leaves your computer.

**Run locally with your own data:** [Download the latest release →](https://github.com/Alp-Digital-Health/Alp.SHD.Generator/releases/latest)

The release includes a bundled build with a portable R runtime, so you can run the app **without installing R**. Unzip and launch — your data stays entirely on your machine.

If you already have R, you can also run the source directly:

```r
# install once
install.packages(c("shiny", "shinyWidgets", "synthpop", "bnlearn", "rpart",
                   "ggplot2", "dplyr", "tidyr", "randomForest", "readxl"))

# then, from the repo folder
shiny::runApp("synthetic_data_fidelity_app.R")
```

---

## What it does

- **Bring your own data** — upload CSV, TSV, TXT, XLSX/XLS, or RDS. The app auto-detects types and drops ID-like, constant, or mostly-empty columns.
- **Compare two models at once** — pick any two of five generators (A vs B) and see them evaluated side by side.
- **Tune fidelity** — four fidelity levels control how closely the synthetic data tracks the real patterns, trading utility against privacy.
- **Fair evaluation** — the real data is split into a training set (used to fit the generators) and a holdout that the generators never see, which is the yardstick for every score.
- **Fidelity sweep** — score both models at all four fidelity levels and plot the utility-vs-privacy trade-off as a curve.
- **Local-first privacy** — the downloadable build processes everything on your own machine; nothing is uploaded.

### Generative models

| Model | Engine |
|---|---|
| Bayesian network | `bnlearn` (hill-climbing structure + Bayesian fit) |
| CART | `synthpop` |
| Random forest | `synthpop` |
| Parametric | `synthpop` |
| Gaussian copula | custom rank-based copula |

### How it's graded

- **Distribution similarity** — Kolmogorov–Smirnov distance (continuous) and Jensen–Shannon divergence (categorical).
- **Association preservation** — mixed pairwise association change (Pearson, Cramér's V, eta).
- **Utility** — train-on-synthetic / test-on-real (TSTR) versus a train-on-real baseline (TRTR), reported as a retention ratio.
- **Privacy** — Gower distance-to-closest-record against a holdout benchmark, exact-match share, and a membership-inference AUC.
- **Distinguishability** — a classifier's attempt to tell real from synthetic (pMSE).

---

## How it works

1. **Load** your dataset (or use the built-in synthetic sample).
2. **Split** into a training set and an unseen holdout.
3. **Generate** new records with two models fit on the training set.
4. **Set fidelity** to control the realism / privacy balance.
5. **Evaluate** the synthetic data against the holdout on realism and on privacy.

Every score is explained in plain language inside the app under **About / Guide**.

---

## Ways to extend it

This project is meant to be built on. Some directions that would plug in cleanly:

- **Add a generator** — e.g. a deep model (CTGAN/TVAE) as a sixth option in the model list.
- **Add metrics** — propagation of correlations across more variable types, additional privacy attacks, or downstream-task utility for specific clinical endpoints.
- **Expand data handling** — richer missing-data strategies, longitudinal/time-to-event support, or hierarchical (multi-table) data.
- **Reporting** — a one-click PDF/HTML report summarising the run for documentation or governance review.

Contributions are welcome — open an issue to discuss an idea, or send a pull request. Bug reports and feature suggestions are equally useful.

---

## A note on responsible use

The fidelity and privacy scores here are a research and quality-check aid, **not** a legal guarantee that synthetic data is safe to release. Always follow your organisation's data-governance rules before sharing data derived from real patients, and never put real patient data into the public demo.

---

## Citation

If you use this tool in teaching or research, please cite it:

> Alvandi, A. (2026). *Alp Synthetic Health Data Generator* (Version SHD.V004.06.2026) [Software]. https://github.com/Alp-Digital-Health/Alp.SHD.Generator

---

## License

Released under the [MIT License](LICENSE). © 2026 Abraham Alvandi.

## Author

Created by **Abraham Alvandi** — [profile](https://alvandi.weebly.com/).
