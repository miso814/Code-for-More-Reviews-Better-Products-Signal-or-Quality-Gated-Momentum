# Code-for-More-Reviews-Better-Products-Signal-or-Quality-Gated-Momentum
R code to build a product-year panel of Amazon All Beauty reviews (2013–2023), construct lagged attention and quality measures, estimate PPML fixed-effects models testing quality signal and quality-gated momentum, compute implied thresholds, and plot binned patterns, marginal effects with CIs, and lifecycle profiles by product age.
## Data
The raw Amazon All_Beauty review data are obtained from the public repository by Hou et al. (2024), accessed on 2026-01-01.
This repository does not redistribute the raw data. Please download `All_Beauty.jsonl` from the original source and place it at `data/All_Beauty.jsonl` (or update the path in the script).

**Source:** Hou, Y., Li, J., He, Z., Yan, A., Chen, X., and McAuley, J. (2024). “Amazon reviews 2023.” URL: https://amazon-reviews-2023.github.io/
