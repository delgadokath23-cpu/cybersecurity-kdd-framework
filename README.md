# KDD-based statistical framework for interpretable cybersecurity incident analysis

Source code for the article:

> Delgado-Paucar, K., Rodriguez-Baca, L. S., Cruzado, C. F., Susanibar Ramirez, E. T., and López-Gonzales, J. L. *A KDD-based statistical framework for interpretable cybersecurity incident analysis and decision support.* Frontiers in Artificial Intelligence (under review, manuscript 2013039).

The framework turns cybersecurity incident records into interpretable evidence through the five stages of the Knowledge Discovery in Databases (KDD) process: exploratory data analysis, principal component analysis (PCA), K-Means clustering and dispersion-aware count models (Poisson / quasi-Poisson / negative binomial). Everything is contained in **one R file**, which runs as an interactive R Shiny application or, without the interface, reproduces all results of the article.

## Contents

| File | Purpose |
|---|---|
| `cybersecurity_framework_v7_3_3.R` | Complete framework (v7.3.3): Shiny app in Spanish or English, sensitivity analyses, article figures 1–11 and `reproducir_articulo()` |
| `sessionInfo.txt` | R and package versions used for the article |
| `data/` | Place the dataset here (not redistributed in this repository) |

## Data

The analyses use the public synthetic **Cybersecurity Incident Dataset** (Habeeb, 2024), available on Kaggle:
https://www.kaggle.com/datasets/mustafahabeeb90/cybersecurity-incident-dataset
(Open Database License v1.0 for the database; Database Contents License v1.0 for its contents).

1. Download the CSV from Kaggle.
2. Save it as `data/cybersecurity_incidents.csv`.

## Requirements

R ≥ 4.3 (the article used R 4.5.3) and the packages:

```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "lubridate", "MASS", "broom",
                   "FactoMineR", "factoextra", "cluster", "psych", "performance",
                   "glmmTMB", "patchwork", "ggrepel", "plotly", "DT", "writexl", "readxl", "scales",
                   "tm", "wordcloud", "RColorBrewer", "tidytext", "base64enc", "htmlwidgets"))
```

Exact versions are listed in `sessionInfo.txt`.

## Option 1 – Interactive application

Open `cybersecurity_framework_v7_3_3.R` in RStudio and click **Run App**. Choose the interface language (Español / English) at the top of the sidebar; the app reloads and shows every menu, table, chart and message in that language. Upload `data/cybersecurity_incidents.csv` in the first tab (*Carga* / *Data upload*).

Article figures (Figure 11 downloads in the *Sensitivity* tab and `reproducir_articulo()`) are always exported in English, as required by the journal.

The **Sensibilidad / Sensitivity** tab shows the sensitivity analyses of the revised article (Section 3.7, Table 6, Figure 11 and Supplementary Tables S1–S3) and lets you download Figure 11 as PDF or 600-dpi JPG. The Conway–Maxwell–Poisson model is fitted on demand with the button in that tab because it can take several minutes. All results are included in the Excel analysis report.

## Option 2 – Reproduce the article without the interface

```r
source("cybersecurity_framework_v7_3_3.R")
reproducir_articulo("data/cybersecurity_incidents.csv")
```

This writes `outputs/revision/revision_resultados.xlsx` (all sensitivity analyses, including the Conway–Maxwell–Poisson model), `outputs/revision/sessionInfo.txt`, the tables in `outputs/tablas/` and Figures 1–11 in `outputs/article_figures/pdf/` and `outputs/article_figures/jpg/`. Use `reproducir_articulo(..., cmp = FALSE)` to skip the slow COM-Poisson model.

The **Resumen visual / Visual summary** tab combines the GLM response surface (fitted frequency by log impact and mean severity), a radar chart of the K-Means profiles and a flow diagram of the priority tiers between the total-impact and mean-impact models.

## Optional organizational questionnaire module

The first tab also accepts an optional **Input B**: an organizational questionnaire in XLSX format (20 Likert items in five dimensions, FI, SI, IE, RD and PG, plus optional open questions). A blank template can be downloaded from the same tab. When it is provided, the app reports reliability (Cronbach's alpha), dimension scores, the EIglobal index and a text-mining module for the open answers. This module is complementary evidence and is **not** part of the statistical model; the article analyzes incident data only.

## Configuration switches

Near the top of the R file:

| Switch | Default in this repository | Effect |
|---|---|---|
| `MODULO_ORGANIZACIONAL` | `TRUE` | Shows the optional questionnaire and text-mining modules (`FALSE` hides them) |
| `MOSTRAR_PUBLICACION` | `FALSE` | Shows the *Publication* tab that batch-exports Figures 1–11 (`TRUE` shows it). `reproducir_articulo()` exports the same figures either way |

## Interface language

The interface is available in Spanish and English. The language can also be set in the URL (`?lang=es` or `?lang=en`), and the default is defined by `APP_LANG_DEFAULT` near the top of the file. All texts live in a single dictionary (`I18N`); adding another language only requires a new column in that dictionary and a new entry in `APP_LANGS`.

## Reproducibility

All stochastic steps use `set.seed(42)`; K-Means uses k = 3 and `nstart = 25`. Silhouette stability uses 100 random subsamples of 400 cells.

## Version history

- **v7.3.3** – Version for the revised article: sensitivity analyses (mean-impact model, priority-tier stability, PCA without derived indicators with Tucker congruence, silhouette stability, on-demand Conway–Maxwell–Poisson model), Figure 11 and `reproducir_articulo()`; interface in Spanish or English with a single translation dictionary; *Visual summary* tab (GLM response surface, K-Means radar, priority-tier flow); compatible with ggplot2 4.x and 3.x; switches `MODULO_ORGANIZACIONAL` (optional questionnaire, enabled here) and `MOSTRAR_PUBLICACION` (*Publication* tab, hidden here); priority labels reworded as exploratory review priorities.
- **v7.3.2** – Version used for the originally submitted manuscript.

## Citation

If you use this code, please cite the article above and the archived software release (Zenodo DOI to be added).

## License

Code released under the MIT License (see `LICENSE`).
