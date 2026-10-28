# Family Size and Parents' Labor Supply

An econometrics course project studying Angrist and Evans (1998), *Children and Their Parents' Labor Supply: Evidence from Exogenous Variation in Family Size*.

The project has two goals: understand the paper's identification strategy by reproducing selected tables in R, and propose an extension for the binary employment outcome using a control-function probit model.

## Research question

How does having more than two children affect parents' employment, hours worked, and income? Fertility choices may be related to unobserved factors that also affect labor supply, so an ordinary least squares regression need not identify a causal effect.

The paper uses the sex composition of the first two children as an instrument for additional childbearing, drawing on parents' preference for a mixed-sex sibling pair. It also considers twins as an alternative source of variation. This project uses the authors' 1980 and 1990 US Census extracts to study those methods.

## Analysis

- **Replication:** data preparation and scripts corresponding to Tables 2 through 8, covering descriptive statistics, fertility instruments, and labor supply regressions.
- **Main extension:** a two-step control-function probit model for whether a mother worked for pay, using the 1980 sample. The first stage predicts having more than two children; its residual enters the second-stage probit. The script reports coefficients and average marginal effects, with 500 bootstrap replications for the marginal effects of additional children and the first-stage residual.
- **Additional analysis:** OLS and two-stage least squares estimates across education subgroups in the 1980 sample.

This is a student replication and extension, not the authors' official replication package. The nonlinear extension requires additional modeling assumptions; it should not be treated as automatically equivalent to the paper's linear IV estimates.

## Repository structure

```text
family-size-labor-supply.Rproj
scripts/
  prepare_data.R
  replication/table_02.R ... table_08.R
  extensions/
    iv_probit_marginal_effects.R
    education_subgroups.R
data/
  README.md
  raw/                       # Downloaded data, excluded from Git
results/
  tables/                    # Existing table images
docs/
  report.md
archive/
  README.md
  scripts/                   # Earlier analysis variants
  tables/                    # Alternative historical outputs
```

## Data

Download **AngEv98.zip** from the [Angrist Data Archive at MIT](https://economics.mit.edu/people/faculty/josh-angrist/angrist-data-archive), under **Angrist and Evans (1998)**. Extract these SAS data files into `data/raw/`:

```text
data/raw/m_d_806.sas7bdat
data/raw/m_d_903.sas7bdat
```

See [data setup](data/README.md) for details. Raw data are not distributed in this repository.

## Running the analysis

Open `family-size-labor-supply.Rproj` in RStudio, or start R with the repository root as the working directory. Install the packages used by the analysis:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "purrr", "broom", "gt", "AER",
  "sandwich", "lmtest", "modelsummary", "boot", "margins", "marginaleffects"
))
```

Run data preparation first, then a table script in the same session:

```r
source("scripts/prepare_data.R")
source("scripts/replication/table_02.R")
publication_table_2
```

Use `table_03.R` through `table_08.R` for the other replication tables. Each table script relies on the cleaned data objects created by `prepare_data.R`.

For the main extension:

```r
source("scripts/prepare_data.R")
source("scripts/extensions/iv_probit_marginal_effects.R")
publication_table_iv_probit
ame_table_all
```

The bootstrap can take substantial time. It runs serially on Windows and uses forked workers on supported systems. Its random seed is fixed at `30413`.

For the education subgroup analysis, start a fresh session and run:

```r
source("scripts/prepare_data.R")
source("scripts/extensions/education_subgroups.R")
publication_table_7
```

Several scripts clear intermediate objects from their execution environment. Use a dedicated R session and export each table before running another script. To save a table without additional screenshot tools:

```r
gt::gtsave(publication_table_2, "results/tables/table_02.html")
```

PNG export commands are provided as comments in the scripts. PNG rendering also requires `webshot2` and a compatible Chrome/Chromium installation.

## Results and limitations

[Table images](results/tables/) are retained from the original project and have not been regenerated as part of the repository reorganization. Alternative images are preserved separately in [the archive](archive/README.md); identical titles do not imply identical estimates.

The main extension bootstraps uncertainty for two marginal effects only. Its coefficient table uses the second-stage model's standard errors, and the remaining marginal effects retain their default model-based standard errors. These do not all account for first-stage estimation uncertainty. The current use of `avg_slopes()` should also be considered when interpreting effects of binary regressors.

The repository does not yet pin package versions or provide an automated numerical comparison against the published tables. Successful execution alone does not establish exact replication of the paper.

## Report and reference

See [report information](docs/report.md) for the project write-up.

Angrist, Joshua D., and William N. Evans. 1998. "Children and Their Parents' Labor Supply: Evidence from Exogenous Variation in Family Size." *American Economic Review*, 88(3): 450-477. [Published paper](https://www.jstor.org/stable/116844). [Replication data](https://doi.org/10.7910/DVN/4W9GW2).
