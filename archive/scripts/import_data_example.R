# Historical analysis variant. See archive/README.md before running.
# Import the 1980 data from the repository root.

library(haven)
dati = read_sas("data/raw/m_d_806.sas7bdat")

# The imported data are available in the current R session.
