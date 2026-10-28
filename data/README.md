# Data setup

The analysis uses the 1980 and 1990 Census extracts distributed with Angrist and Evans (1998).

1. Open the [MIT Angrist Data Archive](https://economics.mit.edu/people/faculty/josh-angrist/angrist-data-archive).
2. Find **Angrist and Evans (1998)** and download **AngEv98.zip**.
3. Create `data/raw/` and extract `m_d_806.sas7bdat` and `m_d_903.sas7bdat` there.
4. Run `scripts/prepare_data.R` from the repository root.

Use the original `.sas7bdat` files, not SAS program files with a `.sas` extension. `haven::read_sas()` reads these data directly. The preparation script loads both years and creates `clean_data_80`, `clean_data_90`, `married_data_80`, and `married_data_90` in the current R session.

Raw data are ignored by Git. Attribute the original study and consult the source archive for data documentation and terms of use. An alternative source is the [authors' Dataverse deposit](https://doi.org/10.7910/DVN/4W9GW2).
