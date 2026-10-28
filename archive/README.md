# Earlier analyses and outputs

These files preserve earlier work. Use `scripts/replication/` and `scripts/extensions/` for the main analyses. Archived scripts are not part of the recommended workflow.

| File | Purpose |
| --- | --- |
| `scripts/iv_probit_coefficient_bootstrap.R` | Earlier control-function probit; percentile interval for the additional-child coefficient (bootstrap index 1). |
| `scripts/iv_probit_residual_bootstrap.R` | Near-duplicate earlier version; requests the residual coefficient interval (index 2), despite its original comment referring to index 1. |
| `scripts/import_data_example.R` | Minimal example of importing the 1980 SAS data. |
| `tables/iv_probit_coefficients_early.png` | Earlier coefficient table image. |
| `tables/iv_probit_coefficients_alternative.png` | Alternative coefficient table image. |
| `tables/iv_probit_marginal_effects_alternative.png` | Alternative marginal-effects table image. |

Alternative images have different file contents from the versions retained in `results/tables/`. They have not been matched to a particular script execution or regenerated. Filenames describe their role without implying verified chronology or numerical correctness.

The archived probit scripts require `scripts/prepare_data.R` to run first in the same session. Paths have been updated for the new layout; their statistical implementations are retained for reference.
