## Code for reproducing analyses in "Stability and Change in Health Risk Behavior Profiles of U.S. Adults" by [insert authors]. Submitted.

### Contents:

1. `5class_gender_death_final_group_single_fit.sas`: SAS script for fitting LTA model (point estimates)
2. `5class_gender_death_final_group_batch.sas`: SAS script for bootstrapping the model
3. `process_results.R`: R script for processing results and preparing data for plotting
4. `plot_results.R`: R script for plotting results after running `process_results.R`
5. `groupBootOut`: Folder for storing boostrap results
6. `singleFitOut`: Folder for storing point estimates
7. `plots`: Folder for storing plots
8. `correlates`: Folder containing stata scripts for obtaining correlations and fitting multinomial logits. First run `ACL_HB_cluster_data_management_080817_to_share.do` then run `Correlates_of_HB_status_080817_to_share.do`. All analyses use the file `acl12345d6b.dta`.