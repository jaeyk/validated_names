# Validated Names for Experimental Studies on Race and Ethnicity

## Session information 

* R version 4.1.2 (2021-11-01)
* Platform: x86_64-apple-darwin17.0 (64-bit)
* Running under: macOS Monterey 12.2.1

## Original datasets

* [prep_datasets.Rmd](https://github.com/jaeyk/validated_names/blob/main/code/prep_datasets.Rmd) produces the following four datasets (the three experiment results plus the names datasets) (row # * column #)
  * [study-1-names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/study-1-names.csv) (14935 * 14) 
  * [study-2-names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/study-2-names.csv) (19043 * 20)
  * [study-3-names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/study-3-names.csv) (10192 * 20)
  * [study123.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/study123.rda) (a combined file) 
  * [names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/names.csv) (600 * 7)

## Validation tests and dataset

* [three_datasets.Rmd](https://github.com/jaeyk/validated_names/blob/main/code/three_datasets.Rmd) produces Figure 2 (results from four OLS models designed to assess data validity)
* [validate_datasets.Rmd](https://github.com/jaeyk/validated_names/blob/main/code/validate_datasets.Rmd) produces estiamte_by_race.png and the following datasets
  * [pct_correct_covariate.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/pct_correct_covariate.rds) (1800 * 6)
  * [names_race_perception.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/names_race_perception.rds) (600 * 7)
  * [hq_group_group_names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/hq_group_group_names.rds) (361 * 6)
  * [hq_white_other_names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/hq_white_other_names.rds) (282 * 6)
  * [hq_other_white_names.rds](https://github.com/jaeyk/validated_names/blob/main/data_outputs/hq_other_white_names.rds) (165 * 6)