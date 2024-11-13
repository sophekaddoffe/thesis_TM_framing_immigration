# thesis_TM_framing_immigration
The code and data in this repository can be used to recreate the master thesis "From Gastarbeiter to Refugees: A Framing Analysis of Immigration in German Parliamentary Discourse Over Seven Decades (1949-2021)". All scripts are written in R language. Unfortunately, the speech data was too large to be added to the repository, however, it can be downloaded via the Harvard Dataverse using this link: https://dataverse.harvard.edu/dataverse/opendiscourse. 

**Contents:**

Scripts for recreation of Seeded Topic Model:

- immigration_TM_ALL.R: R script used for recreating the final seeded topic model
- dictionary2.yml: Dictionary file that contains the seeded topics including seed words
- finalTM_keyword_comparison.R: R script used to compare keyword-search approach vs. topic modeling approach to find immigration-rich speeches
- exploration.R & model_comparison.R & model_eval_10samples.R & nr_of_topics.R: R scripts that were used for exploration and finding the right model configuration
- divergence.R: R script to compute divergence scores on samples of the data for different numbers of k

Script for recreation of Data Set:

- df_and_analysis_prep.R: R script used for manipulating the data extracted from the final seeded topic model used, as well as combining the data with information on the speakers (as well as their respective parties, if applicable) and event data in order to create the variables used for the analysis

Scripts to recreate Analysis and Visualization:

- ols_model.R: R script used to fit OLS models
- descriptives.R: R scripts to compute descriptive statistics
- visualization.R: R script to create plots 
- speech_examples.R: R script to obtain speech samples

CSV-files used:

- CMP_full_data.csv & CMP_party_data.csv: CSV files containing the CMP data, used to create party ideology variable and internal events, as CMP data also includes information on national elections
- all_events.csv: CSV file used to create the external event variable
- factions.csv: CSV file containing party abbreviations and names
- politicians.csv: CSV file containing information on politicians (name, date of birth, occupation etc.)
