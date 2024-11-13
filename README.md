# thesis_TM_framing_immigration
The code and data in this repository can be used to recreate the master thesis "From Gastarbeiter to Refugees: A Framing Analysis of Immigration in German Parliamentary Discourse Over Seven Decades (1949-2021)". All scripts are written in R language. Unfortunately, the speech data was too large to be added to the repository, however, it can be downloaded via the Harvard Dataverse using this link: https://dataverse.harvard.edu/dataverse/opendiscourse. 

Contents:

R scripts used for recreating the seeded topic model, including files that were used for exploration and finding the right model configuration

dictionary2.yml: dictionary file that contains the seeded topics including seed words  

R scripts used for manipulating the data extracted from the final seeded topic model used, as well as combining the data with information on the speakers (as well as their respective parties, if applicable) and event data in order to create the variables used for the analysis

CMP_full_data.csv & CMP_party_data.csv: CSV files containing the CMP data

all_events.csv & electoral_terms.csv: CSV files used to create the event variables

CSV files containing information on politicians and factions

R scripts used to fit OLS models, as well as scripts to compute descriptive statistics, create plots and obtain speech samples
