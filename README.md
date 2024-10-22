# SDSII_IoT_Agriculture

This individual project was carried out for the course of Statistical Methods for Data Science of the Master's degree in Data science at Sapienza University of Rome. It was made at the end of the second semester of the academic year 2023-2024 (September 2024).

Personal information:
| NAME and SURNAME | MATRICOLA | EMAIL |
| --- | --- | --- |
| Sofia Noemi Crobeddu | 2130389 | crobeddu.2130389@studenti.uniroma1.it | 

## PURPOSE

The aim of this project is to analyze data from the "Advanced IoT Agriculture 2024" database, developed from a real IoT system deployed in agricultural environments. This kind of system allows real-time monitoring of environmental conditions within a smart greenhouse. This study examines the effectiveness of IoT environment compared to traditional one, through the application of Bayesian models and MCMC simulation, using libraries for JAGS software implemented in R.

## REPOSITORIES

The repositories are:
- **Advanced_IoT_Dataset.csv**: it contains the csv files with the original datasets. The file with data inside is `Advanced_IoT_Dataset.csv`. The source was Kaggle website. Everything about this is reported in the final report (see References).
- **script**: it contains the codes for the analysis among these files:
  - `project_report[1].Rmd`: it is the final report in Rmarkdown format, with all the analyses, graphs, interpretations and results.
  - `mod_agr.txt`: text file of the first JAGS model applied to data, the Multinomial logistic regression model.
  - `model_jags.txt`: text file of the second JAGS model applied to data, the Multilevel model with Hierarchical Random Terms.
- `project_report[1].html`: it is the final report in html format.
- `project_report[1].pdf`: it is the final report in pdf format.
