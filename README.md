# Trends in Age-Adjusted Opioid Overdose Mortality in the United States, 2013–2024

## Project overview
- The opioid epidemic is a major public health crisis in the United States, with overdose deaths representing one of the leading causes of mortality in recent decades.  
- This project examines temporal trends in opioid overdose mortality from 2013 to 2024 using descriptive analysis, age-adjusted mortality rates, and Joinpoint regression.

## Why this project matters
The opioid overdose crisis remains one of the most pressing public health challenges in the United States.

Understanding how mortality patterns have evolved over time — particularly across opioid subtypes — is essential for identifying the drivers of the epidemic and informing public health responses. This project highlights the role of synthetic opioids, especially fentanyl, in shaping recent mortality trends and provides a structured analysis of how the epidemic has changed between 2013 and 2024.

## Objectives
- Describe national trends in opioid overdose mortality over time  
- Compare trends across opioid subtypes  
- Explore demographic and geographic variation  
- Identify changes in mortality trends using Joinpoint regression  

## Data source
- Data were obtained from the CDC WONDER Online Database (Centers for Disease Control and Prevention – Wide-ranging Online Data for Epidemiologic Research).  
- Data were retrieved using the following filters:  
  - Multiple Cause of Death  
  - Years: 2013–2024  
  - Geography: All U.S. states  
  - Stratification: Age group (15–24 to 75–84 years), sex, and race  
- Case definition included:  
  - Underlying cause of death: ICD-10 codes X40–X44, X60–X64, X85, Y10–Y14  
  - Contributing causes: T40.0–T40.6 (opioid-related codes)

## Methods
The analysis included:
- Data cleaning and harmonization in R  
- Descriptive analysis of observed deaths (counts and proportions by year, sex, age group, race, and opioid subtype)  
- Calculation of age-adjusted mortality rates using direct standardization  
- Joinpoint regression to estimate annual percent change (APC)  

## Main findings
- Opioid overdose mortality increased from 12.2 per 100,000 in 2013 to a peak of 35.1 in 2022, followed by a decline to 22.8 in 2024.  
- Synthetic opioids other than methadone (primarily fentanyl) were the main driver of the epidemic, particularly after 2015.  
- Mortality was concentrated among males and adults aged 25–44 years.  
- Substantial geographic variation was observed, with higher mortality rates in eastern states.  

- Joinpoint analysis identified a significant increase from 2013 to 2022 (APC 12.9%) followed by a decline from 2022 to 2024 (APC −18.9%).  
- Trends for synthetic opioids showed a rapid increase, sustained growth, and a recent decline, consistent with the overall epidemic pattern.  

## Repository structure
- `scripts/` contains R scripts for data cleaning, descriptive analysis, age adjustment, and Joinpoint preparation  
- `figure/` contains visualizations  
- `output/` contains summary tables and Joinpoint outputs  

## Reproducibility
The workflow was developed in R. Joinpoint input files were generated in R and analyzed using the Joinpoint Regression Program (CDC). Final APC estimates were compiled into summary tables.

## Key takeaway
The opioid epidemic in the United States has evolved over time, with a shift from heroin and prescription opioids to synthetic opioids as the dominant driver of mortality.

Although recent data suggest a decline after 2022, mortality levels remain substantially higher than in the early study period, indicating that the crisis is ongoing and structurally driven.

## Note
A mixed-effects count model (generalized linear mixed model) was explored but not retained as the primary analysis because the dataset contained suppressed or truncated low-count cells, limiting the appropriateness of standard count models.
