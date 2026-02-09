# AMBER Generation Scotland Antidepressant Response Classification

Code for classifying antidepressant treatment response in Generation Scotland participants.

## Overview
These scripts implement the rule-based classification framework described in our cohort profile paper: "Cohort Profile: Investigating Antidepressant Response within Generation Scotland"

## Scripts

- **00_startup.R** - Load required packages and set up environment
- **01_get_data.R** - Load questionnaire data
- **02_create_new_cols.R** - Create variables for classification (n_switches, any_ad_effective, etc.)
- **03_check_cols.R** - Quality checks on created variables
- **04_classification.R** - Implement responder/non-responder classification logic
- **main.R** - Main workflow tying all scripts together

## Classification Criteria

**Responders:** Participants who took ≥1 SSRI for ≥6 weeks, rated it "Yes, a lot" effective, did not discontinue due to inefficacy, and never rated any antidepressant as ineffective.

**Non-responders:** Participants with ≥2 failed SSRI trials (each ≥6 weeks, discontinued due to inefficacy, rated ineffective), with no antidepressant ever rated effective.

## Citation


## Contact
Megan Calnan - mcalnan@ed.ac.uk
