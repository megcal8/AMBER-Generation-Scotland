# Check the columns used for classification.
# Get counts of TRUE/FALSE/NA and check raw data cols look sensible
# ----------------------------------------------------------------
# Check that where n_switches_ssri == 0, the n_switches_missing_ssri column is NA or 0
check_na <- questionnaire_data %>%
  select(GsId, n_switches_missing_ssri, n_switches_ssri) %>%
  filter(n_switches_ssri == 0) %>%
  pull(n_switches_missing_ssri) %>%
  unique()

all(check_na %in% c(NA, 0))

# ----------------------------------------------------------------
# Check columns that we have created
# - n_switches_ssri
# - any_ad_effective_ssri
# - on_current_ad_6_weeks

questionnaire_data %>%
  group_by(n_switches_ssri) %>%
  summarise(count = n())

questionnaire_data %>%
  group_by(n_switches_missing_ssri) %>%
  summarise(count = n())

# ----------------------------------------------------------------
