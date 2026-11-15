library(tidyverse)
library(readxl)
library(janitor)
library(tidysynth)
library(knitr)
library(dplyr)


# =============================================================
# PART 1: DATA ENGINEERING & PER CAPITA MATH
# =============================================================

# 1. DEFINE YOUR UNIVERSE
my_states <- c(
  "Colorado",
  "Idaho", "Montana", "Texas", "North Dakota", "South Dakota",
  "Iowa", "Missouri", "Arkansas", "Louisiana", "Mississippi",
  "Alabama", "Georgia", "South Carolina", "North Carolina",
  "Tennessee", "Kentucky", "Indiana", "Ohio", "Wisconsin", "Pennsylvania"
)

# 2. FARS (Traffic Fatalities)
fars_clean <- list.files(path = "D:/Colorado_Project/FARS_Data", #Change the path, because I used Disk D
                         pattern = "*.csv", full.names = TRUE) %>%
  map_dfr(~ {
    read_csv(.x, show_col_types = FALSE) %>%
      select(State, matches("^[0-9]{4}$")) %>% 
      pivot_longer(cols = -State, names_to = "year", values_to = "total_fatalities")
  }) %>%
  clean_names() %>%
  filter(state %in% my_states) %>%
  mutate(year = as.numeric(year), total_fatalities = as.numeric(total_fatalities)) %>%
  distinct(state, year, .keep_all = TRUE)

# 3. FRED (Real GDP)
#Change the path, because I used Disk D
gdp_clean <- read_csv("D:/Colorado_Project/2005-01-01 to 2019-01-01 Real Gross Domestic Product_ All Industry Total by State (Millions of Chained 2017 Dollars).csv", show_col_types = FALSE) %>%
  select(state = `Region Name`, matches("[0-9]{4}")) %>%
  pivot_longer(cols = -state, names_to = "raw_date", values_to = "real_gdp") %>%
  mutate(year = as.numeric(str_extract(raw_date, "[0-9]{4}"))) %>%
  select(state, year, real_gdp) %>%
  filter(state %in% my_states)

# 4. FRED (Unemployment Rate)
#Change the path, because I used Disk D
unemp_clean <- read_csv("D:/Colorado_Project/2005-01-01 to 2019-01-01 Unemployment Rate by State (Percent).csv", show_col_types = FALSE) %>%
  select(state = `Region Name`, matches("[0-9]{4}")) %>%
  pivot_longer(cols = -state, names_to = "raw_date", values_to = "unemp_rate") %>%
  mutate(year = as.numeric(str_extract(raw_date, "[0-9]{4}"))) %>%
  select(state, year, unemp_rate) %>%
  filter(state %in% my_states)

# 5. FRED (State Population) - NEW FOR PATH B
#Change the path, because I used Disk D
pop_clean <- read_csv("D:/Colorado_Project/2005-01-01 to 2019-01-01 Resident Population by State (Thousands of Persons).csv", show_col_types = FALSE) %>%
  select(state = `Region Name`, matches("[0-9]{4}")) %>%
  pivot_longer(cols = -state, names_to = "raw_date", values_to = "pop_thousands") %>%
  mutate(year = as.numeric(str_extract(raw_date, "[0-9]{4}"))) %>%
  select(state, year, pop_thousands) %>%
  filter(state %in% my_states)

# 6. THE MASTER MERGE & PER CAPITA MATH
final_panel <- fars_clean %>%
  inner_join(gdp_clean, by = c("state", "year")) %>%
  inner_join(unemp_clean, by = c("state", "year")) %>%
  inner_join(pop_clean, by = c("state", "year")) %>%
  mutate(
    actual_pop = pop_thousands * 1000,
    fatalities_per_100k = (total_fatalities / actual_pop) * 100000,
    gdp_per_capita = (real_gdp * 1000000) / actual_pop
  ) %>%
  inner_join(tibble(state = my_states, state_id = seq_along(my_states)), by = "state") %>%
  arrange(state, year)


# =============================================================
# PART 2: SYNTHETIC CONTROL MODELS (PER CAPITA)
# =============================================================

# 1. TRAFFIC FATALITIES (Per 100k)
synth_fars <- final_panel %>%
  synthetic_control(outcome = fatalities_per_100k, unit = state, time = year, i_unit = "Colorado", i_time = 2014, generate_placebos = TRUE) %>%
  generate_predictor(time_window = 2005:2013, unemp_rate = mean(unemp_rate, na.rm = TRUE), gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2005:2007, fatalities_early = mean(fatalities_per_100k, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2008:2010, fatalities_recession = mean(fatalities_per_100k, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2011:2013, fatalities_pre = mean(fatalities_per_100k, na.rm = TRUE)) %>%
  generate_weights(optimization_window = 2005:2013, margin_ipop = 5e-3, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()

# 2. REAL GDP
synth_gdp <- final_panel %>%
  synthetic_control(outcome = real_gdp, unit = state, time = year, i_unit = "Colorado", i_time = 2014, generate_placebos = TRUE) %>%
  
  # Predictors: using the rate-based variables
  generate_predictor(time_window = 2005:2013, unemp_rate = mean(unemp_rate, na.rm = TRUE), fatalities_per_100k = mean(fatalities_per_100k, na.rm = TRUE)) %>%
  
  generate_predictor(time_window = 2005:2007, gdp_early = mean(real_gdp, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2008:2010, gdp_recession = mean(real_gdp, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2011:2013, gdp_pre = mean(real_gdp, na.rm = TRUE)) %>%
  generate_weights(optimization_window = 2005:2013, margin_ipop = 5e-3, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()


# 3. UNEMPLOYMENT RATE
synth_unemp <- final_panel %>%
  synthetic_control(outcome = unemp_rate, unit = state, time = year, i_unit = "Colorado", i_time = 2014, generate_placebos = TRUE) %>%
  generate_predictor(time_window = 2005:2013, gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE), fatalities_per_100k = mean(fatalities_per_100k, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2005:2007, unemp_early = mean(unemp_rate, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2008:2010, unemp_recession = mean(unemp_rate, na.rm = TRUE)) %>%
  generate_predictor(time_window = 2011:2013, unemp_pre = mean(unemp_rate, na.rm = TRUE)) %>%
  generate_weights(optimization_window = 2005:2013, margin_ipop = 5e-3, sigf_ipop = 7, bound_ipop = 6) %>%
  generate_control()


# =============================================================
# 3 RESULTS Tables


cat("\n=== GDP PREDICTOR BALANCE ===\n")
synth_gdp %>% grab_balance_table() %>% print()

cat("\n=== UNEMPLOYMENT PREDICTOR BALANCE ===\n")
synth_unemp %>% grab_balance_table() %>% print()

cat("\n=== FATALITIES PREDICTOR BALANCE ===\n")
synth_fars %>% grab_balance_table() %>% print()




#Predictor Balance Table Combined (For convenience in Latex Report)

# Combine all three tables and add a "Model" column to separate them
combined_balance <- bind_rows(
  synth_gdp %>% grab_balance_table() %>% mutate(Model = "Panel A: Real GDP Model"),
  synth_unemp %>% grab_balance_table() %>% mutate(Model = "Panel B: Unemployment Model"),
  synth_fars %>% grab_balance_table() %>% mutate(Model = "Panel C: Traffic Fatalities Model")
) %>%
  # Reorder columns so the Model name comes first
  select(Model, Variable = variable, Colorado = Colorado, 
         `Synthetic Colorado` = synthetic_Colorado, `Donor Average` = donor_sample)



# # (Previously used to save in Latex, removed format = 'Latex')
combined_balance %>%
  kable(
    digits = 2, 
    booktabs = TRUE,
    caption = "Predictor Balance Across All Three Synthetic Control Models",
    label = "tab:combined_balance"
  )  





# Printed Significance Ratios to Console

#GDP
synth_gdp %>% grab_significance() %>% filter(unit_name == "Colorado")

#FARS
synth_fars %>% grab_significance() %>% filter(unit_name == "Colorado")

#Unemployement
synth_unemp %>% grab_significance() %>% filter(unit_name == "Colorado")


# ---------------------------------------------------------
# COMBINED STATISTICAL SIGNIFICANCE TABLE EXPORT
# ---------------------------------------------------------
# Build the significance dataset

combined_sig <- bind_rows(
  synth_gdp %>% grab_significance() %>% filter(unit_name == "Colorado") %>% mutate(Outcome = "Real GDP"),
  synth_unemp %>% grab_significance() %>% filter(unit_name == "Colorado") %>% mutate(Outcome = "Unemployment Rate"),
  synth_fars %>% grab_significance() %>% filter(unit_name == "Colorado") %>% mutate(Outcome = "Traffic Fatalities")
) %>%
  select(Outcome, pre_mspe, post_mspe, mspe_ratio, rank, fishers_exact_pvalue)

# (Previously used to save in Latex, removed format = 'Latex')
combined_sig %>%
  kable(
    digits = 3, 
    booktabs = TRUE,
    col.names = c("Outcome Variable", "Pre-MSPE", "Post-MSPE", "MSPE Ratio", "Rank", "P-Value")
  ) 





#All the plots 



year_scale <- scale_x_continuous(breaks = 2005:2019)

# Trend Plots
synth_gdp %>% plot_trends() + ggtitle("Real GDP: Colorado vs. Synthetic (Millions USD)") + year_scale

synth_unemp %>% plot_trends() + ggtitle("Unemployment Rate: Colorado vs. Synthetic") + year_scale

synth_fars %>% plot_trends() + ggtitle("Fatalities per 100k: Colorado vs. Synthetic") + year_scale



# Gap Plots
synth_gdp %>% plot_differences() + ggtitle("Real GDP: Treatment Effect Gap") + year_scale


synth_unemp %>% plot_differences() + ggtitle("Unemployment Rate: Treatment Effect Gap") + year_scale



# Placebo Plots
synth_gdp %>% plot_placebos() + ggtitle("Placebo Test: Real GDP") + year_scale


synth_unemp %>% plot_placebos() + ggtitle("Placebo Test: Unemployment Rate") + year_scale


synth_fars %>% plot_placebos() + ggtitle("Placebo Test: Fatalities per 100k") + year_scale










