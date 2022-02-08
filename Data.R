# Loading the required libraries
library(data.table)
library(WDI)
library(COVID19)
library(ggplot2)
library(stargazer)
library(plm)

# Determining the high income countries (countries with high GDP per capita)
country_info <- WDI(indicator = "NY.GDP.PCAP.PP.CD",
                    start = 2020,
                    end = 2020)
setDT(country_info)
cinfo <- country_info[order(country_info$NY.GDP.PCAP.PP.CD,decreasing = TRUE)]
isoc <- c('XU','V4','XD','OE','XC','EU','Z7','B8','S4','Z4','S1')
for (isocode in isoc) {
  cinfo <- cinfo[iso2c != isocode]
}
cinfo <- cinfo[1:60]
country_names <- cinfo[['country']]

# Covid19 dataset
data1 <- covid19(country = country_names)
setDT(data1)
data <- data1[,c("date","confirmed","deaths","recovered","tests","people_vaccinated",
                 "people_fully_vaccinated","hosp","icu","vent","school_closing",
                 "workplace_closing","cancel_events","gatherings_restrictions",
                 "transport_closing","stay_home_restrictions",
                 "internal_movement_restrictions","international_movement_restrictions",
                 "information_campaigns","testing_policy","contact_tracing",
                 "facial_coverings","vaccination_policy","elderly_people_protection",
                 "government_response_index","containment_health_index","economic_support_index",
                 "administrative_area_level_1","population","iso_alpha_3",
                 "key_google_mobility")]
data[,confirmedprop := confirmed/population]
data[,deathsprop := deaths/population]
data[,recoveredprop := recovered/population]
data[,testsprop := tests/population]
data[,vaccinatedprop := people_vaccinated/population]
data[,fullvaccinatedprop := people_fully_vaccinated/population]
data <- setnames(data, "administrative_area_level_1", "country")

# Selecting June as the pivotal point to categorize countries
data_june <- data[data$date == "2021-06-01"]
data_june <- data_june[order(data_june$fullvaccinatedprop, decreasing = TRUE),]

data_june[data_june$fullvaccinatedprop >= 0.35]$country
data_june[data_june$fullvaccinatedprop <= 0.05]$country

# Dividing counties as treatment or control group according to their fully vaccinated population percentage
# Countries with fully vaccination percentage higher than 35% are in the treatment group
treatment_group <- c(data_june[data_june$fullvaccinatedprop >= 0.35]$country)
# Countries with fully vaccination percentage lower than 5% are in the control group
control_group <- c(data_june[data_june$fullvaccinatedprop <= 0.05]$country)

# Plotting the data for visualization
clist <- c(treatment_group, control_group)
data_plot <- data[country %in% clist]
data_plot <- data_plot[data_plot$date >= "2021-01-01" & data_plot$date <= "2021-10-01"]
ggplot(data_plot, aes(x = date, y = fullvaccinatedprop, color = country)) +
  geom_line(size = 1) + 
  labs(title = "Vaccinated people % of Countries over the Years",
       y = "Vaccinated people %",
       x = "Date")

# Combining groups into a dataset
countries <- c(treatment_group, control_group)
countries
data_grouped <- data[data$country %in% countries]

# Creating a before-after column to use in diff-in-diff analysis
data_before <- data_grouped[data_grouped$date == "2021-01-01"]
data_after <- data_grouped[data_grouped$date == "2021-10-01"]
data_diff <- rbind(data_before, data_after)
data_diff[, after := ifelse(date >= "2021-05-01", 1, 0)]
data_diff[, date := ifelse(after == 1, "2021 October", "2021 January")]

# Creating a column to index the countries by their groups
data_diff[, treatment := ifelse(country %in% treatment_group, 1, 0)]

# Selecting the required columns in a certain order
data_diff <- data_diff[, c(1, 28, 32, 33, 38, 39, 37, 25, 11, 16)]

# Adding the logarithmic value of the parameters as new columns
data_diff[,log_death_prop := log(deathsprop*100)]
data_diff[,log_case_prop := log(confirmedprop*100)]

# CONFIRMED CASES ANALYSIS
# Plotting the before after situation
plot_data <- data_diff %>% 
  
  mutate(treatment = factor(treatment, labels = c("Control Group", "Treatment Group")),
         after = factor(after, labels = c("2021 January", "2021 October"))) %>% 
  group_by(treatment, after) %>% 
  summarize(mean_log_confirmed_prop = mean(log_case_prop),
            se_log_confirmed_prop = sd(log_case_prop) / sqrt(n()),
            upper = mean_log_confirmed_prop + (1.96 * se_log_confirmed_prop),
            lower = mean_log_confirmed_prop + (-1.96 * se_log_confirmed_prop)) 

ggplot(plot_data, aes(x = treatment, y = mean_log_confirmed_prop)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  color = "darkgreen", size = 1) +
  facet_wrap(vars(after))

# Plotting the before after situation 2
ggplot(plot_data, aes(x = after, y = mean_log_confirmed_prop, color = treatment)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) + 
  geom_line(aes(group = treatment))

# Creating the table showing the required values for diff-in-diff
diffs <- data_diff %>% 
  group_by(after, treatment) %>% 
  summarize(mean_log_confirmed_prop = mean(log_case_prop))
diffs

# Selecting values
before_treatment <- diffs %>% 
  filter(after == 0, treatment == 1) %>% 
  pull(mean_log_confirmed_prop)

before_control <- diffs %>% 
  filter(after == 0, treatment == 0) %>% 
  pull(mean_log_confirmed_prop)

after_treatment <- diffs %>% 
  filter(after == 1, treatment == 1) %>% 
  pull(mean_log_confirmed_prop)

after_control <- diffs %>% 
  filter(after == 1, treatment == 0) %>% 
  pull(mean_log_confirmed_prop)

diff_treatment_before_after <- after_treatment - before_treatment
diff_treatment_before_after

diff_control_before_after <- after_control - before_control
diff_control_before_after

diff_diff <- diff_treatment_before_after - diff_control_before_after
diff_diff

# Detailed diff-in-diff graph
ggplot(diffs, aes(x = as.factor(after), 
                  y = mean_log_confirmed_prop, 
                  color = as.factor(treatment))) + 
  geom_point() +
  geom_line(aes(group = as.factor(treatment))) +
  
  annotate(geom = "segment", x = "0", xend = "1",
           y = before_treatment, yend = after_treatment - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = after_treatment, yend = after_treatment - diff_diff,
           linetype = "dotted", color = "blue")

# Diff-in-diff in regression model
model_simple <- lm(log_case_prop ~ treatment + after + treatment * after,
                   data = data_diff)
tidy(model_simple)

# DEATHS ANALYSIS
# Plotting the before after situation
plot_data_2 <- data_diff %>% 
  
  mutate(treatment = factor(treatment, labels = c("Control Group", "Treatment Group")),
         after = factor(after, labels = c("2021 January", "2021 October"))) %>% 
  group_by(treatment, after) %>% 
  summarize(mean_log_deaths_prop = mean(log_death_prop),
            se_log_deaths_prop = sd(log_death_prop) / sqrt(n()),
            upper = mean_log_deaths_prop + (1.96 * se_log_deaths_prop),
            lower = mean_log_deaths_prop + (-1.96 * se_log_deaths_prop)) 

ggplot(plot_data_2, aes(x = treatment, y = mean_log_deaths_prop)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  color = "darkgreen", size = 1) +
  facet_wrap(vars(after))

# Plotting the before after situation 2
ggplot(plot_data_2, aes(x = after, y = mean_log_deaths_prop, color = treatment)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) + 
  geom_line(aes(group = treatment))

# Creating the table showing the required values for diff-in-diff
diffs_2 <- data_diff %>% 
  group_by(after, treatment) %>% 
  summarize(mean_log_deaths_prop = mean(log_death_prop))
diffs_2

# Selecting values
before_treatment_2 <- diffs_2 %>% 
  filter(after == 0, treatment == 1) %>% 
  pull(mean_log_deaths_prop)

before_control_2 <- diffs_2 %>% 
  filter(after == 0, treatment == 0) %>% 
  pull(mean_log_deaths_prop)

after_treatment_2 <- diffs_2 %>% 
  filter(after == 1, treatment == 1) %>% 
  pull(mean_log_deaths_prop)

after_control_2 <- diffs_2 %>% 
  filter(after == 1, treatment == 0) %>% 
  pull(mean_log_deaths_prop)

diff_treatment_before_after_2 <- after_treatment_2 - before_treatment_2
diff_treatment_before_after_2

diff_control_before_after_2 <- after_control_2 - before_control_2
diff_control_before_after_2

diff_diff_2 <- diff_treatment_before_after_2 - diff_control_before_after_2
diff_diff_2

# Detailed diff-in-diff graph
ggplot(diffs_2, aes(x = as.factor(after), 
                    y = mean_log_deaths_prop, 
                    color = as.factor(treatment))) + 
  geom_point() +
  geom_line(aes(group = as.factor(treatment))) +
  
  annotate(geom = "segment", x = "0", xend = "1",
           y = before_treatment_2, yend = after_treatment_2 - diff_diff_2,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = after_treatment_2, yend = after_treatment_2 - diff_diff_2,
           linetype = "dotted", color = "blue")

# Diff-in-diff in regression model
model_simple_2 <- lm(log_death_prop ~ treatment + after + treatment * after,
                     data = data_diff)
tidy(model_simple_2)
