library(tidyverse)
library(here)
library(readxl)

exposure <- read.csv("ExposureConcentrationsRaw.csv")

urban_rural <- read_xlsx("urban_rural.xlsx")

formaldehyde <- exposure %>% 
  filter(Pollutant.Name == "FORMALDEHYDE") %>% 
  select(c(County, Population, Total.Exposure.Conc, 
  OR.LightDuty.OffNetwork.Gas.Exposure.Conc:BACKGROUND.Exposure.Conc))

formaldehyde <- formaldehyde %>% 
  group_by(County) %>% 
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
  ungroup()

formaldehyde <- formaldehyde[-34,]

formaldehyde_joined <- left_join(formaldehyde, urban_rural, by = "County")

urban <- formaldehyde_joined %>% 
  filter(URBAN_RURAL == "Urban")
rural <- formaldehyde_joined %>% 
  filter(URBAN_RURAL == "Rural")

urban_rural_plot <- ggplot(formaldehyde_joined, aes(x = Total.Exposure.Conc, color = URBAN_RURAL)) +
  geom_boxplot() 
urban_rural_plot

populationPlot <- ggplot(formaldehyde_joined, aes(x = Population, color = URBAN_RURAL)) + 
  geom_histogram()
populationPlot

(exposureXpopulation <- ggplot(formaldehyde_joined, aes(x = Population, y = Total.Exposure.Conc, color = URBAN_RURAL)) + 
  geom_point()) + 
  geom_smooth(method = "lm")

# Checking Normality 
(urban_plot <- ggplot(urban, aes(x = Total.Exposure.Conc)) + 
  geom_histogram(bins = 8))

# checking Normality 
(rural_plot <- ggplot(rural, aes(x = Total.Exposure.Conc)) + 
  geom_histogram(bins = 8))

# Running Mann Whitney U test because the samples aren't normally distributed (Can do this for multiple pollutants or grouping by acitivity)
mann_whit_test <- wilcox.test(urban$Total.Exposure.Conc, rural$Total.Exposure.Conc, paired = FALSE)
mann_whit_test



            
            
            
            


