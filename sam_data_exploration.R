library(tidyverse)
library(here)

exposure <- read.csv("ExposureConcentrationsRaw.csv")

formaldehyde <- exposure %>% 
  filter(Pollutant.Name == "FORMALDEHYDE") %>% 
  select(c(County, Tract, Total.Exposure.Conc, 
  OR.LightDuty.OffNetwork.Gas.Exposure.Conc:OR.HeavyDuty.Hoteling.Exposure.Conc))


