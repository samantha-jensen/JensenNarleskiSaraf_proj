library(tidyverse)
library(here)

exposure <- read.csv("ExposureConcentrationsRaw.csv")

formaldehyde <- exposure %>% 
  filter(Pollutant.Name == "FORMALDEHYDE") %>% 
  select(c(County, Tract, Total.Exposure.Conc, 
  OR.LightDuty.OffNetwork.Gas.Exposure.Conc:OR.HeavyDuty.Hoteling.Exposure.Conc))

onroad_formaldehyde <- formaldehyde %>% 
  mutate(onroad_proportion = (OR.LightDuty.OffNetwork.Gas.Exposure.Conc + 
                         OR.LightDuty.OffNetwork.Diesel.Exposure.Conc + 
                         OR.HeavyDuty.OffNetwork.Gas.Exposure.Conc +
                        OR.HeavyDuty.OffNetwork.Diesel.Exposure.Conc + 
                        OR.LightDuty.OnNetwork.Gas.Exposure.Conc + 
                        OR.LightDuty.OnNetwork.Diesel.Exposure.Conc + 
                       OR.HeavyDuty.OnNetwork.Gas.Exposure.Conc +
                       OR.HeavyDuty.OnNetwork.Diesel.Exposure.Conc + 
                      OR.Refueling.Exposure.Conc + 
                     OR.HeavyDuty.Hoteling.Exposure.Conc)/Total.Exposure.Conc)

nonroad_form <- exposure %>% 
  filter(Pollutant.Name == "FORMALDEHYDE") %>% 
  select(c(County, Tract, Total.Exposure.Conc, 
          NR.Recreational.inc.PleasureCraft.Exposure.Conc:NR..Point.Railyards.Exposure.Conc)) %>% 
  mutate(non_road_proportion = (NR.Recreational.inc.PleasureCraft.Exposure.Conc + 
           NR.Construction.Exposure.Conc + NR.CommercialLawnGarden.Exposure.Conc + 
          NR.ResidentialLawnGarden.Exposure.Conc + NR.Agriculture.Exposure.Conc + 
           NR.CommercialEquipment.Exposure.Conc + NR.AllOther.Exposure.Conc + 
           NR.CMV_C1C2.Exposure.Conc + NR.CMV_C3.Exposure.Conc + 
          NR.CMV_C1C2C3_underway.Exposure.Conc + NR.Locomotives.Exposure.Conc + 
          NR.Point.Airports.Exposure.Conc + NR..Point.Railyards.Exposure.Conc)/ Total.Exposure.Conc)

summary(nonroad_form$non_road_proportion)
summary(onroad_formaldehyde$onroad_proportion)

ggplot(nonroad_form, aes(x = non_road_proportion)) + 
  geom_histogram()

nonroad_bycounty <- nonroad_form %>% 
  group_by(County) %>% 
  summarise(NR.Recreational.inc.PleasureCraft.Exposure.Conc = mean(NR.Recreational.inc.PleasureCraft.Exposure.Conc), 
            NR.Construction.Exposure.Conc = mean(NR.Construction.Exposure.Conc), 
            NR.CommercialLawnGarden.Exposure.Conc = mean(NR.CommercialLawnGarden.Exposure.Conc), 
            NR.ResidentialLawnGarden.Exposure.Conc = mean(NR.ResidentialLawnGarden.Exposure.Conc), 
            NR.Agriculture.Exposure.Conc = mean(NR.Agriculture.Exposure.Conc), 
            NR.CommercialEquipment.Exposure.Conc = mean(NR.CommercialEquipment.Exposure.Conc), 
            NR.AllOther.Exposure.Conc = mean(NR.AllOther.Exposure.Conc), 
            NR.CMV_C1C2.Exposure.Conc = mean(NR.CMV_C1C2.Exposure.Conc), 
            NR.CMV_C3.Exposure.Conc = mean(NR.CMV_C3.Exposure.Conc), 
            NR.CMV_C1C2C3_underway.Exposure.Conc = mean(NR.CMV_C1C2C3_underway.Exposure.Conc), 
            NR.Locomotives.Exposure.Conc = mean(NR.Locomotives.Exposure.Conc), 
            NR.Point.Airports.Exposure.Conc = mean(NR.Point.Airports.Exposure.Conc), 
            NR..Point.Railyards.Exposure.Conc = mean(NR..Point.Railyards.Exposure.Conc))


nonroad_bycounty <- nonroad_bycounty[-34, ]
### Need to subtract entire state row 

            
            
            
            


