########################################
## Monongahela Water Quality
########################################

library(raster)
library(sp)
library(tidyverse)
library(lubridate)
library(rgdal)
library(maptools)
library(maps)
library(lazyeval)


#----------------------------------------------------------------------------
# water quality samples
water_dat <- read.csv("raw_data/StreamChemMasterResults_03152017.csv", 
                      stringsAsFactors = FALSE,
                      na.strings = c(".", "NS", "ND", "<0.04", "<0.03")) %>% 
  select(1:17)

water_dat$Collection <- mdy(water_dat$Collection) # convert to date format


# shapefile with liming
lime_shp <- readOGR("GIS/waterchemsites_master_1213_n83")
lime_shp <- spTransform(lime_shp, CRS("+proj=longlat +datum=WGS84"))
lime_points <- data.frame(Latitude = lime_shp@coords[,2], Longitude = lime_shp@coords[,1], lime_shp@data)
lime_points$ID <- as.character(lime_points$ID)



# join the two together to explore liming and water trends
water <- lime_points %>% 
  select(ID, Latitude, Longitude, Water_Unit_Type = Site_Locat, Monitoring_Site_Name = Waterbody, Limed) %>% 
  left_join(water_dat, by = "ID") %>% 
  mutate(FS_ID = rep(NA, n()),
         CSU_ID = rep(NA, n()),
         ST = rep("WV", n()),
         Project_Name = rep(NA, n()),
         Sample_Type = rep(NA, n()),
         NF_Wilderness = rep("Monongahela NF", n()),
         Time_Sampled = rep(NA, n()),
         Lab_Receive_Date = rep(NA, n()),
         Sample_Collector= rep(NA, n()),
         Smpl_Wgt	= rep(NA, n()),
         Field_Filtered	= rep(NA, n()), 
         Field_pH= rep(NA, n()),
         Field_Cond= rep(NA, n()),
         NH4_mg_per_l = rep(NA, n()),
         NH4_N_mg_per_l = rep(NA, n()),
         NO3_mg_per_l = rep(NA, n()),
         F_mg_per_l = rep(NA, n()),
         TP_mg_per_l = rep(NA, n()),
         ICP_mg_per_l = rep(NA, n())) %>% 
  filter(is.na(Limed)) %>% 
  select(FS_ID,
        CSU_ID,
        ST,
        Project_Name,
        Monitoring_Site_Name,
        Local_Site_ID = ID,
        Latitude,
        Longitude,
        Water_Unit_Type,
        Sample_Type,
        NF_Wilderness,
        Time_Sampled,
        Sample_Date = Collection,
        Lab_Receive_Date,
        Sample_Collector,
        Smpl_Wgt,
        Field_Filtered,
        Field_pH,
        Field_Cond,
        pH,
        EC_Cond_uS_per_cm = Cond_uS_cm,
        ANC_uE_per_L = ANC_ueq_L,
        NH4_mg_per_l,
        NH4_N_mg_per_l,
        NO3_mg_per_l,
        NO3_N_mg_per_l = NO3_N_mg_L,
        CL_mg_per_l = CL_mg_L,
        SO4_mg_per_l = SO4_mg_L,
        F_mg_per_l,
        TP_mg_per_l,
        DOC_mg_per_l = DOC_mg_L,
        TDN_mg_per_l = TN_mg_L,
        ICP_mg_per_l,
        Ca_mg_per_l = Ca_mg_L,
        Mg_mg_per_l = Mg_mg_L,
        Na_mg_per_l = NA_mg_L,
        K_mg_per_l = K_mg_L)

# in MNF data but not template

NH3_N_mg_L











# join the two together to explore liming and water trends
water <- lime_points %>% 
  select(ID, Latitude, Longitude, Water_Unit_Type = Site_Locat, Monitoring_Site_Name = Waterbody, Limed) %>% 
  left_join(water_dat, by = "ID") %>% 
  mutate(FS_ID = rep("", n()),
         CSU_ID = rep("", n()),
         ST = rep("WV", n()),
         Project_Name = rep("", n()),
         Sample_Type = rep("", n()),
         NF_Wilderness = rep("Monongahela NF", n()),
         Time_Sampled = rep("", n()),
         Lab_Receive_Date = rep("", n()),
         Sample_Collector= rep("", n()),
         Smpl_Wgt	= rep("", n()),
         Field_Filtered	= rep("", n()), 
         Field_pH= rep("", n()),
         Field_Cond= rep("", n()),
         NH4_mg_per_l = rep("", n()),
         NH4_N_mg_per_l = rep("", n()),
         NO3_mg_per_l = rep("", n()),
         F_mg_per_l = rep("", n()),
         TP_mg_per_l = rep("", n()),
         ICP_mg_per_l = rep("", n())) %>% 
  filter(is.na(Limed)) %>% 
  select(FS_ID,
         CSU_ID,
         ST,
         Project_Name,
         Monitoring_Site_Name,
         Local_Site_ID = ID,
         Latitude,
         Longitude,
         Water_Unit_Type,
         Sample_Type,
         NF_Wilderness,
         Time_Sampled,
         Sample_Date = Collection,
         Lab_Receive_Date,
         Sample_Collector,
         Smpl_Wgt,
         Field_Filtered,
         Field_pH,
         Field_Cond,
         pH,
         EC_Cond_uS_per_cm = Cond_uS_cm,
         ANC_uE_per_L = ANC_ueq_L,
         NH4_mg_per_l,
         NH4_N_mg_per_l,
         NO3_mg_per_l,
         NO3_N_mg_per_l = NO3_N_mg_L,
         CL_mg_per_l = CL_mg_L,
         SO4_mg_per_l = SO4_mg_L,
         F_mg_per_l,
         TP_mg_per_l,
         DOC_mg_per_l = DOC_mg_L,
         TDN_mg_per_l = TN_mg_L,
         ICP_mg_per_l,
         Ca_mg_per_l = Ca_mg_L,
         Mg_mg_per_l = Mg_mg_L,
         Na_mg_per_l = NA_mg_L,
         K_mg_per_l = K_mg_L)

write_csv(water, "data/MNF_water_data.csv")










