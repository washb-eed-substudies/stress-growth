
rm(list=ls())

source(here::here("0-config.R"))


stress <- readRDS(paste0(dropboxDir,"Data/Cleaned/Andrew/clean_stress_dataset_andrew.RDS"))

anthro <- read.csv("C:/Users/andre/Dropbox/WASHB-Bangladesh-Data/1-primary-outcome-datasets/washb-bangladesh-anthro.csv")
anthro <

load(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-telo-growth-covariates-telolab-anthro.RData"))

d$dataid <- as.numeric(d$dataid)
df <- left_join(stress, d, by = c("childid", "dataid", "clusterid"))
  
head(df)

table(is.na(df$t3_saa_z01), is.na(df$laz_t2))

colnames(df)

# df <- left_join(stress, anthro, by = c("childid"))
# 
# head(df)

saveRDS(df, paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data.RDS"))











