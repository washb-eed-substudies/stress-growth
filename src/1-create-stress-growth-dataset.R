


rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))


d<-read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))




#---------------------------------------------------------------------------------------------
# transform outcome distributions
#---------------------------------------------------------------------------------------------
d <- d %>% 
  mutate(
    t3_saa_z01_raw=t3_saa_z01, 
    t3_saa_z02_raw=t3_saa_z02, 
    t3_cort_z01_raw=t3_cort_z01, 
    t3_cort_z03_raw=t3_cort_z03, 
    t2_f2_8ip_raw=t2_f2_8ip, 
    t2_f2_23d_raw=t2_f2_23d, 
    t2_f2_VI_raw=t2_f2_VI,
    t2_f2_12i_raw=t2_f2_12i, 
    t3_gcr_mean_raw=t3_gcr_mean, 
    t3_gcr_cpg12_raw=t3_gcr_cpg12,
    t3_saa_z01=log(t3_saa_z01), 
    t3_saa_z02=log(t3_saa_z02), 
    t3_cort_z01=log(t3_cort_z01), 
    t3_cort_z03=log(t3_cort_z03), 
    t2_f2_8ip=log(t2_f2_8ip), 
    t2_f2_23d=log(t2_f2_23d), 
    t2_f2_VI=log(t2_f2_VI),
    t2_f2_12i=log(t2_f2_12i), 
    t3_gcr_mean2=logit(t3_gcr_mean/100), 
    t3_gcr_cpg12=logit(t3_gcr_cpg12/100))


#Clean inf values
d <- do.call(data.frame,lapply(d, function(x) replace(x, is.infinite(x),NA)))



#---------------------------------------------------------------------------------------------
# calc combined iso variable
#---------------------------------------------------------------------------------------------



# Combined exposures: To determine overall oxidative stress, we will combine our four measures of urinary F2-
# isoprostanes: iPF(2a)-III, 2,3-dinor-iPF(2a)-III, iPF(2a)-VI, and 8,12-iso-iPF(2a)-VI that are
# consistent with prior oxidative stress operationalization into a single score.29 We will use
# the first principal component of a principal components analysis of the four measures of
# urinary F2-isoprostanes as the oxidative stress score if all measures are correlated with each other (P-value < 0.2), 
# otherwise we will analyze the urinary F2-isoprostanes separately.

#Get correlation of isoprostanes
iso <- d %>% select(c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i"))       

cor(iso, use="pairwise.complete.obs")
cor.test(iso[,1], iso[,2])$p.value < 0.2
cor.test(iso[,1], iso[,3])$p.value < 0.2
cor.test(iso[,1], iso[,4])$p.value < 0.2
cor.test(iso[,2], iso[,3])$p.value < 0.2
cor.test(iso[,2], iso[,4])$p.value < 0.2
cor.test(iso[,3], iso[,4])$p.value < 0.2


#isoprostanes are significantly correlated, so calculate 1st principal component

df <-  d %>% select(c("childid","t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i")) %>% as.data.frame()
dim(df)
df <- df[complete.cases(df),]
dim(df)

# #Select assets and seperate out ID
id<-subset(df, select=c("childid")) #drop subjectid
df<-df[,which(!(colnames(df) %in% c("childid")))]

##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(df) 


## To get the first principal component in a variable ##
load <- loadings(princ.return)[,1]   

pr.cp <- as.matrix(df) %*% load  ## Matrix multiplication of the input data with the loading for the 1st PC gives us the 1st PC in matrix form. 

df$iso.pca <- as.numeric(pr.cp) ## Gives us the 1st PC in numeric form in pr.

#examine combined score
hist(df$t2_f2_8ip)
hist(df$t2_f2_23d)
hist(df$t2_f2_VI)
hist(df$t2_f2_12i)
hist(df$iso.pca)

#check direction between individual isoprostanes and the combined score
ggplot(df, aes(x=t2_f2_8ip, y=iso.pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_23d, y=iso.pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_VI, y=iso.pca)) + geom_point() + geom_smooth()
ggplot(df, aes(x=t2_f2_12i, y=iso.pca)) + geom_point() + geom_smooth()

#merge combined score back into main dataset
df.pca <- data.frame(childid=id, iso.pca=df$iso.pca)

dfull <- left_join(d, df.pca, by="childid")
hist(dfull$iso.pca)


saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Andrew/stress_growth_data.RDS"))


