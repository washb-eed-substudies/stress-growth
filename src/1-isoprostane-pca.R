

# Combined exposures: To determine overall oxidative stress, we will combine our four measures of urinary F2-
# isoprostanes: iPF(2a)-III, 2,3-dinor-iPF(2a)-III, iPF(2a)-VI, and 8,12-iso-iPF(2a)-VI that are
# consistent with prior oxidative stress operationalization into a single score.29 We will use
# the first principal component of a principal components analysis of the four measures of
# urinary F2-isoprostanes as the oxidative stress score if all measures are correlated with each other (P-value < 0.2), 
# otherwise we will analyze the urinary F2-isoprostanes separately.

rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))


d<-read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))

library(psych)

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


