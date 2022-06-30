###---------------------------------------------------
###   06-VALIDATION: BINARY LOGISTIC REGRESSION
###---------------------------------------------------

library( tidyverse )
library( survey )


wd <- "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/"

setwd( paste0(wd,"Data-Rodeo") )
       
dat <- readRDS( "04-Analytic-Data.rds" ) %>%
  dplyr::filter( is.na( WTDR18YR ) == F )

# Import result-generating functions
setwd( paste0(wd,"R") )

source('utils.R')

# covariates to adjust for in model
covars.logit<-c('Race_binary','Gender','Age','KCAL','BMXBMI','HHSize','FoodAsstPnowic',
                'SmokStat','fipr','KCAL','WeekMetMin','Education_bin',
                'PrimaryCAGroup','CCI_Score')

# tabulate results using function and save table
setwd( paste0(wd,"Manuscript/Tables") )

results_function(df=dat,
                 covariates=covars.logit,
                 variables=c('FS_ENet','Age_ENet','FdAs_ENet','HHS_ENet','PC1','PC2'),
                 cuts=5,
                 subset.condition='Diet.ext.ind.reg==1') %>%
  write.table(.,'logit-results.txt',sep=",",row.names=FALSE)


## Restricted-Cubic Spline Curves for Logistic Regression Models

dat.sub <- dat %>%
  dplyr::filter(Diet.ext.ind.reg==1) %>% # use data inclusions/exclusions subset for this section
  dplyr::mutate(wts.norm = WTDR18YR / mean(WTDR18YR)) # create normalized weights

dat.sub$PrimaryCAGroup<-droplevels(dat.sub$PrimaryCAGroup) # drop levels with zero observations

logit_splines(df=dat.sub, x='FS_ENet', y='BinFoodSecHH', knots=5, covariates=covars.logit, 
              wts='wts.norm', referent='median')

#Save
setwd('/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES_FI_CA_Diet/Figures')
ggsave("logit_splines.jpeg",width = 30, height = 20, units = "cm")


