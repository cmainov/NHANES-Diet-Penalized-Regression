### FPED Data Import
# user guide: https://www.ars.usda.gov/ARSUserFiles/80400530/pdf/fped/FPED_0506.pdf

library( haven ) 
library( tidyverse ) 
library( RNHANES ) 

setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Raw/FPED' ) 

#######CYCLE 1999-2002##########

dr99 <- read_sas( 'pyr_tot01.sas7bdat' ) 
names( dr99 ) 
#1999-2002 has only 1 day of dietary interview ( 1 24 hr recall )  whereas 2003-2017 has 2 days
foodgrpshr9902 <- dr99%>%
  mutate( ProcessedMts=M_FRANK ) %>%
  mutate( RedMts=M_MEAT ) %>%
  mutate( OrganMts=M_ORGAN ) %>%
  mutate( Poultry=M_POULT ) %>%
  mutate( Fish_Hi=( M_FISH_HI )  ) %>%
  mutate( Fish_Lo=( M_FISH_LO )  ) %>%
  mutate( Eggs=M_EGG ) %>%
  mutate( SolidFats=DISCFAT_SOL ) %>%
  mutate( Oils=DISCFAT_OIL ) %>%
  mutate( Milk=D_MILK ) %>%
  mutate( Yogurt=D_YOGURT ) %>%
  mutate( Cheese=D_CHEESE ) %>%
  mutate( Alcohol=A_BEV ) %>%
  mutate( FruitOther=F_OTHER ) %>%
  mutate( F_CitMelBer=F_CITMLB ) %>%
  mutate( Tomatoes=V_TOMATO ) %>%
  mutate( GreenLeafy=V_DRKGR ) %>%
  mutate( DarkYlVeg=V_DPYEL ) %>%
  mutate( OtherVeg=V_OTHER ) %>%
  mutate( Potatoes=V_POTATO ) %>%
  mutate( OtherStarchyVeg=V_STARCY ) %>%
  mutate( Legumes=LEGUMES ) %>%
  mutate( Soy=M_SOY ) %>%
  mutate( RefinedGrain=G_NWHL ) %>%
  mutate( WholeGrain=G_WHL ) %>%
  mutate( Nuts=M_NUTSD ) %>%
  mutate( AddedSugars=ADD_SUG ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars,SDDSRVYR ) 


ncol( foodgrpshr9902 ) 

paste( colnames( foodgrpshr9902[,1:28] ) ,collapse=',' ) 

#######CYCLE 2003-2004##########
dr10304 <- read_sas( 'pyr_tot_d1.sas7bdat' ) 
dr20304 <- read_sas( 'pyr_tot_d2.sas7bdat' ) 
mergedde0304 <- inner_join( dr10304,dr20304,by='SEQN' ) 
names( mergedde0304 ) 
levels( as.factor( foodgrpshr0304$DR2DRSTZ )  ) 
levels( as.factor( foodgrpshr0304$DR1DRSTZ )  ) 
#indicates that only observations with dietary recalls ranked as ' 	Reliable and met the minimum criteria' were kept'

foodgrpshr0304 <- mergedde0304%>%
  mutate( ProcessedMts=ifelse( is.na( M_FRANK.x ) ==FALSE & is.na( M_FRANK.y ) ==FALSE,( M_FRANK.x+M_FRANK.y ) /2,
                             ifelse( is.na( M_FRANK.x ) ==TRUE & is.na( M_FRANK.y ) ==FALSE,M_FRANK.y,
                                    ifelse( is.na( M_FRANK.x ) ==FALSE & is.na( M_FRANK.y ) ==TRUE,M_FRANK.x,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( is.na( M_MEAT.x ) ==FALSE & is.na( M_MEAT.y ) ==FALSE,( M_MEAT.x+M_MEAT.y ) /2,
                       ifelse( is.na( M_MEAT.x ) ==TRUE & is.na( M_MEAT.y ) ==FALSE,M_MEAT.y,
                              ifelse( is.na( M_MEAT.x ) ==FALSE & is.na( M_MEAT.y ) ==TRUE,M_MEAT.x,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( is.na( M_ORGAN.x ) ==FALSE & is.na( M_ORGAN.y ) ==FALSE,( M_ORGAN.x+M_ORGAN.y ) /2,
                         ifelse( is.na( M_ORGAN.x ) ==TRUE & is.na( M_ORGAN.y ) ==FALSE,M_ORGAN.y,
                                ifelse( is.na( M_ORGAN.x ) ==FALSE & is.na( M_ORGAN.y ) ==TRUE,M_ORGAN.x,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( is.na( M_POULT.x ) ==FALSE & is.na( M_POULT.y ) ==FALSE,( M_POULT.x+M_POULT.y ) /2,
                        ifelse( is.na( M_POULT.x ) ==TRUE & is.na( M_POULT.y ) ==FALSE,M_POULT.y,
                               ifelse( is.na( M_POULT.x ) ==FALSE & is.na( M_POULT.y ) ==TRUE,M_POULT.x,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( is.na( M_FISH_HI.x ) ==FALSE & is.na( M_FISH_HI.y ) ==FALSE,( M_FISH_HI.x+M_FISH_HI.y ) /2,
                        ifelse( is.na( M_FISH_HI.x ) ==TRUE & is.na( M_FISH_HI.y ) ==FALSE,M_FISH_HI.y,
                               ifelse( is.na( M_FISH_HI.x ) ==FALSE & is.na( M_FISH_HI.y ) ==TRUE,M_FISH_HI.x,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( is.na( M_FISH_LO.x ) ==FALSE & is.na( M_FISH_LO.y ) ==FALSE,( M_FISH_LO.x+M_FISH_LO.y ) /2,
                        ifelse( is.na( M_FISH_LO.x ) ==TRUE & is.na( M_FISH_LO.y ) ==FALSE,M_FISH_LO.y,
                               ifelse( is.na( M_FISH_LO.x ) ==FALSE & is.na( M_FISH_LO.y ) ==TRUE,M_FISH_LO.x,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( is.na( M_EGG.x ) ==FALSE & is.na( M_EGG.y ) ==FALSE,( M_EGG.x+M_EGG.y ) /2,
                     ifelse( is.na( M_EGG.x ) ==TRUE & is.na( M_EGG.y ) ==FALSE,M_EGG.y,
                            ifelse( is.na( M_EGG.x ) ==FALSE & is.na( M_EGG.y ) ==TRUE,M_EGG.x,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( is.na( DISCFAT_SOL.x ) ==FALSE & is.na( DISCFAT_SOL.y ) ==FALSE,( DISCFAT_SOL.x+DISCFAT_SOL.y ) /2,
                          ifelse( is.na( DISCFAT_SOL.x ) ==TRUE & is.na( DISCFAT_SOL.y ) ==FALSE,DISCFAT_SOL.y,
                                 ifelse( is.na( DISCFAT_SOL.x ) ==FALSE & is.na( DISCFAT_SOL.y ) ==TRUE,DISCFAT_SOL.x,NA )  )  )  ) %>%
  mutate( Oils=ifelse( is.na( DISCFAT_OIL.x ) ==FALSE & is.na( DISCFAT_OIL.y ) ==FALSE,( DISCFAT_OIL.x+DISCFAT_OIL.y ) /2,
                     ifelse( is.na( DISCFAT_OIL.x ) ==TRUE & is.na( DISCFAT_OIL.y ) ==FALSE,DISCFAT_OIL.y,
                            ifelse( is.na( DISCFAT_OIL.x ) ==FALSE & is.na( DISCFAT_OIL.y ) ==TRUE,DISCFAT_OIL.x,NA )  )  )  ) %>%
  mutate( Milk=ifelse( is.na( D_MILK.x ) ==FALSE & is.na( D_MILK.y ) ==FALSE,( D_MILK.x+D_MILK.y ) /2,
                     ifelse( is.na( D_MILK.x ) ==TRUE & is.na( D_MILK.y ) ==FALSE,D_MILK.y,
                            ifelse( is.na( D_MILK.x ) ==FALSE & is.na( D_MILK.y ) ==TRUE,D_MILK.x,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( is.na( D_YOGURT.x ) ==FALSE & is.na( D_YOGURT.y ) ==FALSE,( D_YOGURT.x+D_YOGURT.y ) /2,
                       ifelse( is.na( D_YOGURT.x ) ==TRUE & is.na( D_YOGURT.y ) ==FALSE,D_YOGURT.y,
                              ifelse( is.na( D_YOGURT.x ) ==FALSE & is.na( D_YOGURT.y ) ==TRUE,D_YOGURT.x,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( is.na( D_CHEESE.x ) ==FALSE & is.na( D_CHEESE.y ) ==FALSE,( D_CHEESE.x+D_CHEESE.y ) /2,
                       ifelse( is.na( D_CHEESE.x ) ==TRUE & is.na( D_CHEESE.y ) ==FALSE,D_CHEESE.y,
                              ifelse( is.na( D_CHEESE.x ) ==FALSE & is.na( D_CHEESE.y ) ==TRUE,D_CHEESE.x,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( is.na( A_BEV.x ) ==FALSE & is.na( A_BEV.y ) ==FALSE,( A_BEV.x+A_BEV.y ) /2,
                        ifelse( is.na( A_BEV.x ) ==TRUE & is.na( A_BEV.y ) ==FALSE,A_BEV.y,
                               ifelse( is.na( A_BEV.x ) ==FALSE & is.na( A_BEV.y ) ==TRUE,A_BEV.x,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( is.na( F_OTHER.x ) ==FALSE & is.na( F_OTHER.y ) ==FALSE,( F_OTHER.x+F_OTHER.y ) /2,
                      ifelse( is.na( F_OTHER.x ) ==TRUE & is.na( F_OTHER.y ) ==FALSE,F_OTHER.y,
                             ifelse( is.na( F_OTHER.x ) ==FALSE & is.na( F_OTHER.y ) ==TRUE,F_OTHER.x,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( is.na( F_CITMLB.x ) ==FALSE & is.na( F_CITMLB.y ) ==FALSE,( F_CITMLB.x+F_CITMLB.y ) /2,
                           ifelse( is.na( F_CITMLB.x ) ==TRUE & is.na( F_CITMLB.y ) ==FALSE,F_CITMLB.y,
                                  ifelse( is.na( F_CITMLB.x ) ==FALSE & is.na( F_CITMLB.y ) ==TRUE,F_CITMLB.x,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( is.na( V_TOMATO.x ) ==FALSE & is.na( V_TOMATO.y ) ==FALSE,( V_TOMATO.x+V_TOMATO.y ) /2,
                         ifelse( is.na( V_TOMATO.x ) ==TRUE & is.na( V_TOMATO.y ) ==FALSE,V_TOMATO.y,
                                ifelse( is.na( V_TOMATO.x ) ==FALSE & is.na( V_TOMATO.y ) ==TRUE,V_TOMATO.x,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( is.na( V_DRKGR.x ) ==FALSE & is.na( V_DRKGR.y ) ==FALSE,( V_DRKGR.x+V_DRKGR.y ) /2,
                           ifelse( is.na( V_DRKGR.x ) ==TRUE & is.na( V_DRKGR.y ) ==FALSE,V_DRKGR.y,
                                  ifelse( is.na( V_DRKGR.x ) ==FALSE & is.na( V_DRKGR.y ) ==TRUE,V_DRKGR.x,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( is.na( V_ORANGE.x ) ==FALSE & is.na( V_ORANGE.y ) ==FALSE,( V_ORANGE.x+V_ORANGE.y ) /2,
                          ifelse( is.na( V_ORANGE.x ) ==TRUE & is.na( V_ORANGE.y ) ==FALSE,V_ORANGE.y,
                                 ifelse( is.na( V_ORANGE.x ) ==FALSE & is.na( V_ORANGE.y ) ==TRUE,V_ORANGE.x,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( is.na( V_OTHER.x ) ==FALSE & is.na( V_OTHER.y ) ==FALSE,( V_OTHER.x+V_OTHER.y ) /2,
                         ifelse( is.na( V_OTHER.x ) ==TRUE & is.na( V_OTHER.y ) ==FALSE,V_OTHER.y,
                                ifelse( is.na( V_OTHER.x ) ==FALSE & is.na( V_OTHER.y ) ==TRUE,V_OTHER.x,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( is.na( V_POTATO.x ) ==FALSE & is.na( V_POTATO.y ) ==FALSE,( V_POTATO.x+V_POTATO.y ) /2,
                         ifelse( is.na( V_POTATO.x ) ==TRUE & is.na( V_POTATO.y ) ==FALSE,V_POTATO.y,
                                ifelse( is.na( V_POTATO.x ) ==FALSE & is.na( V_POTATO.y ) ==TRUE,V_POTATO.x,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( is.na( V_STARCY.x ) ==FALSE & is.na( V_STARCY.y ) ==FALSE,( V_STARCY.x+V_STARCY.y ) /2,
                                ifelse( is.na( V_STARCY.x ) ==TRUE & is.na( V_STARCY.y ) ==FALSE,V_STARCY.y,
                                       ifelse( is.na( V_STARCY.x ) ==FALSE & is.na( V_STARCY.y ) ==TRUE,V_STARCY.x,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( is.na( LEGUMES.x ) ==FALSE & is.na( LEGUMES.y ) ==FALSE,( LEGUMES.x+LEGUMES.y ) /2,
                        ifelse( is.na( LEGUMES.x ) ==TRUE & is.na( LEGUMES.y ) ==FALSE,LEGUMES.y,
                               ifelse( is.na( LEGUMES.x ) ==FALSE & is.na( LEGUMES.y ) ==TRUE,LEGUMES.x,NA )  )  )  ) %>%
  mutate( Soy=ifelse( is.na( M_SOY.x ) ==FALSE & is.na( M_SOY.y ) ==FALSE,( M_SOY.x+M_SOY.y ) /2,
                    ifelse( is.na( M_SOY.x ) ==TRUE & is.na( M_SOY.y ) ==FALSE,M_SOY.y,
                           ifelse( is.na( M_SOY.x ) ==FALSE & is.na( M_SOY.y ) ==TRUE,M_SOY.x,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( is.na( G_NWHL.x ) ==FALSE & is.na( G_NWHL.y ) ==FALSE,( G_NWHL.x+G_NWHL.y ) /2,
                             ifelse( is.na( G_NWHL.x ) ==TRUE & is.na( G_NWHL.y ) ==FALSE,G_NWHL.y,
                                    ifelse( is.na( G_NWHL.x ) ==FALSE & is.na( G_NWHL.y ) ==TRUE,G_NWHL.x,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( is.na( G_WHL.x ) ==FALSE & is.na( G_WHL.y ) ==FALSE,( G_WHL.x+G_WHL.y ) /2,
                           ifelse( is.na( G_WHL.x ) ==TRUE & is.na( G_WHL.y ) ==FALSE,G_WHL.y,
                                  ifelse( is.na( G_WHL.x ) ==FALSE & is.na( G_WHL.y ) ==TRUE,G_WHL.x,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( is.na( M_NUTSD.x ) ==FALSE & is.na( M_NUTSD.y ) ==FALSE,( M_NUTSD.x+M_NUTSD.y ) /2,
                     ifelse( is.na( M_NUTSD.x ) ==TRUE & is.na( M_NUTSD.y ) ==FALSE,M_NUTSD.y,
                            ifelse( is.na( M_NUTSD.x ) ==FALSE & is.na( M_NUTSD.y ) ==TRUE,M_NUTSD.x,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( is.na( ADD_SUG.x ) ==FALSE & is.na( ADD_SUG.y ) ==FALSE,( ADD_SUG.x+ADD_SUG.y ) /2,
                            ifelse( is.na( ADD_SUG.x ) ==TRUE & is.na( ADD_SUG.y ) ==FALSE,ADD_SUG.y,
                                   ifelse( is.na( ADD_SUG.x ) ==FALSE & is.na( ADD_SUG.y ) ==TRUE,ADD_SUG.x,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 

View( foodgrpshr0304 ) 
#######CYCLE 2005-2006##########
dr10506 <- read_sas( 'fped_dr1tot_0506.sas7bdat' ) 
dr20506 <- read_sas( 'fped_dr2tot_0506.sas7bdat' ) 
mergedde0506 <- inner_join( dr10506[,c( 1,15:51 ) ],dr20506[c( 1,15:51 ) ],by='SEQN' ) 
mergedde0506b <- inner_join( mergedde0506,dr10506[,1:14],by='SEQN' ) 

names( mergedde0506 ) 
foodgrpshr0506 <- mergedde0506b%>%
mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                  ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                      ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                             ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                      ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                             ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 
  
#######CYCLE 2007-2008##########
dr10708 <- read_sas( 'fped_dr1tot_0708.sas7bdat' ) 
dr20708 <- read_sas( 'fped_dr2tot_0708.sas7bdat' ) 
mergedde0708 <- inner_join( dr10708[,c( 1,15:51 ) ],dr20708[c( 1,15:51 ) ],by='SEQN' ) 
mergedde0708b <- inner_join( mergedde0708,dr10708[,1:14],by='SEQN' ) 

names( mergedde0708 ) 
foodgrpshr0708 <- mergedde0708b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 

#######CYCLE 2009-2010##########
dr10910 <- read_sas( 'fped_dr1tot_0910.sas7bdat' ) 
dr20910 <- read_sas( 'fped_dr2tot_0910.sas7bdat' ) 
mergedde0910 <- inner_join( dr10910[,c( 1,15:51 ) ],dr20910[c( 1,15:51 ) ],by='SEQN' ) 
mergedde0910b <- inner_join( mergedde0910,dr10910[,1:14],by='SEQN' ) 

names( mergedde0910 ) 
foodgrpshr0910 <- mergedde0910b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 



#######CYCLE 2011-2012##########
dr11112 <- read_sas( 'fped_dr1tot_1112.sas7bdat' ) 
dr21112 <- read_sas( 'fped_dr2tot_1112.sas7bdat' ) 
mergedde1112 <- inner_join( dr11112[,c( 1,15:51 ) ],dr21112[c( 1,15:51 ) ],by='SEQN' ) 
mergedde1112b <- inner_join( mergedde1112,dr11112[,1:14],by='SEQN' ) 

names( mergedde1112 ) 
foodgrpshr1112 <- mergedde1112b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 

#######CYCLE 2013-2014##########
dr11314 <- read_sas( 'fped_dr1tot_1314.sas7bdat' ) 

dr21314 <- read_sas( 'fped_dr2tot_1314.sas7bdat' ) 
mergedde1314 <- inner_join( dr11314[,c( 1,15:51 ) ],dr21314[c( 1,15:51 ) ],by='SEQN' ) 
mergedde1314b <- inner_join( mergedde1314,dr11314[,1:14],by='SEQN' ) 

names( mergedde0506 ) 
foodgrpshr1314 <- mergedde1314b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 


#######CYCLE 2015-2016##########
dr11516 <- read_sas( 'fped_dr1tot_1516.sas7bdat' ) 

dr21516 <- read_sas( 'fped_dr2tot_1516.sas7bdat' ) 
mergedde1516 <- inner_join( dr11516[,c( 1,15:51 ) ],dr21516[c( 1,15:51 ) ],by='SEQN' ) 
mergedde1516b <- inner_join( mergedde1516,dr11516[,1:14],by='SEQN' ) 

names( mergedde1516 ) 
foodgrpshr1516 <- mergedde1516b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 



#######CYCLE 2017-2018##########
dr11718 <- read_sas( 'fped_dr1tot_1718.sas7bdat' ) 
dr21718 <- read_sas( 'fped_dr2tot_1718.sas7bdat' ) 
mergedde1718 <- inner_join( dr11718[,c( 1,15:51 ) ],dr21718[c( 1,15:51 ) ],by='SEQN' ) 
mergedde1718b <- inner_join( mergedde1718,dr11718[,1:14],by='SEQN' ) 

names( mergedde1718 ) 
foodgrpshr1718 <- mergedde1718b%>%
  mutate( ProcessedMts=ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT ) ==FALSE,DR1T_PF_CUREDMEAT,
                                    ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT ) ==FALSE,DR2T_PF_CUREDMEAT,NA )  )  )  ) %>%
  mutate( RedMts=ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT ) ==FALSE,DR1T_PF_MEAT,
                              ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT ) ==FALSE,DR2T_PF_MEAT,NA )  )  )  ) %>%
  mutate( OrganMts=ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN ) ==FALSE,DR1T_PF_ORGAN,
                                ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN ) ==FALSE,DR2T_PF_ORGAN,NA )  )  )  ) %>%
  mutate( Poultry=ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_POULT ) ==FALSE,DR1T_PF_POULT,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_POULT ) ==FALSE,DR2T_PF_POULT,NA )  )  )  ) %>%
  mutate( Fish_Hi=ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI ) ==FALSE,DR1T_PF_SEAFD_HI,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI ) ==FALSE,DR2T_PF_SEAFD_HI,NA )  )  )  ) %>%
  mutate( Fish_Lo=ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW ) ==FALSE,DR1T_PF_SEAFD_LOW,
                               ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW ) ==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  )  ) %>%
  mutate( Eggs=ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS ) ==FALSE,DR1T_PF_EGGS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS ) ==FALSE,DR2T_PF_EGGS,NA )  )  )  ) %>%
  mutate( SolidFats=ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS ) ==FALSE,DR1T_SOLID_FATS,
                                 ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS ) ==FALSE,DR2T_SOLID_FATS,NA )  )  )  ) %>%
  mutate( Oils=ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_OILS ) ==FALSE,DR1T_OILS,
                            ifelse( DRDINT==1 & is.na( DR2T_OILS ) ==FALSE,DR2T_OILS,NA )  )  )  ) %>%
  mutate( Milk=ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_D_MILK ) ==FALSE,DR1T_D_MILK,
                            ifelse( DRDINT==1 & is.na( DR2T_D_MILK ) ==FALSE,DR2T_D_MILK,NA )  )  )  ) %>%
  mutate( Yogurt=ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT ) ==FALSE,DR1T_D_YOGURT,
                              ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT ) ==FALSE,DR2T_D_YOGURT,NA )  )  )  ) %>%
  mutate( Cheese=ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                       ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE ) ==FALSE,DR1T_D_CHEESE,
                              ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE ) ==FALSE,DR2T_D_CHEESE,NA )  )  )  ) %>%
  mutate( Alcohol=ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS ) ==FALSE,DR1T_A_DRINKS,
                               ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS ) ==FALSE,DR2T_A_DRINKS,NA )  )  )  ) %>%
  mutate( F_CitMelBer=ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB ) ==FALSE,DR1T_F_CITMLB,
                                   ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB ) ==FALSE,DR2T_F_CITMLB,NA )  )  )  ) %>%
  mutate( FruitOther=ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_F_OTHER ) ==FALSE,DR1T_F_OTHER,
                                  ifelse( DRDINT==1 & is.na( DR2T_F_OTHER ) ==FALSE,DR2T_F_OTHER,NA )  )  )  ) %>%
  mutate( Tomatoes=ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO ) ==FALSE,DR1T_V_REDOR_TOMATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO ) ==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  )  ) %>%
  mutate( GreenLeafy=ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR ) ==FALSE,DR1T_V_DRKGR,
                                  ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR ) ==FALSE,DR2T_V_DRKGR,NA )  )  )  ) %>%
  mutate( DarkYlVeg=ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                          ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER ) ==FALSE,DR1T_V_REDOR_OTHER,
                                 ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER ) ==FALSE,DR2T_V_REDOR_OTHER,NA )  )  )  ) %>%
  mutate( OtherVeg=ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_OTHER ) ==FALSE,DR1T_V_OTHER,
                                ifelse( DRDINT==1 & is.na( DR2T_V_OTHER ) ==FALSE,DR2T_V_OTHER,NA )  )  )  ) %>%
  mutate( Potatoes=ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                         ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO ) ==FALSE,DR1T_V_STARCHY_POTATO,
                                ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO ) ==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  )  ) %>%
  mutate( OtherStarchyVeg=ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER ) ==FALSE,DR1T_V_STARCHY_OTHER,
                                       ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER ) ==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  )  ) %>%
  mutate( Legumes=ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                        ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES ) ==FALSE,DR1T_V_LEGUMES,
                               ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES ) ==FALSE,DR2T_V_LEGUMES,NA )  )  )  ) %>%
  mutate( Soy=ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                    ifelse( DRDINT==1 & is.na( DR1T_PF_SOY ) ==FALSE,DR1T_PF_SOY,
                           ifelse( DRDINT==1 & is.na( DR2T_PF_SOY ) ==FALSE,DR2T_PF_SOY,NA )  )  )  ) %>%
  mutate( RefinedGrain=ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                             ifelse( DRDINT==1 & is.na( DR1T_G_REFINED ) ==FALSE,DR1T_G_REFINED,
                                    ifelse( DRDINT==1 & is.na( DR2T_G_REFINED ) ==FALSE,DR2T_G_REFINED,NA )  )  )  ) %>%
  mutate( WholeGrain=ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                           ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE ) ==FALSE,DR1T_G_WHOLE,
                                  ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE ) ==FALSE,DR2T_G_WHOLE,NA )  )  )  ) %>%
  mutate( Nuts=ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                     ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS ) ==FALSE,DR1T_PF_NUTSDS,
                            ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS ) ==FALSE,DR2T_PF_NUTSDS,NA )  )  )  ) %>%
  mutate( AddedSugars=ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                            ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS ) ==FALSE,DR1T_ADD_SUGARS,
                                   ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS ) ==FALSE,DR2T_ADD_SUGARS,NA )  )  )  ) %>%
  select( SEQN,ProcessedMts,RedMts,OrganMts,Poultry,Fish_Hi,Fish_Lo,Eggs,SolidFats,Oils,Milk,
         Yogurt,Cheese,Alcohol,FruitOther,F_CitMelBer,Tomatoes,GreenLeafy,DarkYlVeg,OtherVeg,
         Potatoes,OtherStarchyVeg,Legumes,Soy,RefinedGrain,WholeGrain,Nuts,AddedSugars ) 


#############Merge all diet data together

finalmerge <- rbind( foodgrpshr9902,foodgrpshr0304,foodgrpshr0506,foodgrpshr0708,foodgrpshr0910,
                  foodgrpshr1112,foodgrpshr1314,foodgrpshr1516,
                  foodgrpshr1718 ) 
#n=92744 with quality dietary data available



#get demo data to get interview weights

demodat99 <- nhanes_load_data( 'DEMO','1999-2000' ) 
demodat01 <- nhanes_load_data( 'DEMO','2001-2002' ) 
demodat03 <- nhanes_load_data( 'DEMO','2003-2004' ) 
demodat05 <- nhanes_load_data( 'DEMO','2005-2006' ) 
demodat07 <- nhanes_load_data( 'DEMO','2007-2008' ) 
demodat09 <- nhanes_load_data( 'DEMO','2009-2010' ) 
demodat11 <- nhanes_load_data( 'DEMO','2011-2012' ) 
demodat13 <- nhanes_load_data( 'DEMO','2013-2014' ) 
demodat15 <- read_xpt( file='https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT' ) 
demodat17 <- read_xpt( file='https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT' ) 
demodat03$WTINT4YR <- NA
demodat03$WTMEC4YR <- NA
demodat05$WTINT4YR <- NA
demodat05$WTMEC4YR <- NA
demodat07$WTINT4YR <- NA
demodat07$WTMEC4YR <- NA
demodat09$WTINT4YR <- NA
demodat09$WTMEC4YR <- NA
demodat11$WTINT4YR <- NA
demodat11$WTMEC4YR <- NA
demodat13$WTINT4YR <- NA
demodat13$WTMEC4YR <- NA
demodat15$WTINT4YR <- NA
demodat15$WTMEC4YR <- NA
demodat17$WTINT4YR <- NA
demodat17$WTMEC4YR <- NA

intweights <- rbind( demodat99[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat01[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat03[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat05[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat07[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat09[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat11[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat13[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat15[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat17[,c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ] ) 
  
#Creating 18 year weight variable    
table( intweights$SDDSRVYR ) 
intweights <- intweights%>%
  mutate( WTINT18YR=ifelse( SDDSRVYR %in% c( 1,2 ) ,( 2/10 ) *WTINT4YR,
         ifelse( SDDSRVYR %in% c( 3,4,5,6,7,8,9,10 ) ,WTINT2YR/10,NA )  )  ) %>%
  mutate( WTMEC18YR=ifelse( SDDSRVYR %in% c( 1,2 ) ,( 2/10 ) *WTMEC4YR,
                          ifelse( SDDSRVYR %in% c( 3,4,5,6,7,8,9,10 ) ,WTMEC2YR/10,NA )  )  ) 
         
   
finalmergeb <- left_join( finalmerge,intweights[,c( 1,7,8 ) ],by='SEQN' ) 
nrow( finalmergeb ) 
finalmergec <- na.omit( finalmergeb ) 
nrow( finalmergec ) 

#finalmerge has 92121 rows
#after omitting those without dietary data, it comes down to 85391 ( complete cases ) 


setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES_FI_CA_Diet/Data' ) 
saveRDS( finalmergeb,'24hr_demo_FPED_twoday.rds' ) 



