###---------------------------------------------------
###   01-FPED/MPED DATA IMPORT AND WRANGLING
###---------------------------------------------------

# user guide: https://www.ars.usda.gov/ARSUserFiles/80400530/pdf/fped/FPED_0506.pdf

library( haven ) 
library( tidyverse ) 
library( RNHANES ) 
library( glue )

setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Raw/FPED' ) 

####### CYCLES 1999-2002 (MPED 1.0)##########

dr99 <- read_sas( 'pyr_tot01.sas7bdat' ) 
names( dr99 ) 

#1999-2002 has only 1 day of dietary interview ( 1 24 hr recall )  whereas 2003-2017 has 2 days
foodgrpshr9902 <- dr99 %>%
  mutate( ProcessedMts =  M_FRANK,
   RedMts =  M_MEAT,
   OrganMts =  M_ORGAN,
   Poultry =  M_POULT,
   Fish_Hi =  M_FISH_HI,
   Fish_Lo =  M_FISH_LO,
   Eggs =  M_EGG,
   SolidFats =  DISCFAT_SOL,
   Oils =  DISCFAT_OIL,
   Milk =  D_MILK,
   Yogurt =  D_YOGURT,
   Cheese =  D_CHEESE,
   Alcohol =  A_BEV,
   FruitOther =  F_OTHER,
   F_CitMelBer =  F_CITMLB,
   Tomatoes =  V_TOMATO,
   GreenLeafy =  V_DRKGR,
   DarkYlVeg =  V_DPYEL,
   OtherVeg =  V_OTHER,
   Potatoes =  V_POTATO,
   OtherStarchyVeg =  V_STARCY,
   Legumes =  LEGUMES,
   Soy =  M_SOY,
   RefinedGrain =  G_NWHL,
   WholeGrain =  G_WHL,
   Nuts =  M_NUTSD,
   AddedSugars =  ADD_SUG ) %>%
  select( SEQN, ProcessedMts, RedMts, OrganMts, Poultry, Fish_Hi, Fish_Lo, Eggs, SolidFats, Oils, Milk, 
         Yogurt, Cheese, Alcohol, FruitOther, F_CitMelBer, Tomatoes, GreenLeafy, DarkYlVeg, OtherVeg, 
         Potatoes, OtherStarchyVeg, Legumes, Soy, RefinedGrain, WholeGrain, Nuts, AddedSugars ) 

# check columns
ncol( foodgrpshr9902 ) 
names( foodgrpshr9902 ) 

####### CYCLE 2003-2004 (MPED 2.0) ##########

dr10304 <- read_sas( 'pyr_tot_d1.sas7bdat' ) 
dr20304 <- read_sas( 'pyr_tot_d2.sas7bdat' ) 
mergedde0304 <- inner_join( dr10304, dr20304, by='SEQN' ) 
names( mergedde0304 ) 


foodgrpshr0304 <- mergedde0304%>%
  mutate( ProcessedMts = ifelse( is.na( M_FRANK.x )==FALSE & is.na( M_FRANK.y )==FALSE, ( M_FRANK.x+M_FRANK.y ) /2, 
                             ifelse( is.na( M_FRANK.x )==TRUE & is.na( M_FRANK.y )==FALSE, M_FRANK.y, 
                                    ifelse( is.na( M_FRANK.x )==FALSE & is.na( M_FRANK.y )==TRUE, M_FRANK.x, NA )  )  ),
   RedMts = ifelse( is.na( M_MEAT.x )==FALSE & is.na( M_MEAT.y )==FALSE, ( M_MEAT.x+M_MEAT.y ) /2, 
                       ifelse( is.na( M_MEAT.x )==TRUE & is.na( M_MEAT.y )==FALSE, M_MEAT.y, 
                              ifelse( is.na( M_MEAT.x )==FALSE & is.na( M_MEAT.y )==TRUE, M_MEAT.x, NA )  )  ),
   OrganMts = ifelse( is.na( M_ORGAN.x )==FALSE & is.na( M_ORGAN.y )==FALSE, ( M_ORGAN.x+M_ORGAN.y ) /2, 
                         ifelse( is.na( M_ORGAN.x )==TRUE & is.na( M_ORGAN.y )==FALSE, M_ORGAN.y, 
                                ifelse( is.na( M_ORGAN.x )==FALSE & is.na( M_ORGAN.y )==TRUE, M_ORGAN.x, NA )  )  ),
   Poultry = ifelse( is.na( M_POULT.x )==FALSE & is.na( M_POULT.y )==FALSE, ( M_POULT.x+M_POULT.y ) /2, 
                        ifelse( is.na( M_POULT.x )==TRUE & is.na( M_POULT.y )==FALSE, M_POULT.y, 
                               ifelse( is.na( M_POULT.x )==FALSE & is.na( M_POULT.y )==TRUE, M_POULT.x, NA )  )  ),
   Fish_Hi = ifelse( is.na( M_FISH_HI.x )==FALSE & is.na( M_FISH_HI.y )==FALSE, ( M_FISH_HI.x+M_FISH_HI.y ) /2, 
                        ifelse( is.na( M_FISH_HI.x )==TRUE & is.na( M_FISH_HI.y )==FALSE, M_FISH_HI.y, 
                               ifelse( is.na( M_FISH_HI.x )==FALSE & is.na( M_FISH_HI.y )==TRUE, M_FISH_HI.x, NA )  )  ),
   Fish_Lo = ifelse( is.na( M_FISH_LO.x )==FALSE & is.na( M_FISH_LO.y )==FALSE, ( M_FISH_LO.x+M_FISH_LO.y ) /2, 
                        ifelse( is.na( M_FISH_LO.x )==TRUE & is.na( M_FISH_LO.y )==FALSE, M_FISH_LO.y, 
                               ifelse( is.na( M_FISH_LO.x )==FALSE & is.na( M_FISH_LO.y )==TRUE, M_FISH_LO.x, NA )  )  ),
   Eggs = ifelse( is.na( M_EGG.x )==FALSE & is.na( M_EGG.y )==FALSE, ( M_EGG.x+M_EGG.y ) /2, 
                     ifelse( is.na( M_EGG.x )==TRUE & is.na( M_EGG.y )==FALSE, M_EGG.y, 
                            ifelse( is.na( M_EGG.x )==FALSE & is.na( M_EGG.y )==TRUE, M_EGG.x, NA )  )  ),
   SolidFats = ifelse( is.na( DISCFAT_SOL.x )==FALSE & is.na( DISCFAT_SOL.y )==FALSE, ( DISCFAT_SOL.x+DISCFAT_SOL.y ) /2, 
                          ifelse( is.na( DISCFAT_SOL.x )==TRUE & is.na( DISCFAT_SOL.y )==FALSE, DISCFAT_SOL.y, 
                                 ifelse( is.na( DISCFAT_SOL.x )==FALSE & is.na( DISCFAT_SOL.y )==TRUE, DISCFAT_SOL.x, NA )  )  ),
   Oils = ifelse( is.na( DISCFAT_OIL.x )==FALSE & is.na( DISCFAT_OIL.y )==FALSE, ( DISCFAT_OIL.x+DISCFAT_OIL.y ) /2, 
                     ifelse( is.na( DISCFAT_OIL.x )==TRUE & is.na( DISCFAT_OIL.y )==FALSE, DISCFAT_OIL.y, 
                            ifelse( is.na( DISCFAT_OIL.x )==FALSE & is.na( DISCFAT_OIL.y )==TRUE, DISCFAT_OIL.x, NA )  )  ),
   Milk = ifelse( is.na( D_MILK.x )==FALSE & is.na( D_MILK.y )==FALSE, ( D_MILK.x+D_MILK.y ) /2, 
                     ifelse( is.na( D_MILK.x )==TRUE & is.na( D_MILK.y )==FALSE, D_MILK.y, 
                            ifelse( is.na( D_MILK.x )==FALSE & is.na( D_MILK.y )==TRUE, D_MILK.x, NA )  )  ),
   Yogurt = ifelse( is.na( D_YOGURT.x )==FALSE & is.na( D_YOGURT.y )==FALSE, ( D_YOGURT.x+D_YOGURT.y ) /2, 
                       ifelse( is.na( D_YOGURT.x )==TRUE & is.na( D_YOGURT.y )==FALSE, D_YOGURT.y, 
                              ifelse( is.na( D_YOGURT.x )==FALSE & is.na( D_YOGURT.y )==TRUE, D_YOGURT.x, NA )  )  ),
   Cheese = ifelse( is.na( D_CHEESE.x )==FALSE & is.na( D_CHEESE.y )==FALSE, ( D_CHEESE.x+D_CHEESE.y ) /2, 
                       ifelse( is.na( D_CHEESE.x )==TRUE & is.na( D_CHEESE.y )==FALSE, D_CHEESE.y, 
                              ifelse( is.na( D_CHEESE.x )==FALSE & is.na( D_CHEESE.y )==TRUE, D_CHEESE.x, NA )  )  ),
   Alcohol = ifelse( is.na( A_BEV.x )==FALSE & is.na( A_BEV.y )==FALSE, ( A_BEV.x+A_BEV.y ) /2, 
                        ifelse( is.na( A_BEV.x )==TRUE & is.na( A_BEV.y )==FALSE, A_BEV.y, 
                               ifelse( is.na( A_BEV.x )==FALSE & is.na( A_BEV.y )==TRUE, A_BEV.x, NA )  )  ),
   FruitOther = ifelse( is.na( F_OTHER.x )==FALSE & is.na( F_OTHER.y )==FALSE, ( F_OTHER.x+F_OTHER.y ) /2, 
                      ifelse( is.na( F_OTHER.x )==TRUE & is.na( F_OTHER.y )==FALSE, F_OTHER.y, 
                             ifelse( is.na( F_OTHER.x )==FALSE & is.na( F_OTHER.y )==TRUE, F_OTHER.x, NA )  )  ),
   F_CitMelBer = ifelse( is.na( F_CITMLB.x )==FALSE & is.na( F_CITMLB.y )==FALSE, ( F_CITMLB.x+F_CITMLB.y ) /2, 
                           ifelse( is.na( F_CITMLB.x )==TRUE & is.na( F_CITMLB.y )==FALSE, F_CITMLB.y, 
                                  ifelse( is.na( F_CITMLB.x )==FALSE & is.na( F_CITMLB.y )==TRUE, F_CITMLB.x, NA )  )  ),
   Tomatoes = ifelse( is.na( V_TOMATO.x )==FALSE & is.na( V_TOMATO.y )==FALSE, ( V_TOMATO.x+V_TOMATO.y ) /2, 
                         ifelse( is.na( V_TOMATO.x )==TRUE & is.na( V_TOMATO.y )==FALSE, V_TOMATO.y, 
                                ifelse( is.na( V_TOMATO.x )==FALSE & is.na( V_TOMATO.y )==TRUE, V_TOMATO.x, NA )  )  ),
   GreenLeafy = ifelse( is.na( V_DRKGR.x )==FALSE & is.na( V_DRKGR.y )==FALSE, ( V_DRKGR.x+V_DRKGR.y ) /2, 
                           ifelse( is.na( V_DRKGR.x )==TRUE & is.na( V_DRKGR.y )==FALSE, V_DRKGR.y, 
                                  ifelse( is.na( V_DRKGR.x )==FALSE & is.na( V_DRKGR.y )==TRUE, V_DRKGR.x, NA )  )  ),
   DarkYlVeg = ifelse( is.na( V_ORANGE.x )==FALSE & is.na( V_ORANGE.y )==FALSE, ( V_ORANGE.x+V_ORANGE.y ) /2, 
                          ifelse( is.na( V_ORANGE.x )==TRUE & is.na( V_ORANGE.y )==FALSE, V_ORANGE.y, 
                                 ifelse( is.na( V_ORANGE.x )==FALSE & is.na( V_ORANGE.y )==TRUE, V_ORANGE.x, NA )  )  ),
   OtherVeg = ifelse( is.na( V_OTHER.x )==FALSE & is.na( V_OTHER.y )==FALSE, ( V_OTHER.x+V_OTHER.y ) /2, 
                         ifelse( is.na( V_OTHER.x )==TRUE & is.na( V_OTHER.y )==FALSE, V_OTHER.y, 
                                ifelse( is.na( V_OTHER.x )==FALSE & is.na( V_OTHER.y )==TRUE, V_OTHER.x, NA )  )  ),
   Potatoes = ifelse( is.na( V_POTATO.x )==FALSE & is.na( V_POTATO.y )==FALSE, ( V_POTATO.x+V_POTATO.y ) /2, 
                         ifelse( is.na( V_POTATO.x )==TRUE & is.na( V_POTATO.y )==FALSE, V_POTATO.y, 
                                ifelse( is.na( V_POTATO.x )==FALSE & is.na( V_POTATO.y )==TRUE, V_POTATO.x, NA )  )  ),
   OtherStarchyVeg = ifelse( is.na( V_STARCY.x )==FALSE & is.na( V_STARCY.y )==FALSE, ( V_STARCY.x+V_STARCY.y ) /2, 
                                ifelse( is.na( V_STARCY.x )==TRUE & is.na( V_STARCY.y )==FALSE, V_STARCY.y, 
                                       ifelse( is.na( V_STARCY.x )==FALSE & is.na( V_STARCY.y )==TRUE, V_STARCY.x, NA )  )  ),
   Legumes = ifelse( is.na( LEGUMES.x )==FALSE & is.na( LEGUMES.y )==FALSE, ( LEGUMES.x+LEGUMES.y ) /2, 
                        ifelse( is.na( LEGUMES.x )==TRUE & is.na( LEGUMES.y )==FALSE, LEGUMES.y, 
                               ifelse( is.na( LEGUMES.x )==FALSE & is.na( LEGUMES.y )==TRUE, LEGUMES.x, NA )  )  ),
   Soy = ifelse( is.na( M_SOY.x )==FALSE & is.na( M_SOY.y )==FALSE, ( M_SOY.x+M_SOY.y ) /2, 
                    ifelse( is.na( M_SOY.x )==TRUE & is.na( M_SOY.y )==FALSE, M_SOY.y, 
                           ifelse( is.na( M_SOY.x )==FALSE & is.na( M_SOY.y )==TRUE, M_SOY.x, NA )  )  ),
   RefinedGrain = ifelse( is.na( G_NWHL.x )==FALSE & is.na( G_NWHL.y )==FALSE, ( G_NWHL.x+G_NWHL.y ) /2, 
                             ifelse( is.na( G_NWHL.x )==TRUE & is.na( G_NWHL.y )==FALSE, G_NWHL.y, 
                                    ifelse( is.na( G_NWHL.x )==FALSE & is.na( G_NWHL.y )==TRUE, G_NWHL.x, NA )  )  ),
   WholeGrain = ifelse( is.na( G_WHL.x )==FALSE & is.na( G_WHL.y )==FALSE, ( G_WHL.x+G_WHL.y ) /2, 
                           ifelse( is.na( G_WHL.x )==TRUE & is.na( G_WHL.y )==FALSE, G_WHL.y, 
                                  ifelse( is.na( G_WHL.x )==FALSE & is.na( G_WHL.y )==TRUE, G_WHL.x, NA )  )  ),
   Nuts = ifelse( is.na( M_NUTSD.x )==FALSE & is.na( M_NUTSD.y )==FALSE, ( M_NUTSD.x+M_NUTSD.y ) /2, 
                     ifelse( is.na( M_NUTSD.x )==TRUE & is.na( M_NUTSD.y )==FALSE, M_NUTSD.y, 
                            ifelse( is.na( M_NUTSD.x )==FALSE & is.na( M_NUTSD.y )==TRUE, M_NUTSD.x, NA )  )  ),
   AddedSugars = ifelse( is.na( ADD_SUG.x )==FALSE & is.na( ADD_SUG.y )==FALSE, ( ADD_SUG.x+ADD_SUG.y ) /2, 
                            ifelse( is.na( ADD_SUG.x )==TRUE & is.na( ADD_SUG.y )==FALSE, ADD_SUG.y, 
                                   ifelse( is.na( ADD_SUG.x )==FALSE & is.na( ADD_SUG.y )==TRUE, ADD_SUG.x, NA )  )  ) ) %>%
  select( SEQN, ProcessedMts, RedMts, OrganMts, Poultry, Fish_Hi, Fish_Lo, Eggs, SolidFats, Oils, Milk, 
         Yogurt, Cheese, Alcohol, FruitOther, F_CitMelBer, Tomatoes, GreenLeafy, DarkYlVeg, OtherVeg, 
         Potatoes, OtherStarchyVeg, Legumes, Soy, RefinedGrain, WholeGrain, Nuts, AddedSugars ) 


####### CYCLES 2005-2018 (FPED) ##########

import_fped <- function( yrs.cycle ) { 
  
  import::from('magrittr','%>%')
  
  dr1 <- haven::read_sas( glue::glue( 'fped_dr1tot_{yrs.cycle}.sas7bdat' ) )
  dr2 <- haven::read_sas( glue::glue( 'fped_dr2tot_{yrs.cycle}.sas7bdat' ) )
  keep.dr1 <- c( 1, which( stringr::str_detect( colnames( dr1 ), 'DR1T_' ) ) ) # keep intake columns and SEQN
  keep.dr2 <- c( 1, which( stringr::str_detect( colnames( dr2 ), 'DR2T_' ) ) )
  
  # merge day 1 and day 2 data
  merged.dr <- dplyr::inner_join( dr1[ keep.dr1 ],dr2[ keep.dr2 ], by='SEQN' ) %>%
    dplyr::inner_join( ., dr1[, 1:14 ],by='SEQN' ) # merge in metadata
  
  
  merged.dr <- merged.dr %>%
    dplyr::mutate( ProcessedMts = ifelse( DRDINT==2,( DR1T_PF_CUREDMEAT+DR2T_PF_CUREDMEAT ) /2,
                                          ifelse( DRDINT==1 & is.na( DR1T_PF_CUREDMEAT )==FALSE,DR1T_PF_CUREDMEAT,
                                                  ifelse( DRDINT==1 & is.na( DR2T_PF_CUREDMEAT )==FALSE,DR2T_PF_CUREDMEAT,NA )  )  ),
                   RedMts = ifelse( DRDINT==2,( DR1T_PF_MEAT+DR2T_PF_MEAT ) /2,
                                    ifelse( DRDINT==1 & is.na( DR1T_PF_MEAT )==FALSE,DR1T_PF_MEAT,
                                            ifelse( DRDINT==1 & is.na( DR2T_PF_MEAT )==FALSE,DR2T_PF_MEAT,NA )  )  ),
                   OrganMts = ifelse( DRDINT==2,( DR1T_PF_ORGAN+DR2T_PF_ORGAN ) /2,
                                      ifelse( DRDINT==1 & is.na( DR1T_PF_ORGAN )==FALSE,DR1T_PF_ORGAN,
                                              ifelse( DRDINT==1 & is.na( DR2T_PF_ORGAN )==FALSE,DR2T_PF_ORGAN,NA )  )  ),
                   Poultry = ifelse( DRDINT==2,( DR1T_PF_POULT+DR2T_PF_POULT ) /2,
                                     ifelse( DRDINT==1 & is.na( DR1T_PF_POULT )==FALSE,DR1T_PF_POULT,
                                             ifelse( DRDINT==1 & is.na( DR2T_PF_POULT )==FALSE,DR2T_PF_POULT,NA )  )  ),
                   Fish_Hi = ifelse( DRDINT==2,( DR1T_PF_SEAFD_HI+DR2T_PF_SEAFD_HI ) /2,
                                     ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_HI )==FALSE,DR1T_PF_SEAFD_HI,
                                             ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_HI )==FALSE,DR2T_PF_SEAFD_HI,NA )  )  ),
                   Fish_Lo = ifelse( DRDINT==2,( DR1T_PF_SEAFD_LOW+DR2T_PF_SEAFD_LOW ) /2,
                                     ifelse( DRDINT==1 & is.na( DR1T_PF_SEAFD_LOW )==FALSE,DR1T_PF_SEAFD_LOW,
                                             ifelse( DRDINT==1 & is.na( DR2T_PF_SEAFD_LOW )==FALSE,DR2T_PF_SEAFD_LOW,NA )  )  ),
                   Eggs = ifelse( DRDINT==2,( DR1T_PF_EGGS+DR2T_PF_EGGS ) /2,
                                  ifelse( DRDINT==1 & is.na( DR1T_PF_EGGS )==FALSE,DR1T_PF_EGGS,
                                          ifelse( DRDINT==1 & is.na( DR2T_PF_EGGS )==FALSE,DR2T_PF_EGGS,NA )  )  ),
                   SolidFats = ifelse( DRDINT==2,( DR1T_SOLID_FATS+DR2T_SOLID_FATS ) /2,
                                       ifelse( DRDINT==1 & is.na( DR1T_SOLID_FATS )==FALSE,DR1T_SOLID_FATS,
                                               ifelse( DRDINT==1 & is.na( DR2T_SOLID_FATS )==FALSE,DR2T_SOLID_FATS,NA )  )  ),
                   Oils = ifelse( DRDINT==2,( DR1T_OILS+DR2T_OILS ) /2,
                                  ifelse( DRDINT==1 & is.na( DR1T_OILS )==FALSE,DR1T_OILS,
                                          ifelse( DRDINT==1 & is.na( DR2T_OILS )==FALSE,DR2T_OILS,NA )  )  ),
                   Milk = ifelse( DRDINT==2,( DR1T_D_MILK+DR2T_D_MILK ) /2,
                                  ifelse( DRDINT==1 & is.na( DR1T_D_MILK )==FALSE,DR1T_D_MILK,
                                          ifelse( DRDINT==1 & is.na( DR2T_D_MILK )==FALSE,DR2T_D_MILK,NA )  )  ),
                   Yogurt = ifelse( DRDINT==2,( DR1T_D_YOGURT+DR2T_D_YOGURT ) /2,
                                    ifelse( DRDINT==1 & is.na( DR1T_D_YOGURT )==FALSE,DR1T_D_YOGURT,
                                            ifelse( DRDINT==1 & is.na( DR2T_D_YOGURT )==FALSE,DR2T_D_YOGURT,NA )  )  ),
                   Cheese = ifelse( DRDINT==2,( DR1T_D_CHEESE+DR2T_D_CHEESE ) /2,
                                    ifelse( DRDINT==1 & is.na( DR1T_D_CHEESE )==FALSE,DR1T_D_CHEESE,
                                            ifelse( DRDINT==1 & is.na( DR2T_D_CHEESE )==FALSE,DR2T_D_CHEESE,NA )  )  ),
                   Alcohol = ifelse( DRDINT==2,( DR1T_A_DRINKS+DR2T_A_DRINKS ) /2,
                                     ifelse( DRDINT==1 & is.na( DR1T_A_DRINKS )==FALSE,DR1T_A_DRINKS,
                                             ifelse( DRDINT==1 & is.na( DR2T_A_DRINKS )==FALSE,DR2T_A_DRINKS,NA )  )  ),
                   F_CitMelBer = ifelse( DRDINT==2,( DR1T_F_CITMLB+DR2T_F_CITMLB ) /2,
                                         ifelse( DRDINT==1 & is.na( DR1T_F_CITMLB )==FALSE,DR1T_F_CITMLB,
                                                 ifelse( DRDINT==1 & is.na( DR2T_F_CITMLB )==FALSE,DR2T_F_CITMLB,NA )  )  ),
                   FruitOther = ifelse( DRDINT==2,( DR1T_F_OTHER+DR2T_F_OTHER ) /2,
                                        ifelse( DRDINT==1 & is.na( DR1T_F_OTHER )==FALSE,DR1T_F_OTHER,
                                                ifelse( DRDINT==1 & is.na( DR2T_F_OTHER )==FALSE,DR2T_F_OTHER,NA )  )  ),
                   Tomatoes = ifelse( DRDINT==2,( DR1T_V_REDOR_TOMATO+DR2T_V_REDOR_TOMATO ) /2,
                                      ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_TOMATO )==FALSE,DR1T_V_REDOR_TOMATO,
                                              ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_TOMATO )==FALSE,DR2T_V_REDOR_TOMATO,NA )  )  ),
                   GreenLeafy = ifelse( DRDINT==2,( DR1T_V_DRKGR+DR2T_V_DRKGR ) /2,
                                        ifelse( DRDINT==1 & is.na( DR1T_V_DRKGR )==FALSE,DR1T_V_DRKGR,
                                                ifelse( DRDINT==1 & is.na( DR2T_V_DRKGR )==FALSE,DR2T_V_DRKGR,NA )  )  ),
                   DarkYlVeg = ifelse( DRDINT==2,( DR1T_V_REDOR_OTHER+DR2T_V_REDOR_OTHER ) /2,
                                       ifelse( DRDINT==1 & is.na( DR1T_V_REDOR_OTHER )==FALSE,DR1T_V_REDOR_OTHER,
                                               ifelse( DRDINT==1 & is.na( DR2T_V_REDOR_OTHER )==FALSE,DR2T_V_REDOR_OTHER,NA )  )  ),
                   OtherVeg = ifelse( DRDINT==2,( DR1T_V_OTHER+DR2T_V_OTHER ) /2,
                                      ifelse( DRDINT==1 & is.na( DR1T_V_OTHER )==FALSE,DR1T_V_OTHER,
                                              ifelse( DRDINT==1 & is.na( DR2T_V_OTHER )==FALSE,DR2T_V_OTHER,NA )  )  ),
                   Potatoes = ifelse( DRDINT==2,( DR1T_V_STARCHY_POTATO+DR2T_V_STARCHY_POTATO ) /2,
                                      ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_POTATO )==FALSE,DR1T_V_STARCHY_POTATO,
                                              ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_POTATO )==FALSE,DR2T_V_STARCHY_POTATO,NA )  )  ),
                   OtherStarchyVeg = ifelse( DRDINT==2,( DR1T_V_STARCHY_OTHER+DR2T_V_STARCHY_OTHER ) /2,
                                             ifelse( DRDINT==1 & is.na( DR1T_V_STARCHY_OTHER )==FALSE,DR1T_V_STARCHY_OTHER,
                                                     ifelse( DRDINT==1 & is.na( DR2T_V_STARCHY_OTHER )==FALSE,DR2T_V_STARCHY_OTHER,NA )  )  ),
                   Legumes = ifelse( DRDINT==2,( DR1T_V_LEGUMES+DR2T_V_LEGUMES ) /2,
                                     ifelse( DRDINT==1 & is.na( DR1T_V_LEGUMES )==FALSE,DR1T_V_LEGUMES,
                                             ifelse( DRDINT==1 & is.na( DR2T_V_LEGUMES )==FALSE,DR2T_V_LEGUMES,NA )  )  ),
                   Soy = ifelse( DRDINT==2,( DR1T_PF_SOY+DR2T_PF_SOY ) /2,
                                 ifelse( DRDINT==1 & is.na( DR1T_PF_SOY )==FALSE,DR1T_PF_SOY,
                                         ifelse( DRDINT==1 & is.na( DR2T_PF_SOY )==FALSE,DR2T_PF_SOY,NA )  )  ),
                   RefinedGrain = ifelse( DRDINT==2,( DR1T_G_REFINED+DR2T_G_REFINED ) /2,
                                          ifelse( DRDINT==1 & is.na( DR1T_G_REFINED )==FALSE,DR1T_G_REFINED,
                                                  ifelse( DRDINT==1 & is.na( DR2T_G_REFINED )==FALSE,DR2T_G_REFINED,NA )  )  ),
                   WholeGrain = ifelse( DRDINT==2,( DR1T_G_WHOLE+DR2T_G_WHOLE ) /2,
                                        ifelse( DRDINT==1 & is.na( DR1T_G_WHOLE )==FALSE,DR1T_G_WHOLE,
                                                ifelse( DRDINT==1 & is.na( DR2T_G_WHOLE )==FALSE,DR2T_G_WHOLE,NA )  )  ),
                   Nuts = ifelse( DRDINT==2,( DR1T_PF_NUTSDS+DR2T_PF_NUTSDS ) /2,
                                  ifelse( DRDINT==1 & is.na( DR1T_PF_NUTSDS )==FALSE,DR1T_PF_NUTSDS,
                                          ifelse( DRDINT==1 & is.na( DR2T_PF_NUTSDS )==FALSE,DR2T_PF_NUTSDS,NA )  )  ),
                   AddedSugars = ifelse( DRDINT==2,( DR1T_ADD_SUGARS+DR2T_ADD_SUGARS ) /2,
                                         ifelse( DRDINT==1 & is.na( DR1T_ADD_SUGARS )==FALSE,DR1T_ADD_SUGARS,
                                                 ifelse( DRDINT==1 & is.na( DR2T_ADD_SUGARS )==FALSE,DR2T_ADD_SUGARS,NA ) ) ) ) %>%
    select( SEQN, ProcessedMts, RedMts, OrganMts, Poultry, Fish_Hi, Fish_Lo, Eggs, SolidFats, Oils, Milk,
            Yogurt, Cheese, Alcohol, FruitOther, F_CitMelBer, Tomatoes, GreenLeafy, DarkYlVeg, OtherVeg,
            Potatoes, OtherStarchyVeg, Legumes, Soy, RefinedGrain, WholeGrain, Nuts, AddedSugars ) 
  
  return( merged.dr) 
}

# apply function to return list of wrangled datasets
list.0518 <- lapply( c( '0506', '0708', '0910', '1112', '1314',
                        '1516', '1718'), function( x ) import_fped( x ))
fped.0518 <- do.call( 'rbind', list.0518 ) 

############# Merge all diet data together

finalmerge <- bind_rows( foodgrpshr9902, foodgrpshr0304 ,do.call( 'rbind', list.0518 ) )
nrow( finalmerge )
# n=92121 with quality dietary data available



# get demo data to get interview weights

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

intweights <- rbind( demodat99[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat01[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat03[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat05[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat07[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat09[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat11[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat13[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],demodat15[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ],
      demodat17[ , c( 'SEQN','SDDSRVYR','WTINT2YR','WTINT4YR','WTMEC2YR','WTMEC4YR' ) ] ) 
  
# creating 18 year weight variable    

table( intweights$SDDSRVYR ) 

intweights <- intweights %>%
  mutate( WTINT18YR = ifelse( SDDSRVYR %in% c( 1,2 ) ,( 2 / 10 ) * WTINT4YR,
         ifelse( SDDSRVYR %in% c( 3,4,5,6,7,8,9,10 ) ,WTINT2YR/10,NA )  )  ) %>%
  mutate( WTMEC18YR = ifelse( SDDSRVYR %in% c( 1,2 ) ,( 2 / 10 ) * WTMEC4YR,
                          ifelse( SDDSRVYR %in% c( 3,4,5,6,7,8,9,10 ) ,WTMEC2YR / 10,NA )  )  ) 
     
# join final datasets and save

setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Wrangled' )

# join
finalmerge.b <- left_join( finalmerge, 
                          intweights[ , c( 'SEQN', 'WTINT18YR' , 'WTMEC18YR' ) ], 
                          by = 'SEQN' )

saveRDS( finalmerge.b, '01-FPED-Wrangled.rds' ) # save

nrow( finalmerge.b ) # final merge file has 92121 rows
finalmerge.c <- na.omit( finalmerge.b ) 
nrow( finalmerge.c ) # after omitting those without dietary data, it comes down to 85391 ( complete cases ) 

