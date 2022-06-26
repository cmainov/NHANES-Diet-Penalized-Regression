###-------------------------------------------------- -
###   02-OTHER COVARIATE DATA WRANGLING
###-------------------------------------------------- -

library( RNHANES )
library( haven )
library( tidyverse )

## MEDICAL CONDITIONS AND CANCER DATA
mcqdat99 <- nhanes_load_data( 'MCQ', '1999-2000', demographics = TRUE )
mcqdat01 <- nhanes_load_data( 'MCQ', '2001-2002', demographics = TRUE )
mcqdat03 <- nhanes_load_data( 'MCQ', '2003-2004', demographics = TRUE )
mcqdat05 <- nhanes_load_data( 'MCQ', '2005-2006', demographics = TRUE )
mcqdat07 <- nhanes_load_data( 'MCQ', '2007-2008', demographics = TRUE )
mcqdat09 <- nhanes_load_data( 'MCQ', '2009-2010', demographics = TRUE )
mcqdat11 <- nhanes_load_data( 'MCQ', '2011-2012', demographics = TRUE )
mcqdat13 <- nhanes_load_data( 'MCQ', '2013-2014', demographics = TRUE )
mcqdat15raw <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT' )
mcqdat17raw <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT' )
demodat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT' )
demodat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT' )
demodat15$cycle <- '2015-2016'
demodat17$cycle <- '2017-2018'
mcqdat15 <- left_join( mcqdat15raw, demodat15 )
mcqdat17 <- left_join( mcqdat17raw, demodat17 )

# rename "what type of arthritis question" to be homogeneous across cycles to facilitate merge
mcqdat09 <- mcqdat09 %>%
  rename( MCQ190 = MCQ191 )
mcqdat11 <- mcqdat11 %>%
  rename( MCQ190 = MCQ195 )
mcqdat13 <- mcqdat13 %>%
  rename( MCQ190 = MCQ195 )
mcqdat15 <- mcqdat15 %>%
  rename( MCQ190 = MCQ195 )
mcqdat17 <- mcqdat17 %>%
  rename( MCQ190 = MCQ195 )

mcqall <- ( rbind( mcqdat99[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat01[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat03[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat05[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat07[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat09[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat11[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat13[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat15[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ], 
               mcqdat17[ , c( 'SEQN', 'cycle', 'RIDAGEMN', 'RIDAGEYR', 'MCQ220', 'MCQ230A', 'MCQ230B', 'MCQ230C', 'MCQ230D', 'MCQ160B', 'MCQ160C', 'MCQ160D', 'MCQ160E', 'MCQ160F', 'MCQ160A', 'MCQ160L', 'MCQ190' ) ] ) )

# CA, CVD, and cancer site variables
mcqall <- mcqall %>% 
  mutate( CA = factor( ifelse( MCQ220 == 1, 1, 
                         ifelse( MCQ220 == 2, 0, NA ) ) ),
         CVD = ifelse( MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | 
                       MCQ160E == 1 | MCQ160F == 1, 1, 
                     ifelse( MCQ160B %in% c( 7, 9 ) | MCQ160C %in% c( 7, 9 ) |
                              MCQ160D %in% c( 7, 9 ) | MCQ160E %in% c( 7, 9 ) |
                              MCQ160F %in% c( 7, 9 ), NA, 0 ) ),
         CATYPEA = factor( MCQ230A, 
                       levels = c( 9:39, 99 ), 
                       labels = c( 'Unknown', 'Bladder', 'Blood', 'Bone', 'Brain', 'Breast', 'Cervical', 'Colon', 
                                'Esophageal', 'Gallbladder', 'Kidney', 'Larynx', 'Leukemia', 'Liver', 'Lung', 'Lymphoma', 'Melanoma', 
                                'Oral/lip', 'Nervous System', 'Ovarian', 'Pancreatic', 'Prostate', 'Rectal', 'Non-Melanoma Skin', 
                                'Skin-Other', 'Soft Tissue', 'Stomach', 'Testicular', 'Thyroid', 'Uterus', 'Other', 'Unknown' ) ),
          CATYPEB = factor( MCQ230B, 
                       levels = c( 10:39 ), 
                       labels = c( 'Bladder', 'Blood', 'Bone', 'Brain', 'Breast', 'Cervical', 'Colon', 
                                'Esophageal', 'Gallbladder', 'Kidney', 'Larynx', 'Leukemia', 'Liver', 'Lung', 'Lymphoma', 'Melanoma', 
                                'Oral/lip', 'Nervous System', 'Ovarian', 'Pancreatic', 'Prostate', 'Rectal', 'Non-Melanoma Skin', 
                                'Skin-Other', 'Soft Tissue', 'Stomach', 'Testicular', 'Thyroid', 'Uterus', 'Other' ) ),
          CATYPEC = factor( MCQ230C, 
                       levels = c( 10:39 ), 
                       labels = c( 'Bladder', 'Blood', 'Bone', 'Brain', 'Breast', 'Cervical', 'Colon', 
                                'Esophageal', 'Gallbladder', 'Kidney', 'Larynx', 'Leukemia', 'Liver', 'Lung', 'Lymphoma', 'Melanoma', 
                                'Oral/lip', 'Nervous System', 'Ovarian', 'Pancreatic', 'Prostate', 'Rectal', 'Non-Melanoma Skin', 
                                'Skin-Other', 'Soft Tissue', 'Stomach', 'Testicular', 'Thyroid', 'Uterus', 'Other' ) ) )

# TIME SINCE DIAGNOSIS

# bind columns indicating age at diagnosis for specific cancer types
l <- c( 'mcqdat99',
     paste0( 'mcqdat0', c( 1, 3, 5, 7, 9 ) ),
     paste0( 'mcqdat', c( 11, 13, 15 ) ) )

l.2 <- list( )
for( i in 1:length( l ) ){
  l.2[[ i ]] <- get( l[ i ] )[ , c( "SEQN", "MCQ240A", "MCQ240B", "MCQ240C", "MCQ240D", "MCQ240E", 
                                  "MCQ240F", "MCQ240G", "MCQ240H", "MCQ240I", "MCQ240J", 
                                  "MCQ240K", "MCQ240L", "MCQ240M", "MCQ240N", "MCQ240O", 
                                  "MCQ240P", "MCQ240Q", "MCQ240R", "MCQ240S", "MCQ240T", 
                                  "MCQ240U", "MCQ240V", "MCQ240W", "MCQ240X", "MCQ240Y", 
                                  "MCQ240Z", "MCQ240AA", "MCQ240BB", "MCQ240CC", "MCQ240DD" ) ] 
  
    timeall <- do.call( 'rbind', l.2 )
}


# bind columns indicating age at diagnosis of cancer
l.3 <- c( 'mcqdat05', 'mcqdat07', 'mcqdat09', 'mcqdat11', 'mcqdat13', 'mcqdat15' )

l.4 <- list( )
for ( i in 1:length( l.3 ) ){
  
l.4[[ i ]] <- get( l.3[ i ] )[ , c( "SEQN", "MCQ240DK" ) ]

dkall <- do.call( 'rbind', l.4 )
}

# 2017-2018 cycle columns
time1718 <- mcqdat17[ , c( 'SEQN', 'MCD240A', 'MCD240B', 'MCD240C' ) ]

# merge
mcqall.b <- left_join( timeall, dkall, by = 'SEQN' ) %>%
  left_join( mcqall, ., by = 'SEQN' ) %>%
  left_join( ., time1718, by = 'SEQN' )

# cancer site labels
csites <- c( 'Bladder', 'Blood', 'Bone', 'Brain', 'Breast', 'Cervical', 'Colon', 
  'Esophageal', 'Gallbladder', 'Kidney', 'Larynx', 'Leukemia', 'Liver', 'Lung', 'Lymphoma', 'Melanoma', 
  'Oral/lip', 'Nervous System', 'Ovarian', 'Pancreatic', 'Prostate', 'Rectal', 'Non-Melanoma Skin', 
  'Skin-Other', 'Soft Tissue', 'Stomach', 'Testicular', 'Thyroid', 'Uterus', 'Other', 'Unknown' )

age.dx.var <- c( paste0( 'MCQ240', LETTERS ), paste0( 'MCQ240', c( 'AA', 'BB', 'CC', 'DD', 'DK' ) ) )


# Create three new variables
# Note: ( no subjects answered "Unknown" for the 2nd and 3rd cancer sites )

mcqall.b$AgeDxA <- NA
for ( i in 1:length( csites ) ){
  mcqall.b[ which( mcqall.b$CATYPEA == csites[ i ] ), 'AgeDxA' ] <- mcqall.b[ which( mcqall.b$CATYPEA == csites[ i ] ), age.dx.var[ i ]]
}

mcqall.b$AgeDxB <- NA
mcqall.b$AgeDxC <- NA
for ( i in 1:30 ){
  mcqall.b[ which( mcqall.b$CATYPEB == csites[ i ] ), 'AgeDxB' ] <- mcqall.b[ which( mcqall.b$CATYPEB == csites[ i ] ), age.dx.var[ i ]]
  mcqall.b[ which( mcqall.b$CATYPEC == csites[ i ] ), 'AgeDxC' ] <- mcqall.b[ which( mcqall.b$CATYPEC == csites[ i ] ), age.dx.var[ i ]]
}




########## CREATE FINAL TIME SINCE DX VARIABLE AND

# cycle 10 ( 2017-2018 ) already has a variable with age at a, b,  and c cancer dx
# so I will append them to new variables created
mcqall.bc <- mcqall.b %>%
  mutate( AgeDxA = ifelse( cycle == '2017-2018', MCD240A, AgeDxA ),
          AgeDxB = ifelse( cycle == '2017-2018', MCD240B, AgeDxB ),
          AgeDxC = ifelse( cycle == '2017-2018', MCD240C, AgeDxC ) )

# CREATE VARIABLES
mcqall.bcd <- mcqall.bc %>%
  mutate(   AgeDxAmn = AgeDxA * 12 ,
            AgeDxBmn = AgeDxB * 12 ,
            AgeDxCmn = AgeDxC * 12 ) %>%
  rowwise(  ) %>%
  mutate( RIDAGEMN = ifelse( is.na( RIDAGEMN ) == T, RIDAGEYR * 12, RIDAGEMN ),
          
          TimeSinceCADX = ifelse( is.na( AgeDxA ) == T & is.na( AgeDxB ) == T & is.na( AgeDxC ) == T, NA, 
                                  ifelse( CA == 1, RIDAGEYR-min( c( AgeDxA, AgeDxB, AgeDxC ),  na.rm = TRUE ), NA ) ),
          
          TimeSinceCADXmn = ifelse( is.na( AgeDxAmn ) == T & is.na( AgeDxBmn ) == T & is.na( AgeDxCmn ) == T, NA, 
                                    ifelse( CA == 1, RIDAGEMN-min( c( AgeDxAmn, AgeDxBmn, AgeDxCmn ),  na.rm = TRUE ), NA ) ) ) %>%
  ungroup( ) %>%
  mutate( PrimaryCA = factor( as.character( ifelse( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == T & is.na ( CATYPEC ) == T, paste0(  CATYPEA ),
                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB != 'Non-Melanoma Skin', paste0( CATYPEB ),
                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA != 'Non-Melanoma Skin', paste0( CATYPEA ),
                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin', paste0( CATYPEA ),
                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin', paste0( CATYPEB ),
                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB <= AgeDxC ), paste0( CATYPEB ),
                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxC ), paste0( CATYPEA ),
                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxB ), paste0( CATYPEA ),
                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxA > AgeDxB ), paste0( CATYPEB ),
                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxC ), paste0( CATYPEA ),
                                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA > AgeDxC ), paste0( CATYPEC ),
                                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB <= AgeDxC ), paste0( CATYPEB ),
                                                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB > AgeDxC ), paste0( CATYPEC ),
                                                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA != 'Non-Melanoma Skin', paste0( CATYPEA ),                                                                                                         
                                                                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB != 'Non-Melanoma Skin', paste0( CATYPEB ),
                                                                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC != 'Non-Melanoma Skin', paste0( CATYPEC ),
                                                                                                                                                                            NA ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ),
          TimeSinceCADX = ifelse( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == T & is.na ( CATYPEC ) == T, RIDAGEYR-AgeDxA,
                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB != 'Non-Melanoma Skin', RIDAGEYR-AgeDxB,
                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA != 'Non-Melanoma Skin', RIDAGEYR-AgeDxA,
                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin', RIDAGEYR-AgeDxA,
                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin', RIDAGEYR-AgeDxB,
                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB <= AgeDxC ), RIDAGEYR-AgeDxB,
                                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxC ), RIDAGEYR-AgeDxA,
                                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxB ), RIDAGEYR-AgeDxA,
                                                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxA >= AgeDxB ), RIDAGEYR-AgeDxB,
                                                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA <= AgeDxC ), RIDAGEYR-AgeDxA,
                                                                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxA >= AgeDxC ), RIDAGEYR-AgeDxC,
                                                                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB <= AgeDxC ), RIDAGEYR-AgeDxB,
                                                                                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxB >= AgeDxC ), RIDAGEYR-AgeDxC,
                                                                                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxA ) & CATYPEA != 'Non-Melanoma Skin', RIDAGEYR-AgeDxA,                                                                                                         
                                                                                                                                          ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxB ) & CATYPEB != 'Non-Melanoma Skin', RIDAGEYR-AgeDxB,
                                                                                                                                                  ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADX == RIDAGEYR-AgeDxC ) & CATYPEC != 'Non-Melanoma Skin', RIDAGEYR-AgeDxC,
                                                                                                                                                          TimeSinceCADX ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ),
          TimeSinceCADXmn = ifelse( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == T & is.na ( CATYPEC ) == T, RIDAGEMN-AgeDxAmn,
                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB != 'Non-Melanoma Skin', RIDAGEMN-AgeDxBmn,
                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA != 'Non-Melanoma Skin', RIDAGEMN-AgeDxAmn,
                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB == 'Non-Melanoma Skin', RIDAGEMN-AgeDxAmn,
                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == T ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA == 'Non-Melanoma Skin', RIDAGEMN-AgeDxBmn,
                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxBmn <= AgeDxCmn ), RIDAGEMN-AgeDxBmn,
                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxAmn <= AgeDxCmn ), RIDAGEMN-AgeDxAmn,
                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxCmn ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxAmn <= AgeDxBmn ), RIDAGEMN-AgeDxAmn,
                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxCmn ) & CATYPEC == 'Non-Melanoma Skin' & ( AgeDxAmn >= AgeDxBmn ), RIDAGEMN-AgeDxBmn,
                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxAmn <= AgeDxCmn ), RIDAGEMN-AgeDxAmn,
                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB == 'Non-Melanoma Skin' & ( AgeDxAmn >= AgeDxCmn ), RIDAGEMN-AgeDxCmn,
                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxBmn <= AgeDxCmn ), RIDAGEMN-AgeDxBmn,
                                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA == 'Non-Melanoma Skin' & ( AgeDxBmn >= AgeDxCmn ), RIDAGEMN-AgeDxCmn,
                                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxAmn ) & CATYPEA != 'Non-Melanoma Skin', RIDAGEMN-AgeDxAmn,                                                                                                         
                                                                                                                                            ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxBmn ) & CATYPEB != 'Non-Melanoma Skin', RIDAGEMN-AgeDxBmn,
                                                                                                                                                    ifelse( ( is.na ( CATYPEA ) == F & is.na ( CATYPEB ) == F & is.na ( CATYPEC ) == F ) & ( TimeSinceCADXmn == RIDAGEMN-AgeDxCmn ) & CATYPEC != 'Non-Melanoma Skin', RIDAGEMN-AgeDxCmn,
                                                                                                                                                            TimeSinceCADXmn ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ),
          TimeCAFactor = as.factor( ifelse( TimeSinceCADX < 2,  '2 < years', 
                                            ifelse( TimeSinceCADX >= 2 & TimeSinceCADX < 6, ' >= 2 and < 6 years', 
                                                    ifelse( TimeSinceCADX >= 6,  ' >= 6 years', NA ) ) ) ) ,
          TimeCAFactormn = as.factor( ifelse( TimeSinceCADXmn < 24,  '2 < years', 
                                              ifelse( TimeSinceCADXmn >= 24 & TimeSinceCADXmn < 72, ' >= 2 and < 6 years', 
                                                      ifelse( TimeSinceCADXmn >= 72,  ' >= 6 years', NA ) ) ) ),
          TimeSinceCADXmn = ifelse( TimeSinceCADXmn < 0, NA, TimeSinceCADXmn ) ,
          TimeCAFactormn = ifelse( TimeSinceCADXmn < 0, NA, as.character( TimeCAFactormn ) ) ,
          TimeSinceCADX = ifelse( TimeSinceCADX < 0, NA, TimeSinceCADX ) ,
          TimeCAFactor = factor( ifelse( TimeSinceCADX < 0, NA, as.character( TimeCAFactor ) ) ) ) %>%
  select( SEQN, TimeSinceCADX, PrimaryCA, CA, TimeCAFactor, RIDAGEYR, RIDAGEMN, AgeDxA, AgeDxB, AgeDxC, CATYPEA, CATYPEB, CATYPEC,
         TimeSinceCADXmn, TimeCAFactormn, AgeDxAmn, AgeDxBmn, AgeDxCmn )



# check to see if above code chunk succeeded and all cancer have a cancer type on record:
mcqall.bcd %>%
  filter( is.na( PrimaryCA ) == T & CA == 1 ) %>%
  select( CATYPEA, CATYPEB, CATYPEC, AgeDxA, AgeDxB, AgeDxC, SEQN ) 
# SEQN = 66141 has 2 cancer diagnoses but missing age at second diagnosis. We will assume
# his first diagnosis was his first and group him based on it
mcqall.bcd <- mcqall.bcd %>%
  mutate( PrimaryCA = factor( ifelse( SEQN == 66141, paste0( CATYPEA ), as.character( PrimaryCA ) ) ) ) 

`%notin%` <- Negate( `%in%` ) # use this operator in subsequent code chunk


# final cancer site grouping based on previous report in the literature:
# ref: https://mdpi-res.com/d_attachment/cancers/cancers-13-03368/article_deploy/cancers-13-03368-v2.pdf?version = 1625568797

mcqall.bcd <- mcqall.bcd %>%
  mutate( PrimaryCAGroup = factor( ifelse( PrimaryCA %in% c( 'Colon', 'Pancreatic', 'Rectal', 'Stomach', 'Gallbladder', 'Liver' , 'Esophageal' ), 'Gastrointestinal', 
                                      ifelse( PrimaryCA %in% c( 'Kidney', 'Bladder' ), 'Genitourinary', 
                                              ifelse( PrimaryCA %in% c( 'Ovarian', 'Cervical', 'Uterus' ), 'Gynecological', 
                                                      ifelse( PrimaryCA %in% c( 'Testicular', 'Prostate' ), 'MaleReproductive', 
                                                              ifelse( PrimaryCA %in% c( 'Breast' ), 'Breast', 
                                                                      ifelse( PrimaryCA %in% c( 'Melanoma' ), 'Melanoma',
                                                                              ifelse( PrimaryCA %in% c( 'Non-Melanoma Skin' ), 'Non-Melanoma Skin',
                                                                                     ifelse( PrimaryCA %in% c( 'Skin-Other' ), 'Skin-Unknown', 
                                                           ifelse( ( PrimaryCA %notin% c( 'Colon', 'Pancreatic', 'Rectal', 'Stomach', 'Gallbladder', 'Liver' , 'Esophageal',
                                                                                         'Kidney', 'Bladder', 'Ovarian', 'Cervical', 'Uterus', 'Testicular', 'Prostate',
                                                                                         'Breast', 'Melanoma', 'Non-Melanoma Skin', 'Skin-Other' ) ) & is.na( PrimaryCA ) == FALSE, 'Other', NA ) ) ) ) ) ) ) ) ) ) ) 


### END CANCER DATA IMPORT


## SMOKING
smddat99 <- nhanes_load_data( 'SMQ', '1999-2000', demographics = FALSE )
smddat01 <- nhanes_load_data( 'SMQ', '2001-2002', demographics = FALSE )
smddat03 <- nhanes_load_data( 'SMQ', '2003-2004', demographics = FALSE )
smddat05 <- nhanes_load_data( 'SMQ', '2005-2006', demographics = FALSE )
smddat07 <- nhanes_load_data( 'SMQ', '2007-2008', demographics = FALSE )
smddat09 <- nhanes_load_data( 'SMQ', '2009-2010', demographics = FALSE )
smddat11 <- nhanes_load_data( 'SMQ', '2011-2012', demographics = FALSE )
smddat13 <- nhanes_load_data( 'SMQ', '2013-2014', demographics = FALSE )
smddat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT' )
smddat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT' )

smdall <- ( rbind( smddat99[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat01[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat03[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat05[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat07[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat09[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat11[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat13[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat15[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ], 
               smddat17[ , c( 'SEQN', 'SMQ020', 'SMQ050U', 'SMQ050Q', 'SMQ040' ) ] ) )


smdall$SmokStat <- factor( ifelse( smdall$SMQ040 %in% c( 1, 2 ), 'Current', 
                               ifelse( smdall$SMQ040 == 3 & smdall$SMQ020 == 1,  'Former', 
                                      ifelse( smdall$SMQ020 ==  2, 'Never',  
                                             ifelse( smdall$SMQ020 == 2 & is.na( smdall$SMQ040 ) == T, 'Never', NA ) ) ) ) )


                              
# DRINKING
alqdat99 <- nhanes_load_data( 'ALQ', '1999-2000', demographics = TRUE )
alqdat01 <- nhanes_load_data( 'ALQ', '2001-2002', demographics = TRUE )
alqdat03 <- nhanes_load_data( 'ALQ', '2003-2004', demographics = TRUE )
alqdat05 <- nhanes_load_data( 'ALQ', '2005-2006', demographics = TRUE )
alqdat07 <- nhanes_load_data( 'ALQ', '2007-2008', demographics = TRUE )
alqdat09 <- nhanes_load_data( 'ALQ', '2009-2010', demographics = TRUE )
alqdat11 <- nhanes_load_data( 'ALQ', '2011-2012', demographics = TRUE )
alqdat13 <- nhanes_load_data( 'ALQ', '2013-2014', demographics = TRUE )
alqdat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/ALQ_I.XPT' )
alqdat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/ALQ_J.XPT' )
demodat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT' )
demodat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT' )
alqdat15 <- left_join( alqdat15, demodat15, by = 'SEQN' )
alqdat17 <- left_join( alqdat17, demodat17, by = 'SEQN' )

alqall <- ( rbind( alqdat99[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat01[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat03[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat05[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat07[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat09[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat11[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat13[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat15[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ], 
               alqdat17[ , c( 'SEQN', 'RIAGENDR', 'ALQ130' ) ] ) )

alqall$ALCUSE <- factor( ifelse( alqall$RIAGENDR == 1 & alqall$ALQ130 > 2 & alqall$ALQ130 < 999 & is.na( alqall$ALQ130 ) == F, 1, 
                               ifelse( alqall$RIAGENDR == 1 & alqall$ALQ130 <= 2 & is.na( alqall$ALQ130 ) == F, 0, 
                                      ifelse( alqall$RIAGENDR == 2 & alqall$ALQ130 > 1 & alqall$ALQ130 < 999 & is.na( alqall$ALQ130 ) == F, 1, 
                                             ifelse( alqall$RIAGENDR == 2 & alqall$ALQ130 <= 1 & is.na( alqall$ALQ130 ) == F, 0, 
                                      ifelse( alqall$ALQ130 == 999 & is.na( alqall$ALQ130 ) == F, NA, NA ) ) ) ) ), levels = c( 0, 1 ), labels = c( 'None/moderate', 'Heavy' ) )


## FOOD SECURITY

## food security data is not available for 2017
fsddat99 <- nhanes_load_data( 'FSQ', '1999-2000', demographics = FALSE )
fsddat01 <- nhanes_load_data( 'FSQ', '2001-2002', demographics = FALSE )
fsddat03 <- nhanes_load_data( 'FSQ', '2003-2004', demographics = FALSE )
fsddat05 <- nhanes_load_data( 'FSQ', '2005-2006', demographics = FALSE )
fsddat07 <- nhanes_load_data( 'FSQ', '2007-2008', demographics = FALSE )
fsddat09 <- nhanes_load_data( 'FSQ', '2009-2010', demographics = FALSE )
fsddat11 <- nhanes_load_data( 'FSQ', '2011-2012', demographics = FALSE )
fsddat13 <- nhanes_load_data( 'FSQ', '2013-2014', demographics = FALSE )
fsddat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/FSQ_I.XPT' )
fsddat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/FSQ_J.XPT' )

fsd0305 <- rbind( fsddat03[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ170' ) ], fsddat05[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ170' ) ] )
fsd0712 <- rbind( fsddat07[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ171', 'FSQ165' ) ], fsddat09[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ171', 'FSQ165' ) ], 
               fsddat11[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ171', 'FSQ165' ) ] )

fsd0712 <- fsd0712 %>%
  mutate( FSQ171 = ifelse( FSQ165 ==  2, 2, FSQ171 ) ) %>%
  mutate( FSQ170 = ifelse( FSQ165 == 2, 2, 
                       ifelse( FSQ171 == 1, 1, 
                              ifelse( FSQ171 == 2, 2, NA ) ) ) ) %>%
  select(-FSQ165,-FSQ171 )

sum( is.na( fsd0712$FSQ170 ) )


fsd1317 <- rbind( fsddat13[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ012', 'FSQ165' ) ], fsddat15[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ012', 'FSQ165' ) ], 
               fsddat17[ , c( 'SEQN', 'FSDHH', 'FSDAD', 'FSQ162', 'FSQ012', 'FSQ165' ) ] )

fsd1317 <- fsd1317 %>%
  mutate( FSQ012 = ifelse( FSQ165 ==  2, 2, FSQ012 ) ) %>%
  mutate( FSQ170 = ifelse( FSQ165 == 2, 2, 
                       ifelse( FSQ012 == 1, 1, 
                              ifelse( FSQ012 == 2, 2, NA ) ) ) ) %>%
  select(-FSQ165,-FSQ012 )



fsd9901 <- rbind( fsddat99[ , c( 'SEQN', 'HHFDSEC', 'ADFDSEC', 'FSD160', 'FSD170N' ) ], fsddat01[ , c( 'SEQN', 'HHFDSEC', 'ADFDSEC', 'FSD160', 'FSD170N' ) ] )
table( fsd9901$FSD170N )
fsd9901 <- fsd9901 %>%
  mutate( FSQ170 = ifelse( FSD170N %in% c( 1:7 ), 1, 
                       ifelse( is.na( FSD170N ) == T, 2, NA ) ) ) %>%
  select(-FSD170N )

colnames( fsd9901 ) <- c( 'SEQN', 'FSDHH', 'FSDAD', 'FSD160', 'FSQ170' )
colnames( fsd0305 ) <- c( 'SEQN', 'FSDHH', 'FSDAD', 'FSD160', 'FSQ170' )
colnames( fsd0712 ) <- c( 'SEQN', 'FSDHH', 'FSDAD', 'FSD160', 'FSQ170' )
colnames( fsd1317 ) <- c( 'SEQN', 'FSDHH', 'FSDAD', 'FSD160', 'FSQ170' )


fsdall <- rbind( fsd9901, fsd0305, fsd0712, fsd1317 )
nrow( fsdall )
table( fsdall$FSQ170 ) 

fsdall <- fsdall %>%
  mutate( FSQ170 = ifelse( FSQ170 %in% c( 7, 9 ), NA, FSQ170 ) )
  
table( fsdall$FSQ170 ) 


# ordinal FS variables
fsdall$FoodSecCatHH <- factor( fsdall$FSDHH, levels = c( 1, 2, 3, 4 ), labels = c( 'Full', 'Marginal', 'Low', 'Very Low' ), 
                             ordered = TRUE )
fsdall$FoodSecCatAD <- factor( fsdall$FSDAD, levels = c( 1, 2, 3, 4 ), labels = c( 'Full', 'Marginal', 'Low', 'Very Low' ), 
                             ordered = TRUE )

# Food assistance ( WIC or Food Stamps at household level in last 12 months )

fsdall$FoodAsstP <- factor( ifelse( ( fsdall$FSD160 == 1 | fsdall$FSQ170 == 1 ), 'yes', 
                                ifelse( ( fsdall$FSD160 == 2 | fsdall$FSQ170 == 2 ), 'no', NA ) ) )

# Food assistance ( Food Stamps at household level in last 12 months-- No WIC included )

fsdall$FoodAsstPnowic <- factor( ifelse( fsdall$FSQ170 == 1, 'yes', 
                                ifelse( fsdall$FSQ170 == 2, 'no', NA ) ) )

table( fsdall$FSQ170 )
table( fsdall$FoodAsstP )
table( fsdall$FoodAsstPnowic )

table( fsd9901$FSQ170 )

# binary FS variables
fsdall$BinFoodSecHH <- factor( fsdall$FSDHH, levels = c( 1, 2, 3, 4 ), labels = c( 'High', 'High', 'Low', 'Low' ) )
fsdall$BinFoodSecAD <- factor( fsdall$FSDAD, levels = c( 1, 2, 3, 4 ), labels = c( 'High', 'High', 'Low', 'Low' ) )


# DEMOGRAPHIC DATA

demodat99 <- nhanes_load_data( 'DEMO', '1999-2000' )
demodat01 <- nhanes_load_data( 'DEMO', '2001-2002' )
demodat03 <- nhanes_load_data( 'DEMO', '2003-2004' )
demodat05 <- nhanes_load_data( 'DEMO', '2005-2006' )
demodat07 <- nhanes_load_data( 'DEMO', '2007-2008' )
demodat09 <- nhanes_load_data( 'DEMO', '2009-2010' )
demodat11 <- nhanes_load_data( 'DEMO', '2011-2012' )
demodat13 <- nhanes_load_data( 'DEMO', '2013-2014' )
demodat15$cycle <- '2015-2016'
demodat17$cycle <- '2017-2018'
demo9905 <- rbind( demodat99[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHINC', 'DMDEDUC2' ) ], 
               demodat01[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHINC', 'DMDEDUC2' ) ], 
               demodat03[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHINC', 'DMDEDUC2' ) ], 
               demodat05[ c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHINC', 'DMDEDUC2' ) ] )

demo0717 <- rbind( demodat07[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ], 
               demodat09[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ], 
               demodat11[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ], 
               demodat13[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ], 
               demodat15[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ], 
               demodat17[ , c( 'SEQN', 'RIAGENDR', 'SDDSRVYR', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'RIDAGEYR', 'DMDMARTL', 'RIDRETH1', 'INDFMPIR', 'DMDHHSIZ', 'INDHHIN2', 'DMDEDUC2' ) ] )

colnames( demo9905 ) <- c( 'SEQN', 'Gender', 'Cycle', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'Age', 'MaritalStatus', 'Race', 'INDFMPIR', 'HHSize', 'HHIncome', 'Education' )
colnames( demo0717 ) <- c( 'SEQN', 'Gender', 'Cycle', 'SDMVPSU', 'SDMVSTRA', 'WTINT2YR', 'Age', 'MaritalStatus', 'Race', 'INDFMPIR', 'HHSize', 'HHIncome', 'Education' )
demoall <- rbind( demo9905, demo0717 )
nrow( demoall )
sum( is.na( demoall$HHIncome ) )
demoall <- demoall %>%
  mutate( Gender = factor( Gender, levels = c( 1, 2 ), labels = c( 'Male', 'Female' ) ) ) %>%
  mutate( Agecat = factor( ifelse( Age >= 60,  'elderly', 
                       ifelse( Age < 60, 'non-elderly',  NA ) ) ) ) %>%
  mutate( Race_binary = factor( ifelse( Race %in% c( 1, 2, 4, 5 ), 'Minority', 
                            ifelse( Race == 3, 'White', NA ) ) ) ) %>%
  mutate( Race = factor( Race, levels = c( 1, 2, 3, 4, 5 ), labels = c( 'Mexican American', 'Other Hispanic', 
                                                       'Non-Hispanic White', 'Non-Hispanic Black', 'Other/Multiracial' ) ) ) %>%
  mutate( MaritalStatus = factor( MaritalStatus, levels = c( 1, 2, 3, 4, 5, 6, 77, 99 ), labels = c( 'Married', 'Widowed', 
                                                                                 'Divorced/Separated', 
                                                                                 'Divorced/Separated', 
                                                                                 'Never Married', 
                                                                                 'Living w/ Partner' ), 
                                                                                 exclude = c( 77, 99 ) ) ) %>%
  mutate( HHIncome = factor( HHIncome, levels = c( 1:15, 77, 99 ), labels = c( ' < 35k', ' < 35k', ' < 35k', ' < 35k', ' < 35k', 
                                                                ' < 35k', '35-< 75k', '35-< 75k', '35-< 75k', '35-< 75k', ' >= 75k', ' < 35k', 
                                                                ' >= 75k', ' >= 75k' ), exclude = c( 12, 77, 99 ) ) ) %>%
  mutate( HHIncome = factor( ifelse( HHIncome %in% c( ' > 20k', 'NA' ), NA, as.character( HHIncome ) ) ) ) %>%
  mutate( fipr = factor( ifelse( INDFMPIR < 1.3, 1, 
                            ifelse( INDFMPIR >= 1.3, 0, NA ) ) ) ) %>%
  mutate( Education = factor( Education, levels = c( 1, 2, 3, 4, 5, 7, 9 ), labels = c( ' < 9th Gr', '9th-11 Gr', 
                                                                     'HSchool Diploma/GED', 
                                                                     'Some college/associates', 
                                                                     'College Grad/above' ), 
                                                                     exclude = c( 7, 9 ) ) ) %>%
  mutate( Education_bin = factor( ifelse( Education %in% c( ' < 9th Gr', '9th-11 Gr', 'HSchool Diploma/GED' ), ' <= HS', 
                                  ifelse( Education %in% c( 'Some college/associates', 'College Grad/above' ), ' >= College', NA ) ) ) )
  



# BMI DATA
bmidat99 <- nhanes_load_data( 'BMX', '1999-2000', demographics = FALSE )
bmidat01 <- nhanes_load_data( 'BMX', '2001-2002', demographics = FALSE )
bmidat03 <- nhanes_load_data( 'BMX', '2003-2004', demographics = FALSE )
bmidat05 <- nhanes_load_data( 'BMX', '2005-2006', demographics = FALSE )
bmidat07 <- nhanes_load_data( 'BMX', '2007-2008', demographics = FALSE )
bmidat09 <- nhanes_load_data( 'BMX', '2009-2010', demographics = FALSE )
bmidat11 <- nhanes_load_data( 'BMX', '2011-2012', demographics = FALSE )
bmidat13 <- nhanes_load_data( 'BMX', '2013-2014', demographics = FALSE )
bmidat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT' )
bmidat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT' )

bmiall <- rbind( bmidat99[ , c( 'SEQN', 'BMXBMI' ) ], bmidat01[ , c( 'SEQN', 'BMXBMI' ) ], bmidat03[ , c( 'SEQN', 'BMXBMI' ) ], bmidat05[ , c( 'SEQN', 'BMXBMI' ) ], 
              bmidat07[ , c( 'SEQN', 'BMXBMI' ) ], bmidat09[ , c( 'SEQN', 'BMXBMI' ) ], bmidat11[ , c( 'SEQN', 'BMXBMI' ) ], 
              bmidat13[ , c( 'SEQN', 'BMXBMI' ) ], bmidat15[ , c( 'SEQN', 'BMXBMI' ) ], bmidat17[ , c( 'SEQN', 'BMXBMI' ) ] )


sum( is.na( bmiall$BMXBMI ) )


# NUTRIENT INTAKES ESTIMATED FROM 24 HR RECALLS

nutrdat99 <- nhanes_load_data( 'DRXTOT', '1999-2000', demographics = FALSE )
nutrdat01 <- nhanes_load_data( 'DRXTOT', '2001-2002', demographics = FALSE )
nutrdat03d1 <- nhanes_load_data( 'DR1TOT', '2003-2004', demographics = FALSE )
nutrdat03d2 <- nhanes_load_data( 'DR2TOT', '2003-2004', demographics = FALSE )
nutrdat05d1 <- nhanes_load_data( 'DR1TOT', '2005-2006', demographics = FALSE )
nutrdat05d2 <- nhanes_load_data( 'DR2TOT', '2005-2006', demographics = FALSE )
nutrdat07d1 <- nhanes_load_data( 'DR1TOT', '2007-2008', demographics = FALSE )
nutrdat07d2 <- nhanes_load_data( 'DR2TOT', '2007-2008', demographics = FALSE )
nutrdat09d1 <- nhanes_load_data( 'DR1TOT', '2009-2010', demographics = FALSE )
nutrdat09d2 <- nhanes_load_data( 'DR2TOT', '2009-2010', demographics = FALSE )
nutrdat11d1 <- nhanes_load_data( 'DR1TOT', '2011-2012', demographics = FALSE )
nutrdat11d2 <- nhanes_load_data( 'DR2TOT', '2011-2012', demographics = FALSE )
nutrdat13d1 <- nhanes_load_data( 'DR1TOT', '2013-2014', demographics = FALSE )
nutrdat13d2 <- nhanes_load_data( 'DR2TOT', '2013-2014', demographics = FALSE )
nutrdat15d1 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.XPT' )
nutrdat15d2 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2TOT_I.XPT' )
nutrdat17d1 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1TOT_J.XPT' )
nutrdat17d2 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR2TOT_J.XPT' )

nutr9901 <- rbind( nutrdat99[ c( "SEQN", 'WTDRD1', "DRXTKCAL", "DRXTCARB", "DRXTPROT", "DRXTTFAT", "DRXTSFAT", 
                            "DRXTMFAT", "DRXTPFAT", "DRXTCHOL", "DRXTFIBE", 'DRXTALCO', 'DRXTVC', 'DRXTZINC', 'DRXTMAGN', 'DRXTVB1', 'DRXTVB2', 'DRXTFOLA', 'DRXTNIAC', 
                            'DRXTVB6', 'DRXTSELE', 'DRXTCALC', 'DRXTIRON', 'DRDTSODI', 'WTDR4YR' ) ], nutrdat01[ c( "SEQN", "DRXTKCAL", "DRXTPROT", "DRXTCARB", "DRXTTFAT", "DRXTSFAT", 
                                                                                                 "DRXTMFAT", "DRXTPFAT", "DRXTCHOL", "DRXTFIBE", 'DRXTALCO', 'DRXTVC', 
                                                                                                 'DRXTZINC', 'DRXTMAGN', 'DRXTVB1', 'DRXTVB2', 'DRXTFOLA', 'DRXTNIAC', 
                                                                                                 'DRXTVB6', 'DRXTSELE', 'DRXTCALC', 'DRXTIRON', 'DRDTSODI', 'WTDRD1', 'WTDR4YR' ) ] )


names( nutrdat03d1 )
nutr9901 <- nutr9901 %>%
  mutate( `PMUFA:SFA` = ( DRXTMFAT + DRXTPFAT )/DRXTSFAT ) %>%
  mutate( WTDR2D = NA ) %>% # set 2nd day of 24hr recall weight to NA in order to successfully merge data
  select( SEQN, WTDRD1, WTDR2D, DRXTKCAL, DRXTPROT, DRXTCARB, DRXTTFAT, DRXTSFAT, 
         DRXTMFAT, DRXTPFAT, DRXTCHOL, DRXTFIBE, DRXTALCO, DRXTVC, DRXTZINC, DRXTMAGN, DRXTVB1, DRXTVB2, DRXTFOLA, DRXTNIAC, 
         DRXTVB6, DRXTSELE, DRXTCALC, DRXTIRON, DRDTSODI, WTDR4YR, `PMUFA:SFA` )


# 2003-2004 nutrient
nutrdat03 <- merge( nutrdat03d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat03d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat03 <- nutrdat03 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )



# 2005-2006 nutrient
nutrdat05 <- merge( nutrdat05d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat05d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat05 <- nutrdat05 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )

# 2007-2008 nutrient
nutrdat07 <- merge( nutrdat07d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat07d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat07 <- nutrdat07 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )


# 2009-2010 nutrient
nutrdat09 <- merge( nutrdat09d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat09d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat09 <- nutrdat09 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )


# 2011-2012 nutrient
nutrdat11 <- merge( nutrdat11d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat11d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat11 <- nutrdat11 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )


# 2013-2014 nutrient
nutrdat13 <- merge( nutrdat13d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat13d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat13 <- nutrdat13 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )

# 2015-2016 nutrient
nutrdat15 <- merge( nutrdat15d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat15d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat15 <- nutrdat15 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )
# 2017-2018 nutrient
nutrdat17 <- merge( nutrdat17d1[ c( "SEQN", "DRDINT", "DR1TKCAL", "DR1TPROT", "DR1TCARB", "DR1TTFAT", "DR1TSFAT", 
                               "DR1TMFAT", "DR1TPFAT", "DR1TCHOL", "DR1TFIBE", 'DR1TALCO', 'DR1TVC', 'DR1TZINC', 'DR1TMAGN', 
                               'DR1TVB1', 'DR1TVB2', 'DR1TFOLA', 'DR1TNIAC', 'DR1TVB6', 'DR1TSELE', 'DR1TCALC', 'DR1TIRON', 'DR1TSODI', 'WTDRD1', 'WTDR2D' ) ], 
                 nutrdat17d2[ c( "SEQN", "DRDINT", "DR2TKCAL", "DR2TPROT", "DR2TCARB", "DR2TTFAT", "DR2TSFAT", 
                               "DR2TMFAT", "DR2TPFAT", "DR2TCHOL", "DR2TFIBE", 'DR2TALCO', 'DR2TVC', 'DR2TZINC', 'DR2TMAGN', 
                               'DR2TVB1', 'DR2TVB2', 'DR2TFOLA', 'DR2TNIAC', 'DR2TVB6', 'DR2TSELE', 'DR2TCALC', 'DR2TIRON', 'DR2TSODI', 'WTDR2D' ) ] )
nutrdat17 <- nutrdat17 %>%
  mutate( KCAL = ifelse( DRDINT == 2, ( DR1TKCAL + DR2TKCAL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TKCAL ) == FALSE, DR1TKCAL, 
                            ifelse( DRDINT == 1 & is.na( DR2TKCAL ) == FALSE, DR2TKCAL, NA ) ) ) ) %>%
  mutate( PROT = ifelse( DRDINT == 2, ( DR1TPROT + DR2TPROT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPROT ) == FALSE, DR1TPROT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPROT ) == FALSE, DR2TPROT, NA ) ) ) ) %>%
  mutate( CARB = ifelse( DRDINT == 2, ( DR1TCARB + DR2TCARB )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCARB ) == FALSE, DR1TCARB, 
                            ifelse( DRDINT == 1 & is.na( DR2TCARB ) == FALSE, DR2TCARB, NA ) ) ) ) %>%
  mutate( TFAT = ifelse( DRDINT == 2, ( DR1TTFAT + DR2TTFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TTFAT ) == FALSE, DR1TTFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TTFAT ) == FALSE, DR2TTFAT, NA ) ) ) ) %>%
  mutate( SFAT = ifelse( DRDINT == 2, ( DR1TSFAT + DR2TSFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSFAT ) == FALSE, DR1TSFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TSFAT ) == FALSE, DR2TSFAT, NA ) ) ) ) %>%
  mutate( MFAT = ifelse( DRDINT == 2, ( DR1TMFAT + DR2TMFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMFAT ) == FALSE, DR1TMFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TMFAT ) == FALSE, DR2TMFAT, NA ) ) ) ) %>%
  mutate( PFAT = ifelse( DRDINT == 2, ( DR1TPFAT + DR2TPFAT )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TPFAT ) == FALSE, DR1TPFAT, 
                            ifelse( DRDINT == 1 & is.na( DR2TPFAT ) == FALSE, DR2TPFAT, NA ) ) ) ) %>%
  mutate( CHOL = ifelse( DRDINT == 2, ( DR1TCHOL + DR2TCHOL )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCHOL ) == FALSE, DR1TCHOL, 
                            ifelse( DRDINT == 1 & is.na( DR2TCHOL ) == FALSE, DR2TCHOL, NA ) ) ) ) %>%
  mutate( FIBE = ifelse( DRDINT == 2, ( DR1TFIBE + DR2TFIBE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFIBE ) == FALSE, DR1TFIBE, 
                            ifelse( DRDINT == 1 & is.na( DR2TFIBE ) == FALSE, DR2TFIBE, NA ) ) ) ) %>%
  mutate( ALCO = ifelse( DRDINT == 2, ( DR1TALCO + DR2TALCO )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TALCO ) == FALSE, DR1TALCO, 
                            ifelse( DRDINT == 1 & is.na( DR2TALCO ) == FALSE, DR2TALCO, NA ) ) ) ) %>%
  mutate( VITC = ifelse( DRDINT == 2, ( DR1TVC + DR2TVC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TVC ) == FALSE, DR1TVC, 
                            ifelse( DRDINT == 1 & is.na( DR2TVC ) == FALSE, DR2TVC, NA ) ) ) ) %>%
  mutate( ZINC = ifelse( DRDINT == 2, ( DR1TZINC + DR2TZINC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TZINC ) == FALSE, DR1TZINC, 
                            ifelse( DRDINT == 1 & is.na( DR2TZINC ) == FALSE, DR2TZINC, NA ) ) ) ) %>%
  mutate( MAGN = ifelse( DRDINT == 2, ( DR1TMAGN + DR2TMAGN )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TMAGN ) == FALSE, DR1TMAGN, 
                            ifelse( DRDINT == 1 & is.na( DR2TMAGN ) == FALSE, DR2TMAGN, NA ) ) ) ) %>%
  mutate( B1 = ifelse( DRDINT == 2, ( DR1TVB1 + DR2TVB1 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB1 ) == FALSE, DR1TVB1, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB1 ) == FALSE, DR2TVB1, NA ) ) ) ) %>%
  mutate( B2 = ifelse( DRDINT == 2, ( DR1TVB2 + DR2TVB2 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB2 ) == FALSE, DR1TVB2, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB2 ) == FALSE, DR2TVB2, NA ) ) ) ) %>%
  mutate( FOLA = ifelse( DRDINT == 2, ( DR1TFOLA + DR2TFOLA )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TFOLA ) == FALSE, DR1TFOLA, 
                            ifelse( DRDINT == 1 & is.na( DR2TFOLA ) == FALSE, DR2TFOLA, NA ) ) ) ) %>%
  mutate( B5 = ifelse( DRDINT == 2, ( DR1TNIAC + DR2TNIAC )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TNIAC ) == FALSE, DR1TNIAC, 
                          ifelse( DRDINT == 1 & is.na( DR2TNIAC ) == FALSE, DR2TNIAC, NA ) ) ) ) %>%
  mutate( B6 = ifelse( DRDINT == 2, ( DR1TVB6 + DR2TVB6 )/2, 
                   ifelse( DRDINT == 1 & is.na( DR1TVB6 ) == FALSE, DR1TVB6, 
                          ifelse( DRDINT == 1 & is.na( DR2TVB6 ) == FALSE, DR2TVB6, NA ) ) ) ) %>%
  mutate( SELE = ifelse( DRDINT == 2, ( DR1TSELE + DR2TSELE )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TSELE ) == FALSE, DR1TSELE, 
                            ifelse( DRDINT == 1 & is.na( DR2TSELE ) == FALSE, DR2TSELE, NA ) ) ) ) %>%
  mutate( CALC = ifelse( DRDINT == 2, ( DR1TCALC + DR2TCALC )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TCALC ) == FALSE, DR1TCALC, 
                            ifelse( DRDINT == 1 & is.na( DR2TCALC ) == FALSE, DR2TCALC, NA ) ) ) ) %>%
  mutate( IRON = ifelse( DRDINT == 2, ( DR1TIRON + DR2TIRON )/2, 
                     ifelse( DRDINT == 1 & is.na( DR1TIRON ) == FALSE, DR1TIRON, 
                            ifelse( DRDINT == 1 & is.na( DR2TIRON ) == FALSE, DR2TIRON, NA ) ) ) ) %>%
  mutate( SODIUM = ifelse( DRDINT == 2, ( DR1TSODI + DR2TSODI )/2, 
                       ifelse( DRDINT == 1 & is.na( DR1TSODI ) == FALSE, DR1TSODI, 
                              ifelse( DRDINT == 1 & is.na( DR2TSODI ) == FALSE, DR2TSODI, NA ) ) ) ) %>%
  mutate( `PMUFA:SFA` = ( MFAT + PFAT )/SFAT ) %>%
  mutate( WTDR4YR = NA ) %>%  ## weights for multiple cycles require 4 yr weights for years 99-01
  select( SEQN, WTDRD1, WTDR2D, KCAL, PROT, CARB, TFAT, SFAT, MFAT, PFAT, CHOL, FIBE, ALCO, VITC, 
         ZINC, MAGN, B1, B2, FOLA, B5, B6, SELE, CALC, IRON, SODIUM, WTDR4YR, `PMUFA:SFA` )

# merge nutrient data
colnames( nutr9901 ) <- colnames( nutrdat03 )

nutrall <- rbind( nutr9901, nutrdat03, nutrdat05, nutrdat07, nutrdat09, nutrdat11, nutrdat13, nutrdat15, nutrdat17 )
length( unique( nutrall$SEQN ) )
nrow( nutrall )
nutrall <- nutrall[ unique( nutrall$SEQN ), ]


###### CHARLSON COMORBIDITY SCORE ( CCI ) #######

# Diabetes questionnaire
diqdat99 <- nhanes_load_data( 'DIQ', '1999-2000', demographics = FALSE )
diqdat01 <- nhanes_load_data( 'DIQ', '2001-2002', demographics = FALSE )
diqdat03 <- nhanes_load_data( 'DIQ', '2003-2004', demographics = FALSE )
diqdat05 <- nhanes_load_data( 'DIQ', '2005-2006', demographics = FALSE )
diqdat07 <- nhanes_load_data( 'DIQ', '2007-2008', demographics = FALSE )
diqdat09 <- nhanes_load_data( 'DIQ', '2009-2010', demographics = FALSE )
diqdat11 <- nhanes_load_data( 'DIQ', '2011-2012', demographics = FALSE )
diqdat13 <- nhanes_load_data( 'DIQ', '2013-2014', demographics = FALSE )
diqdat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT' )
diqdat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT' )

# Kidney conditions questionnaire
kiqdat99 <- nhanes_load_data( 'KIQ', '1999-2000', demographics = FALSE )
kiqdat01 <- nhanes_load_data( 'KIQ_U_B', '2001-2002', demographics = FALSE )
kiqdat03 <- nhanes_load_data( 'KIQ_U_C', '2003-2004', demographics = FALSE )
kiqdat05 <- nhanes_load_data( 'KIQ_U_D', '2005-2006', demographics = FALSE )
kiqdat07 <- nhanes_load_data( 'KIQ_U_E', '2007-2008', demographics = FALSE )
kiqdat09 <- nhanes_load_data( 'KIQ_U_F', '2009-2010', demographics = FALSE )
kiqdat11 <- nhanes_load_data( 'KIQ_U_G', '2011-2012', demographics = FALSE )
kiqdat13 <- nhanes_load_data( 'KIQ_U_H', '2013-2014', demographics = FALSE )
kiqdat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/KIQ_U_I.XPT' )
kiqdat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/KIQ_U_J.XPT' )


diqdat <- rbind( diqdat99[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat01[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat03[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat05[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat07[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat09[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat11[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat13[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat15[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ], 
              diqdat17[ , c( 'SEQN', 'DIQ010', 'DIQ080' ) ] ) # diabetes and retinopathy


kiqdat99 <- kiqdat99 %>%
  rename( KIQ022 = KIQ020 )

kiqdat <- rbind( kiqdat99[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat01[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat03[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat05[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat07[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat09[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat11[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat13[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat15[ , c( 'SEQN', 'KIQ022' ) ], 
              kiqdat17[ , c( 'SEQN', 'KIQ022' ) ] ) # kidney failure






# Cancer data
cadata <- mcqall.bcd[ , c( 'SEQN', 'CATYPEA', 'CATYPEB', 'CATYPEC' ) ] ### fix here

# merge data together and create flag variables
cci_dat <- left_join( diqdat, kiqdat, by = 'SEQN' ) %>%
  left_join( ., mcqall, by = 'SEQN' ) %>%
  mutate( diab_flag = ifelse( DIQ010 == 1, 1, 
                          ifelse( DIQ010 == 2, 0, 
                                 ifelse( DIQ010 == 3, 0, NA ) ) ) ) %>%
  mutate( retin_flag = ifelse( ( DIQ010 == 2 & is.na( DIQ080 ) == T ), 0, 
                           ifelse( DIQ080 == 1, 2, 
                                  ifelse( DIQ080 == 2, 0, NA ) ) ) ) %>%
  mutate( kidn_flag = ifelse( KIQ022 == 1, 2, 
                          ifelse( KIQ022 == 2, 0, NA ) ) ) %>%
  mutate( stroke_flag = ifelse( MCQ160F == 1, 1, 
                            ifelse( MCQ160F == 2, 0, NA ) ) ) %>%
  mutate( chf_flag = ifelse( MCQ160B == 1, 1, 
                         ifelse( MCQ160B == 2, 0, NA ) ) ) %>%
  mutate( liver_flag = ifelse( MCQ160L == 1, 2, 
                           ifelse( MCQ160L == 2, 0, NA ) ) ) %>%
  mutate( RA_flag = ifelse( MCQ160A == 1 & MCQ190 == 1, 1, 
                        ifelse( MCQ160A == 1 & MCQ190 != 1, 0, 
                               ifelse( MCQ160A == 2, 0, NA ) ) ) ) %>%
  mutate( CA_flag = ifelse( MCQ220 == 2, 0, 
                        ifelse( MCQ220 == 1 & is.na( CATYPEA ) == F & is.na( CATYPEB ) == T & is.na( CATYPEC ) == T, 2, 
                               ifelse( MCQ220 == 1 & is.na( CATYPEA ) == F & is.na( CATYPEB ) == F & is.na( CATYPEC ) == T, 4, 
                                      ifelse( MCQ220 == 1 & is.na( CATYPEA ) == F & is.na( CATYPEB ) == F & is.na( CATYPEC ) == F, 6, NA ) ) ) ) ) %>%
  mutate( CCI_Score = diab_flag + retin_flag + kidn_flag + stroke_flag + chf_flag + liver_flag + RA_flag + CA_flag ) %>%
  select( SEQN, CCI_Score )



###### PHYSICAL ACTIVITY DATA #######

PAQdat99 <- nhanes_load_data( 'PAQIAF', '1999-2000', demographics = FALSE )
PAQdat01 <- nhanes_load_data( 'PAQIAF', '2001-2002', demographics = FALSE )
PAQdat03 <- nhanes_load_data( 'PAQIAF', '2003-2004', demographics = FALSE )
PAQdat05 <- nhanes_load_data( 'PAQIAF', '2005-2006', demographics = FALSE )#### PAQ Variable
# Change after 2005
PAQdat07 <- nhanes_load_data( 'PAQ', '2007-2008', demographics = FALSE )
PAQdat09 <- nhanes_load_data( 'PAQ', '2009-2010', demographics = FALSE )
PAQdat11 <- nhanes_load_data( 'PAQ', '2011-2012', demographics = FALSE )
PAQdat13 <- nhanes_load_data( 'PAQ', '2013-2014', demographics = FALSE )
PAQdat15 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT' )
PAQdat17 <- read_xpt( file = 'https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PAQ_J.XPT' )

PA9905 <- list( PAQdat99, PAQdat01, PAQdat03, PAQdat05 )
PA0717 <- list( PAQdat07, PAQdat09, PAQdat11, PAQdat13, PAQdat15, PAQdat17 )

# for cycles 2007-2017
for( i in 1:length( PA0717 ) ){
  
  PA0717[[ i ]] <- PA0717[[ i ]] %>%
    mutate( MinutesVig = ifelse( PAD615 %in% c( 7777, 9999 ), NA, 
                             ifelse( PAQ605 == 2, 0, PAD615 ) ) ) %>%
    mutate( DaysVig = ifelse( PAQ610 %in% c( 77, 99 ), NA, 
                          ifelse( PAQ605 == 2, 0, PAQ610 ) ) ) %>%
    mutate( MinutesMod = ifelse( PAD630 %in% c( 7777, 9999 ), NA, 
                             ifelse( PAQ620 == 2, 0, PAD630 ) ) ) %>%
    mutate( DaysMod = ifelse( PAQ625 %in% c( 77, 99 ), NA, 
                          ifelse( PAQ620 == 2, 0, PAQ625 ) ) ) %>%
    mutate( MinutesBikeWalk = ifelse( PAD645 %in% c( 7777, 9999 ), NA, 
                                  ifelse( PAQ635 == 2, 0, PAD645 ) ) ) %>%
    mutate( DaysBikeWalk = ifelse( PAQ640 %in% c( 77, 99 ), NA, 
                               ifelse( PAQ635 == 2, 0, PAQ640 ) ) ) %>%
    mutate( MinutesVigRec = ifelse( PAD660 %in% c( 7777, 9999 ), NA, 
                                ifelse( PAQ650 == 2, 0, PAD660 ) ) ) %>%
    mutate( DaysVigRec = ifelse( PAQ655 %in% c( 77, 99 ), NA, 
                             ifelse( PAQ650 == 2, 0, PAQ655 ) ) ) %>%
    mutate( MinutesModRec = ifelse( PAD675 %in% c( 7777, 9999 ), NA, 
                                ifelse( PAQ665 == 2, 0, PAD675 ) ) ) %>%
    mutate( DaysModRec = ifelse( PAQ670 %in% c( 77, 99 ), NA, 
                             ifelse( PAQ665 == 2, 0, PAQ670 ) ) ) %>%
    mutate( MinutesSedent = ifelse( PAD680 %in% c( 7777, 9999 ), NA, PAD680 ) ) %>%
    mutate( VigWeeklyMetMin = ( MinutesVig * DaysVig ) * 8 ) %>%
    mutate( ModWeeklyMetMin = ( MinutesMod * DaysMod ) * 4 ) %>%
    mutate( BikeWalkWeeklyMetMin = ( MinutesBikeWalk * DaysBikeWalk ) * 4 ) %>%
    mutate( VigRecWeeklyMetMin = ( MinutesVigRec * DaysVigRec ) * 8 ) %>%
    mutate( ModRecWeeklyMetMin = ( MinutesModRec * DaysModRec ) * 4 ) %>%
    mutate( WeekMetMin = VigWeeklyMetMin + ModWeeklyMetMin +
             BikeWalkWeeklyMetMin + VigRecWeeklyMetMin + ModRecWeeklyMetMin ) %>%
    select( WeekMetMin, SEQN )
  
}


### for cycles 1999-2005
for( i in 1:length( PA9905 ) ){
  PA9905[[ i ]] <- PA9905[[ i ]] %>%
    mutate( MetMin30 = PADDURAT * PADMETS * PADTIMES ) %>%# met minutes in a month for a single activity
    mutate( WeekMetMinInd = MetMin30/4 ) %>%# average met minutes in a week fr a single activity
    group_by( SEQN ) %>%
    mutate( WeekMetMin = sum( WeekMetMinInd ) ) %>%
    select( SEQN, WeekMetMin ) %>%
    distinct( SEQN, WeekMetMin )
  
}


## people with no activity in NHANES 99-05 since they are not included in the PAQIAF
PQdat99 <- nhanes_load_data( 'PAQ', '1999-2000', demographics = FALSE )
PQdat01 <- nhanes_load_data( 'PAQ', '2001-2002', demographics = FALSE )
PQdat03 <- nhanes_load_data( 'PAQ', '2003-2004', demographics = FALSE )
PQdat05 <- nhanes_load_data( 'PAQ', '2005-2006', demographics = FALSE )#### PAQ Variable

PQ9905 <- list( PQdat99, PQdat01, PQdat03, PQdat05 )
for ( i in 1: length( PQ9905 ) ){
  PQ9905[[ i ]] <- PQ9905[[ i ]] %>%
    filter( ( PAD200 %in% c( 2, 3 ) ) & ( PAD320 %in% c( 2, 3 ) ) & ( PAD440 %in% c( 2, 3 ) ) ) %>%
    select( SEQN ) %>%
    mutate( WeekMetMin = 0 ) # give them 0 weekly met minutes 
}


mergall.pa <- bind_rows( do.call( 'rbind', PA9905 ),
          do.call( 'rbind', PQ9905 ),
          do.call( 'rbind', PA0717 ) )



##### MERGE and SAVE
# merge all individual datasets together and then, finally, merge with FPED data from previous R script
setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Wrangled' )

left_join( demoall, mcqall.bcd[ ,-which( colnames( mcqall.bcd ) == 'RIDAGEYR' ) ], by = 'SEQN' ) %>%
  left_join( ., smdall[ , c( 'SEQN', 'SmokStat' ) ], by = 'SEQN' ) %>%
  left_join( ., alqall[ , c( 'SEQN', 'ALCUSE' ) ], by = 'SEQN' ) %>%
  left_join( ., fsdall[ , c( 'SEQN', 'FoodSecCatHH', 'FoodSecCatAD', 'FoodAsstP', 
                                 'FoodAsstPnowic', 'BinFoodSecHH', 'BinFoodSecAD' ) ], by = 'SEQN' ) %>%
  left_join( ., bmiall, by = "SEQN" ) %>%
  left_join( ., nutrall, by = "SEQN" ) %>%
  left_join( ., cci_dat, by = "SEQN" ) %>%
  left_join( ., mergall.pa, by = "SEQN" ) %>%
  left_join( ., readRDS( '01-FPED-Wrangled.rds' ), by = 'SEQN' ) %>%
  # Create 18yr and 16yr weights using 2 days of dietary data ( WTDR2D weights )
  mutate( WTDR18YR = ifelse( Cycle %in% c( 1, 2 ), ( 2/10 ) * WTDR4YR,  # 1999-2017
                             ifelse( Cycle %in% c( 3:10 ), ( 8/10 ) * WTDR2D, NA ) ),
          WTDR16YR = ifelse( Cycle %in% c( 1, 2 ), ( 2/9 ) * WTDR4YR,  # 1999-2015
                             ifelse( Cycle %in% c( 3:9 ), ( 7/9 ) * WTDR2D, NA ) ),
          WTDR14YR = ifelse( Cycle %in% c( 1, 2 ), ( 2/8 ) * WTDR4YR, # 1999-2013
                             ifelse( Cycle %in% c( 3:8 ), ( 6/8 ) * WTDR2D, NA ) ),
          # setting `PMUFA:SFA` in those with zero intake in fat to 0 since dividing
          # by zero caused them to show up as NA incorrectly
          `PMUFA:SFA` = ifelse( is.na( `PMUFA:SFA` ) == TRUE & PFAT == 0
                                        & MFAT == 0 &SFAT == 0, 0, `PMUFA:SFA` ),
          # make alcohol intake categorical variable using dietary intake data
          alc_cat = factor( ifelse( ALCO < 0.1, 'non-drinking', 
                                            ifelse( ALCO < 28 & ALCO >= 0.1 & Gender == 'Male', 'moderate', 
                                                    ifelse( ALCO < 14 & ALCO >= 0.1 & Gender == 'Female', 'moderate', 
                                                            ifelse( ALCO >= 28  & Gender == 'Male', 'heavy', 
                                                                    ifelse( ALCO >= 14  & Gender == 'Female', 'heavy', NA ) ) ) ) ) ),
          # Create an indicator variable for cancer and food insecure subjects
          CAFS = ifelse( CA == 1 & BinFoodSecHH == 'Low' & Age >= 20, 1, 
                  ifelse( is.na( CA ) == TRUE | is.na( BinFoodSecHH ) == TRUE, NA, 
                          0 ) ) ) %>%
  # sort rows by age
   arrange( Age ) %>%
  saveRDS( ., '02-Covariates-Wrangled.rds' )

          
