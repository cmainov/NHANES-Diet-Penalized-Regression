###---------------------------------------------------
###   03-INCLUSIONS/EXCLUSIONS
###---------------------------------------------------

library( tidyverse )

setwd( '/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Wrangled' )

start.data <- readRDS( '02-Covariates-Wrangled.rds' )

####################################################################################################
nrow( start.data ) ######### start: 101316 observations ##############################################
####################################################################################################
####################################################################################################
######## See Figure 1 for detailed overview of the steps in the exclusion/inclusion process ######## 
####################################################################################################
####################################################################################################


####################################################################################################
############################ Exclusions for Dietary Patterns Extraction ############################
####################################################################################################



############## Step 1 ##############

## age >= 20 and response of "1" ( "yes" ) to the MCQ220 probe on cancer history ##
( ex.1 <- as.numeric( ( step1.data <- start.data %>%
  filter( Age >= 20 & CA == 1 ) ) %>%
  summarise( subjects.remaining = n( ) ) ) ) # 5166 subjects remain




############## Step 2 ##############

## exclude those without complete dietary data/missing weights from the 24-hr recall subsample ##

( step2a.data <- step1.data %>%
  filter( is.na( WTDR18YR ) == F & WTDR18YR != 0 ) ) %>%
  summarise( subjects.remaining = n( ) , subjects.excluded = ex.1 - n( ) )

# subjects.remaining subjects.excluded
#               3959              1207

ex.2a <- nrow( step2a.data )


## exclude those with a history of non-melanoma skin cancer as their only cancer diagnosis or ##
## outright missing cancer diagnosis data ##

( step2b.data <- step2a.data %>%
    filter( !( PrimaryCA == 'Non-Melanoma Skin' ) ) ) %>%
  summarise( subjects.remaining = n( ) , subjects.excluded = ex.2a - n( ) )

# subjects.remaining subjects.excluded
#               3382               577

ex.2b <- nrow( step2b.data )


## missing age at diagnosis of first cancer ##

( step2c.data <- step2b.data %>% 
  rowwise( ) %>%
  mutate( first.age = suppressWarnings( min( AgeDxA, AgeDxB, AgeDxC, na.rm = T ) ),
          first.age = ifelse( is.infinite( first.age ), NA, first.age ) ) %>%
  ungroup( ) %>%
  filter( is.na( first.age ) == F ) ) %>%
  data.frame( ) %>%
  summarise( subjects.remaining = n( ) , subjects.excluded = ex.2b - n( ) )

# subjects.remaining subjects.excluded
#               3378                 4

ex.2c <- nrow( step2c.data )




############## Step 3 ##############

## missing covariate data: food security status, SNAP benefits status, age, and household size ##


( step3.data <- step2c.data %>%
  filter( is.na( FoodAsstPnowic ) == F & 
           is.na( BinFoodSecHH ) == F &
           is.na( HHSize ) == F &
           is.na( Agecat ) == F ) %>%
    mutate( Diet.ext.ind.reg = 1,
            Diet.ext.ind.pca = ifelse( BinFoodSecHH == 'Low', 1, 0 ) ) %>%
    data.frame( ) ) %>%
  summarise( subjects.remaining = n( ) , subjects.excluded = ex.2c - n( ) )

# subjects.remaining subjects.excluded
#               3316                62



####################################################################################################
######################################## Final Sample Sizes ########################################
####################################################################################################

## Sample Size for Dietary Patterns Extraction with Penalized Regression and Validation with Logistic Regression ##

step3.data %>%
  count( Diet.ext.ind.reg )
# n = 3316

## Sample Size for Dietary Patterns Extraction with Principal Components Analysis ( PCA ) ##

step3.data %>%
  count( Diet.ext.ind.pca )
# n = 433


## final merge of indicator variables to original dataset with all rows possessing a dietary interview ##
## subsample weight and save data ##

start.data %>%
  filter( is.na( WTDR18YR ) == F ) %>%
  full_join( ., step3.data ) %>%
  saveRDS( ., '03-Inclusions-Exclusions.rds' )
