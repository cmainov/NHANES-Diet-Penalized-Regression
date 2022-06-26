###---------------------------------------------------
###   04-DIETARY PATTERNS EXTRACTION
###---------------------------------------------------

library( tidyverse )
library( survey )
library( glmnet )
library( caret )
library( jtools )
library( weights )
library( latex2exp) # to add LaTeX to plots

setwd( "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Wrangled" )

xdata <- readRDS( "03-Inclusions-Exclusions.rds" )


# collapse red mt and organ mt to same group give very low intake of organ mts
xdata$Meat <- xdata$RedMts + xdata$OrganMts

# copy
x.data <- xdata

####################################################################################################
############################ Dietary Patterns Extraction: Elastic Net ##############################
####################################################################################################



# function
enet_pat <- function( xmat, yvec, wts, plot.title ){

  colorss <- c( "black", "red", "green3", "navyblue",   "cyan",   "magenta", "gold", "gray",
             'pink', 'brown', 'goldenrod' ) # colors for plot
  
  # initialize lists to store outputs
  store <- list( )
  coefsdt <- list( )
  
  alpha.grid <- seq( 0, 1, 0.1 ) # range of alpha values for tuning grid
  
  for ( i in 1:length( alpha.grid ) ){ # set the grid of alpha values
    
    set.seed( 28 ) # seed to reproduce results
    
    # call glmnet with 10-fold cv
    enetr <- cv.glmnet( x = xmat, y = yvec, family ='binomial', weights = wts,
                     nfold = 10, alpha = alpha.grid[ i ] )
    
    # bind values of lambda to cross validation error
    evm <- data.frame( cbind( enetr$lambda, enetr$cvm ) )
    colnames( evm ) <- c( 'lambda', 'av.error' )
    
    # now create a list that stores each coefficients matrix for each value of alpha
    # at the lambda minimizer
    coefsdt[[ i ]] <- list( alpha = paste0( alpha.grid )[ i ], 
                            coefs = coef( enetr, s = "lambda.min" ) )
    
    # create a dataframe that houses the alpha, min lambda, and average error
    resdf <- data.frame( alpha = alpha.grid[ i ], 
                         evm[ which( evm$av.error == min( evm$av.error ) ), 'lambda' ],
                         av.error = evm[ which( evm$av.error == min( evm$av.error ) ), 'av.error' ] )
    colnames( resdf ) <- c( 'alpha', 'lambda', 'av.error' )
      
    store[[ i ]] <- resdf
    
    ###### generate plot ###### 
    
    if ( i == 1 ){ # for the first value of 'i'
      plot( x = enetr$lambda, y = enetr$cvm, type ='l', 
            ylim = c( min( enetr$cvm )-0.02, max( enetr$cvm )-0.02 ),
           xlim = c( min( evm$lambda ), ( resdf$lambda*1.05 ) ), 
           las = 0, 
           cex.axis = 0.7 )
    }
    else if ( i != 1 ){ # each additional line will be superimposed on the plot with a different color
      lines( x = enetr$lambda, 
             y = enetr$cvm, 
             col = colorss[ i ] )
    }
  }
  
  ###### superimpose intersecting lines at the minimizer ###### 
   cverr <- do.call( 'rbind', store ) # this gives the table of errors for each combination of alpha and lambda
  abline( h = cverr[ which( cverr$av.error == min( cverr$av.error ) ), 'av.error' ],
         lty = 2 )
  abline( v = cverr[ which( cverr$av.error == min( cverr$av.error ) ), 'lambda' ],
         lty = 2 )
  
  
  ###### add optimal lambda and alpha values to plot title ###### 
  optimall <- cverr[ which( cverr$av.error == min( cverr$av.error ) ), ] # here I extract the optimal combination of
  # lambda and alpha
  optlam <- signif( optimall[ 2 ], 2 )
  opta <- optimall[ 1 ]
  title( main = TeX( paste0( plot.title, ' ( $\\lambda_{optimal} =$', optlam, ' and $\\alpha_{optimal} =$', opta, ' )' ) ),
         cex.main = 0.8,
         cex.lab = 0.8,
         xlab = TeX( '$\\lambda$' ), 
         mgp = c( 2, 1, 0 ),
         ylab ='Deviance', mgp = c( 2, 1, 0 ) )
  
  
  
  # the function returns the optimal lambda alpha combo and the set of coefficients that 
  # correspond to that combination of parameters
  return( list( optimall, coefs = as.matrix( coefsdt[[ which( alpha.grid == optimall$alpha ) ]]$coefs )[ -1, ] ) )
}
#### END FUNCTION #####



#### SUBSET DATA MATRIX FOR ELASTIC NET PROCEDURE
caonly <- x.data[ which( x.data$Diet.ext.ind.reg == 1 ), ]

# set categories to numerical for glmnet model

caonly <- caonly %>%
  mutate( FoodAsstPnowic = ifelse( FoodAsstPnowic =='yes', 1,
                               ifelse( FoodAsstPnowic =='no', 0, NA ) ) ) %>%
  mutate( Agecat = ifelse( Agecat =='elderly', 1,
                       ifelse( Agecat =='non-elderly', 0, NA ) ) ) %>%
  mutate( BinFSH = ifelse( BinFoodSecHH =='Low', 1,
                       ifelse( BinFoodSecHH =='High', 0, NA ) ) ) %>%
  mutate( HHSize_bin = ifelse( HHSize>= 5, 1,
                           ifelse( HHSize<5, 0, NA ) ) )


fdgrp.columns <- which( colnames( caonly ) %in% c( 'ProcessedMts', 'Meat', 'Poultry', 'Fish_Hi', 'Fish_Lo',
                                             'Eggs', 'SolidFats', 'Oils', 'Milk', 'Yogurt', 'Cheese',
                                             'Alcohol', 'FruitOther', 'F_CitMelBer', 'Tomatoes',
                                             'GreenLeafy',
                                             'DarkYlVeg', 'OtherVeg',
                                             'Potatoes', 'OtherStarchyVeg',
                                             'Legumes', 'Soy', 'RefinedGrain', 'WholeGrain', 'Nuts',
                                             'AddedSugars' ) )

fdgrp.columns <- fdgrp.columns[ c( 1, 26, 2:25 ) ] # re-arrange so that Meat column index is second column index
fs.outcome.column <- which( colnames( caonly ) =='BinFSH' )
fdas.outcome.column <- which( colnames( caonly ) =='FoodAsstPnowic' )
age.outcome.column <- which( colnames( caonly ) =='Agecat' )
hhsize.outcome.column <- which( colnames( caonly ) =='HHSize_bin' )
weight.column <- which( colnames( caonly ) =='WTDR18YR' )
kcal.column <- which( colnames( caonly ) =='KCAL' )
seqn.column <- which( colnames( caonly ) =='SEQN' )


### ADJUSTMENT FOR TOTAL ENERGY INTAKE PRIOR TO EXTRACTION ###
# divide by total energy intake for multivariate density approach to energy adjustment
for ( j in fdgrp.columns ){# ensure proper variables are indicated by the column index in this line of code before proceeding
  caonly[ , j ] <- caonly[ , j ] / caonly[ , kcal.column ]
}

# center and scale food group variables before regressions
for ( j in fdgrp.columns ){# ensure proper variables are indicated by the column index in this line of code before proceeding
  caonly[ , j ] <- ( caonly[ , j ]-mean( caonly[ , j ], na.rm = T ) ) / sd( caonly[ , j ], na.rm = T )
}



##############################################################################
#################### Run Function for Patterns Extraction #################### 
##############################################################################

# Save plot figure
setwd( "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Manuscript/Figures" )
pdf( "optimal-lambdas-enet-patterns.pdf" )

# Food insecurity binary outcome
par( mfrow = c( 2, 2 ), mar = c( 3, 3, 2, 1 ) )
mat <- na.omit( as.matrix( caonly[ , c( fdgrp.columns, fs.outcome.column, weight.column ) ] ) )
xmat <- mat[ , 1:26 ] # food grps
yvec <- mat[ , 27 ] # binary response
xwts <- mat[ , 28 ] / mean( mat[ , 28 ] ) # normalize weights

fsoc <- enet_pat( xmat, yvec, xwts, plot.title ='Food Insecurity' ) 

colorss <- c( "black", "red", "green3", "navyblue",   "cyan",   "magenta", "gold", "gray",
           'pink', 'brown', 'goldenrod' )
legend( "topright", # Add legend to plot
       legend = c( paste( seq( 0, 1, by = 0.1 ) ), 'Minimizer' ), col = c( colorss, 'grey' ),
       lty = c( rep( 1, 11 ), 2 ), title = TeX( '$\\alpha$' ),
       cex = 0.45, inset = 0, y.intersp = 0.5 )


# Age outcome
mat <- na.omit( as.matrix( caonly[ , c( fdgrp.columns, age.outcome.column, weight.column ) ] ) )
xmat <- mat[ , 1:26 ] # food grps
yvec <- mat[ , 27 ] # binary response
xwts <- mat[ , 28 ] / mean( mat[ , 28 ] ) # normalize weights
ageoc <- enet_pat( xmat, yvec, xwts, plot.title = 'Age' )



# Reciept of food assistance outcome
mat <- na.omit( as.matrix( caonly[ , c( fdgrp.columns, fdas.outcome.column, weight.column ) ] ) )
xmat <- mat[ , 1:26 ] # food grps
yvec <- mat[ , 27 ] # binary response
xwts <- mat[ , 28 ] / mean( mat[ , 28 ] ) # normalize weights
fdasoc <- enet_pat( xmat, yvec, xwts, plot.title = 'Food Assistance ( SNAP )' )


# HHSize outcome
mat <- na.omit( as.matrix( caonly[ , c( fdgrp.columns, hhsize.outcome.column, weight.column ) ] ) )
xmat <- mat[ , 1:26 ] # food grps
yvec <- mat[ , 27 ] # binary response
xwts <- mat[ , 28 ] / mean( mat[ , 28 ] ) # normalize weights
hhssoc <- enet_pat( xmat, yvec, xwts, plot.title ='Household Size' )

# save plot
dev.off( )



#################### Generate Pattern Scores in the Data #################### 

# generate scores on data
xmatrix <- as.matrix( x.data[ which( x.data$Diet.ext.ind.reg == 1 ), c( fdgrp.columns, kcal.column ) ] ) # subset as a matrix
d <- x.data[ which( x.data$Diet.ext.ind.reg == 1 ), ] # keep only those satisfying inclusions/exclusions

# divide fd grps by kcal
for ( i in 1:( ncol( xmatrix )-1 ) ){
  xmatrix[ , i ] <- ( xmatrix[ , i ] / xmatrix[ , ncol( xmatrix ) ] )
}

# remove kcal column
xmatrix <- xmatrix[ , -ncol( xmatrix ) ]

# center and scale testing data before generating scores
for ( i in 1:ncol( xmatrix ) ){
  xmatrix[ , i ] <- ( xmatrix[ , i ] - mean( xmatrix[ , i ], na.rm = T ) ) / sd( xmatrix[ , i ], na.rm = T )
}



d$FS_ENet <- t( fsoc$coefs %*% t( xmatrix ) )
d$Age_ENet <- t( ageoc$coefs %*% t( xmatrix ) )
d$FdAs_ENet <- t( fdasoc$coefs %*% t( xmatrix ) )
d$HHS_ENet <- t( hhssoc$coefs %*% t( xmatrix ) )



# Save loading matrices

cfenet <- round( data.frame( fs = fsoc$coefs,
                         age = ageoc$coefs,
                         fdas = fdasoc$coefs,
                         hhs = hhssoc$coefs ), digits = 2 )


setwd( "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Manuscript/Tables" )
write.table( cfenet, "Enet-factor-loadings.txt", sep =", ", row.names = FALSE )




##############################################################################
######################## Patterns Extraction with PCA ########################
##############################################################################


# ensure all rows have a sample weight present before using `survey` functions
pca.data <- x.data[ !is.na( x.data$WTDR18YR ) == T, ] 

# divide by total energy intake for multivariate density approach
for ( j in fdgrp.columns ){# ensure proper variables are indicated by the column index in this line of code before proceeding
  pca.data[ , j ] <- pca.data[ , j ] / pca.data[ , kcal.column ]
}

# to have weights and no missing weights

pca.design <- svydesign( id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
                       nest = TRUE, survey.lonely.psu = "adjust", data = pca.data )

varest <- subset( pca.design, Diet.ext.ind.pca == 1 ) # inclusions


# PCA using svyprcomp
pcaobj <- svyprcomp( ~ ProcessedMts + Meat + Poultry + Fish_Hi + Fish_Lo + Eggs + SolidFats + Oils + 
                    Milk + Yogurt + Cheese + Alcohol + FruitOther + F_CitMelBer + Tomatoes + GreenLeafy + 
                    DarkYlVeg + OtherVeg + Potatoes + OtherStarchyVeg + Legumes + Soy + RefinedGrain + 
                    WholeGrain + Nuts + AddedSugars, design = varest, center = T,
                    scale = T, 
                    scores = TRUE )

# scree plot
# eigenvalues/Scree plot
plot( pcaobj$sdev, type = 'b',
     main = 'Scree Plot for Diet Patterns PCA', xlab = 'Component', ylab = 'Eigenvalue' )
abline( h = 1, lty = 2 )
# elbow present after the second principal component

# assign factor loading matrix
coefspc <- pcaobj$rotation

# save raw loading matrix
write.table( round( coefspc[ , 1:2 ], digits = 2 ), "PCA-Factorloadings.txt", sep =", ", row.names = FALSE )


sum( pcaobj$sdev[ 1:2 ] / sum( pcaobj$sdev ) ) # percent of variation accounted for by first two components ( 0.1412 )

# Generate Scores
xmatrix <- as.matrix( x.data[ which( x.data$Diet.ext.ind.reg == 1 ), c( fdgrp.columns, kcal.column ) ] )

# divide fd grps by kcal
for ( i in 1:( ncol( xmatrix ) - 1 ) ){
  xmatrix[ , i ] <- ( xmatrix[ , i ] / xmatrix[ , ncol( xmatrix ) ] )
}

# remove kcal column
xmatrix <- xmatrix[ , -ncol( xmatrix ) ]

# center and scale testing data before generating scores
for ( i in 1:ncol( xmatrix ) ){
  xmatrix[ , i ] <- ( xmatrix[ , i ] - mean( xmatrix[ , i ], na.rm = T ) ) / sd( xmatrix[ , i ], na.rm = T )
}

d$PC1 <- t( coefspc[ , 1 ] %*% t( xmatrix ) )
d$PC2 <- t( coefspc[ , 2 ] %*% t( xmatrix ) )

## Add to original data and save

setwd( "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Data-Rodeo" )

( x.data3 <- left_join( xdata, d[ , c( "SEQN", "FS_ENet", "Age_ENet",
                                      "FdAs_ENet", "HHS_ENet", "PC1", "PC2" ) ] ) ) %>%
  saveRDS( ., "04-Analytic-Data.rds" )



##############################################################################
################## Pearson Correlation Matrix (Table 3) PCA ##################
##############################################################################


# subset individuals meeting criteria
cordata <- x.data3[ which( x.data3$Diet.ext.ind.reg == 1 ), ]

# food group column indices
fdgrp.columns <- which( colnames( cordata ) %in% c( "ProcessedMts", "Meat", "Poultry", "Fish_Hi", "Fish_Lo",
                                             "Eggs", "SolidFats", "Oils", "Milk", "Yogurt", "Cheese",
                                             "Alcohol", "FruitOther", "F_CitMelBer", "Tomatoes",
                                             "GreenLeafy",
                                             "DarkYlVeg", "OtherVeg",
                                             "Potatoes", "OtherStarchyVeg",
                                             "Legumes", "Soy", "RefinedGrain", "WholeGrain", "Nuts",
                                             "AddedSugars" ) )

fdgrp.columns <- fdgrp.columns[ c( 1, 26, 2:25 ) ] # re-arrange so that Meat column index is second column index

### ADJUSTMENT FOR TOTAL ENERGY ###
# divide by total energy intake for multivariate density approach to energy adjustment
for ( j in fdgrp.columns ){# ensure proper variables are indicated by the column index in this line of code before proceeding
  cordata[ , j ] <- cordata[ , j ] / cordata[ , kcal.column ]
}

# center and scale food group variables before correlation analysis
for ( j in fdgrp.columns ){# ensure proper variables are indicated by the column index in this line of code before proceeding
  cordata[ , j ] <- ( cordata[ , j ] - mean( cordata[ , j ], na.rm = T ) ) / sd( cordata[ , j ], na.rm = T )
}

# re-join adjusted, centered, and scaled variables to original data
cordat2 <- left_join( x.data3[ , -fdgrp.columns ], cordata[ , c( 1, fdgrp.columns ) ] )

# subset since svy procedures require all rows in data
# to have weights and no missing weights
mod.data <- cordat2[ !is.na( cordat2$WTDR18YR ) == T, ]
mod1 <- svydesign( id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
                nest = TRUE, survey.lonely.psu = "adjust", data = mod.data )
mod1 <- subset( mod1, Diet.ext.ind.reg == 1 )# inclusions



fdgrp.diet.names <- c( "ProcessedMts", "Meat", "Poultry", "Fish_Hi", "Fish_Lo",
         "Eggs", "SolidFats", "Oils", "Milk", "Yogurt", "Cheese",
         "Alcohol", "FruitOther", "F_CitMelBer", "Tomatoes",
         "GreenLeafy",
         "DarkYlVeg", "OtherVeg",
         "Potatoes", "OtherStarchyVeg",
         "Legumes", "Soy", "RefinedGrain", "WholeGrain", "Nuts",
         "AddedSugars", "FS_ENet", "Age_ENet",
         "FdAs_ENet", "HHS_ENet", "PC1", "PC2" )

diet.patt.names <- c( "FS_ENet", "Age_ENet",
                   "FdAs_ENet", "HHS_ENet", "PC1", "PC2" )

## Loop to generate correlation matrix using svycor function

# Correlation Coefficients
corr.matrix <- matrix( NA, ncol = length( diet.patt.names ), nrow = length( fdgrp.diet.names ) )

for ( g in 1:length( diet.patt.names ) ){

  loadings.vector <- vector( )

for ( i in 1:length( fdgrp.diet.names ) ){
  loadings.vector[ i ] <- round( svycor( as.formula( paste0( "~", fdgrp.diet.names[ i ], "+", diet.patt.names[ g ] ) ), design = mod1 )$cors[ 2 ], digits = 2 )
}
corr.matrix[ , g ] <- loadings.vector
}


## Text process correlation matrix
# Assign column names and rownames to matrix

colnames( corr.matrix ) <- diet.patt.names
rownames( corr.matrix ) <- fdgrp.diet.names

# Significant digits and trailing zeros:
corr.matrix.b <- print( formatC( corr.matrix, digits = 2, format ="fg", flag ="#" ) )

# Replace instances of "0" with "0.00"
corr.matrix.b[ corr.matrix.b =="0" ] <- "0.00"

# Eliminate excess trailing zeros
for ( i in 1:ncol( corr.matrix.b ) ){
  
  corr.matrix.b[ , i ] <- str_replace( corr.matrix.b[ , i ], "( ?<=\\.\\d\\d )0", "" )
  
}

# Replace NA"s with "--"
corr.matrix.b[ corr.matrix.b ==" NA" ] <- "--"

setwd( "/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES-Diet-Penalized-Regression/Manuscript/Tables" )
write.table( corr.matrix.b, "corr_matrix.txt", sep =",", row.names = FALSE )

