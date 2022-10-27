###---------------------------------------------------
###   06-VALIDATION: BINARY LOGISTIC REGRESSION
###---------------------------------------------------

library( tidyverse )
library( survey )
library( ggpubr )

dat <- readRDS( "Data-Rodeo/04-Analytic-Data.rds" ) %>%
  dplyr::filter( is.na( WTDR18YR ) == F )

# Import result-generating functions
source( "R/utils.R" ) 

# covariates to adjust for in model
covars.logit <- c( 'Race_binary', 'Gender', 'Age', 'KCAL', 'BMXBMI', 'HHSize', 'FoodAsstPnowic',
                'SmokStat', 'fipr', 'KCAL', 'WeekMetMin', 'Education_bin',
                'PrimaryCAGroup', 'CCI_Score' )

## tabulate results using function and save table ##

m <- results_function( df = dat,
                 covariates = covars.logit, y = 'BinFoodSecHH',
                 variables = c( 'FS_ENet', 'Age_ENet', 'FdAs_ENet', 'HHS_ENet', 'PC1', 'PC2' ) ,
                 cuts = 5,
                 subset.condition = 'Diet.ext.ind.reg == 1 & Gender == "Female"' )  %>%
  write.table( ., "Manuscript/Tables/logit-results.txt", sep = ", ", row.names = FALSE ) 


## stratified models ##

# sex

this.s <- levels( factor( dat$Gender ) ) 

out.s <- data.frame( ) # initialize data.frame to hold results
for ( i in 1:length( this.s )  ){
  out.s <- rbind( out.s, data.frame( results_function( df = dat, covariates = covars.logit,
                  variables = c( 'FS_ENet', 'Age_ENet', 'FdAs_ENet', 'HHS_ENet', 'PC1', 'PC2' ) ,
                  cuts = 5, y = "BinFoodSecHH",
                  subset.condition = glue::glue( "Diet.ext.ind.reg == 1 & Gender == '{this.s[i]}' " ) ) ) )

}

# time since first CA diagnosis

this.t <- levels( factor( dat$TimeCAFactor ) ) 

out.t <- data.frame( ) # initialize data.frame to hold results
for ( i in 1:length( this.t )  ){
  out.t <- rbind( out.t, data.frame( results_function( df = dat, covariates = covars.logit,
                                                variables = c( 'FS_ENet', 'Age_ENet', 'FdAs_ENet', 'HHS_ENet', 'PC1', 'PC2' ) ,
                                                cuts = 5, y = "BinFoodSecHH",
                                                subset.condition = glue::glue( "Diet.ext.ind.reg == 1 & TimeCAFactor == '{this.t[i]}' " ) ) ) )
  
}

# education

this.e <- levels( factor( dat$Education_bin ) ) 

out.e <- data.frame( ) # initialize data.frame to hold results
for ( i in 1:length( this.e )  ){
  out.e <- rbind( out.e, data.frame( results_function( df = dat, covariates = covars.logit,
                                                variables = c( 'FS_ENet', 'Age_ENet', 'FdAs_ENet', 'HHS_ENet', 'PC1', 'PC2' ) ,
                                                cuts = 5, y = "BinFoodSecHH",
                                                subset.condition = glue::glue( "Diet.ext.ind.reg == 1 & Education_bin == '{this.e[i]}' " ) ) ) )
  
}


rbind( out.s, out.t, out.e ) %>%
  write.table( ., "Manuscript/Tables/stratified-results.txt", sep = ", ", row.names = FALSE ) 



## Restricted-Cubic Spline Curves for Logistic Regression Models ##

dat.sub <- dat %>%
  dplyr::filter( Diet.ext.ind.reg == 1 )  %>% # use data inclusions/exclusions subset for this section
  dplyr::mutate( wts.norm = WTDR18YR / mean( WTDR18YR )  )  # create normalized weights

dat.sub$PrimaryCAGroup <- droplevels( dat.sub$PrimaryCAGroup )  # drop levels with zero observations  ( NMSC ) 



patterns <- c( "FS_ENet", 'Age_ENet', 'FdAs_ENet', 'HHS_ENet', 'PC1', 'PC2' ) 
pattern.labels <- paste0( c( "FI", "Age", "SNAP", "Household Size", "Modified Western", "Prudent" ) , " Pattern Score" ) 

p <- list( ) # initialize list to store plots
for ( i in 1:length( patterns )  ) {
  
  if ( i == 1 ) {
    p[[i]] <- logit_splines( df = dat.sub, x = patterns[i], y = 'BinFoodSecHH', knots = 5, covariates = covars.logit, 
                  wts = 'WTDR18YR', referent = 'median', ylab = "Odds Ratio ( Food Insecure ) ", xlab = pattern.labels[i],
                  legend.pos = c( 0.34, 0.8 )  ) 
    }

    else if ( i == 4 ) {
  p[[i]] <- logit_splines( df = dat.sub, x = patterns[i], y = 'BinFoodSecHH', knots = 5, covariates = covars.logit, 
                wts = 'WTDR18YR', referent = 'median', ylab = "Odds Ratio ( Food Insecure ) ", xlab = pattern.labels[i],
                legend.pos = "none" ) 
    }
  
  else {
    p[[i]] <- logit_splines( df = dat.sub, x = patterns[i], y = 'BinFoodSecHH', knots = 5, covariates = covars.logit, 
                            wts = 'WTDR18YR', referent = 'median', ylab = NULL, xlab = pattern.labels[i],
                            legend.pos = "none" ) 
  }
  
}

do.call( "ggarrange", p ) 

# save
ggsave( "Manuscript/Figures/logit-splines.jpeg", width = 30, height = 20, units = "cm" ) 


