###---------------------------------------------------
###   05-VALIDATION: TABLES 1 AND 3
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will fit generate Table 1 (epidemiological characteristics of the study sample and 
# Table 3 (epidemiological characteristics by high and low fractions of the diet quality indices) for the manuscript.
# 
# INPUT DATA FILE: "03-Data-Rodeo/04-Analytic-Data.rds"
#
# OUTPUT FILES: "04-Manuscript/Tables/table-1.txt", "Manuscript/Tables/table-3.txt"
#
# Resources: see "utils.R" for functions used to generate tables 1 and 3
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( survey )


# read in helper functions
source( "R/utils.R" )


### Read in Data and Create Survey Design Objects for this Analysis ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

dat  <- readRDS( "03-Data-Rodeo/04-Analytic-Data.rds" ) %>%
  dplyr::filter( is.na( WTDR18YR ) == F ) %>% # subset to those having non-missing weights 
  dplyr::mutate( HHsize.bin = ifelse( HHSize >= 5, 1,
                                     ifelse( HHSize < 5, 0, NA ) ) )# dichotomize household size column before generating table

# designs
nhc <- svydesign( id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
               nest = TRUE, survey.lonely.psu = "adjust", data = dat )

# create three subsets, food insecure with cancer ( fiw ), food secure with cancer ( fsw ), and combined 
# food secure and insecure cancer survivors
fiw <- subset( nhc, BinFoodSecHH == "Low" & Diet.ext.ind.reg == 1 )
fsw <- subset( nhc, BinFoodSecHH == "High" & Diet.ext.ind.reg == 1 )
gen <- subset( nhc, Diet.ext.ind.reg == 1 )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------


### Table 1 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# write function to return table 1 with specific variables

cafs_table1 <- function( design, df ){
sex <- epitab( var = "Gender", data.fr = df, des = design, table.var = "Gender" )
alcuse <- epitab( var = "alc_cat", data.fr = df, des = design, table.var = "Alcohol Use" )
race <- epitab( var = "Race", data.fr = df, des = design, table.var = "Race/Ethnicity" )
smokstat <- epitab( var = "SmokStat", data.fr = df, des = design, table.var = "Smoking Status" )
income <- epitab( var = "fipr", data.fr = df, des = design, table.var = "Income:Poverty" )
hhsize <- epitab( var = "HHsize.bin", data.fr = df, des = design, table.var = "Household Size" )
education <- epitab( var = "Education_bin", data.fr = df, des = design, table.var = "Education Attained" )
bmi <- epitab.means( cont.var = "BMXBMI", des = design, table.var = "BMI" )
metmins <- epitab.means( cont.var = "WeekMetMin", des = design, table.var = "MET Minutes" )
calor <- epitab.means( cont.var = "KCAL", des = design, table.var = "Calories" )
age <- epitab.means( cont.var = "Age", des = design, table.var = "Age" )
cci <- epitab.means( cont.var = "CCI_Score", des = design, table.var = "CCI" )
site <- epitab( var = "PrimaryCAGroup", data.fr = df, des = design, table.var = "Cancer Site" )
snap <- epitab( var = "FoodAsstPnowic", data.fr = df, des = design, table.var = "SNAP Assistance" )
time <- epitab( var = "TimeCAFactor", data.fr = df, des = design, table.var = "Years Since Diagnosis" )


table1 <- rbind( age, sex, race, education, income, hhsize, bmi, metmins, calor, cci, snap, site, time, smokstat, alcuse )
return( table1 )
}



# generate table columns for each of the subsets described above
fiw.tab <- cafs_table1( design = fiw, df = dat )
fsw.tab <- cafs_table1( design = fsw, df = dat )
gen.tab <- cafs_table1( design = gen, df = dat )

# merge columns into table
final.tab <- cbind( gen.tab, fiw.tab, fsw.tab )


## add column for p value for t tests and chi square test ##

# vector of strings containing elements that singal to a given row in the table
chi  <- c( "Smoking", "Alcohol", "Gender", "Income", "Size", "Education", "Race",
          "Years", "Site", "SNAP" )
these  <- c( "SmokStat", "alc_cat", "Gender", "fipr", "HHsize.bin", "Education_bin",
            "Race", "TimeCAFactor", "PrimaryCAGroup", "FoodAsstPnowic" )

# chi square
for ( i in 1:length( chi ) ){
  
  final.tab[ which( str_detect( final.tab[ , 1 ], chi[i] ) ), 7 ] <- ifelse( svychisq( as.formula( paste0( "~BinFoodSecHH + ", these[i] ) ), design = gen )$p.value < 0.01, "< 0.01",
                                                                 round( svychisq( as.formula( paste0( "~BinFoodSecHH + ", these[i] ) ), design = gen )$p.value, digits = 2 ) )
  
}

tt  <- c( "Age", "BMI", "HHSize", "MET", "Calories", "CCI" )
these.b  <- c( "Age", "BMXBMI", "HHSize", "WeekMetMin", "KCAL", "CCI_Score" ) 


# t test
for ( i in 1:length( tt ) ){
  final.tab[ which( str_detect( final.tab[ , 1 ], tt[i] ) ), 7 ] <- ifelse( svyttest( as.formula( paste0( these.b[i], "~BinFoodSecHH" ) ), design = gen )$p.value < 0.01, "< 0.01",
                                                                round( svyttest( as.formula( paste0( these.b[i], "~BinFoodSecHH" ) ), design = gen )$p.value, digits = 2 ) )
  
}

# remove empty "( )" in table
final.tab[ final.tab == "  ( )" ] <- ""

# save
write.table( final.tab, "04-Manuscript/Tables/table-1.txt", sep = ", ", row.names = FALSE )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------




### Table 3 ###
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# categorize diet variables via a median split

cnames <- c( "FS_ENet", "Age_ENet", "FdAs_ENet", "HHS_ENet", "PC1", "PC2" )

( cut.names <- paste0( colnames( dat[ , cnames ] ), "_Q2" ) )

dat.b  <- dat%>%
         filter( Diet.ext.ind.reg == 1 )

# generate variables that cut at median ( use only data from subset we are working with and then merge back to larger dataset )
for ( i in 1:length( cnames ) ){
  dat.b[ , cut.names[i]] <- as.factor( quant_cut( var = cnames[i], df = dat.b, x = 2 ) )
  
}

# join back with full data
dat.c  <- full_join( dat.b, dat )


# design
t3  <- svydesign( id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
                 nest = TRUE, survey.lonely.psu = "adjust", data = dat.c )


# use `table_3` function to generate table ( utils.R )
tab3 <- list( )
for ( i in 1:length( cut.names ) ){
tab3[[i]] <- table_3( cut.names[i], design = t3, df = dat.c )
}

# clean-up table aesthetically
tab3df <- do.call( "cbind", tab3 )
tab3df <- as.matrix( tab3df[ , -c( 4, 7 ) ] )
tab3df <- tab3df[ , -c( 8, 11, 14 ) ]

tab3df[ tab3df == "  ( )" ] <- ""
tab3df[ tab3df == "  ( )*" ] <- "*"
tab3df[ tab3df == "  ( )**" ] <- "**"

# further text process the table
tab3df <- data.frame( tab3df )
tab3df <- sapply( tab3df, function(x) str_replace_all( x, "(?<=\\d\\d)(\\))",".0)" ))

# quantify quantiles
table( dat.b$PC1_Q )
table( dat.b$PC2_Q )
table( dat.b$FS_ENet_Q2 )
table( dat.b$Age_ENet_Q2 )
table( dat.b$FdAs_ENet_Q2 )
table( dat.b$HHS_ENet_Q2 )

# save table
write.table( tab3df, "Manuscript/Tables/table-3.txt", sep = ", ", row.names = FALSE )

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
