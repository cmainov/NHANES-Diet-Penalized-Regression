###---------------------------------------------------
###   RESULT-GENERATING AND OTHER HELPER FUNCTIONS
###---------------------------------------------------


########## %notin% operator #########
`%notin%` <- Negate( `%in%` )

####################################################################################################
#################################### Quantile Cutting Function #####################################
####################################################################################################

quant_cut<-function(var,x,df){
  
  xvec<-vector() # initialize null vector to store
  
  for (i in 1:x){
    xvec[i]<-i/x
  }
  
  qs<-c(min(df[[var]],na.rm=T), quantile(df[[var]],xvec,na.rm=T))
  
  df[['new']]=x+1 # initialize variable
  
  for (i in 1:(x)){
    df[['new']]<-ifelse(df[[var]]<qs[i+1] & df[[var]]>=qs[i],
                        c(1:length(qs))[i],
                        ifelse(df[[var]]==qs[qs==max(qs)],x,df[['new']]))
  }
  
  return(df[['new']])
}


####################################################################################################
#################################### Trend Variable Function #######################################
####################################################################################################

trend_func<-function(rank.var,cont.var,df,trend.var,x){
  
  df[[trend.var]] = 1
  
  medians<-vector()
  
  for (i in 1:x){
    
    newdf<-df[df[[rank.var]]==i,]
    
    medians[i]<-median(newdf[[cont.var]],na.rm=T)
    
    df[[trend.var]]<-ifelse(df[[rank.var]]==i,medians[i],df[[trend.var]])
    
  }
  
  return(df)
}

####################################################################################################
########################## Model Fitting and Results-Tabulating Function ###########################
####################################################################################################

results_function<-function(df, covariates, variables, y, cuts, subset.condition=NULL,... ) {
  
  start_time <- Sys.time( )# document start times
  
  import::from(magrittr, '%>%')

  
  # index names and naming index quantile variables to be introduced into data
  var.names<-variables
  var.names.q<-paste0(var.names,'_q') # create variable names for new cut variables, pasting '_q' at end of names
  var.names.trend<-paste0(var.names,'_trend') # create variable names for new cut variables, pasting '_q' at end of names
  
  # subset analytical sample to calculate quantiles and trend variables
  
  if (is.null( subset.condition )){
    
    df.sub <- df
    
  }
  
  else if (is.null( subset.condition )==F){
    
    attach(df) # attach to forgo use of the dollar sign operator
    df.sub<-df[which(eval(parse(text=paste0(subset.condition)))),] 
    
    # remove any variables we are subsetting on from the vector of covariates
    covariates <- covariates[ which( covariates %notin% unlist( str_split(subset.condition,"\\s") ) ) ]
    
  }
  
  # create quantile, trend, standardized, and transformed (square) variables on data subset to use in regressions
  for(i in 1:length(variables)){
    
    df.sub[,var.names.q[i]]<-quant_cut(var = var.names[i],
                                       x=cuts,df=df.sub) # cut
    
    df.sub<-trend_func(rank.var=var.names.q[i],
                       cont.var=var.names[i],df=df.sub, trend.var=var.names.trend[i],x=cuts) # trend
    
    df.sub[,paste0(var.names[i],'_sc')]<- df.sub[,var.names[i]] / sd( df.sub[,var.names[i]], na.rm=T ) # standardized
    
    df.sub[,paste0(var.names[i],'_sc2')]<- ( df.sub[,var.names[i]] / sd( df.sub[,var.names[i]], na.rm=T ) )^2 # squared
    
  }
  
  # join with input dataset
  df.new <- suppressMessages( left_join( df, df.sub ) )
  
  design<-survey::svydesign(id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
                            nest = TRUE, survey.lonely.psu = "adjust", data = df.new)
  
  if(is.null(subset.condition)==F){
    design<-subset(design, eval(parse(text=(subset.condition)))) # inclusions/exclusions
  }
  
  # fit models
  model.list<- list() # empty list to store results
  for(i in 1:length(var.names)){
    
    modelfull<-survey::svyglm(formula(paste0( y, '~factor(',var.names[i],'_q)+',
                                             paste0(covariates,collapse = '+'))),
                              design=design,
                              family = 'quasibinomial')
    modelfulla_T<-survey::svyglm(formula(paste0( y, '~',var.names[i],'_trend+',
                                                paste0(covariates,collapse = '+'))),
                                 design=design,
                                 family = 'quasibinomial')
    modelfulla_L<-survey::svyglm(formula(paste0( y, '~',var.names[i],'_sc+',
                                                paste0(covariates,collapse = '+'))),
                                 design=design,
                                 family = 'quasibinomial')
    modelfulla_L2<-survey::svyglm(formula(paste0( y, '~',var.names[i],'_sc2+',
                                                 paste0(covariates,collapse = '+'))),
                                  design=design,
                                  family = 'quasibinomial')
    
    
    
    xsum1<-round(exp(summary(modelfull)$coefficients),digits=2)
    xsum1nr<-summary(modelfull)$coefficients # not rounded to get correct asterisks in table
    xsci<-round(exp(confint(modelfull)),digits=2)
    xtrend<-round(summary(modelfulla_T)$coefficients,digits=2)
    xlin<-round(exp(summary(modelfulla_L)$coefficients),digits=2)
    xlinci<-round(exp(confint(modelfulla_L)),digits=2)
    xlin2<-round(summary(modelfulla_L2)$coefficients,digits=2)
    
    if (is.null(subset.condition)==F) {
      tab.row<-data.frame(patt=stringr::str_remove_all(stringr::str_remove_all(rownames(xsum1)[2],'factor\\('),'_q\\)\\d'),
                          subset=subset.condition,
                          Q1=1,Q2=paste0(xsum1[2,1],' (',xsci[2,1],'-',xsci[2,2],')'),
                          Q3=paste0(xsum1[3,1],' (',xsci[3,1],'-',xsci[3,2],')'),
                          Q4=paste0(xsum1[4,1],' (',xsci[4,1],'-',xsci[4,2],')'),
                          Q5=paste0(xsum1[5,1],' (',xsci[5,1],'-',xsci[5,2],')'),
                          pq5=round(xsum1nr[5,4],digits=2),ptrend=xtrend[2,4],
                          lin=paste0(xlin[2,1],' (',xlinci[2,1],'-',xlinci[2,2],')'),
                          quadp=xlin2[2,4])
    }
    
    else if (is.null(subset.condition)) {
      tab.row<-data.frame(patt=stringr::str_remove_all(stringr::str_remove_all(rownames(xsum1)[2],'factor\\('),'_q\\)\\d'),
                          Q1=1,Q2=paste0(xsum1[2,1],' (',xsci[2,1],'-',xsci[2,2],')'),
                          Q3=paste0(xsum1[3,1],' (',xsci[3,1],'-',xsci[3,2],')'),
                          Q4=paste0(xsum1[4,1],' (',xsci[4,1],'-',xsci[4,2],')'),
                          Q5=paste0(xsum1[5,1],' (',xsci[5,1],'-',xsci[5,2],')'),
                          pq5=round(xsum1nr[5,4],digits=2),ptrend=xtrend[2,4],
                          lin=paste0(xlin[2,1],' (',xlinci[2,1],'-',xlinci[2,2],')'),
                          quadp=xlin2[2,4])
    }
    
    tab.row<-tab.row%>%
      mutate(Q2=ifelse(xsum1nr[2,4]<0.05 & xsum1nr[2,4]>=0.01,stringr::str_replace(Q2,'$','*'),
                       ifelse(xsum1nr[2,4]<0.01,stringr::str_replace(Q2,'$','**'),Q2)))%>%
      mutate(Q3=ifelse(xsum1nr[3,4]<0.05 & xsum1nr[3,4]>=0.01,stringr::str_replace(Q3,'$','*'),
                       ifelse(xsum1nr[3,4]<0.01,stringr::str_replace(Q3,'$','**'),Q3)))%>%
      mutate(Q4=ifelse(xsum1nr[4,4]<0.05 & xsum1nr[4,4]>=0.01,stringr::str_replace(Q4,'$','*'),
                       ifelse(xsum1nr[4,4]<0.01,stringr::str_replace(Q4,'$','**'),Q4)))%>%
      mutate(Q5=ifelse(xsum1nr[5,4]<0.05 & xsum1nr[5,4]>=0.01,stringr::str_replace(Q5,'$','*'),
                       ifelse(xsum1nr[5,4]<0.01,stringr::str_replace(Q5,'$','**'),Q5)))%>%
      mutate(pq5=ifelse(xsum1nr[5,4]>=0.05,pq5,
                        ifelse(xsum1nr[5,4]<0.05 & xsum1nr[5,4]>=0.01,stringr::str_replace(pq5,'$','*'),
                               ifelse(xsum1nr[5,4]<0.01,'<0.01**',`pq5`))))%>%
      mutate(lin=ifelse(summary(modelfulla_L)$coefficients[2,4]<0.05 & summary(modelfulla_L)$coefficients[2,4]>=0.01,stringr::str_replace(lin,'$','*'),
                        ifelse(summary(modelfulla_L)$coefficients[2,4]<0.01,stringr::str_replace(lin,'$','**'),lin)))%>%
      mutate(ptrend=(ifelse(xtrend[2,4]>=0.05,ptrend,
                            ifelse(xtrend[2,4]<0.05 & xtrend[2,4]>=0.01,stringr::str_replace(ptrend,'$','*'),
                                   ifelse(xtrend[2,4]<0.01,'<0.01**',ptrend)))))%>%
      mutate(quadp=(ifelse(xlin2[2,4]>=0.05,quadp,
                           ifelse(xlin2[2,4]<0.05 & xlin2[2,4]>=0.01,stringr::str_replace(quadp,'$','*'),
                                  ifelse(xlin2[2,4]<0.01,'<0.01**',quadp)))))
    
    model.list[[i]]<-modelfull
    
    if( i ==1){
      out.results<-tab.row
    }
    
    else if( i !=1){
      out.results<-rbind(out.results,tab.row)
      
    }
  }
  
  
  # Text process results table
  
  if (is.null(subset.condition)==F) {
    for(i in c(3:6,9)){
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\))','0)')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\s)','0 ')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\-)','0-')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\-\\d)(\\))','.00)')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=^\\d)(\\s\\()','.00 (')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\(\\d)\\-','.00-')
      
    }
    
    for(i in c(7,8,10)){
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)($)','0')
    }
  }  
  
  else if (is.null(subset.condition)==F){
    for(i in c(2:5,8)){
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\))','0)')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\s)','0 ')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)(\\-)','0-')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\-\\d)(\\))','.00)')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=^\\d)(\\s\\()','.00 (')
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\(\\d)\\-','.00-')
      
    }
    
    for(i in c(6,7,9)){
      out.results[,i]<-stringr::str_replace(out.results[,i],'(?<=\\.\\d)($)','0')
    }
  }
  
  end_time <- Sys.time( )# document start times
  print( end_time - start_time ) # print system time to keep track of function progress
  
  return(list(results=out.results))
}




####################################################################################################
##################################### RCS Plotting Function ########################################
####################################################################################################


logit_splines <- function(df, x, y, knots, covariates, wts, referent='median', xlab, ylab,
                          legend.pos){
  
  df$x<-as.numeric(eval(parse(text=paste0('df$',x))))
  
  dd <- rms::datadist(df)
  dd$limits$x[2] <- unique(df[GenKern::nearest(df$x,eval(parse(text=paste0(referent,"( ","df$x,na.rm=T)")))),'x'] )
  options(datadist = "dd")
  
  # Fit logit model with spline term
  modelspline<-rms::lrm(formula(glue::glue('{y} ~ rms::rcs( x, {knots} ) + ',
                                           paste0(covariates,collapse='+'))),
                        data=df,
                        weights=get(wts),
                        normwt = T) # normalize weights for survey data
  
  pdata1 <- rms::Predict(modelspline, 
                         x,
                         ref.zero = TRUE, 
                         fun = exp)
  
  newdf<-data.frame(pdata1)
  newdf$relative<-1
  newdf$all<-'Referent (OR=1)'
  newdf$ci<-'95% Confidence Bounds'
  
  ggplot2::ggplot(data=newdf,mapping=aes(x=x,y=yhat))+
    geom_line(size=0.8)+
    geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+
    theme_classic()+
    geom_line(aes(y=relative, x=x,linetype=all))+
    scale_linetype_manual(values=c('dashed'))+
    theme(legend.position=legend.pos,
          text=element_text(family='Avenir'),
          legend.title=element_blank(),
          legend.spacing.y =unit(0.01,'cm'),
          legend.text = element_text(size=8))+
    coord_cartesian(ylim=c(0,max=(max(newdf$yhat)*3)))+
    labs( x=xlab, y=ylab )
}




####################################################################################################
################################# Table 1 (Categorical Variables) ##################################
####################################################################################################


epitab <- function(var,data.fr,des,table.var){
  
  attach(data.fr)
  
  typ<-paste0('~',var)
  sumcat=0
  for (i in 1:length(levels(factor(data.fr[[var]])))){
    sumcat<-sumcat+((svytable(formula(typ),design=des)[i]))
  }
  
  wtpct<-vector()
  
  for (i in 1:length(levels(factor(data.fr[[var]])))){
    wtpct[i]<-round((svytable(formula(typ),design=des)[i]/sumcat*100),digits=1)
  }
  wtpct
  wtpct<-c(' ',wtpct,' ')
  total<-vector()
  
  for (i in 1:length(levels(factor(data.fr[[var]])))){
    total[i]<-table(des[["variables"]][var])[i]
  }
  total<-c(' ',total,' ')
  
  levelnames<-c(table.var,levels(as.factor(data.fr[[var]])),' ')
  levelnames<-levelnames[!is.na(levelnames)==T]
  levelnames<-levelnames[!levelnames=='Missing/unknown']
  merged<-data.frame(cbind(levelnames,paste0(total,' (',wtpct,')')))
  
  colnames(merged)<-c('levelnames','mn')
  merged
  detach(data.fr)
  return(merged)
  
}



####################################################################################################
################################## Table 1 (Continuous Variables) ##################################
####################################################################################################

epitab.means <- function(cont.var, des, table.var){
  
  mn<-paste0(round(svymean(as.formula(paste0('~',cont.var)),design = des,na.rm=T)[1],digits=1),
             ' (',round(sqrt(svyvar(as.formula(paste0('~',cont.var)),design = des,na.rm=T))[1],digits=1),')')
  
  ms2<-data.frame(c('',table.var,''),c('',mn,''))
  
  colnames(ms2)<-c('levelnames','mn')
  
  return(ms2)
}




####################################################################################################
#################### Table 3 (Diet Indices by Epidemiologic Characteristics ) ######################
####################################################################################################


# function to generate table
table_3<-function(diet.index, design, df){
  
  low.des<-subset(design,design[['variables']][diet.index]=='1')
  high.des<-subset(design,design[['variables']][diet.index]=='2')
  both<-subset(design,is.na(design[['variables']][diet.index])==F)
  
  datadflow <- df[which(eval(parse(text=glue::glue("df${diet.index}")))=='1'),]
  datadfhigh <- df[which(eval(parse(text=glue::glue("df${diet.index}")))=='2'),]
  
  nms <- c( "Age", "BMI", 'MET', "Calories", "CCI","Smoking", "Alcohol", "Gender", "Income", "Education", "Race",
            "Years", "Site", "SNAP")
  these <- c( "Age", "BMXBMI", 'WeekMetMin', "KCAL", "CCI_Score","SmokStat", "alc_cat", "Gender", "fipr", 
              "Education_bin", "Race", "TimeCAFactor", "PrimaryCAGroup", "FoodAsstPnowic") 
  
  low <- list()
  high <- list()
  for (i in 1: length(nms)){
    
    if ( i %in% 1:5 ) {
      low[[i]] <- epitab.means(cont.var=these[i],des=low.des,table.var = nms[i])
      
      high[[i]] <- agehigh<-epitab.means(cont.var=these[i], des=high.des,table.var = nms[i])
      
      high[[i]][1,2]<-ifelse(svyttest(formula(paste0(these[i], "~",diet.index)),design=both)$p.value<0.01,paste0(high[[i]][1,2],'**'),
                             ifelse(svyttest(formula(paste0(these[i],"~",diet.index)),design=both)$p.value<0.05 & svyttest(formula(paste0(these[i], "~",diet.index)),design=both)$p.value>=0.01,paste0(high[[i]][1,2],'*'),high[[i]][1,2]))
      
    }  
    
    else if ( i %in% 6:length( these ) ) {
      low[[i]]<-epitab(var= these[i],data.fr=datadflow, des=low.des,table.var=nms[i])
      
      high[[i]]<-epitab(var= these[i],data.fr=datadfhigh, des=high.des,table.var=nms[i])
      
      high[[i]][1,2]<-ifelse(svychisq(formula(paste0("~",these[i]," + ", diet.index)),design=both)$p.value<0.01,paste0(high[[i]][1,2],'**'),
                             ifelse(svychisq(formula(paste0("~",these[i]," + ", diet.index)),design=both)$p.value<0.05 & svychisq(formula(paste0("~",these[i]," + ", diet.index)),design=both)$p.value>=0.01,paste0(high[[i]][1,2],'*'),high[[i]][1,2]))
      
    }
    
  }
  
  m1<-str_extract(diet.index,'^.*(?=((\\_Q)|(\\_q)))')
  m2<-str_extract(diet.index,'^.*(?=((\\_Q)|(\\_q)))')
  
  # column bind
  rws <- list()
  for( i in 1:length( high ) ) {
    rws[[i]] <- cbind( low[[i]], high[[i]]['mn'])
    
    colnames(rws[[i]]) <- c('Characteristic', paste0( m1,'_M1' ),paste0( m1,'_M2' ))
    
  }
  
  # row bind
  table3 <- do.call( "rbind", rws )
  
  return( table3 )
  
}
