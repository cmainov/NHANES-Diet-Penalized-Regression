###---------------------------------------------------
###   05-VALIDATION: BINARY LOGISTIC REGRESSION
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


pattern.labels<-c('FI Pattern','Age Pattern',
                  'SNAP Pattern','Household Size Pattern',
                  'Modified Western Pattern','Prudent Pattern')

indices<-c('FS_ENet','Age_ENet',
          'FdAs_ENet','HHS_ENet','PC1','PC2')





rcs.curves<-list() # Initialize list

for(i in 1:6){
  df<-design$variables
  df$x<-as.numeric(eval(parse(text=paste0('df$',indices[i]))))
  df$x_trend<-as.numeric(eval(parse(text=paste0('df$',indices[i],'_trend'))))
  
  dd <- datadist(df)
  dd$limits$x[2] <- unique(df[nearest(df$x,median(df$x,na.rm=T)),'x'])
  options(datadist = "dd")
  
  # Fitt model logit model
  modelspline<-lrm(formula(paste0('BinFoodSecHH ~ rcs(x, as.numeric(levels(factor(df$x_trend)))) + ',
                                  paste0(covars.logit,collapse='+'))),
                   data=df,weights=WTDR18YR,normwt = T)
  
  pdata1 <- Predict(modelspline, x,ref.zero = TRUE, fun = exp)
  
  newdf<-data.frame(pdata1)
  newdf$relative<-1
  newdf$all<-'Reference (HR=1)'
  newdf$ci<-'95% Confidence Bounds'
  
  
  if(i==1){
    
    rcs.curves[[i]]<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position=c(0.34,0.8),
            text=element_text(),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      coord_cartesian(ylim=c(0,max=(max(newdf$yhat)*3)))+
      labs(x=pattern.labels[i], y='Odds Ratio (Food Insecure)')
  }
  
  else if(i==4){
    
    rcs.curves[[i]]<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position='none',
            text=element_text(),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      coord_cartesian(ylim=c(0,max=(max(newdf$yhat)*3)))+
      labs(x=pattern.labels[i], y='Odds Ratio (Food Insecure)')
  }
  
  else if(i!=1 & i!=4){
    
    rcs.curves[[i]]<-ggplot(data=newdf,mapping=aes(x=x,y=yhat))+geom_line(size=0.8)+
      geom_ribbon(aes(ymin=lower, ymax=upper,col=ci,fill=ci), alpha=0.2)+theme_classic()+
      geom_line(aes(y=relative, x=x,linetype=all))+
      scale_linetype_manual(values=c('dashed'))+
      theme(legend.position='none',
            text=element_text(),
            legend.title=element_blank(),
            legend.spacing.y =unit(0.01,'cm'),
            legend.text = element_text(size=8))+
      coord_cartesian(ylim=c(0,max=(max(newdf$yhat)*3)))+
      labs(x=pattern.labels[i], y='')
  }
}


spline.plots.logit<-do.call('ggarrange',rcs.curves)
spline.plots.logit

#Save
setwd('/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES_FI_CA_Diet/Figures')
ggsave("logit_splines.jpeg",width = 30, height = 20, units = "cm")


