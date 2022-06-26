##### LOGIT MODELS EXAMINING ASSOCIATIONS BETWEEN DIET SCORES AND ODDS OF BEING FOOD INSECURE######

# Generate data with quintile and trend variables calculated on the subset of cancer survivors
logit.data<-results.function(subset.condition = 'Diet.ext.ind.reg==1',
                             datf=mortdat,
                             censor='cstat',
                             covars=cvs)$all.data # Use function to get data with quantiles and trend 
# variables calculated for the subset condition specified

logit.data.b<-logit.data[which(is.na(logit.data$WTDR18YR)==F),]
mod.design<-svydesign(id = ~SDMVPSU, weights = ~WTDR18YR, strata = ~SDMVSTRA, 
                      nest = TRUE, survey.lonely.psu = "adjust", data = logit.data.b)

mod.design<-subset(mod.design,eval(parse(text='Diet.ext.ind.reg==1')))#inclusions

covars.logit<-c('Race_binary','Gender','Age','KCAL','BMXBMI','HHSize','FoodAsstPnowic',
                'SmokStat','fipr','KCAL','WeekMetMin','Education_bin',
                'PrimaryCAGroup','CCI_Score')

for(i in 1:length(indics)){
  modelfull<-svyglm(formula(paste0('BinFoodSecHH~factor(',indics[i],'_q)+',
                                   paste0(covars.logit,collapse = '+'))),
                    design=mod.design,
                    family = 'quasibinomial')
  modelfulla_T<-svyglm(formula(paste0('BinFoodSecHH~',indics[i],'_trend+',
                                      paste0(covars.logit,collapse = '+'))),
                       design=mod.design,
                       family = 'quasibinomial')
  modelfulla_L<-svyglm(formula(paste0('BinFoodSecHH~',indics[i],'_sc+',
                                      paste0(covars.logit,collapse = '+'))),
                       design=mod.design,
                       family = 'quasibinomial')
  modelfulla_L2<-svyglm(formula(paste0('BinFoodSecHH~',indics[i],'_sc2+',
                                       paste0(covars.logit,collapse = '+'))),
                        design=mod.design,
                        family = 'quasibinomial')
  
  
  
  xsum1<-round(exp(summary(modelfull)$coefficients),digits=2)
  xsum1nr<-summary(modelfull)$coefficients #not rounded to get correct astricks in table
  xsci<-round(exp(confint(modelfull)),digits=2)
  xtrend<-round(summary(modelfulla_T)$coefficients,digits=2)
  xlin<-round(exp(summary(modelfulla_L)$coefficients),digits=2)
  xlinci<-round(exp(confint(modelfulla_L)),digits=2)
  xlin2<-round(summary(modelfulla_L2)$coefficients,digits=2)
  
  tab.row<-data.frame(patt=str_remove_all(str_remove_all(rownames(xsum1)[2],'factor\\('),'_q\\)\\d'),
                      subset=subset.condition,
                      Q1=1,Q2=paste0(xsum1[2,1],' (',xsci[2,1],'-',xsci[2,2],')'),
                      Q3=paste0(xsum1[3,1],' (',xsci[3,1],'-',xsci[3,2],')'),
                      Q4=paste0(xsum1[4,1],' (',xsci[4,1],'-',xsci[4,2],')'),
                      Q5=paste0(xsum1[5,1],' (',xsci[5,1],'-',xsci[5,2],')'),
                      pq5=round(xsum1nr[5,4],digits=2),ptrend=xtrend[2,4],
                      lin=paste0(xlin[2,1],' (',xlinci[2,1],'-',xlinci[2,2],')'),
                      quadp=xlin2[2,4])
  
  tab.row<-tab.row%>%
    mutate(Q2=ifelse(xsum1nr[2,4]<0.05 & xsum1nr[2,4]>=0.01,str_replace(Q2,'$','*'),
                     ifelse(xsum1nr[2,4]<0.01,str_replace(Q2,'$','**'),Q2)))%>%
    mutate(Q3=ifelse(xsum1nr[3,4]<0.05 & xsum1nr[3,4]>=0.01,str_replace(Q3,'$','*'),
                     ifelse(xsum1nr[3,4]<0.01,str_replace(Q3,'$','**'),Q3)))%>%
    mutate(Q4=ifelse(xsum1nr[4,4]<0.05 & xsum1nr[4,4]>=0.01,str_replace(Q4,'$','*'),
                     ifelse(xsum1nr[4,4]<0.01,str_replace(Q4,'$','**'),Q4)))%>%
    mutate(Q5=ifelse(xsum1nr[5,4]<0.05 & xsum1nr[5,4]>=0.01,str_replace(Q5,'$','*'),
                     ifelse(xsum1nr[5,4]<0.01,str_replace(Q5,'$','**'),Q5)))%>%
    mutate(pq5=ifelse(xsum1nr[5,4]>=0.05,pq5,
                      ifelse(xsum1nr[5,4]<0.05 & xsum1nr[5,4]>=0.01,str_replace(pq5,'$','*'),
                             ifelse(xsum1nr[5,4]<0.01,'<0.01**',`pq5`))))%>%
    mutate(lin=ifelse(summary(modelfulla_L)$coefficients[2,4]<0.05 & summary(modelfulla_L)$coefficients[2,4]>=0.01,str_replace(lin,'$','*'),
                      ifelse(summary(modelfulla_L)$coefficients[2,4]<0.01,str_replace(lin,'$','**'),lin)))%>%
    mutate(ptrend=(ifelse(xtrend[2,4]>=0.05,ptrend,
                          ifelse(xtrend[2,4]<0.05 & xtrend[2,4]>=0.01,str_replace(ptrend,'$','*'),
                                 ifelse(xtrend[2,4]<0.01,'<0.01**',ptrend)))))%>%
    mutate(quadp=(ifelse(xlin2[2,4]>=0.05,quadp,
                         ifelse(xlin2[2,4]<0.05 & xlin2[2,4]>=0.01,str_replace(quadp,'$','*'),
                                ifelse(xlin2[2,4]<0.01,'<0.01**',quadp)))))
  
  model.list[[i]]<-modelfull
  
  if( i ==1){
    out.res.cafi<-tab.row
  }
  
  else if( i !=1){
    out.res.cafi<-rbind(out.res.cafi,tab.row)
    
  }
}


# Text process results table
for(i in c(2:6,9)){
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\.\\d)(\\))','0)')
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\.\\d)(\\s)','0 ')
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\.\\d)(\\-)','0-')
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\-\\d)(\\))','.00)')
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=^\\d)(\\s\\()','.00 (')
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\(\\d)\\-','.00-')
  
}

for(i in c(7,8,10)){
  out.res.cafi[,i]<-str_replace(out.res.cafi[,i],'(?<=\\.\\d)($)','0')
}


setwd('/Volumes/My Passport for Mac/Arthur Lab/FPED Raw Data/Analysis files/GitHub Repository Files /NHANES_FI_CA_Diet/Tables')
write.table(out.res.cafi,'logit_results.txt',sep=",",row.names=FALSE)


## Restricted-Cubic Spline Curves for Logistic Regression Models


pattern.labels<-c('FI Pattern','Age Pattern',
                  'SNAP Pattern','Household Size Pattern',
                  'Modified Western Pattern','Prudent Pattern')

indics<-c('FS_ENet','Age_ENet',
          'FdAs_ENet','HHS_ENet','PC1','PC2')





rcs.curves<-list() # Initialize list

for(i in 1:6){
  df<-mod.design$variables
  df$x<-as.numeric(eval(parse(text=paste0('df$',indics[i]))))
  df$x_trend<-as.numeric(eval(parse(text=paste0('df$',indics[i],'_trend'))))
  
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


