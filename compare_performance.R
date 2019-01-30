compare_performance <- function(data)
{
  
  #data <- read_tsv('/Users/bkoester/Box Sync/SEISMIC/SEISMIC_Data/students.tsv')
  #data <- data %>% mutate(GENDER=as.character(STDNT_GNDR_SHORT_DES),
  #                        FG=as.character(FIRST_GEN),
  #                        URM=as.character(STDNT_UNDREP_MNRTY_IND))
  #crse <- read_tsv('/Users/bkoester/Box Sync/SEISMIC/SEISMIC/STEM_courses_UM.tab')
  
  
  #data <- left_join(crse,data,by=c('SBJCT_CD'='SBJCT_CD','CATLG_NBR'='CATLG_NBR')) %>% mutate(scCNAME=str_c(SBJCT_CD,CATLG_NBR))
  
  bin  <- cut_width(data$EXCL_CLASS_CUM_GPA,0.33)
  bin  <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", bin))-0.33
  data <- add_column(data,bin)
  
  scNEST  <- data %>% group_by(scCNAME) %>% nest()
  
  #Regression results
  scNESTr     <- scNEST %>% mutate(model=map(data,reg_func),N=map(data,tally)) %>% unnest(N) 
  
  scOUT    <- scNESTr %>% transmute(scCNAME,n,URM=map_dbl(model,coeff_func_mnrty), 
                                              beta_urm_err=map_dbl(model,coeff_func_mnrty_se),                 
                                              GNDR=map_dbl(model,coeff_func_gndr), 
                                              beta_gndr_err=map_dbl(model,coeff_func_gndr_se),
                                              FG=map_dbl(model,coeff_func_first_gen), 
                                              beta_fg_err=map_dbl(model,coeff_func_first_gen_se))
  
  #p <- scOUT %>% ggplot(aes(x=URM,y=GNDR))+geom_point()+
  #  geom_errorbarh(aes(xmin=URM-beta_urm_err,xmax=URM+beta_urm_err))+
  #  geom_errorbar(aes(ymin=GNDR-beta_gndr_err,ymax=GNDR+beta_gndr_err))+
  #  annotate("text", x = scOUT$URM, y = scOUT$GNDR, label = scOUT$scCNAME)  
  #print(p)
  
  ncrse <- dim(scOUT)[1]
  
  for (i in 1:ncrse)
  {
  
    scNESTgndr  <- scNEST %>% mutate(model=map(data,gpao.binned.gndr),N=map(data,tally)) %>% unnest(N)
    scNESTurm   <- scNEST %>% mutate(model=map(data,gpao.binned.urm),N=map(data,tally)) %>% unnest(N)
    scNESTfg    <- scNEST %>% mutate(model=map(data,gpao.binned.first.gen),N=map(data,tally)) %>% unnest(N)
  
    #print(names(scNESTgndr$model[[1]]))
  
    p <- scNESTgndr$model[[i]] %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD))) + geom_point() + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=GENDER), width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE)+geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(scOUT$scCNAME[i],": GNDR"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    p <- scNESTurm$model[[i]] %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD))) + geom_point() + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=URM), width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE)+geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(scOUT$scCNAME[i],": URM"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
    p <- scNESTfg$model[[i]] %>% ggplot(aes(x=bin, y=mnGRD, weight = 1/sqrt(seGRD))) + geom_point() + 
      geom_errorbar(aes(ymin=mnGRD-seGRD, ymax=mnGRD+seGRD,color=FG), width=0.09) + 
      geom_smooth(method='lm',formula=y ~ x,na.rm=TRUE)+geom_abline(intercept=0,slope=1)+
      scale_y_continuous(limits = c(-0.5, 4))+
      labs(x='GPAO',y='Mean Grade')+labs(title=str_c(scOUT$scCNAME[i],": FIRST GEN"))+
      theme(legend.position='right',text = element_text(size=20))
    print(p) 
  }
  
  #finally, full regression with interaction
  scNESTrint  <- scNEST %>% mutate(model=map(data,reg_func_int),N=map(data,tally)) %>% unnest(N)
  
  return()                 
  
}

reg_func <- function(df)
{
  r <- glm(GRD_PNTS_PER_UNIT_NBR ~ EXCL_CLASS_CUM_GPA + STDNT_GNDR_SHORT_DES + ACAD_LVL_BOT_SHORT_DES + 
             MAX_ACT_MATH_SCR + HS_GPA + STDNT_UNDREP_MNRTY_IND + PRMRY_CRER_CD + FIRST_GEN,data=df)
  #print(summary(r))
  return(r)
}

reg_func_int <- function(df)
{
  r <- glm(GRD_PNTS_PER_UNIT_NBR ~ EXCL_CLASS_CUM_GPA + ACAD_LVL_BOT_SHORT_DES + PRMRY_CRER_CD +
             MAX_ACT_MATH_SCR + HS_GPA + 
             URM*FG*GENDER ,data=df)
  
  tab <- data.frame(coef(summary(r)))
  tab <- tab[grepl('URM',rownames(tab)) | grepl('GENDER',rownames(tab)) | grepl('FG',rownames(tab)),] 
  tab <- xtable(tab, digits=c(0, 3, 3, 3, 5))
  #print(names(tab))
  
  print(tab,type='latex')
  #scan()
  return(r)
}

coeff_func <- function(scRES){signif(summary(scRES)$coefficients[3,1],3)}
coeff_func_mnrty <- function(scRES)
{
  a <- grepl('MNRTY',names(summary(scRES)$coefficients[,1]))
  out <- summary(scRES)$coefficients[a,1]
  return(out)
}

coeff_func_mnrty_se <- function(scRES)
{
  a <- grepl('MNRTY',names(summary(scRES)$coefficients[,2]))
  out <- summary(scRES)$coefficients[a,2]
return(out)
}

coeff_func_gndr <- function(scRES)
{
  a <- grepl('GNDR',names(summary(scRES)$coefficients[,1]))
  out <- summary(scRES)$coefficients[a,1]
  return(out)
}

coeff_func_gndr_se <- function(scRES)
{
  a <- grepl('GNDR',names(summary(scRES)$coefficients[,2]))
  out <- summary(scRES)$coefficients[a,2]
  return(out)
}

coeff_func_first_gen <- function(scRES)
{
  a <- grepl('FIRST_GEN',names(summary(scRES)$coefficients[,1]))
  out <- summary(scRES)$coefficients[a,1]
  return(out)
}

coeff_func_first_gen_se <- function(scRES)
{
  a <- grepl('FIRST_GEN',names(summary(scRES)$coefficients[,2]))
  out <- summary(scRES)$coefficients[a,2]
  return(out)
}

gpao.binned.gndr <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(GENDER,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)
}

gpao.binned.urm <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(URM,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)
}

gpao.binned.first.gen <- function(scRES)
{
  #scRES %>% group_by(STDNT_GNDR_SHORT_DES) %>% summarsize(meanGRD)
  tt <- scRES %>% group_by(FG,bin) %>% 
    summarise(mnGRD=signif(mean(GRD_PNTS_PER_UNIT_NBR),3),seGRD=signif(sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE)/sqrt(n()),3))
  maxSE <- max(tt$seGRD,na.rm=TRUE)
  tt <- tt %>% replace_na(list(seGRD=maxSE))
  tt$seGRD[e  <- tt$seGRD == 0] <- maxSE 
  return(tt)
}