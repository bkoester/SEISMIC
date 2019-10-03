###########################
#quick and dirty analysis of factors associated with 
#STEM outcomes, orginally done at UC Davis.
#student population:
#MAJORS: all undergraduate
#INTERNATIONAL STATUS: all
#DEGREE SEEKING STATUS: all
#TRANSFER STATUS: all
#COHORTS: FA 2015 to present.
#INPUT tables:
#---------------------
#sr - student record:
#STDNT_ID - unique ID
#FIRST_GEN: 0 or 1
#STDNT_DMSTC_UNDREP_MNRTY_CD: 0 or 1
#STDNT_GNDR_SHORT_DES: Male or female
#FIRST_TERM_ATTND_CD: first term attended college
#MEDNUM: median income of permanent zipocde
#------------------------
#sc: student course - 
#STDNT_ID:unique student ID
#SBJCT_CD: course subject
#CATLG_NBR: course catalog number
#TERM_CD: Term code
#GRD_PNTS_PER_UNIT_NBR: course grade
#EXCL_CLASS_CUM_GPA: cumultative GPA in other classes up to and including this term
#UNITS_ERND_NBR: credits/units award for course completion.
#TERMYR: the year in the student's career the course was taken
#-----------
#ctab: STEM course list (read in on the fly)
#SBJCT_CD:  course subject
#CATLG_NBR: course catalog number
#-----
#OPTIONS:
#OPP: T/F. Calculate/plot the UC Davis Opportunity Factors?
#PRIV: T/F. Calculate/plot H. Rypkema's Privilege Index?
#GA: subtract EXCL_CLASS_CUM_GPA from grade to compute grade anomoly, and plot it.
#    Be sure to change the ymin/ymax values appropriately (ymin=-1,ymax=1)
###############################
replicate_molinaro_sept_2019 <- function(sr,sc,PRIV=TRUE,OPP=TRUE,GA=FALSE,ymin=0,ymax=4.0)
{
    require(tidyverse)
    
    #use a local script to compute the year in student's career that a 
    #course was a taken. This script is particular to Michigan, so not it's not shared.
    source('/Users/bkoester/Google\ Drive/code/Mellon/TOF/term_count.R')
  
    #these are our two main tables. I read them in to the R session and give them as
    #arguments.
    #sr   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20190529.tab")
    #sc   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20190529.tab")
   
    #the course table, also read from my local disk (also on github)
    ctab  <- read_tsv("/Users/bkoester/Google Drive/code/SEISMIC/gitSEISMIC/SEISMIC/STEM_courses_UM.tab")
    ctab  <- ctab %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR))
    ctab  <- ctab %>% select(-c(SBJCT_CD,CATLG_NBR))
  
    sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 2060) %>% #1650) %>% 
      select(c(STDNT_ID,FIRST_GEN,STDNT_DMSTC_UNDREP_MNRTY_CD,
               STDNT_GNDR_SHORT_DES,FIRST_TERM_ATTND_CD,MEDNUM))
    sr <- sr %>% drop_na()
    
    #compute the UCD
    sr <- ucd_opportunity(sr)
    
    #compute the privilege index
    sr <- privilege_index(sr)
    
    sr <- sr %>% drop_na()
    
    sc <- term_count(sr,sc)
  
    sc <- sc   %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR)) %>% filter(TERM_CD >= 1650)
    sc <- sc %>% left_join(sr,by='STDNT_ID')
    
    sc <- sc %>% mutate(GRD_PNTS_PER_UNIT_NBR=GRD_PNTS_PER_UNIT_NBR-EXCL_CLASS_CUM_GPA)
    
    #now the analyses
    if (OPP == TRUE)
    {
      opp_output  <- opportunity_analysis(sc,ctab)
      tt <- plot_opp_results(opp_output,ymin=ymin,ymax=ymax)
    }
    if (PRIV == TRUE)
    {
      priv_output <- privilege_analysis(sc,ctab)
      tt <- plot_priv_results(priv_output,ymin=ymin,ymax=ymax)
    }
    
    return()
}  
 
opportunity_analysis <- function(sc,ctab)
{
  sc <- sc %>% drop_na(OPP)
  
  fy_results <- sc %>% filter(TERMYR == 1.5) %>% distinct(STDNT_ID,.keep_all=TRUE) %>% group_by(OPP) %>% 
    summarize(FY_MEAN=mean(EOT_GPA,na.rm=TRUE),N=n())
  
  scsci      <- ctab %>% left_join(sc,by='CNAME')
  
  crse_results <- scsci %>% group_by(CNAME,OPP) %>% 
    summarize(CRSE_MEAN=mean(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE),
              N=n()) %>% ungroup()
  sci_results <- scsci %>% filter(TERMYR <= 1.5) %>% group_by(STDNT_ID) %>% 
    mutate(EOT_SCI=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup() %>% 
    distinct(STDNT_ID,.keep_all=TRUE) %>% group_by(OPP) %>%
    summarize(FY_SCI_MEAN=mean(EOT_SCI,na.rm=TRUE),N=n())
  
  sci_results_ind <- scsci %>% filter(TERMYR <= 1.5) %>% group_by(STDNT_ID) %>% 
    mutate(EOT_SCI=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup() %>% 
    distinct(STDNT_ID,.keep_all=TRUE)
  
  return(list(fy_results,crse_results,sci_results,sci_results_ind))
  
  
}

privilege_analysis <- function(sc,ctab)
{
  sc <- sc %>% drop_na(PRIV_INDEX)
  fy_results <- sc %>% filter(TERMYR == 1.5) %>% distinct(STDNT_ID,.keep_all=TRUE) %>% group_by(PRIV_INDEX) %>% 
    summarize(FY_MEAN=mean(EOT_GPA,na.rm=TRUE),N=n())
  
  scsci      <- ctab %>% left_join(sc,by='CNAME')
  
  crse_results <- scsci %>% group_by(CNAME,PRIV_INDEX) %>% 
    summarize(CRSE_MEAN=mean(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE),
              N=n()) %>% ungroup()
  sci_results <- scsci %>% filter(TERMYR <= 1.5) %>% group_by(STDNT_ID) %>% 
    mutate(EOT_SCI=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup() %>% 
    distinct(STDNT_ID,.keep_all=TRUE) %>% group_by(PRIV_INDEX) %>%
    summarize(FY_SCI_MEAN=mean(EOT_SCI,na.rm=TRUE),N=n())
  
  sci_results_ind <- scsci %>% filter(TERMYR <= 1.5) %>% group_by(STDNT_ID) %>% 
    mutate(EOT_SCI=weighted.mean(GRD_PNTS_PER_UNIT_NBR,UNITS_ERND_NBR,na.rm=TRUE)) %>% ungroup() %>% 
    distinct(STDNT_ID,.keep_all=TRUE)
  
  return(list(fy_results,crse_results,sci_results,sci_results_ind))
  
  
}
#takes the list output of the replicate_molinaro_sept_2019 
plot_opp_results <- function(aa,ymin=0,ymax=4.0)
{
  
  p1 <- aa[[1]] %>% ggplot(aes(x=OPP,y=FY_MEAN))+
    geom_point(aes(size=N))+ggtitle('First Year GPA')+
    geom_text(aes(label=N),vjust=1,hjust=1)#+ylim(c(ymin,ymax))
  p2 <- aa[[2]] %>% ggplot(aes(x=OPP,y=CRSE_MEAN))+
    geom_point(aes(size=N))+ggtitle('STEM Course Results')+ylim(c(ymin,ymax))
  p3<- aa[[3]] %>% ggplot(aes(x=OPP,y=FY_SCI_MEAN))+
                   geom_point(aes(size=N))+ggtitle('First Year Science GPA')+
                   geom_text(aes(label=N),vjust=1,hjust=1)+ylim(c(ymin,ymax))
  p4<- aa[[4]] %>% group_by(OPP) %>% ggplot(aes(x=OPP,y=EOT_SCI))+
    geom_boxplot()+ggtitle('First Year Science GPA')+ylim(c(ymin,ymax))
  
  print(p1)
  print(p2)
  print(p3)
  #print(p4)
}

#takes the list output of the replicate_molinaro_sept_2019 
plot_priv_results <- function(aa,ymin=0,ymax=4.0)
{
  
  p1 <- aa[[1]] %>% ggplot(aes(x=PRIV_INDEX,y=FY_MEAN))+
    geom_point(aes(size=N))+ggtitle('First Year GPA')+
    geom_text(aes(label=N),vjust=1,hjust=1)#+ylim(c(ymin,ymax))
  p2 <- aa[[2]] %>% ggplot(aes(x=PRIV_INDEX,y=CRSE_MEAN))+
    geom_point(aes(size=N))+ggtitle('STEM Course Results')+ylim(c(ymin,ymax))
  p3<- aa[[3]] %>% ggplot(aes(x=PRIV_INDEX,y=FY_SCI_MEAN))+
    geom_point(aes(size=N))+ggtitle('First Year Science GPA')+
    geom_text(aes(label=N),vjust=1,hjust=1)+ylim(c(ymin,ymax))
  p4<- aa[[4]] %>% group_by(PRIV_INDEX) %>% ggplot(aes(x=PRIV_INDEX,y=EOT_SCI))+
    geom_boxplot()+ggtitle('First Year Science GPA')+ylim(c(ymin,ymax))
  
  print(p1)
  print(p2)
  print(p3)
  #print(p4)
}

ucd_opportunity <- function(sr)
{
  sr <- sr %>% mutate(OPP='Other')
  sr$OPP <- 'Other'
  
  sr$OPP[which(sr$FIRST_GEN == 1)] <- "FG"
  sr$OPP[which(sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 1)] <- "URM"
  #sr$OPP[which(sr$STDNT_GNDR_SHORT_DES == 'Female')] <- "Female"
  sr$OPP[which(sr$MEDNUM <= 20000)] <- "LI"
  
  #sr$OPP[which(sr$FIRST_GEN == 1 & sr$STDNT_GNDR_SHORT_DES == 'Female')] <- "FG&Fem"
  #sr$OPP[which(sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 1 & sr$STDNT_GNDR_SHORT_DES == 'Female')] <- "URM&Fem"
  sr$OPP[which(sr$FIRST_GEN == 1 & sr$MEDNUM < 20000)] <- "FG&LI"
  sr$OPP[which(sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 1 & sr$MEDNUM < 20000)] <- "URM&LI"
  
  
  sr$OPP[which(sr$FIRST_GEN == 1 & sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 1)] <- "URM&FG"
  
  sr$OPP[which(sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 0 & 
           sr$MEDNUM > 20000 & 
           sr$FIRST_GEN == 0)] <- "NotTriple"
  
  sr$OPP[which(sr$STDNT_DMSTC_UNDREP_MNRTY_CD == 1 & 
         sr$MEDNUM < 20000 & 
         sr$FIRST_GEN == 1)] <- "Triple"
  
  
  
  return(sr)
  
}
  
privilege_index <- function(sr)
{
  sr <- sr %>% mutate(PRIV_INDEX=4)
  ind<-sr$STDNT_DMSTC_UNDREP_MNRTY_CD==0 | is.na(sr$STDNT_DMSTC_UNDREP_MNRTY_CD)
  sr$PRIV_INDEX[!ind]<-sr$PRIV_INDEX[!ind]-1
  ind<-sr$FIRST_GEN==0 | is.na(sr$FIRST_GEN)
  sr$PRIV_INDEX[!ind]<-sr$PRIV_INDEX[!ind]-1   
  ind<-sr$MEDNUM > 20000 | is.na(sr$MEDNUM)
  sr$PRIV_INDEX[!ind]<-sr$PRIV_INDEX[!ind]-1  
  ind<-sr$STDNT_GNDR_SHORT_DES=="Female"
  sr$PRIV_INDEX[ind]<-sr$PRIV_INDEX[ind]-1
  
  return(sr)
}