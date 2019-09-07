replicate_molinaro_sept_2019 <- function(sr,sc)
{
    library(tidyverse)
  
    source('/Users/bkoester/Google\ Drive/code/Mellon/TOF/term_count.R')
  
    #sr   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20190529.tab")
    #sc   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20190529.tab")
    sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 2060) %>% #1650) %>% 
      select(c(STDNT_ID,FIRST_GEN,STDNT_DMSTC_UNDREP_MNRTY_CD,
               STDNT_GNDR_SHORT_DES,FIRST_TERM_ATTND_CD,MEDNUM))
    sr <- sr %>% drop_na()
    sr <- uci_opportunity(sr)
    sr <- sr %>% drop_na()
    
    
    sc <- term_count(sr,sc)
  
    ctab  <- read_tsv("/Users/bkoester/Google Drive/code/SEISMIC/gitSEISMIC/SEISMIC/STEM_courses_UM.tab")
    ctab  <- ctab %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR))
    ctab  <- ctab %>% select(-c(SBJCT_CD,CATLG_NBR))
    sc <- sc   %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR)) %>% filter(TERM_CD >= 1650)
    sc <- sc %>% left_join(sr,by='STDNT_ID')
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
 
#takes the list output of the replicate_molinaro_sept_2019 
plot_results <- function(aa)
{
  p1 <- aa[[1]] %>% ggplot(aes(x=OPP,y=FY_MEAN))+
    geom_point(aes(size=N))+ggtitle('First Year GPA')+
    geom_text(aes(label=N),vjust=1,hjust=1)
  p2 <- aa[[2]] %>% ggplot(aes(x=OPP,y=CRSE_MEAN))+
    geom_point(aes(size=N))+ggtitle('STEM Course Results')
  p3<- aa[[3]] %>% ggplot(aes(x=OPP,y=FY_SCI_MEAN))+
                   geom_point(aes(size=N))+ggtitle('First Year Science GPA')+
                   geom_text(aes(label=N),vjust=1,hjust=1)
  p4<- aa[[4]] %>% group_by(OPP) %>% ggplot(aes(x=OPP,y=EOT_SCI))+
    geom_boxplot()+ggtitle('First Year Science GPA')
  
  print(p1)
  print(p2)
  print(p3)
  print(p4)
}


uci_opportunity <- function(sr)
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
  