zscores_seismic_nsf <- function(sr,sc)
{
  library(tidyverse)
  #sr   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_RECORD_20190529.tab")
  #sc   <- read_tsv("~/Box Sync/LARC.WORKING/BPK_LARC_STUDENT_COURSE_20190529.tab")
  ctab <- read_tsv("/Users/bkoester/Google Drive/code/SEISMIC/gitSEISMIC/SEISMIC/STEM_courses_UM.tab")
  ctab <- ctab %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR))
  ctab <- ctab %>% select(-c(SBJCT_CD,CATLG_NBR))
  sc   <- sc   %>% mutate(CNAME=str_c(SBJCT_CD,CATLG_NBR)) %>% filter(TERM_CD >= 1650)
  sc   <- ctab %>% left_join(sc,by='CNAME')
  
  sr <- sr %>% filter(FIRST_TERM_ATTND_CD >= 1650) %>% 
               select(c(STDNT_ID,FIRST_GEN,STDNT_DMSTC_UNDREP_MNRTY_CD,STDNT_GNDR_SHORT_DES))  
  
  sc <- sc %>% left_join(sr,by='STDNT_ID')
  sc <- sc %>% group_by(CNAME) %>% 
               mutate(CRSE_MEAN=mean(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE),
                      CRSE_SD  =  sd(GRD_PNTS_PER_UNIT_NBR,na.rm=TRUE),N=n()) %>% ungroup()
  
  
  fg   <- sc %>% drop_na() %>% group_by(CNAME,FIRST_GEN) %>% 
          summarize(N=n(),Z=mean(GRD_PNTS_PER_UNIT_NBR-CRSE_MEAN[1],na.rm=TRUE)/CRSE_SD[1]) #%>% spread(FIRST_GEN,c(Z,N))
  fg1  <- fg  %>% select(-c(N)) %>% spread(FIRST_GEN,Z)
  fg2  <- fg  %>% select(-c(Z)) %>% spread(FIRST_GEN,N)
  
  gend <- sc %>% drop_na() %>% group_by(CNAME,STDNT_GNDR_SHORT_DES) %>% 
          summarize(N=n(),Z=mean(GRD_PNTS_PER_UNIT_NBR-CRSE_MEAN[1],na.rm=TRUE)/CRSE_SD[1]) #%>% spread(STDNT_GNDR_SHORT_DES,c(Z,N))
  gend1  <- gend %>% select(-c(N)) %>% spread(STDNT_GNDR_SHORT_DES,Z)
  gend2  <- gend %>% select(-c(Z)) %>% spread(STDNT_GNDR_SHORT_DES,N)
  
  urm  <- sc %>% drop_na() %>% group_by(CNAME,STDNT_DMSTC_UNDREP_MNRTY_CD) %>% 
          summarize(N=n(),Z=mean(GRD_PNTS_PER_UNIT_NBR-CRSE_MEAN[1],na.rm=TRUE)/CRSE_SD[1]) #%>% spread(STDNT_DMSTC_UNDREP_MNRTY_CD,c(Z,N))
  urm1 <- urm %>% select(-c(N)) %>% spread(STDNT_DMSTC_UNDREP_MNRTY_CD,Z)
  urm2 <- urm %>% select(-c(Z)) %>% spread(STDNT_DMSTC_UNDREP_MNRTY_CD,N)
  
    
  return(list(fg1,fg2,gend1,gend2,urm1,urm2))
  
}