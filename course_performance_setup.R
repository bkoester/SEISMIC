course_performance_setup <- function()
{
  require(tidyverse)
  require(xtable)
  
  data <- read_tsv('/Users/bkoester/Box Sync/SEISMIC/SEISMIC_Data/students.tsv')
  data <- data %>% mutate(GENDER=case_when(STDNT_GNDR_SHORT_DES == "Male" ~ '0',STDNT_GNDR_SHORT_DES == "Female" ~ '1'),
                          FG=as.character(FIRST_GEN),
                          URM=as.character(STDNT_UNDREP_MNRTY_IND))
  crse <- read_tsv('/Users/bkoester/Box Sync/SEISMIC/SEISMIC/STEM_courses_UM.tab')
  
  data <- left_join(crse,data,by=c('SBJCT_CD'='SBJCT_CD','CATLG_NBR'='CATLG_NBR')) %>% 
    mutate(scCNAME=str_c(SBJCT_CD,CATLG_NBR))
  
  return(data)
  
}