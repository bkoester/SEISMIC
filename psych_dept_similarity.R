psych.dept.similarity <- function(stud)
{
  gg <- temp[[1]] %>% drop_na(UM_DGR_1_MAJOR_1_DES_PAIR) %>% 
        filter(UM_DGR_1_MAJOR_1_DES_ID %in% 
                 c('Psychology BA','Psychology BS','Brain,Behavior & Cognit Sc BS','Neuroscience BS',
                   'Brain,Behavior & Cognit Sc BA','Biopsych, Cognit & Neurosci BS'))
  ggg <- gg %>% filter(STDNT_GNDR_SHORT_DES_PAIR != STDNT_GNDR_SHORT_DES_ID & 
                       UM_DGR_1_MAJOR_1_DES_ID == UM_DGR_1_MAJOR_1_DES_PAIR) %>%
                       group_by(UM_DGR_1_MAJOR_1_DES_ID) %>% summarize(mn=mean(SIM))  
               
}

read_data <- function()
{
  stud <- read_tsv("Box Sync/SEISMIC/SEISMIC_Data/students.tsv")
  temp <- basic_compute_pairwise(stud %>% select(STDNT_ID,CATLG_NBR,CRSE_ID_CD) %>% filter(CATLG_NBR < 500),
                                 stud %>% filter(FIRST_TERM_ATTND_CD == 1660) %>% 
                                   select(c(UM_DGR_1_MAJOR_1_DES,STDNT_GNDR_SHORT_DES,STDNT_UNDREP_MNRTY_IND,FIRST_GEN,STDNT_ID)) %>% 
                                   distinct(STDNT_ID,.keep_all=TRUE),
                                 keep_cols=c('UM_DGR_1_MAJOR_1_DES','STDNT_GNDR_SHORT_DES','STDNT_UNDREP_MNRTY_IND','FIRST_GEN'))
  return(temp)
}