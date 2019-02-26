#sr: must contain STDNT_ID
#sc: must contain STDNT_ID,CRSE_ID_CD
#both must be cleaned.
#this will keep the subset of columns you select from SR.
#keep cols: if set to NONE, keeps nothing from sr. Otherwise set to a vector of columns names to keep.
#MIN_COURSE_SIZE: omit courses with total enrollment less than this (over the lifetime of the data set)
##returns the result in a list with 2 elements
#ll:  the full similarity matrix in tibble form for easy stats.
#     for 6000 students in ~ 5000 courses this is > 33 x 10^6 rows.
#sr1: the assignment of students to clusters because it's fun. 
#     in the same order as was given.
#
#
#EXAMPLE for UMich, beginning with the output of LARC.sql (
#temp <- basic_compute_pairwise(larc_sql_tab %>% select(STDNT_ID,CATLG_NBR,CRSE_ID_CD) %>% filter(CATLG_NBR < 500),
#                               larc_sql_tab %>% filter(FIRST_TERM_ATTND_CD == 1660) %>% select(STDNT_ID) %>% distinct(STDNT_ID,.keep_all=TRUE))
#
##########
basic_compute_pairwise <- function(sc,sr,keep_cols='NONE',MIN_COURSE_SIZE=0)
{
  require(coop)
  require(tidyverse)
  
  #we need this reformat to work in tidyverse.
  if (!is_tibble(sc)){sc <- as_tibble(sc)}
  if (!is_tibble(sr)){sr <- as_tibble(sr)}
  
  print(Sys.time())
  
  if (keep_cols[1] != 'NONE')
    {
      sr1 <- sr %>% select(c('STDNT_ID',keep_cols))
    }
  else
    {
      sr1 <- sr %>% select(STDNT_ID)
    }
  
  print('trimmed student record columns')
  
  #spits out a warning and exits if there are too many students (which maxes out the mem on my laptop)
  if (dim(sr1)[1] > 6500){print('danger: > 6500 students'); return()}
  
  #this creates/cuts the list of courses that will considered for each student's courses vector.
  CLIST <- sc %>% distinct(CRSE_ID_CD) %>% select(CRSE_ID_CD)
  
  print('cleaned student-course table to select courses')
  
  #Now, reduce the input student course table (sc) to include ONLY the courses that passed our cuts.
  subsc <- left_join(CLIST,sc)
  
  #finally join the student course table with the student record table and compute some statistics
  sc  <- left_join(sr1,subsc) %>% distinct(CRSE_ID_CD,STDNT_ID,.keep_all=TRUE) %>% 
    mutate(TOT_STD_CRSE=n()) %>% mutate(TOT_STD=n_distinct(STDNT_ID)) %>% ungroup() %>%
    group_by(CRSE_ID_CD) %>% mutate(N_CRSE_ALL=n()) %>% ungroup() %>% drop_na(STDNT_ID)
  
  #cut on course size (cut is at 0 by default), and at a course taken indicator
  sc <- sc %>% filter(N_CRSE_ALL > MIN_COURSE_SIZE) %>% mutate(TAKEN_IND=1)
  
  #create the input matrix for the similarlity calc
  sc <- sc %>% select(STDNT_ID,CRSE_ID_CD,TAKEN_IND) 
  tt <- sc %>% spread(CRSE_ID_CD,TAKEN_IND,fill=0) #fancy function from tidyverse finish the one-hot coding
  STDNT_ID <- as.character(pull(tt[,1],STDNT_ID))
  cos_input <- tt[,c(-1)] #drop the STDNT_ID column b/c we don't need it for the similarity calc.
  
  print('computing similarities')
  ll <- cosine(t(cos_input)) #from the "coop" package. need to do this efficiently.
  
  print('gathering similarity tibble back into a few columns and fixing names')
  ll <- as_tibble(ll)
  ll <- ll %>% rename_at(vars(starts_with('V')), ~ STDNT_ID)
  
  ll <- ll %>% mutate(ID=STDNT_ID)
  ll <- ll %>% gather(1:length(STDNT_ID),key="PAIR",value="SIM")
  ll <- ll %>% mutate(ID=as.integer(ID),PAIR=as.integer(PAIR))
  
  #now add the original sr1 columns to this table for easy downstream analysis. 
  ll <- left_join(ll,sr1,by=c("ID"="STDNT_ID"))
  ll <- left_join(ll,sr1,by=c("PAIR"="STDNT_ID"),suffix=c('_ID','_PAIR'))
  
  len <- dim(ll)[1]
  
  #remove self pairs
  ll <- ll %>% filter(ID != PAIR)
  
  #create matrix for clustering, and assign studetns to clusters
  print('doing clustering')
  mm <- ll %>% select(ID,PAIR,SIM) %>% spread(PAIR,SIM,drop=FALSE)
  mtx <- as.matrix(mm)
  rownames(mtx) <- mtx[,1]
  mtx <- as.dist(-1.0*(mtx[,-1]-1))
  ss  <- hclust(mtx,method='ward.D2')
  tt <- cutree(ss,h=7) #cutting the tree at level 7, somewhat arbitrarily
  tt <- tibble(CL=tt,STDNT_ID=as.integer(names(tt))) #rearrange into a tibble
  
  sr1 <- left_join(sr1,tt)
  print(Sys.time())
  
  #return the result in a list
  #ll:  the full similarity matrix in tibble form for easy stats.
  #     for 6000 student in ~ 5000 courses this is > 33 x 10^6 rows.
  #sr1: the assignment of students to clusters because it's fun. 
  #     in the same order as was given.
  
  return(list(ll,sr1))
  
}
