SELECT 
/*course term columns*/
  a.STDNT_ID,a.TERM_CD,a.CLASS_NBR,a.TERM_SHORT_DES,a.CRSE_GRD_OFFCL_CD,
a.GRD_PNTS_PER_UNIT_NBR,a.EXCL_CLASS_CUM_GPA,
a.SBJCT_CD,a.CATLG_NBR,a.CRSE_ID_CD,


WHERE a.INCL_GPA_IND = 1;

/*untested: still need to try running this*/
