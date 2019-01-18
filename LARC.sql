SELECT 
  /*course term columns*/
  a.STDNT_ID,a.TERM_CD,a.CLASS_NBR,a.TERM_SHORT_DES,a.CRSE_GRD_OFFCL_CD,
  a.GRD_PNTS_PER_UNIT_NBR,a.EXCL_CLASS_CUM_GPA,
  a.SBJCT_CD,a.CATLG_NBR,a.CRSE_ID_CD,
  
  /*student term columns*/
  b.ACAD_LVL_BOT_SHORT_DES,b.PRMRY_CRER_CD,
  
  /*student info columns*/
  c.STDNT_GNDR_SHORT_DES,c.STDNT_ETHNC_GRP_SHORT_DES,c.STDNT_UNDREP_MNRTY_IND,
  c.MAX_ACT_MATH_SCR,c.MAX_SATI_MATH_SCR,c.HS_GPA,
  c.STDNT_CTZN_STAT_SHORT_DES,
  c.PRNT_MAX_ED_LVL_DES,c.EST_GROSS_FAM_INC_DES,
  c.FIRST_TERM_ATTND_CD,c.FIRST_TERM_ATTND_SHORT_DES,
  c.UM_DGR_1_MAJOR_1_DES
  
  /*JOIN it all...hopefully in the right way.*/
FROM CNLYR002.CURR_STDNT_TERM_CLASS_INFO a
    LEFT JOIN CNLYR002.CURR_STDNT_INFO c ON
              c.STDNT_ID=a.STDNT_ID
    LEFT JOIN CNLYR002.CURR_STDNT_TERM_INFO b ON
              a.STDNT_ID=b.STDNT_ID AND
              a.TERM_CD=b.TERM_CD
  /*clean up and filter as appropriate*/
WHERE a.INCL_GPA_IND = 1 AND c.FIRST_TERM_ATTND_CD >= '1560';