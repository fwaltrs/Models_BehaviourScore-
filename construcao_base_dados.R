# TRATAMENTO DOS DADOS ----------------------------------------------------
tratamento_dados <- function(df){
    
    df$CAT_var_13 <- as.factor(ifelse(is.na(df$var_13), "C0","C1"))
  
  
  
    df$CAT_var_14 <- as.factor(ifelse(is.na(df$var_14), "C0",
                                      ifelse(df$var_14 < median(df$var_14, na.rm=T), "C1", "C2")))
    
    
    
    
    df$CAT_var_15 <- as.factor(ifelse(is.na(df$var_15), "C0",
                                      ifelse(df$var_15 < median(df$var_15, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_16 <- as.factor(ifelse(is.na(df$var_16), "C0",
                                      ifelse(df$var_16 < median(df$var_16, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_17 <- as.factor(ifelse(is.na(df$var_17), "C0",
                                      ifelse(df$var_17 < median(df$var_17, na.rm=T), "C1", "C2")))
    
    
    
    
    ### variáveis relacionadas com a média de aging de vencimentos 
    df$CAT_var_18 <- as.factor(ifelse(is.na(df$var_18), "C0",
                                      ifelse(df$var_18 < median(df$var_18, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_19 <-  as.factor(ifelse(is.na(df$var_19), "C0",
                                       ifelse(df$var_19 < median(df$var_19, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_20 <-  as.factor(ifelse(is.na(df$var_20), "C0",
                                       ifelse(df$var_20 < median(df$var_20, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_21 <- as.factor(ifelse(is.na(df$var_21), "C0",
                                      ifelse(df$var_21 < median(df$var_21, na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_22 <- as.factor(ifelse(is.na(df$var_22), "C0", 
                                      ifelse(df$var_22 <= median(df$var_22,na.rm = T),"C1", 'C2')))
    
    
    
    
    ### aging_vencimentos_quitados_antecip MIN
    
    
    df$CAT_var_33 <- as.factor(ifelse(is.na(df$var_33), "C0",
                                      ifelse(df$var_33 < quantile(df$var_33, 0.25,na.rm=T), "C1",  "C2")))
    
    
    
    df$CAT_var_34 <- as.factor(ifelse(is.na(df$var_34), "C0",
                                      ifelse(df$var_34 < quantile(df$var_34, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_34 < quantile(df$var_34, 0.50,na.rm=T) , "C1",
                                                    ifelse(df$var_34 < quantile(df$var_34, 0.75,na.rm=T), "C3","C1" )))))
    
    
    
    df$CAT_var_35 <- as.factor(ifelse(is.na(df$var_35), "C0","C1" ))
    
    
    df$CAT_var_36 <- as.factor(ifelse(is.na(df$var_36), "C0",
                                      ifelse(df$var_36 < quantile(df$var_36, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_36 < quantile(df$var_36, 0.50,na.rm=T) , "C2",
                                                    ifelse(df$var_36 < quantile(df$var_36, 0.75,na.rm=T), "C1","C2" )))))
    
    
    df$CAT_var_37 <- as.factor(ifelse(is.na(df$var_37), "C0",
                                      ifelse(df$var_37 < quantile(df$var_37, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_37 < quantile(df$var_37, 0.50,na.rm=T) , "C1",
                                                    ifelse(df$var_37 < quantile(df$var_37, 0.75,na.rm=T), "C1","C4" )))))
    
    
    
    
    ### aging_vencimentos_quitados_antecip_ult_ avg 
    
    df$CAT_var_38 <- as.factor(ifelse(is.na(df$var_38), "C0",
                                      ifelse(df$var_38 < quantile(df$var_38, 0.25,na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_39 <- as.factor(ifelse(is.na(df$var_39), "C0",
                                      ifelse(df$var_39 < quantile(df$var_39, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_39 < quantile(df$var_39, 0.50,na.rm=T) , "C2",
                                                    ifelse(df$var_39 < quantile(df$var_39, 0.75,na.rm=T), "C3","C3" )))))
    
    df$CAT_var_40 <- as.factor(ifelse(is.na(df$var_40), "C0",
                                      ifelse(df$var_40 < quantile(df$var_40, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_40 < quantile(df$var_40, 0.50,na.rm=T) , "C2",
                                                    ifelse(df$var_40 < quantile(df$var_40, 0.75,na.rm=T), "C2","C2" )))))
    
    df$CAT_var_41 <- as.factor(ifelse(is.na(df$var_41), "C0",
                                      ifelse(df$var_41 < quantile(df$var_41, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_41 < quantile(df$var_41, 0.50,na.rm=T) , "C2",
                                                    ifelse(df$var_41 < quantile(df$var_41, 0.75,na.rm=T), "C2","C2" )))))
    
    df$CAT_var_42 <- as.factor(ifelse(is.na(df$var_42), "C0",
                                      ifelse(df$var_42 < quantile(df$var_42, 0.25,na.rm=T), "C1", 
                                             ifelse(df$var_42 < quantile(df$var_42, 0.50,na.rm=T) , "C2",
                                                    ifelse(df$var_42 < quantile(df$var_42, 0.75,na.rm=T), "C2","C2" )))))
    
    
    
    # qtd_contratos_ult_01m
    df$var_53 <- as.numeric(df$var_53)
    
    
    df$CAT_var_54 <- as.factor(ifelse(df$var_54 <= quantile(df$var_54,0.25), "C0",
                                                       ifelse(df$var_54 <= median(df$var_54,na.rm=T)  , "C1", "C2")))
    
    
    
    df$CAT_var_55 <- as.factor(ifelse(df$var_55  == 0, "C0",
                                      ifelse(df$var_55 <= median(df$var_55,na.rm=T)  , "C1",  "C2"))) 
    
    
    df$CAT_var_56 <- as.factor(ifelse(df$var_56  <= quantile(df$var_56, 0.25,na.rm=T), "C0",
                                      ifelse(df$var_56 <= quantile(df$var_56, 0.50,na.rm=T)  , "C0", 
                                             ifelse(df$var_56 <= quantile(df$var_56, 0.75,na.rm=T)  ,"C0", "C3"))))
    
    
    df$CAT_var_57 <- as.factor(ifelse(df$var_57  <= quantile(df$var_57, 0.25,na.rm=T), "C0",
                                      ifelse(df$var_57 <= quantile(df$var_57, 0.50,na.rm=T)  , "C1", 
                                             ifelse(df$var_57 <= quantile(df$var_57, 0.75,na.rm=T)  ,"C2", "C3"))))
    
    df$var_64 <- as.numeric(df$var_64)
    
    df$var_66 <- as.numeric(df$var_66)
    
    df$CAT_var_68 <- as.factor(ifelse(df$var_68  <= quantile(df$var_68, 0.25,na.rm=T), "C0",
                                      ifelse(df$var_68 <= quantile(df$var_68, 0.50,na.rm=T)  , "C1", 
                                             ifelse(df$var_68 <= quantile(df$var_68, 0.75,na.rm=T),"C2", "C3")))) 
    
    ## qtd_parcelas_contratos_ult_12m 
    df$CAT_var_63 <- as.factor(ifelse(is.na(df$var_63), "C0",
                                      ifelse(df$var_63 <= quantile(df$var_63, 0.50,na.rm=T), "C1",  "C2")))
    
    
    df$CAT_var_65 <- as.factor(ifelse(is.na(df$var_65), "C0","C1"))
      
      
    df$CAT_var_67 <- as.factor(ifelse(is.na(df$var_67), "C0",
                                      ifelse(df$var_67 <= quantile(df$var_67, 0.50,na.rm=T), "C1",  "C2")))
    
    
    
    ### qtd_vencimentos_quitados_antecip_ult 
    
    df$CAT_var_69 <- as.factor(ifelse(is.na(df$var_69), "C0",
                                      ifelse(df$var_69 < quantile(df$var_69, 0.50,na.rm=T), "C1", "C1")))
    
    df$CAT_var_70 <- as.factor(ifelse(is.na(df$var_70), "C0",
                                      ifelse(df$var_70 < quantile(df$var_70, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_71 <- as.factor(ifelse(is.na(df$var_71), "C0",
                                      ifelse(df$var_71 < quantile(df$var_71, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_72 <- as.factor(ifelse(is.na(df$var_72), "C0",
                                      ifelse(df$var_72 < quantile(df$var_72, 0.50,na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_73 <- as.factor(ifelse(is.na(df$var_73), "C0",
                                      ifelse(df$var_73 < quantile(df$var_73, 0.50,na.rm=T), "C1", "C2")))
    
    
    
    ### vlr_vencimentos_quitados_antecip_ult_01m_sum
    df$CAT_var_74 <- as.factor(ifelse(is.na(df$var_74), "C0",
                                      ifelse(df$var_74  <= quantile(df$var_74, 0.50,na.rm=T), "C1","C2" )))
    
    
    df$CAT_var_75 <-  as.factor(ifelse(is.na(df$var_75), "C0",
                                       ifelse(df$var_75  <= quantile(df$var_75, 0.50,na.rm=T), "C1","C2" )))
    
    
    df$CAT_var_76 <-  as.factor(ifelse(is.na(df$var_76), "C0",
                                       ifelse(df$var_76  <= quantile(df$var_76, 0.50,na.rm=T), "C1","C2" )))
    
    df$CAT_var_77 <-  as.factor(ifelse(is.na(df$var_77), "C0",
                                       ifelse(df$var_77  <= quantile(df$var_77, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_77 <= quantile(df$var_77, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_77 <= quantile(df$var_77, 0.75,na.rm=T)  ,"C3", "C4")))))
    
    
    df$CAT_var_78 <-  as.factor(ifelse(is.na(df$var_78), "C0",
                                       ifelse(df$var_78  <= quantile(df$var_78, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_78 <= quantile(df$var_78, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_78 <= quantile(df$var_78, 0.75,na.rm=T)  ,"C3", "C4")))))
    
    
    ## vlr_vencimentos_quitados_antecip_ult_01m_avg
    df$CAT_var_79 <- as.factor(ifelse(is.na(df$var_79), "C0",
                                      ifelse(df$var_79  <= quantile(df$var_79, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_80 <- as.factor(ifelse(is.na(df$var_80), "C0",
                                      ifelse(df$var_80  <= quantile(df$var_80, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_81 <- as.factor(ifelse(is.na(df$var_81), "C0",
                                      ifelse(df$var_81  <= quantile(df$var_81, 0.25,na.rm=T), "C1",
                                             ifelse(df$var_81 <= quantile(df$var_81, 0.50,na.rm=T)  , "C2", 
                                                    ifelse(df$var_81 <= quantile(df$var_81, 0.75,na.rm=T)  ,"C2", "C4")))))
    
    df$CAT_var_82 <- as.factor(ifelse(is.na(df$var_82), "C0",
                                      ifelse(df$var_82  <= quantile(df$var_82, 0.25,na.rm=T), "C1",
                                             ifelse(df$var_82 <= quantile(df$var_82, 0.50,na.rm=T)  , "C1", 
                                                    ifelse(df$var_82 <= quantile(df$var_82, 0.75,na.rm=T)  ,"C1", "C4")))))
    
    df$CAT_var_83 <- as.factor(ifelse(is.na(df$var_83), "C0",
                                      ifelse(df$var_83  <= quantile(df$var_83, 0.25,na.rm=T), "C1",
                                             ifelse(df$var_83 <= quantile(df$var_83, 0.50,na.rm=T)  , "C1", 
                                                    ifelse(df$var_83 <= quantile(df$var_83, 0.75,na.rm=T)  ,"C1", "C4")))))
    
    
    #### qtd_vencimentos_quitados_atraso_ult_03m
    
    df$CAT_var_85 <- as.factor(ifelse(is.na(df$var_85), "C0",
                                      ifelse(df$var_85 < quantile(df$var_85, 0.50,na.rm=T), "C1", "C2")))
  
    
    df$CAT_var_86 <- as.factor(ifelse(is.na(df$var_86), "C0",
                                      ifelse(df$var_86 < quantile(df$var_86, 0.50,na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_87 <- as.factor(ifelse(is.na(df$var_87), "C0",
                                      ifelse(df$var_87 < quantile(df$var_87, 0.50,na.rm=T), "C1", "C2")))
    
    
    df$CAT_var_88 <- as.factor(ifelse(is.na(df$var_88), "C0",
                                      ifelse(df$var_88 < quantile(df$var_88, 0.50,na.rm=T), "C1", "C2")))
    
    
    ###vlr_vencimentos_quitados_atraso_ult_03m_sum
    
    df$CAT_var_90 <- as.factor(ifelse(is.na(df$var_90), "C0",
                                      ifelse(df$var_90 < quantile(df$var_90, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_91 <-  as.factor(ifelse(is.na(df$var_91), "C0",
                                       ifelse(df$var_91 < quantile(df$var_91, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_92 <-  as.factor(ifelse(is.na(df$var_92), "C0",
                                       ifelse(df$var_92 < quantile(df$var_92, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_93 <-  as.factor(ifelse(is.na(df$var_93), "C0",
                                       ifelse(df$var_93 < quantile(df$var_93, 0.50,na.rm=T), "C1","C2")))
    
    
    ###  vlr_vencimentos_quitados_atraso_ult_03m_avg
    df$CAT_var_95 <- as.factor(ifelse(is.na(df$var_95), "C0",
                                      ifelse(df$var_95 < quantile(df$var_95, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_96 <- as.factor(ifelse(is.na(df$var_96), "C0",
                                      ifelse(df$var_96 < quantile(df$var_96, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_97 <- as.factor(ifelse(is.na(df$var_97), "C0",
                                      ifelse(df$var_97 < quantile(df$var_97, 0.50,na.rm=T), "C1", "C2")))
    
    df$CAT_var_98 <- as.factor(ifelse(is.na(df$var_98), "C0",
                                      ifelse(df$var_98 < quantile(df$var_98, 0.50,na.rm=T), "C1","C2")))
    
    
    ### qtd_vencimentos_quitados_pontual_ult_01m
    
    df$CAT_var_99 <- as.factor(ifelse(is.na(df$var_99), "C0",
                                      ifelse(df$var_99 < quantile(df$var_99, 0.50,na.rm=T), "C1","C0")))
    
    df$CAT_var_100 <- as.factor(ifelse(is.na(df$var_100), "C0",
                                       ifelse(df$var_100 < quantile(df$var_100, 0.50,na.rm=T), "C1","C1")))
    
    df$CAT_var_101 <- as.factor(ifelse(is.na(df$var_101), "C0",
                                       ifelse(df$var_101 < quantile(df$var_101, 0.50,na.rm=T), "C1","C1")))
    
    df$CAT_var_102 <- as.factor(ifelse(is.na(df$var_102), "C0",
                                       ifelse(df$var_102  <= quantile(df$var_102, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_102 <= quantile(df$var_102, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_102 <= quantile(df$var_102, 0.75,na.rm=T)  ,"C2", "C0")))))
    
    
    
    df$CAT_var_103 <- as.factor(ifelse(is.na(df$var_103), "C0",
                                       ifelse(df$var_103  <= quantile(df$var_103, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_103 <= quantile(df$var_103, 0.50,na.rm=T)  , "C0", 
                                                     ifelse(df$var_103 <= quantile(df$var_103, 0.75,na.rm=T)  ,"C3", "C4")))))
    
    
    
    
    
    ## vlr_vencimentos_quitados_pontual_ult_01m_sum
    
    df$CAT_var_104 <- as.factor(ifelse(is.na(df$var_104), "C0",
                                       ifelse(df$var_104 < quantile(df$var_104, 0.50,na.rm=T), "C1","C0")))
    
    df$CAT_var_105 <- as.factor(ifelse(is.na(df$var_105), "C0",
                                       ifelse(df$var_105 < quantile(df$var_105, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_106 <- as.factor(ifelse(is.na(df$var_106), "C0",
                                       ifelse(df$var_106 < quantile(df$var_106, 0.50,na.rm=T), "C1","C1")))
    
    df$CAT_var_107 <- as.factor(ifelse(is.na(df$var_107), "C0",
                                       ifelse(df$var_107 < quantile(df$var_107, 0.50,na.rm=T), "C1","C1")))
    
    df$CAT_var_108 <- as.factor(ifelse(is.na(df$var_108), "C0",
                                       ifelse(df$var_108  <= quantile(df$var_108, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_108 <= quantile(df$var_108, 0.50,na.rm=T)  , "C1", 
                                                     ifelse(df$var_108 <= quantile(df$var_108, 0.75,na.rm=T)  ,"C3", "C3")))))
    
    
    
    ## vlr_vencimentos_quitados_pontual_ult_01m_avg
    
    df$CAT_var_109 <- as.factor(ifelse(is.na(df$var_109), "C0",
                                       ifelse(df$var_109 < quantile(df$var_109, 0.50,na.rm=T), "C1","C0")))
    
    
    df$CAT_var_110 <- as.factor(ifelse(is.na(df$var_110), "C0",
                                       ifelse(df$var_110 < quantile(df$var_110, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_111 <- as.factor(ifelse(is.na(df$var_111), "C0",
                                       ifelse(df$var_111 < quantile(df$var_111, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_112 <- as.factor(ifelse(is.na(df$var_112), "C0",
                                       ifelse(df$var_112  <= quantile(df$var_112, 0.25,na.rm=T), "C0",
                                              ifelse(df$var_112 <= quantile(df$var_112, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_112 <= quantile(df$var_112, 0.75,na.rm=T)  ,"C2", "C4")))))
    
    
    df$CAT_var_113 <- as.factor(ifelse(is.na(df$var_113), "C0",
                                       ifelse(df$var_113  <= quantile(df$var_113, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_113 <= quantile(df$var_113, 0.50,na.rm=T)  , "C0", 
                                                     ifelse(df$var_113 <= quantile(df$var_113, 0.75,na.rm=T)  ,"C0", "C4")))))
    
    
    ### qtd_vencimentos_quitados_ult_01m
    
    df$CAT_var_199 <- as.factor(ifelse(is.na(df$var_199), "C0",
                                       ifelse(df$var_199 < quantile(df$var_199, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_200 <- as.factor(ifelse(is.na(df$var_200), "C0",
                                       ifelse(df$var_200  <= quantile(df$var_200, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_200 <= quantile(df$var_200, 0.50,na.rm=T)  , "C1", 
                                                     ifelse(df$var_200 <= quantile(df$var_200, 0.75,na.rm=T)  ,"C1", "C4")))))
    
    
    df$CAT_var_201 <- as.factor(ifelse(is.na(df$var_201), "C0",
                                       ifelse(df$var_201  <= quantile(df$var_201, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_201 <= quantile(df$var_201, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_201 <= quantile(df$var_201, 0.75,na.rm=T)  ,"C2", "C4")))))
    
    
    ## vlr_vencimentos_quitados_ult_01m_sum
    
    df$CAT_var_202 <- as.factor(ifelse(df$var_202  <= quantile(df$var_202, 0.25,na.rm=T), "C0",
                                       ifelse(df$var_202 <= quantile(df$var_202, 0.50,na.rm=T)  , "C1", 
                                              ifelse(df$var_202 <= quantile(df$var_202, 0.75,na.rm=T)  ,"C2", "C3"))))
    
    
    df$CAT_var_203 <- as.factor(ifelse(df$var_203  <= quantile(df$var_203, 0.25,na.rm=T), "C0",
                                       ifelse(df$var_203 <= quantile(df$var_203, 0.50,na.rm=T)  , "C1", 
                                              ifelse(df$var_203 <= quantile(df$var_203, 0.75,na.rm=T)  ,"C2", "C2"))))
    
    
    
    
    df$CAT_var_204 <- as.factor(ifelse(is.na(df$var_204), "C0",
                                       ifelse(df$var_204  <= quantile(df$var_204, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_204 <= quantile(df$var_204, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_204 <= quantile(df$var_204, 0.75,na.rm=T)  ,"C3", "C3")))))
    
    
    
    df$CAT_var_205 <- as.factor(ifelse(is.na(df$var_205), "C0",
                                       ifelse(df$var_205  <= quantile(df$var_205, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_205 <= quantile(df$var_205, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_205 <= quantile(df$var_205, 0.75,na.rm=T)  ,"C3", "C2")))))
    
    
    
    df$CAT_var_206 <- as.factor(ifelse(is.na(df$var_206), "C0",
                                       ifelse(df$var_206  <= quantile(df$var_206, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_206 <= quantile(df$var_206, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_206 <= quantile(df$var_206, 0.75,na.rm=T)  ,"C2", "C1")))))
    
    
    
    df$CAT_var_207 <- as.factor(ifelse(df$var_207  <= quantile(df$var_207, 0.25,na.rm=T), "C0",
                                       ifelse(df$var_207 <= quantile(df$var_207, 0.50,na.rm=T)  , "C1", 
                                              ifelse(df$var_207 <= quantile(df$var_207, 0.75,na.rm=T)  ,"C2", "C2"))))
    
    
    df$CAT_var_208 <- as.factor(ifelse(df$var_208  <= quantile(df$var_208, 0.25,na.rm=T), "C0",
                                       ifelse(df$var_208 <= quantile(df$var_208, 0.50,na.rm=T)  , "C1", 
                                              ifelse(df$var_208 <= quantile(df$var_208, 0.75,na.rm=T)  ,"C2", "C2"))))
    
    
    ### vlr_vencimentos_quitados_ult_01m_max
    
    
    df$CAT_var_209 <- as.factor(ifelse(is.na(df$var_209), "C0",
                                       ifelse(df$var_209 < quantile(df$var_209, 0.50,na.rm=T), "C1","C2")))
    
    
    
    df$CAT_var_210 <- as.factor(ifelse(is.na(df$var_210), "C0",
                                       ifelse(df$var_210 < quantile(df$var_210, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_211 <- as.factor(ifelse(is.na(df$var_211), "C0",
                                       ifelse(df$var_211 < quantile(df$var_211, 0.50,na.rm=T), "C1","C2")))
    
    
    df$CAT_var_212 <- as.factor(ifelse(df$var_212 < quantile(df$var_212, 0.50,na.rm=T), "C0","C1"))
    
    df$var_213 <- as.numeric(df$var_213)
    
    
    
    ## vlr_vencimentos_quitados_ult_01m_avg
    
    df$CAT_var_214 <- as.factor(ifelse(is.na(df$var_214), "C0",
                                       ifelse(df$var_214 < quantile(df$var_214, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_215 <- as.factor(ifelse(is.na(df$var_215), "C0",
                                       ifelse(df$var_215 < quantile(df$var_215, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_216 <- as.factor(ifelse(is.na(df$var_216), "C0",
                                       ifelse(df$var_216 < quantile(df$var_216, 0.50,na.rm=T), "C1","C2")))
    
    df$CAT_var_217 <-  as.factor(ifelse(df$var_217  <= quantile(df$var_217, 0.25,na.rm=T), "C0",
                                        ifelse(df$var_217 <= quantile(df$var_217, 0.50,na.rm=T)  , "C1", 
                                               ifelse(df$var_217 <= quantile(df$var_217, 0.75,na.rm=T)  ,"C1", "C3"))))
    
    df$CAT_var_218 <-  as.factor(ifelse(df$var_218  <= quantile(df$var_218, 0.25,na.rm=T), "C0",
                                        ifelse(df$var_218 <= quantile(df$var_218, 0.50,na.rm=T)  , "C1", 
                                               ifelse(df$var_218 <= quantile(df$var_218, 0.75,na.rm=T)  ,"C1", "C3"))))
    
    
    ## vlr_vencimentos_quitados_ult_01m_min
    df$CAT_var_219 <- as.factor(ifelse(is.na(df$var_219), "C0",
                                       ifelse(df$var_219  <= quantile(df$var_219, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_219 <= quantile(df$var_219, 0.50,na.rm=T)  , "C1", 
                                                     ifelse(df$var_219 <= quantile(df$var_219, 0.75,na.rm=T)  ,"C1", "C1")))))
    
    
    df$CAT_var_220 <- as.factor(ifelse(is.na(df$var_220), "C0",
                                       ifelse(df$var_220  <= quantile(df$var_220, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_220 <= quantile(df$var_220, 0.50,na.rm=T)  , "C1", 
                                                     ifelse(df$var_220 <= quantile(df$var_220, 0.75,na.rm=T)  ,"C1", "C1")))))
    
    
    df$CAT_var_221 <- as.factor(ifelse(is.na(df$var_221), "C0",
                                       ifelse(df$var_221  <= quantile(df$var_221, 0.25,na.rm=T), "C1",
                                              ifelse(df$var_221 <= quantile(df$var_221, 0.50,na.rm=T)  , "C2", 
                                                     ifelse(df$var_221 <= quantile(df$var_221, 0.75,na.rm=T)  ,"C1", "C2")))))
    
    
    
    df$var_222 <- as.numeric(df$var_222)
    df$var_223 <- as.numeric(df$var_223)
    df$CAT_var_224 <- as.factor(ifelse(df$var_224 < quantile(df$var_224, 0.5,na.rm=T), "C0","C1"))
    
    
    ### Porcentagens 
    df$CAT_var_227 <- as.factor(ifelse(is.na(df$var_227), "C0",
                                       ifelse(df$var_227 <= median(df$var_227, na.rm = T),"C1","C2")))
    
    df$CAT_var_228 <- as.factor(ifelse(is.na(df$var_228), "C0", "C1"))
    
    df$CAT_var_229 <- as.factor(ifelse(is.na(df$var_229), "C0",
                                       ifelse(df$var_229 < quantile(df$var_229, 0.5,na.rm=T), "C0","C2")))
    
    df$CAT_var_230 <- as.factor(ifelse(is.na(df$var_230), "C0",
                                       ifelse(df$var_230 < quantile(df$var_230, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_231 <- as.factor(ifelse(is.na(df$var_231), "C0",
                                       ifelse(df$var_231 < quantile(df$var_231, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_232 <- as.factor(ifelse(is.na(df$var_232), "C0",
                                       ifelse(df$var_232 < quantile(df$var_232, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_233 <- as.factor(ifelse(is.na(df$var_233), "C0",
                                       ifelse(df$var_233 < quantile(df$var_233, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_236 <- as.factor(ifelse(is.na(df$var_236), "C0",
                                       ifelse(df$var_236 < quantile(df$var_236, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_239 <- as.factor(ifelse(is.na(df$var_239), "C0",
                                       ifelse(df$var_239 < quantile(df$var_239, 0.5,na.rm=T), "C1","C1")))
    
    df$CAT_var_240 <- as.factor(ifelse(is.na(df$var_240), "C0",
                                       ifelse(df$var_240 < quantile(df$var_240, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_241 <- as.factor(ifelse(is.na(df$var_241), "C0",
                                       ifelse(df$var_241 < quantile(df$var_241, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_242 <- as.factor(ifelse(is.na(df$var_242), "C0",
                                       ifelse(df$var_242 < quantile(df$var_242, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_243 <- as.factor(ifelse(is.na(df$var_243), "C0",
                                       ifelse(df$var_243 < quantile(df$var_243, 0.5,na.rm=T), "C1","C2")))
    
    df$CAT_var_244 <- as.factor(ifelse(is.na(df$var_244), "C0",
                                       ifelse(df$var_244 < quantile(df$var_244, 0.5,na.rm=T), "C1","C2")))
    
    
    # df <- subset(df,select = -c(var_13,var_14,var_15,var_16,var_17,var_18 ,var_19, var_20,   var_21  ,var_22  ,var_33        
    #                              , var_34  , var_35  ,var_36,   var_37,     var_38,           var_39
    #                             ,  var_40  , var_41  ,var_42,     var_54,           var_55
    #                             ,  var_56  , var_57  ,var_63,   var_65
    #                             ,  var_67  , var_68  ,var_69,   var_70,     var_71,           var_72
    #                             ,  var_73  , var_74  ,var_75,   var_76,     var_77,           var_78
    #                             ,  var_79  , var_80  ,var_81,   var_82,     var_83,           var_85
    #                             ,  var_86  , var_87  ,var_88,   var_90,     var_91,           var_92
    #                             ,  var_93  , var_95  ,var_96 ,  var_97 ,    var_98,           var_99        
    #                             ,  var_100 , var_101 ,var_102,  var_103,    var_104,          var_105       
    #                             ,  var_106 , var_107 ,var_108,  var_109,    var_110,          var_111       
    #                             ,  var_112 , var_113 ,var_199,  var_200,    var_201,          var_202       
    #                             ,  var_203 , var_204 ,var_205,  var_206,    var_207,          var_208       
    #                             ,  var_209 , var_210 ,var_211,  var_212,    var_213,          var_214       
    #                             ,  var_215 , var_216 ,var_217,  var_218,    var_219,          var_220       
    #                             ,  var_221 , var_222, var_223,  var_224,    var_227,          var_228       
    #                             ,  var_229 , var_230, var_231,  var_232,    var_233,          var_236       
    #                            ,  var_239 , var_240, var_241,  var_242,    var_243,          var_244 
    #                            , var_54, var_55, var_56, var_57, var_64, var_66
    #                            , var_202,var_203,  var_207, var_208,var_212,var_213,var_217,var_218, var_222,var_223,var_224
    #                          ))
    
    df <- subset(df,select = -c(var_54,
                                 var_55,
                                 var_56,
                                 var_57,
                                 var_68,
                                 var_202,
                                 var_203,
                                 var_207,
                                 var_208,
                                 var_212,
                                 var_217,
                                 var_218,
                                 var_224) )                            
    
    
    
    
    
    
    
    # TRATAMENTO DAS VARIAVEIS NÚMERICAS --------------------------------------
    ## Renda pressumida
    pos=which(is.na(df$var_251))
    
    #df$var_resposta[pos]
    media_renda_var0 <- mean(df$var_251[which(!is.na(df$var_251) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_251))) {
      df$var_251[which(is.na(df$var_251))] <- media_renda_var0
    }
    
    ## Score de Propensão de pagamento
    #table(df$var_252,exclude=NULL)
    pos=which(is.na(df$var_252))
    #df$var_resposta[pos]
    media_scorepp_var0 <- mean(df$var_252[which(!is.na(df$var_252) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_252))) {
      df$var_252[which(is.na(df$var_252))] <- media_scorepp_var0
    }
    
    
    ## Score de % de domicilios com acesso a tratamento esgoto
    #table(df$var_377,exclude=NULL)
    pos=which(is.na(df$var_377))
    #df$var_resposta[pos]
    media_esgoto_var0 <- mean(df$var_377[which(!is.na(df$var_377) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_377))) {
      df$var_377[which(is.na(df$var_377))] <- media_esgoto_var0
    }
    
    
    ## Score de % de rede eletrica em situações de não estabilizade
    #table(df$var_378,exclude=NULL)
    pos=which(is.na(df$var_378))
    #df$var_resposta[pos]
    media_redeeletrica_var0 <- mean(df$var_378[which(!is.na(df$var_378) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_378))) {
      df$var_378[which(is.na(df$var_378))] <- media_redeeletrica_var0
    }
    
    
    # qtde_cnpj_raiz_distintos_ativa
    table(df$var_264,exclude=NULL)
    pos=which(is.na(df$var_264))
    table(df$var_resposta[pos])
    if (any(is.na(df$var_264))) {
      df$var_264[which(is.na(df$var_264))] <- 0
    }
    
    ### var_272, qtde_cnpj_raiz_distintos_ativa
    
    table(df$var_272,exclude=NULL)
    pos=which(is.na(df$var_272))
    table(df$var_resposta[pos])
    if (any(is.na(df$var_272))) {
      df$var_272[which(is.na(df$var_272))] <- 0
      
    }
    
    ## var_275: auxilios 
    #table(df$var_275,exclude=NULL) # 1 tem auxilio e 0 não tem
    #pos=which(is.na(df$var_275))
    ##table(df$var_resposta[pos])
    #if (any(is.na(df$var_275))) {
    #  df$var_275[which(is.na(df$var_275) & df$var_resposta == 1)] <- 1
    #  df$var_275[which(is.na(df$var_275) & df$var_resposta == 0)] <- 0
    #}
    
    df$CAT_var_275 <- as.factor(ifelse(is.na(df$var_275), "C0",
                                       ifelse(df$var_275 == 1,"C1","C2")))
    
    
    ## var_306, grau de urbanização
    
    #table(df$var_306,exclude=NULL)
    pos=which(is.na(df$var_306))
    #table(df$var_resposta[pos])
    media <- mean(df$var_306[which(!is.na(df$var_306) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_306))) {
      df$var_306[which(is.na(df$var_306))] <- media
    }
    
    
    ## var_418, arborização
    
    
    pos=which(is.na(df$var_418))
    table(df$var_resposta[pos])
    media <- mean(df$var_418[which(!is.na(df$var_418) & df$var_resposta==0)])  ## 4078.684
    
    if (any(is.na(df$var_418))) {
      df$var_418[which(is.na(df$var_418))] <- media
    }
    
    ## var_421, % de esgoto a ceu aberto
    pos=which(is.na(df$var_421))
    #table(df$var_resposta[pos])
    media0 <- mean(df$var_421[which(!is.na(df$var_421) & df$var_resposta==0)])  ## 4078.684
    media1 <- mean(df$var_421[which(!is.na(df$var_421) & df$var_resposta==1)])  ## 4078.684
    if (any(is.na(df$var_421))) {
      df$var_421[which(is.na(df$var_421) & df$var_resposta == 1)] <- media1
      df$var_421[which(is.na(df$var_421) & df$var_resposta == 0)] <- media0
    }
    
    
    ## var_423: iluminação pública 
    pos=which(is.na(df$var_423))
    #table(df$var_resposta[pos])
    media0 <- mean(df$var_423[which(!is.na(df$var_423) & df$var_resposta==0)])  
    media1 <- mean(df$var_423[which(!is.na(df$var_423) & df$var_resposta==1)])  
    
    if (any(is.na(df$var_423))) {
      df$var_423[which(is.na(df$var_423) & df$var_resposta == 1)] <- media1
      df$var_423[which(is.na(df$var_423) & df$var_resposta == 0)] <- media0
    }
    
    ## var_424: pct pavimentação 
    pos=which(is.na(df$var_424))
    #table(df$var_resposta[pos])
    media0 <- mean(df$var_424[which(!is.na(df$var_424) & df$var_resposta==0)])  
    media1 <- mean(df$var_424[which(!is.na(df$var_424) & df$var_resposta==1)])  
    
    if (any(is.na(df$var_424))) {
      df$var_424[which(is.na(df$var_424) & df$var_resposta == 1)] <- media1
      df$var_424[which(is.na(df$var_424) & df$var_resposta == 0)] <- media0
    }
    
    # demais manipulações
    df$CAT_var_1 = as.factor(ifelse(df$var_1=="masculino","C1","C0"))
    df$CAT_var_339=as.factor(df$var_339)
    df$CAT_var_325=as.factor(df$var_325)
    
    df$CAT_var_304 <- as.factor(ifelse(df$var_304 != "Padrao", "C0", "C1"))
    df$CAT_var_309 <- as.factor(ifelse(df$var_309 != "Urbano", "C0", "C1"))
    df$CAT_var_365 <- as.factor(ifelse(df$var_365 != "C", "C0", "C1"))
    
    
    df$var_272 <- as.numeric(df$var_272)
    df$var_338 = as.numeric(df$var_338)
    df$var_340 <- as.numeric(df$var_340)
    df$var_341 <- as.numeric(df$var_341)
    df$var_342 <- as.numeric(df$var_342)
    df$var_343 <- as.numeric(df$var_343)
    df$var_359 <- as.numeric(df$var_359)
    df$var_360 <- as.numeric(df$var_360)
    
    df=subset(df, select=-c(var_367,var_489,var_275,var_325,var_365
                            ,var_309,var_304,var_1,var_339,var_258,var_305,var_308,var_259))
    
    
    replace_negative_age <- function(x) {
      ifelse(x < 0, floor(mean(df$var_2 , na.rm=T)), x)
    }
    df$var_2 <- replace_negative_age(df$var_2)
    
    
    porcentagem_zero <- c()
    NAs <- matrix(rep(0,2*ncol(df)),ncol=2)
    for (i in 1:ncol(df)){
      NAs[i,1] <- colnames(df)[i]} 
    for (i in 1:ncol(df)){
      porcentagem_zero <- c(porcentagem_zero,sum(is.na(df[,i]))/nrow(df))
      NAs[i,2] <- sum(is.na(df[,i]))
      
    }
    
    df = df[,NAs[,1][NAs[,2] == 0]]
    df$var_resposta1 = 1- df$var_resposta
    df$var_resposta = as.factor(df$var_resposta)
    
    return(df)
    
}


library(arrow)
df <- arrow::read_parquet("G:/Meu Drive/8ºSEMESTRE/TG/Base-TG/dados_DES_OOS_OOT.parquet")

base_tratada <- tratamento_dados(df)
base = base_tratada

arrow::write_parquet(base, "H:/Meu Drive/9º SEMESTRE/TG/bases/base_V7.parquet")


# Usando a função model.matrix para converter as colunas categóricas em dummies
dados_dummies <- model.matrix(~ . , data = base_tratada[,-c(1,2,3,4,220)])

# Combinando as variáveis numéricas originais com as variáveis dummies
base_tratada <- cbind(base_tratada[,c(1,2,3,4,220)],dados_dummies)
base_tratada <- subset(base_tratada,select=-c(`(Intercept)`))
arrow::write_parquet(base_tratada, "H:/Meu Drive/9º SEMESTRE/TG/bases/base_dummies_V7.parquet")



matriz <- matrix(NA, ncol = 3, nrow= length(100:ncol(base_tratada)))
for (i in 100:ncol(base_tratada)){
  matriz[i-99,1] <-  colnames(base_tratada)[i]
  matriz[i-99,2] <-  table(base_tratada[i])[1] # conta valores 0 
  matriz[i-99,3] <-  table(base_tratada[i])[2] # conta valores 1
}

# CAT_var_339TRUE
# CAT_var_65
data = data.frame(matriz)
data$X2 <- as.numeric(data$X2)
data$X3 <- as.numeric(data$X3)
