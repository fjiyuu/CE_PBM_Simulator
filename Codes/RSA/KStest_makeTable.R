for (tgt_product in product_list){
  # BorderRatio <- 10
  for (BorderRatio in c(10,90)){
    df_KS_TMR_temp <- str_c(path,"Outputs/Results/",tgt_product,"/RSA/KSResTable/",
                            tgt_product,"_KStest",BorderRatio,"_","TMR.xlsx") %>% read_excel() %>% 
      rename(p_TMR=p値,d_TMR=検定統計量d)
    df_KS_FD_temp <- str_c(path,"Outputs/Results/",tgt_product,"/RSA/KSResTable/",
                           tgt_product,"_KStest",BorderRatio,"_","FD.xlsx") %>% read_excel() %>% 
      rename(p_FD=p値,d_FD=検定統計量d)
    df_KS_GHG_temp <- str_c(path,"Outputs/Results/",tgt_product,"/RSA/KSResTable/",
                            tgt_product,"_KStest",BorderRatio,"_","GHG.xlsx") %>% read_excel() %>% 
      rename(p_GHG=p値,d_GHG=検定統計量d)
    
    df_KS2 <- df_KS_TMR_temp %>% full_join(df_KS_FD_temp,by="変数") %>% 
      full_join(df_KS_GHG_temp,by="変数") %>% 
      mutate_at(vars(starts_with("p")), ~ case_when(.<0.001 ~ "< 0.001",
                                                    TRUE ~ as.character(.))) 
    
    df_KS2 %>% 
      write.xlsx(file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/KSResTable/",
                            tgt_product,"_KStest_All_revised",BorderRatio,".xlsx"))
  }
}