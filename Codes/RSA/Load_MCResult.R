if(tgt_product=="デジタルカメラ"){
  result_path <- str_c("Outputs/Results/",tgt_product,"/RSA/MonteCarlo/Beta_dgc_0112_12000.csv")
} else if(tgt_product=="スマートフォン"){
  result_path <- str_c("Outputs/Results/",tgt_product,"/RSA/MonteCarlo/Beta_smp_0112_12000.csv")
} else{
  print("ERROR")
}

# filter前
df_result_temp <- 
  str_c(path,result_path) %>% read.csv()
# Beta分布に変換して計算している変数について，こちらのデータも変換
k <- 7
df_result_temp_2 <- df_result_temp %>% as_tibble() %>% 
  mutate(r_drusA_max = trans_beta(r_drusA_max_01,k=k,mode=77609/488046,min=0,max=1),
         epsilon_max = trans_beta(epsilon_max_01,k=k,mode=3952/100398,min=0,max=1),
         r_rmn_max = trans_beta(r_rmn_max_01,k=k,mode=0,min=0,max=1),
         r_rcy_max = trans_beta(r_rcy_max_01,k=k,mode=88260/96446,min=0,max=1),
         w_max = trans_beta(w_max_01,k=k,mode=0,min=0,max=1),
         r_LE_max = trans_beta(r_LE_max_01,k=k,mode=0,min=-0.2,max=0.5),
         r_rcy2_max = trans_beta(r_rcy2_max_01,k=k,mode=138996/311039,min=0,max=1),
         cr_o = trans_beta(cr_o_01,k=k,mode=100398/488046,min=0,max=1),
         sr_B = trans_beta(sr_B_01,k=k,mode=0.7,min=0.3,max=0.9),
         Rental_period = trans_beta(Rental_period_01,k=k,mode=0.25,min=1/28,max=1),
         Rir = trans_beta(Rir_01,k=k,mode=1.5,min=1,max=2), 
         CE_period = trans_beta(CE_period_01,k=k,mode=25,min=10,max=30)
  ) %>% select(-ends_with("_01"))

# filtering
df_result_filtered_temp <- df_result_temp_2 %>% 
  mutate(zzz= cr_o+r_drusA_max) %>% 
  filter(zzz<=1) %>% select(-zzz)
df_result_filtered <- df_result_filtered_temp %>% head(10000) %>% 
  select(-c(GHG_MaterialProduction:GHG_Recycle)) %>% 
  select(r_drusA_max,epsilon_max,r_rmn_max,r_rcy_max,r_rcy2_max,w_max,r_LE_max,
       cr_o,sr_B,Rental_period,Rir,CE_period,TMR_sum,FD_sum,GHG_sum)
