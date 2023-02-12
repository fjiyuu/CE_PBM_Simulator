# 入力を0-1の一様乱数で入れると内部で一般化第一種ベータ分布に変換して計算
model_CE_beta <-
  function(r_drusA_max_01, epsilon_max_01, r_rmn_max_01, r_rcy_max_01, 
           r_rcy2_max_01, w_max_01, r_LE_max_01,cr_o_01,sr_B_01,
           Rental_period_01,Rir_01,CE_period_01,sr_D=0.5,cr_r=1.0){
    # browser()
    k <- 7
    r_drusA_max <- trans_beta(r_drusA_max_01,k=k,mode=77609/488046,min=0,max=1)
    epsilon_max <- trans_beta(epsilon_max_01,k=k,mode=3952/100398,min=0,max=1)
    r_rmn_max <- trans_beta(r_rmn_max_01,k=k,mode=0,min=0,max=1)
    r_rcy_max <- trans_beta(r_rcy_max_01,k=k,mode=88260/96446,min=0,max=1)
    w_max <- trans_beta(w_max_01,k=k,mode=0,min=0,max=1)
    r_LE_max <- trans_beta(r_LE_max_01,k=k,mode=0,min=-0.2,max=0.5)
    r_rcy2_max <- trans_beta(r_rcy2_max_01,k=k,mode=138996/311039,min=0,max=1)
    cr_o <- trans_beta(cr_o_01,k=k,mode=100398/488046,min=0,max=1)
    sr_B <- trans_beta(sr_B_01,k=k,mode=0.7,min=0.3,max=0.9)
    Rental_period <- trans_beta(Rental_period_01,k=k,mode=0.25,min=1/28,max=1)
    Rir <- trans_beta(Rir_01,k=k,mode=1.5,min=1,max=2) #デジカメ
    CE_period <- trans_beta(CE_period_01,k=k,mode=25,min=10,max=30)
    model_CE(r_drusA_max=r_drusA_max, epsilon_max=epsilon_max, r_rmn_max=r_rmn_max,
                     r_rcy_max=r_rcy_max, r_rcy2_max=r_rcy2_max, w_max=w_max,
                     r_LE_max=r_LE_max,cr_o=cr_o, sr_B=sr_B,Rental_period=Rental_period,
                     Rir=Rir,CE_period=CE_period,sr_D=sr_D,cr_r=cr_r) %>% return()
  }

model_CE_beta_py <- 
  function(r_drusA_max_01, epsilon_max_01, r_rmn_max_01, r_rcy_max_01, 
           r_rcy2_max_01, w_max_01, r_LE_max_01,cr_o_01,sr_B_01,
           Rental_period_01,Rir_01,CE_period_01,sr_D=0.5,cr_r=1.0){
    # browser()
    df_temp <- model_CE_beta(r_drusA_max_01=r_drusA_max_01, epsilon_max_01=epsilon_max_01, 
                                     r_rmn_max_01=r_rmn_max_01, r_rcy_max_01=r_rcy_max_01,
                                     r_rcy2_max_01=r_rcy2_max_01, w_max_01=w_max_01, 
                                     r_LE_max_01=r_LE_max_01,cr_o_01=cr_o_01,sr_B_01=sr_B_01,
                                     Rental_period_01=Rental_period_01,Rir_01=Rir_01,CE_period_01=CE_period_01,
                                     sr_D=0.5,cr_r=1.0)
    c_GHG_BD <- df_temp$df_GHG[[1]] %>% filter(Year>2020) %>%
      summarise_at(vars(GHG_MaterialProduction:GHG_Recycle), sum) %>%
      pivot_longer(cols=everything()) %>% .$value
    c(df_temp$TMR_sum[[1]],df_temp$FD_sum[[1]],df_temp$GHG_sum[[1]],c_GHG_BD) %>% return()
  }
