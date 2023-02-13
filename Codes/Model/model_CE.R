### 製品を選択
tgt_product <- "デジタルカメラ"
# tgt_product <- "スマートフォン"

# Preparation -------------------------------------------------------------
# General data
df_Demand <- make_df_Demand(df_bau,df_historical)
df_params_template <- 
  str_c(path,"Inputs/Parameters/params_template.csv") %>% 
  read_csv()
GHG_rmn_ratio <- 0.005 #製品製造に対するGHG係数の割合0.5%
GHG_rcy_ratio <- 0.0 #製品製造に対するGHG係数の割合0%
transAT_dist <- 100 #リユース・リマン・リサイクル後の輸送距離、仮置き

df_GHGcoef_read <- 
  str_c(path,"Inputs/Data/df_GHGcoef.xlsx") %>%
  read_xlsx(sheet="df_GHGcoef") %>% 
  mutate(Remanufacturing=(MaterialProduction+Assembly)*GHG_rmn_ratio,
         Recycling=(MaterialProduction+Assembly)*GHG_rcy_ratio)
# 素材製造のGHG排出（各元素ごと）[kg-CO2 eq./kg]
df_GHG_element <- str_c(path,"Inputs/Data/df_GHG_element.csv") %>% 
  read_csv() %>% replace_na(list(GHG_MatProd_elem=0))
df_composition_all <- make_df_composition_all(path)
df_TMR_coef <- str_c(path,"Inputs/Data/TMR_coef.csv") %>% read_csv() %>% 
  replace_na(list(TMR_coef=0))

# Specific data for products
prod_weight <- case_when(tgt_product=="デジタルカメラ" ~ 0.22, #compは0.22, DSLRは0.51[kg]
                         tgt_product=="スマートフォン" ~ 0.16)
df_GHG_coef <- df_GHGcoef_read %>% filter(Product==tgt_product)
# df_TMR_Prod <- make_df_TMR_Prod(path,df_composition_all,df_TMR_coef)
# TMR_Prod <- df_TMR_Prod %>% filter(Product==tgt_product) %>%
#   pull(TMR_Prod)# 製品1台あたりのTMR
df_composition_temp <- df_composition_all %>% filter(Product==tgt_product) %>% 
  unnest(Comp_prod) %>% select(-Product,comp_prod=value)
Mass_metal_sum <- df_composition_temp %>% summarise(Mass_metal_sum=sum(comp_prod)) %>% 
  pull(Mass_metal_sum)
df_composition <- df_composition_temp %>% 
  add_row(num=0,sym="Others",element="その他",comp_prod=prod_weight-Mass_metal_sum)
comp_prod <- df_composition$comp_prod
# df_material_GHG: 各材料が背負うGHG排出量[kgCO2eq/台]
if(tgt_product=="デジタルカメラ"){
  rawdata_path <- "Inputs/Data/Remanufacturing/Dgc_material_IDEA_rawdata.csv"
  mat_elem_comp_path <- "Inputs/Data/Remanufacturing/Dgc_material_element_composition.csv"
  mat_comp_path <- "Inputs/Data/Remanufacturing/Dgc_material_composition.csv"
  Reman_mat_list <- c("普通鋼","電磁鋼板","ステンレス鋼","アルミニウム","ガラス")
  # Reman_mat_list <- c("普通鋼","電磁鋼板","ステンレス鋼","アルミニウム","ガラス","電池")
  #リマン歩留を算出する
  df_Dgc_mat_elem_comp_read <- 
    str_c(path,mat_elem_comp_path) %>% read_csv() %>% mutate_if(is.numeric, ~replace_na(.,0))
  df_Dgc_mat_elem_comp <- df_TMR_coef %>% select(-TMR_coef) %>% left_join(df_Dgc_mat_elem_comp_read,by="sym") %>% 
    mutate_if(is.numeric, ~replace_na(.,0))
  df_y_rmn <- df_Dgc_mat_elem_comp %>% rowwise() %>% 
    mutate(Reman_mat_sum=sum(c_across(all_of(Reman_mat_list))),
           All_sum=sum(c_across(普通鋼:実装回路基板)),
           y_rmn=case_when(All_sum==0 ~ 0,
                           TRUE ~ Reman_mat_sum/All_sum)
    ) %>% ungroup() %>% 
    select(sym,y_rmn)
  y_rmn <- df_y_rmn$y_rmn
  df_material <- make_df_material(tgt_product=tgt_product,path=path,rawdata_path,mat_comp_path)
  df_material_GHG <- df_material %>% 
    mutate(GHG_ratio=GHG/sum(GHG),
           GHG_new=GHG_ratio*df_GHG_coef$MaterialProduction) %>% select(Material,GHG=GHG_new)
} else if (tgt_product=="スマートフォン"){
  Reman_mat_list <- c("Metal frame and case: Aluminum","Metal frame and case: Steel",
                      "LCD") 
  mat_elem_comp_path <- "Inputs/Data/Remanufacturing/Smp_material_element_composition.csv"
  df_Smp_mat_elem_comp_read <- 
    str_c(path,mat_elem_comp_path) %>% read_csv() %>% mutate_if(is.numeric, ~replace_na(.,0))
  df_Smp_mat_elem_comp <- df_TMR_coef %>% select(-TMR_coef) %>% left_join(df_Smp_mat_elem_comp_read,by="sym") %>% 
    mutate_if(is.numeric, ~replace_na(.,0))
  df_y_rmn <- df_Smp_mat_elem_comp %>% rowwise() %>% 
    mutate(Reman_mat_sum=sum(c_across(all_of(Reman_mat_list))),
           All_sum=sum(c_across(`Plastic elements`:`Other parts`)),
           y_rmn=case_when(All_sum==0 ~ 0,
                           TRUE ~ Reman_mat_sum/All_sum)
    ) %>% ungroup() %>% 
    select(sym,y_rmn)
  y_rmn <- df_y_rmn$y_rmn
  df_material_GHG <- 
    str_c(path,"Inputs/Data/Remanufacturing/Smp_material_composition.csv") %>% read_csv() %>% 
    mutate(`GWP[kgCO2eq]`=`GHG[g/part]`/1000) %>% 
    select(Material=材料,GHG=`GWP[kgCO2eq]`)
} else{
  print("Error")
}

y_rcy <- str_c(path,'Inputs/Data/df_y_rcy.csv') %>% read_csv() %>% .$y_rcy
# RemanによるGHG削減量
df_GHG_coef$ReducReman <- df_material_GHG %>% filter(Material %in% Reman_mat_list) %>% 
  summarise(GHG_Reduc_Reman=sum(GHG))

# Model -------------------------------------------------------------------

model_CE <-
  function(r_drusA_max=77609/488046, epsilon_max=3952/100398, r_rmn_max=0,
           r_rcy_max=88260/96446, r_rcy2_max=138996/311039, w_max=0,r_LE_max=0,
           cr_o=100398/488046, sr_B=0.7,Rental_period=0.25,
           Rir=1.5,CE_period=25,sr_D=0.5,cr_r=1.0){
    # browser()
    df_prod <- make_df_prod(df_historical,tgt_product,sr_B,sr_D,Rir)
    InitYear <- df_prod$Data[[1]]$Year[[1]] #各製品についてデータがある最初の年
    df_prod_Demand <- df_Demand %>% filter(Product==tgt_product) %>%
      .$Demand %>% .[[1]]
    
    df_params_read <- df_params_template %>% mutate(
      Max=case_when(Name=="epsilon" ~ epsilon_max,
                    Name=="w" ~ w_max,
                    Name=="r_LE" ~ r_LE_max,
                    Name=="r_rmn" ~ r_rmn_max,
                    Name=="r_rcy" ~ r_rcy_max,
                    Name=="r_drusA" ~ r_drusA_max,
                    Name=="r_rcy2" ~ r_rcy2_max),
      Period = CE_period)
    df_params <- make_df_params(df_params_read) %>% 
      mutate(theta=epsilon) # theta=epsilonという仮定
    # DoU Extension
    # tic()
    df_params_read_LE <- df_params_read %>% filter(Name=="r_LE")
    prod_type_temp <- df_params_read_LE %>% 
      pull(Prod_type)
    if (prod_type_temp %>% is.na()){
      prod_type_list_LE <- c("")
    } else if (prod_type_temp=="All"){
      prod_type_list_LE <- c("A","B","C","D")
    } else{
      prod_type_list_LE <- str_split(prod_type_temp, pattern = ",")[[1]]
    }
    period_LE <- df_params_read_LE %>% pull(Period)
    SY_LE <- df_params_read_LE %>% pull(SY)
    delta_LE <- df_params_read_LE %>% pull(Max)/period_LE
    # toc()
    # update DoU changed by "Lifetime Extension"
    df_prod_LE <- make_df_prod_LE(df_prod,SY_LE,period_LE,delta_LE,prod_type_list_LE)
    
    ### Reuse_Rental 
    # Initialize
    df_prod1 <- df_prod_LE
    # tgt_year=2021
    # tic()
    for (tgt_year in 2021:2050) {
      # tic()
      epsilon_i <- df_params$epsilon[[tgt_year-2020]]
      theta_i <- df_params$theta[[tgt_year-2020]]
      eta_i <- df_params$eta[[tgt_year-2020]]
      kappa_i <- df_params$kappa[[tgt_year-2020]]
      w_i <- df_params$w[[tgt_year-2020]]
      r_drusA_i <- df_params$r_drusA[[tgt_year-2020]]
      # toc()
      ########## 計算 ##########
      ### 1.Obsを求める
      df_new_Obs <- df_prod1 %>% mutate(new_Obs=map_dbl(Data,function(df_PF_temp){
        # browser()
        df_PF_temp %>% 
          mutate(
            Pdf_i=map2_dbl(Year,DoU,function(year,df_DoU){
              # browser()
              if (year<tgt_year){
                df_DoU$annual_est$Pdf[[tgt_year-year+1]] %>% return()
              } else{return(0)}
            }),
            Ship_Pdf=(Ship*Pdf_i) %>% ceiling()
          ) %>% 
          summarise(new_Obs=sum(Ship_Pdf, na.rm=TRUE)) %>% pull(new_Obs) %>% 
          return()
      })) %>% select(type,new_Obs)
      ### 2. S_t^BとS_t^Dを算出
      new_Obs_A <- df_new_Obs$new_Obs[[1]]
      new_Obs_C <- df_new_Obs$new_Obs[[3]]
      new_Ship_B <- ((epsilon_i*cr_o+r_drusA_i)*new_Obs_A  + theta_i*cr_r*new_Obs_C) %>% ceiling()
      new_Ship_D <- (eta_i*cr_o*new_Obs_A + kappa_i*cr_r*new_Obs_C) %>% ceiling()
      # new_Ship_B, Dの値をチェックして、new_Ship_A,Cが負の値を取らないように設定
      ddd <- df_prod_Demand$Demand[[tgt_year-InitYear+1]] # D_t^all：dddと命名
      sigma_S_Surv <- make_sigma_S_Surv(df_prod1,tgt_year,Rir)
      new_Ship_BD_temp <- new_Ship_B+Rir*new_Ship_D
      ddd_SSS <- ddd-sigma_S_Surv
      if (new_Ship_BD_temp>ddd_SSS){ #もし超えていたら修正
        new_Ship_B <- (new_Ship_B*ddd_SSS/new_Ship_BD_temp) %>% floor()
        new_Ship_D <- (new_Ship_D**ddd_SSS/new_Ship_BD_temp) %>% floor()
      }
      new_Ship_BD <- new_Ship_B+Rir*new_Ship_D
      ### 3. S_t^AとS_t^Cを算出
      new_Ship_A <- 
        ((1-w_i)/(1+(Rir-1)*w_i)*(ddd_SSS-new_Ship_BD)) %>% ceiling()
      new_Ship_C <- 
        (w_i/(1+(Rir-1)*w_i)*(ddd_SSS-new_Ship_BD)) %>% ceiling()
      ### 4. 最後にまとめて代入
      df_prod_comp <- df_prod1 %>% mutate(Data=map2(type,Data,function(type_i,df_PF_temp){
        # browser()
        if(type_i=="A"){df_PF_temp %>% mutate(
          Obs=case_when(Year==tgt_year ~ new_Obs_A,
                        TRUE ~ Obs),
          Ship=case_when(Year==tgt_year ~ new_Ship_A,
                         TRUE ~ Ship)) %>% return()}
        else if(type_i=="B"){df_PF_temp %>% mutate(
          Obs=case_when(Year==tgt_year ~ df_new_Obs$new_Obs[[2]],
                        TRUE ~ Obs),
          Ship=case_when(Year==tgt_year ~ new_Ship_B,
                         TRUE ~ Ship)) %>% return()}
        else if(type_i=="C"){df_PF_temp %>% mutate(
          Obs=case_when(Year==tgt_year ~ new_Obs_C,
                        TRUE ~ Obs),
          Ship=case_when(Year==tgt_year ~ new_Ship_C,
                         TRUE ~ Ship)) %>% return()}
        else if(type_i=="D"){df_PF_temp %>% mutate(
          Obs=case_when(Year==tgt_year ~ df_new_Obs$new_Obs[[4]],
                        TRUE ~ Obs),
          Ship=case_when(Year==tgt_year ~ new_Ship_D,
                         TRUE ~ Ship)) %>% return()}
        else{df_PF_temp %>% return()}
      }))
      # 更新
      df_prod1 <- df_prod_comp
    }
    ### 以下元素ごと循環に変更
    df_prod_comp_2 <- make_df_prod_comp_2_new(df_prod_comp,df_params,cr_o,cr_r,y_rmn,y_rcy,InitYear,comp_prod)
    ### 評価
    # 製造台数 = (AとCの出荷台数) - (A,B,C,Dのリマン/リサイクルの台数)
    # (AとCの出荷台数)=(新品の出荷台数)
    df_ship_new_temp <- df_prod_comp_2 %>% unnest(Data) %>% 
      filter(type %in% c("A","C")) %>% 
      group_by(Year) %>% summarise(Ship_new=sum(Ship))
    # (A,B,C,Dのリマン/リサイクルの重量)
    df_rmn_rcy_temp <- df_prod_comp_2 %>% unnest(Data) %>% 
      group_by(Year) %>%
      select(type,Year,Reman_rcv_element,Recycle_rcv_element) %>% 
      summarise(Reman_rcv_element=Reduce("+",Reman_rcv_element) %>% list(),
                Recycle_rcv_element=Reduce("+",Recycle_rcv_element) %>% list())
    # 資源投入量=新品出荷-(リマン+リサイクル)
    df_TMR <-
      df_ship_new_temp %>% left_join(df_rmn_rcy_temp,by="Year") %>% rowwise() %>% 
      mutate(Resource_Input=list(Ship_new*comp_prod-Reman_rcv_element-Recycle_rcv_element), #資源投入量
             TMR_element=list(df_TMR_coef$TMR_coef*Resource_Input), #元素ごとTMR
             TMR=Reduce("+",TMR_element)) %>% ungroup() %>% 
      select(Year,TMR)
    # 合計TMR(2021以降)
    TMR_sum <- df_TMR %>% filter(Year>=2021) %>% summarise(TMR_sum=sum(TMR)) %>% 
      pull(TMR_sum)
    df_FD <- df_prod_comp_2 %>% unnest(Data) %>% group_by(Year) %>% 
      summarise(FD_element=Reduce("+",FD_element) %>% list()) %>% rowwise() %>% 
      mutate(FD=Reduce("+",FD_element)) %>% ungroup()
    FD_sum <- df_FD %>% filter(Year>=2021) %>% summarise(FD_sum=sum(FD)) %>% 
      pull(FD_sum)
    
    df_PF_detail <- df_prod_comp_2 %>% unnest(Data) %>% 
      group_by(type) %>% 
      mutate(InUse=cumsum(Ship)-cumsum(Obs)) %>% ungroup() %>% group_by(Year) %>% 
      mutate(InUse_Rental=case_when(type %in% c("C","D") ~ InUse,
                                    type %in% c("A","B") ~ 0),
             InUse_Owned=case_when(type %in% c("A","B") ~ InUse,
                                   type %in% c("C","D") ~ 0),
             Ship_New=case_when(type %in% c("A","C") ~ Ship,
                                type %in% c("B","D") ~ 0)) %>% 
      group_by(Year) %>% 
      summarise(Reman_rcv_element=Reduce("+",Reman_rcv_element) %>% list(),
                Recycle_inp_element=Reduce("+",Recycle_inp_element) %>% list(),
                Recycle_rcv_element=Reduce("+",Recycle_rcv_element) %>% list(),
                FD_element=Reduce("+",FD_element) %>% list(),
                Ship_New=sum(Ship_New),
                Reuse=sum(Reuse),
                Reman_inp=sum(Reman_inp),
                InUse_Owned=sum(InUse_Owned),
                InUse_Rental=sum(InUse_Rental)) %>% rowwise() %>% 
      mutate(Resource_Input=list(Ship_New*comp_prod-Reman_rcv_element-Recycle_rcv_element)) %>% 
      ungroup()
    
    df_GHG <- df_PF_detail %>% rowwise() %>% 
      mutate(GHG_MaterialProduction=Ship_New*df_GHG_coef$MaterialProduction,
             GHG_Assembly=Ship_New*df_GHG_coef$Assembly, #組み立て台数
             # Remanによる削減効果
             GHG_Reduc_Reman=Reman_inp*df_GHG_coef$ReducReman[[1]]*(-1), 
             GHG_Reduc_Rcy_element=list(Recycle_inp_element*df_GHG_element$GHG_MatProd_elem*(-1)),
             GHG_Reduc_Rcy=Reduce("+",GHG_Reduc_Rcy_element),
             GHG_Trans1st=Ship_New*df_GHG_coef$Trans1st,
             GHG_Use_Owned=InUse_Owned*df_GHG_coef$Use,
             GHG_Use_Rental=InUse_Rental*df_GHG_coef$Use*Rir+
               InUse_Rental*df_GHG_coef$DistrRental*(1/Rental_period),
             GHG_Disposal=Reduce("+",FD_element)/prod_weight*df_GHG_coef$Disposal, # 仮定:重量に比例
             GHG_Reuse=(Reuse*prod_weight*transAT_dist)*df_GHG_coef$TransAT,
             GHG_Reman=Reman_inp*df_GHG_coef$Remanufacturing + 
               (Reduce("+",Reman_rcv_element)*transAT_dist)*df_GHG_coef$TransAT,
             GHG_Recycle=Reduce("+",Recycle_inp_element)/prod_weight*df_GHG_coef$Recycling + # 仮定:重量に比例
               (Reduce("+",Recycle_rcv_element)*transAT_dist)*df_GHG_coef$TransAT) %>% 
      select(Year,starts_with("GHG")) %>% select(-GHG_Reduc_Rcy_element) %>% 
      mutate(GHG_total=sum(c_across(GHG_MaterialProduction:GHG_Recycle))) %>% ungroup()
    GHG_sum <- df_GHG %>% filter(Year>=2021) %>% summarise(GHG_sum=sum(GHG_total)) %>% 
      pull(GHG_sum)
    # toc()
    tibble(df_prod_comp_2=df_prod_comp_2 %>% nest(data=everything()) %>% pull(data),
           df_TMR=df_TMR %>% nest(data=everything()) %>% pull(data),
           df_FD=df_FD %>% nest(data=everything()) %>% pull(data),
           df_GHG=df_GHG %>% nest(data=everything()) %>% pull(data),
           TMR_sum=TMR_sum,
           FD_sum=FD_sum,
           GHG_sum=GHG_sum) %>% return()
  }

# model_CE_py <- function(r_drusA_max=77609/488046, epsilon_max=3952/100398, r_rmn_max=0,
#                                 r_rcy_max=88260/96446, r_rcy2_max=138996/311039, w_max=0,r_LE_max=0,
#                                 cr_o=100398/488046, sr_B=0.7,Rental_period=0.25,
#                                 Rir=1.5,CE_period=25,sr_D=0.5,cr_r=1.0){
#   df_temp <- model_CE(r_drusA_max, epsilon_max, r_rmn_max,r_rcy_max, r_rcy2_max, w_max,r_LE_max,
#                               cr_o, sr_B,Rental_period,Rir,CE_period,sr_D,cr_r)
#   c_GHG_BD <- df_temp$df_GHG[[1]] %>% filter(Year>2020) %>% 
#     summarise_at(vars(GHG_MaterialProduction:GHG_Recycle), sum) %>% 
#     pivot_longer(cols=everything()) %>% .$value
#   c(df_temp$TMR_sum[[1]],df_temp$FD_sum[[1]],df_temp$GHG_sum[[1]],c_GHG_BD) %>% return()
# }

