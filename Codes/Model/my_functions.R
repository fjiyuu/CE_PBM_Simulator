DoU_scale_change <- function(df_DoU,scale_ratio) {
  # browser()
  scale_param <- df_DoU$params$Scale[[1]] * scale_ratio
  shape_param <- df_DoU$params$Shape[[1]]
  mean_lifetime <- gamma(1 + 1/shape_param) * scale_param
  params <- tibble(DistType=df_DoU$params$DistType,
                   Scale=scale_param, Shape=shape_param, Mean=mean_lifetime)
  annual_est <- tibble(time=seq(0, 100)) %>%
    mutate(Surv=1-exec(str_c("p", df_DoU$params$DistType),
                       q=time, scale=scale_param, shape=shape_param),
           Pdf=-c(0, diff(Surv)))
  list(params=params, annual_est=annual_est) %>% return()
}

make_df_Demand <- function(df_bau,df_historical){
  df_bau %>% select(Product,bau_UseNum=UseNumber) %>% 
    full_join(df_historical %>% select(Product,hist_InUse=InUse),by="Product") %>% 
    mutate(Demand=map2(bau_UseNum,hist_InUse,function(df_b,df_h){
      df_temp1 <- df_b %>% (function(df1){
        if ("Survival" %in% df1$DoUEstType){
          df1 %>% filter(DoUEstType=="Survival",YearType=="CY") %>% return()
        }
        else{
          df1 %>% filter(DoUEstType=="Solver",YearType=="CY") %>% return()
        }}) %>%
        .$UseNumberForecast %>% .[[1]] %>% 
        select(Year,Demand=InUse) %>% filter(Year>2020)
      df_temp2 <- df_h %>% (function(df2){
        if ("Survival" %in% (df2$DoUEstType %>% unique())){
          df2 %>% filter(DoUEstType=="Survival",YearType=="CY") %>% return()
        } else{
          df2 %>% filter(DoUEstType=="Solver",YearType=="CY") %>% return()
        }
      }) %>% group_by(Year=EY) %>% summarise(Demand=sum(value))
      df_temp2 %>% bind_rows(df_temp1) %>% return()
    })) %>% select(Product,Demand) %>% return()
}

make_df_composition_all <- function(path){
  df_smp_comp <- str_c(path,"Inputs/Data/Smp_composition_Manhart.csv") %>% read_csv() %>% 
    mutate(スマートフォン=weight/1000) %>%  #gをkgに変換
    select(sym,スマートフォン)
  str_c(path,"Inputs/Data/Composition_matrix.csv") %>% 
    read_csv() %>% mutate_at(colnames(.)[-3:-1], ~replace_na(.,0)) %>% 
    mutate_at(colnames(.)[-3:-1],~ ./.[[1]]) %>% slice(-1) %>% 
    left_join(df_smp_comp,by="sym") %>% 
    mutate(スマートフォン = スマートフォン %>% replace_na(.,0)) %>% 
    pivot_longer(cols=-c(num,sym,element),names_to="Product") %>% 
    group_by(Product) %>% nest() %>% ungroup() %>% rename(Comp_prod=data) %>% 
    mutate(
      Product=case_when(Product=="PC（デスクトップ型）" ~ "デスクトップパソコン",
                        Product=="PC（ノートブック型）" ~ "ノートパソコン",
                        TRUE ~ Product))
}
make_df_TMR_Prod <- function(path,df_composition_all,df_TMR_coef){
  # browser()
  df_TMR_Prod <- df_composition_all %>% 
    mutate(TMR_Prod=map_dbl(Comp_prod,function(df_comp_prod){
      # browser()
      df_comp_prod %>% left_join(df_TMR_coef,by="sym") %>% 
        mutate(each_TMR=value*TMR_coef) %>% 
        summarise(TMR=sum(each_TMR)) %>% pull(TMR) %>% return()
    })) %>% select(Product,TMR_Prod) %>% return()
}

make_df_prod <- function(df_historical,tgt_product,sr_B,sr_D,Rir){
  df_historical %>% filter(Product==tgt_product) %>% 
    select(Product,Shipment,Obsolescence,DoU) %>% 
    mutate(type_A="A",type_B="B",type_C="C",type_D="D") %>% 
    pivot_longer(cols=c(type_A,type_B,type_C,type_D),values_to="type") %>% 
    select(-name) %>% 
    unnest(DoU) %>%
    filter(DoUEstType=="Survival" |
             Product %in% c("ビデオカメラ","レコーダ/プレーヤ","洗濯機")) %>%
    mutate(DoU=map2(Data,type,function(df_Data,type){
      # browser()
      scale_ratio <- case_when(type=="A" ~ 1,
                               type=="B" ~ sr_B,
                               type=="C" ~ 1/Rir,
                               type=="D" ~ sr_D)
      DoU_scale_change(df_Data,scale_ratio) %>% return()
    })) %>% select(Product,Shipment,Obsolescence,DoU,type) %>%
    mutate(
      Shipment=case_when(
        type %in% c("A") ~ map(Shipment,function(df_ship){
          df_ship %>% filter(YearType=="CY") %>% select(Year,Ship=value) %>% return()
        }),
        type %in% c("B","C","D") ~ map(Shipment,function(df_ship){
          df_ship %>% filter(YearType=="CY") %>% select(Year,Ship=value) %>% 
            mutate(Ship=0) %>% return()
        })
      ),
      Obsolescence=case_when(
        # typeAのとき、historicalから値を抽出
        type %in% c("A") ~ map(Obsolescence,function(df_obs){
          df_obs %>% filter(YearType=="CY") %>% (function(df){
            if ("Survival" %in% df$DoUEstType){
              df %>% filter(DoUEstType=="Survival") %>% return()
            }
            else{
              df %>% filter(DoUEstType=="Solver") %>% return()
            }
          }) %>% group_by(EY) %>% summarise(Obs=sum(value)) %>%
            rename(Year=EY) %>% return()
        }),
        # typeA以外のとき、Obs=0
        type %in% c("B","C","D") ~ map(Obsolescence,function(df_obs){
          # browser()
          df_obs %>% filter(YearType=="CY") %>% (function(df){
            if ("Survival" %in% df$DoUEstType){
              df %>% filter(DoUEstType=="Survival") %>% return()
            }
            else{
              df %>% filter(DoUEstType=="Solver") %>% return()
            }
          }) %>% group_by(EY) %>% summarise(Obs=sum(value)) %>%
            rename(Year=EY) %>% mutate(Obs=0) %>% return()
        })
      ),
      Data=pmap(list(Shipment,Obsolescence,DoU,type),function(df_ship,df_obs,df_dou,df_type){
        # browser()
        df_ship %>% full_join(df_obs,by="Year") %>% 
          replace_na(list(Ship=0,Obs=0)) %>% 
          filter(Year<=2020) %>% arrange(Year) %>% 
          mutate(DoU=map(Ship,function(df){df_dou %>% return()})) %>% 
          return()
      })
    ) %>% select(-c(Shipment,Obsolescence,DoU)) %>% group_by(Product) %>% nest() %>% 
    ungroup() %>% .$data %>% .[[1]] %>% 
    return()
}
make_df_params <- function(df_params_read){
  df_params_read %>%
    mutate(Data=pmap(list(Name,Change,SY,Period,Init,Max),
                     function(name,change,sy,period,init,max){
                       # browser()
                       if (change %>% is.na()){
                         tibble(Year=seq(2021,2050,1),Data=init) %>% 
                           return()
                       } else if (change==TRUE){
                         delta_temp <- (max-init)/period
                         tibble(Year=seq(2021,2050,1),Data=case_when(
                           Year<sy ~ init,
                           (Year>=sy)&(Year<=sy+period-1) ~ init+(Year-sy+1)*delta_temp,
                           TRUE ~ max)
                         ) %>% return()
                       } else{
                         print("Error: Wrong Input")
                       }
                     })) %>% 
    unnest(Data) %>% select(Name,Year,Data) %>% 
    pivot_wider(names_from=Name,values_from=Data) %>% return()
}
make_df_prod_LE <- function(df_prod,SY_LE,period_LE,delta_LE,prod_type_list_LE){
  df_prod %>% mutate(Data=map2(type,Data,function(type_i,df_temp){
    # browser()
    if (type_i %in% prod_type_list_LE){
      df_temp %>% add_row(Year=seq(2021,2050,1),DoU=list(.$DoU[[1]])) %>% mutate(
        DoU=map2(DoU,Year,function(df_dou,year){
          # browser()
          if(year %in% seq(SY_LE,SY_LE+period_LE-1,1)) {
            DoU_scale_change(df_dou,1+(year-SY_LE+1)*delta_LE) %>% return() # 対象年の場合
          }else if(year<SY_LE){
            df_dou %>% return() # SY_LEより前はhistoricalといっしょ
          } else{
            DoU_scale_change(df_dou,1+period_LE*delta_LE) #対象期間後は最後の年と同じ
          }
        })
      ) %>% return()
    } else{
      df_temp %>% add_row(Year=seq(2021,2050,1),DoU=list(.$DoU[[1]])) %>% return()
    }
  }))
}

make_sigma_S_Surv <- function(df_prod1,tgt_year,Rir){
  df_prod1 %>% mutate(
    sum_Ship_Surv=map_dbl(Data,function(df_temp){
      # browser()
      df_temp %>% filter(Year<tgt_year) %>% 
        mutate(
          Surv_i=map2_dbl(Year,DoU,function(year,df_DoU){
            df_DoU$annual_est$Surv[[tgt_year-year+1]] %>% return()
          }
          ),
          Ship_Surv=(Ship*Surv_i) %>% ceiling()
        ) %>% 
        summarise(sum_Ship_Surv=sum(Ship_Surv, na.rm=TRUE)) %>% pull(sum_Ship_Surv) %>% 
        return()
    }),
    Rir_weight=c(1,1,Rir,Rir),
    weighted_sum_Ship_Surv = Rir_weight*sum_Ship_Surv
  ) %>% summarise(sigma_S_Surv=sum(weighted_sum_Ship_Surv)) %>% pull(sigma_S_Surv) %>% 
    return()
}

make_df_prod_comp_2_new <- function(df_prod_comp,df_params,cr_o,cr_r,y_rmn,y_rcy,InitYear,comp_prod){
  df_params_all_year <- df_params %>% select(-r_LE) %>% 
    add_row(Year=seq(InitYear,2020,1), epsilon=0,eta=0,theta=0,kappa=0,w=0,
            r_rmn=0,r_rcy=0,r_drusA=0,r_rcy2=0) %>% 
    arrange(Year)
  df_prod_comp %>% unnest(Data) %>% 
    mutate(Reuse=case_when(type %in% c("B","D") ~ Ship,
                           type %in% c("A","C") ~ 0)) %>% 
    nest(Data=!type) %>% 
    mutate(Data=map2(Data,type,function(df_data,type_i){
      # browser()
      df_data %>% left_join(df_params_all_year,by="Year") %>% 
        mutate(
          r_drus=case_when(type_i=="A" ~ r_drusA, TRUE ~ 0),
          r_rusAC=case_when(type_i=="A" ~ epsilon+eta,
                            type_i=="C" ~ theta+kappa,
                            TRUE ~ 0),
          cr=case_when(type_i %in% c("A","B") ~ cr_o,
                       type_i %in% c("C","D") ~ cr_r)) %>% rowwise() %>% 
        mutate(
          # リマン処理に入る台数
          Reman_inp= r_rmn*(1-r_rusAC)*cr*Obs,
          # リマンされた部品(元素構成)
          Reman_rcv_element=list(y_rmn*comp_prod*Reman_inp),
          # 製品としてリサイクル処理に入る台数
          Recycle_inp_unit=r_rcy*(1-r_rmn)*(1-r_rusAC)*cr*Obs + r_rcy2*(1-cr-r_drus)*Obs,
          # Recycleに入る素材(元素構成)
          ### (リサイクル歩留まり)*{(リマン処理残さ)+(組成)*(リサイクル台数)}
          Recycle_inp_element=
            list((1-y_rmn)*comp_prod*Reman_inp + comp_prod*Recycle_inp_unit),
          Recycle_rcv_element=list(y_rcy*Recycle_inp_element),
          FD_element=
            list(Recycle_inp_element*(1-y_rcy)+
                   comp_prod*((1-r_rcy)*(1-r_rmn)*(1-r_rusAC)*cr*Obs+(1-r_rcy2)*(1-cr-r_drus)*Obs))
        ) %>% ungroup() %>% select(-c(Recycle_inp_unit)) %>% return()
    })
    )
}
make_df_material <- function(tgt_product=tgt_product,path=path,rawdata_path,mat_comp_path) {
  if (tgt_product=="デジタルカメラ"){
    # IDEAの該当する項目のrawdata
    df_Dgc_material_IDEA_rawdata <- 
      str_c(path,rawdata_path) %>% read_csv() %>% mutate_if(is.logical, ~replace_na(.,0)) %>% 
      rename(Material=材料)
    # 材料の重量[kg/台] (エコリーフより)
    df_Dgc_material_composition <- 
      str_c(path,mat_comp_path) %>% read_csv() %>% rename(Material=材料, MassComposition=`質量[kg]`)
    # データを整理
    # 1台あたりの各材料の金属含有量，GHG排出量を算出
    df_Dgc_material <- 
      df_Dgc_material_IDEA_rawdata %>% rowwise() %>% mutate(MassMetalSum=sum(c_across(Ag:Zr))) %>% 
      ungroup() %>% full_join(df_Dgc_material_composition,by="Material") %>% 
      mutate(base_flow=case_when(Material == .$Material[[6]] ~ MassComposition/45, 
                                 # 1換算箱は45kg
                                 # (https://www.env.go.jp/council/06earth/y060-43/mat04.pdf)
                                 Material == .$Material[10] ~ MassComposition/MassMetalSum, #基板は金属で全て構成されていると仮定
                                 Material == .$Material[11] ~ 1, #電池は1個
                                 TRUE ~ MassComposition)) %>% 
      mutate_at(vars(Ag:`GWP[kgCO2eq]`),~ .*base_flow) %>% 
      select(Material,GHG=`GWP[kgCO2eq]`,c(Ag:Zr)) %>% return()
  } else if (tgt_product=="スマートフォン"){
    # IDEAの該当する項目のrawdata
    df_Smp_material_IDEA_rawdata <- 
      str_c(path,rawdata_path) %>% read_csv() %>% mutate_at(vars(Ag:Zr), ~replace_na(.,0)) %>% 
      rename(Material=材料)
    # 材料の重量[kg/台] (Cordella,2020)
    df_Smp_material_composition <- 
      str_c(path,mat_comp_path) %>% read_csv() %>% 
      mutate(MassComposition=`質量[g]`/1000) %>% 
      select(Material=材料, MassComposition)
    df_Smp_material <- 
      df_Smp_material_IDEA_rawdata %>% rowwise() %>% 
      mutate(MassMetalSum=sum(c_across(Ag:Zr))) %>% 
      ungroup() %>% full_join(df_Smp_material_composition,by="Material") %>%
      mutate(base_flow=case_when(単位 == "個" ~ 1, # 1個
                                 単位 == "円" ~ MassComposition/MassMetalSum, # 金属で全て構成されていると仮定
                                 単位 == "kg" ~ MassComposition,
                                 TRUE ~ 0)) %>% 
      mutate_at(vars(Ag:Zr),~ .*base_flow) %>% 
      select(Material,c(Ag:Zr)) %>% return()
  }
}
make_df_y_rmn <- function(df_material) {
  ### Remanの計算
  # リマン部品の元素構成(34元素)
  df_Reman_buhin <- 
    df_material %>% filter(Material %in% Reman_mat_list) %>% 
    summarise_at(vars(Ag:Zr), sum) %>% 
    pivot_longer(cols=everything(),names_to="sym",values_to="buhin_comp")
  # 全部品の元素構成(34元素)
  df_all_buhin <- df_material %>% #filter(Material %in% Reman_mat_list) %>% 
    summarise_at(vars(Ag:Zr), sum) %>% 
    pivot_longer(cols=everything(),names_to="sym",values_to="all_buhin_comp")
  # 歩留まり(34元素)
  df_y_rmn_temp <- df_all_buhin %>% 
    left_join(df_Reman_buhin, by="sym") %>% 
    mutate(y_rmn=case_when(all_buhin_comp>0 ~ buhin_comp/all_buhin_comp,
                           TRUE ~ 0)) %>% select(sym,y_rmn)
  # 歩留まり(71元素)
  df_composition %>% left_join(df_y_rmn_temp,by="sym") %>% 
    replace_na(list(y_rmn=0)) %>% select(-comp_prod) %>% return()
}
trans_beta <- function(param, k, mode, min, max) {
  param_beta <- qgenbeta(param, shape1=(mode-min)/(max-min)*(k-2)+1, 
           shape2=(1-(mode-min)/(max-min))*(k-2)+1, 
           shape3=1, scale=max-min) + min
  param_beta %>% return()
}


