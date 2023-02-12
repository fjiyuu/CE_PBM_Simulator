createEmptyDf <-  function(nrow, ncol, colnames = c()){
  if( missing( ncol ) && length( colnames ) > 0 ){
    ncol <-  length( colnames )
  }
  data.frame( matrix( vector(), nrow, ncol, dimnames = list( c(), colnames ) ) )
}

for (tgt_product in product_list){
  source(str_c(path,"Codes/RSA/Load_MCResult.R"))
  param_list <- df_result_filtered %>% 
    select(-c(TMR_sum,FD_sum,GHG_sum)) %>% colnames()
  # seqs <- seq(100, nrow(df_result_filtered), by = 100) %>% c(.,df_result_filtered %>% nrow())
  seqs <- seq(100, 2000, by = 10)
  seqs %>% length()
  df_p <- createEmptyDf(length(seqs),length(param_list),colnames = param_list)
  df_D <- createEmptyDf(length(seqs),length(param_list),colnames = param_list)
  df_res_label_all <- df_result_filtered %>% mutate(random=runif(nrow(df_result_filtered)),
                                label=case_when(random<=0.1 ~ "Behavioral",
                                                TRUE ~ "Non-Behavioral"))
  # df_res_label_all %>% filter(label=="Behavioral") %>% nrow()
  
  for (i in 1:length(seqs)){
    iter <- seqs[[i]]
    # print(i)
    if(i %% 10==0) {print(i)}
    # tic()
    # df_res_label <- df_result_filtered %>% head(iter) %>% 
    #   mutate(random=runif(iter),
    #          label=case_when(random<=0.1 ~ "Behavioral",
    #                          TRUE ~ "Non-Behavioral"))
    df_res_label <- df_res_label_all %>% head(iter)
    df_res_B <- df_res_label %>% filter(label=="Behavioral")
    df_res_NB <- df_res_label %>% filter(label=="Non-Behavioral")
    p_list <- c()
    D_list <- c()
    for (col in param_list){
      ks_temp <- ks.test(df_res_B %>% pull(col),
                         df_res_NB %>% pull(col),
                         exact = T
                         # exact=NULL
      )
      p_temp <- ks_temp$p.value
      D_temp <- ks_temp$statistic
      p_list <- c(p_list,p_temp)
      D_list <- c(D_list,D_temp)
    }
    df_p[i,] <-  p_list
    df_D[i,] <-  D_list
  }
  
  # p値と検定量Dの推移 --------------------------------------------------------------
  p_plt <- df_p %>% mutate(Num=seqs) %>% 
    select(Num,リユース率1=r_drusA_max, リユース率2=epsilon_max, リマン率=r_rmn_max,
           リサイクル率1=r_rcy_max,リサイクル率2=r_rcy2_max, レンタル率=w_max,
           使用期間延長割合=r_LE_max, 回収率=cr_o, リユース品使用期間比率=sr_B,
           レンタル期間=Rental_period, レンタル品使用頻度比率=Rir, 戦略導入期間=CE_period) %>%
    # select(Num,レンタル期間=Rental_period, レンタル品使用頻度比率=Rir) %>% 
    pivot_longer(cols=colnames(.)[-1],values_to="pvalue", names_to="param") %>% 
    ggplot() +
    aes(x=Num,y=pvalue, colour=param) +
    geom_line() + geom_point(size=0.7) +
    theme(legend.title=element_blank(),
          legend.text = element_text(size = 15),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(1, 'cm'),
    ) +
    labs(x="試行回数",y="p値",title=NULL)
  # p_plt
  D_plt <- df_D %>% mutate(Num=seqs) %>% 
    select(Num,リユース率1=r_drusA_max, リユース率2=epsilon_max, リマン率=r_rmn_max,
           リサイクル率1=r_rcy_max,リサイクル率2=r_rcy2_max, レンタル率=w_max,
           使用期間延長割合=r_LE_max, 回収率=cr_o, リユース品使用期間比率=sr_B,
           レンタル期間=Rental_period, レンタル品使用頻度比率=Rir, 戦略導入期間=CE_period) %>% 
    pivot_longer(cols=colnames(.)[-1],values_to="Dvalue", names_to="param") %>% 
    ggplot() +
    aes(x=Num,y=Dvalue, colour=param) +
    geom_line() + geom_point(size=0.7) +
    theme(legend.title=element_blank(),
          legend.text = element_text(size = 15),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
          legend.key.height = unit(1, 'cm'),
          legend.key.width = unit(1, 'cm'),
    ) +
    labs(x="試行回数",y="検定統計量",title=NULL)
  # D_plt
  ggsave(plot=p_plt,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Extra/",
                    tgt_product,"_convP","RandomLabel","_.jpg"),
         width=15, height=6, dpi="print")
  ggsave(plot=D_plt,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Extra/",
                    tgt_product,"_convD","RandomLabel","_.jpg"),
         width=15, height=6, dpi="print")
}

