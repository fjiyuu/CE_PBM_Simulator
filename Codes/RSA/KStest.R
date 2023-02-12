for (tgt_product in product_list){
  source(str_c(path,"Codes/RSA/Load_MCResult.R"))
  
  # 試行回数ごとの結果まとめ -----------------------------------------------------------------
  createEmptyDf <-  function(nrow, ncol, colnames = c()){
    if( missing( ncol ) && length( colnames ) > 0 ){
      ncol <-  length( colnames )
    }
    data.frame( matrix( vector(), nrow, ncol, dimnames = list( c(), colnames ) ) )
  }
  param_list <- df_result_filtered %>% 
    select(-c(TMR_sum,FD_sum,GHG_sum)) %>% colnames()
  seqs <- seq(100, nrow(df_result_filtered), by = 100) %>% c(.,df_result_filtered %>% nrow())
  seqs %>% length()
  df_p <- createEmptyDf(length(seqs),length(param_list),colnames = param_list)
  df_D <- createEmptyDf(length(seqs),length(param_list),colnames = param_list)
  # BorderRatio <- 90
  # target_evaluator <- "TMR"
  for (BorderRatio in c(10,90)){
    for (target_evaluator in c("TMR","FD","GHG")){
      for (i in 1:length(seqs)){
        iter <- seqs[[i]]
        # print(i)
        if(i %% 10==0) {print(i)}
        # tic()
        df_result_filtered_i <- df_result_filtered %>% head(iter)
        TMR_boundary <- df_result_filtered_i %>% 
          arrange(TMR_sum) %>% pull(TMR_sum) %>% 
          .[[floor(length(.)*BorderRatio/100)]]
        GHG_boundary <- df_result_filtered_i %>% 
          arrange(GHG_sum) %>% pull(GHG_sum) %>% 
          .[[floor(length(.)*BorderRatio/100)]]
        FD_boundary <- df_result_filtered %>% 
          arrange(FD_sum) %>% pull(FD_sum) %>% 
          .[[floor(length(.)*BorderRatio/100)]]
        if (target_evaluator=="All"){
          df_res_label <- df_result_filtered_i %>%
            mutate(label=case_when(TMR_sum<=TMR_boundary & FD_sum<=FD_boundary & 
                                     GHG_sum<=GHG_boundary ~ "Behavioral",
                                   TRUE ~ "Non-Behavioral"))
        } else if(target_evaluator=="TMR"){
          df_res_label <- df_result_filtered_i %>%
            mutate(label=case_when(TMR_sum<=TMR_boundary ~ "Behavioral",
                                   TRUE ~ "Non-Behavioral"))
        } else if(target_evaluator=="GHG"){
          df_res_label <- df_result_filtered_i %>%
            mutate(label=case_when(GHG_sum<=GHG_boundary ~ "Behavioral",
                                   TRUE ~ "Non-Behavioral"))
        } else if(target_evaluator=="FD"){
          df_res_label <- df_result_filtered_i %>%
            mutate(label=case_when(FD_sum<=FD_boundary ~ "Behavioral",
                                   TRUE ~ "Non-Behavioral"))
        } else{
          print("ERROR")
        }
        df_res_B <- df_res_label %>% filter(label=="Behavioral")
        df_res_NB <- df_res_label %>% filter(label=="Non-Behavioral")
        p_list <- c()
        D_list <- c()
        for (col in param_list){
          ks_temp <- ks.test(df_res_B %>% pull(col),
                             df_res_NB %>% pull(col),
                             # exact = T
                             exact=NULL
          )
          p_temp <- ks_temp$p.value
          D_temp <- ks_temp$statistic
          p_list <- c(p_list,p_temp)
          D_list <- c(D_list,D_temp)
        }
        df_p[i,] <-  p_list
        df_D[i,] <-  D_list
      }
      
      # KS検定の表作成 ----------------------------------------------------------------
      df_p_last <- df_p %>% as_tibble() %>% filter(row_number()==nrow(df_p))
      df_D_last <- df_D %>% as_tibble() %>% filter(row_number()==nrow(df_D))
      df_p_last %>% rbind(df_D_last) %>% mutate(Name=c("p値","検定統計量d")) %>% 
        select(Name,リユース率1=r_drusA_max, リユース率2=epsilon_max, リマン率=r_rmn_max,
               リサイクル率1=r_rcy_max,リサイクル率2=r_rcy2_max, レンタル率=w_max,
               使用期間延長割合=r_LE_max, 回収率=cr_o, リユース品使用期間比率=sr_B,
               レンタル期間=Rental_period, レンタル品使用頻度比率=Rir, 戦略導入期間=CE_period) %>% 
        pivot_longer(cols=-Name, names_to = "変数") %>% 
        pivot_wider(names_from=Name,values_from=value) %>% 
        write.xlsx(file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/KSResTable/",
                              tgt_product,"_KStest",BorderRatio,"_",target_evaluator,".xlsx"))
      # p値と検定量Dの推移 --------------------------------------------------------------
      p_plt <- df_p %>% mutate(Num=seqs) %>% 
        select(Num,リユース率1=r_drusA_max, リユース率2=epsilon_max, リマン率=r_rmn_max,
               リサイクル率1=r_rcy_max,リサイクル率2=r_rcy2_max, レンタル率=w_max,
               使用期間延長割合=r_LE_max, 回収率=cr_o, リユース品使用期間比率=sr_B,
               レンタル期間=Rental_period, レンタル品使用頻度比率=Rir, 戦略導入期間=CE_period) %>% 
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
             file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Convergence/",tgt_product,
                        "_convP",BorderRatio,"_",target_evaluator,".jpg"),
             width=15, height=6, dpi="print")
      ggsave(plot=D_plt,
             file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Convergence/",tgt_product,
                        "_convD",BorderRatio,"_",target_evaluator,".jpg"),
             width=15, height=6, dpi="print")
    }
  }
}