### 製品の選択
# tgt_product <- "デジタルカメラ"
tgt_product <- "スマートフォン"

### シナリオの選択
OneOrTwo <- "One"
# OneOrTwo <- "Two"

if (OneOrTwo=="One"){
  scenario_path <- "Inputs/Parameters/Scenarios/df_input_OneStrategySA.csv"
  result_path <- str_c("Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_df_result_OneStrategy.csv")
} else if (OneOrTwo=="Two"){
  scenario_path <- ifelse(tgt_product=="デジタルカメラ",
                          "Inputs/Parameters/Scenarios/df_input_TwoStrategies_dgc.csv",
                          ifelse(tgt_product=="スマートフォン",
                                 "Inputs/Parameters/Scenarios/df_input_TwoStrategies_smp.csv",
                                 "ERROR")
                          )
  result_path <- str_c("Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_df_result_TwoStrategies.csv")
}

df_Input_read <- str_c(path,scenario_path) %>% read_csv()
df_Input <- df_Input_read %>% 
  replace_na(replace = list(epsilon=3952/100398, eta=0, theta=0, kappa=0,
                            w=0, r_LE=0, r_rmn=0, r_rcy=88260/96446,
                            r_drusA=77609/488046, r_rcy2=138996/311039))
strategy_list <- df_Input$strategy
case_num <- 1
strategy_list %>% length()
for (str in strategy_list){
  print(str_c(str))
  epsilon_max <- df_Input %>% filter(strategy==str) %>% pull(epsilon)
  r_rcy_max <- df_Input %>% filter(strategy==str) %>% pull(r_rcy)
  r_rmn_max <- df_Input %>% filter(strategy==str) %>% pull(r_rmn)
  w_max <- df_Input %>% filter(strategy==str) %>% pull(w)
  r_LE_max <- df_Input %>% filter(strategy==str) %>% pull(r_LE)
  r_rcy2_max <- df_Input %>% filter(strategy==str) %>% pull(r_rcy2)
  r_drusA_max <- df_Input %>% filter(strategy==str) %>% pull(r_drusA)
  res <- 
    model_CE(
      epsilon_max=epsilon_max, r_rcy_max=r_rcy_max, r_rmn_max=r_rmn_max,
      w_max=w_max, r_LE_max=r_LE_max, r_rcy2_max=r_rcy2_max,
      r_drusA_max=r_drusA_max,CE_period = 25,
      # Rir=1.5
      )
  df_prod_comp_2 <- res$df_prod_comp_2[[1]]
  df_TMR <- res$df_TMR[[1]]
  df_FD <- res$df_FD[[1]]
  df_GHG <- res$df_GHG[[1]]
  TMR_sum <- res$TMR_sum[[1]]
  FD_sum <- res$FD_sum[[1]]
  GHG_sum <- res$GHG_sum[[1]]
  # 記録
  PF_data <- df_prod_comp_2 %>% unnest(Data) %>% 
    nest(data=everything()) %>% pull(data)
  nested_TMR_FD_GHG <- 
    df_TMR %>% full_join(df_FD,by="Year") %>% 
    full_join(df_GHG,by="Year") %>% 
    nest(data=everything()) %>% pull(data)
  if(case_num==1){
    df_result <- tibble(strategy=str,PF_data=PF_data,eval_data=nested_TMR_FD_GHG,
                        TMR_sum=TMR_sum,FD_sum=FD_sum,GHG_sum=GHG_sum)
  } else{
    df_result <- df_result %>% 
      add_row(strategy=str,PF_data=PF_data,eval_data=nested_TMR_FD_GHG,
              TMR_sum=TMR_sum,FD_sum=FD_sum,GHG_sum=GHG_sum)
  }
  case_num <- case_num + 1
}
df_result %>% select(strategy,TMR_sum,FD_sum,GHG_sum) %>%
  write_csv(str_c(path,result_path))
