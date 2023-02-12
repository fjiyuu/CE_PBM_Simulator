### 製品の選択
# tgt_product <- "デジタルカメラ"
tgt_product <- "スマートフォン"

df_res_temp <- 
  str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_df_result_OneStrategy.csv") %>% 
  read_csv()

df_Input_read <- 
  str_c(path,"Inputs/Parameters/Scenarios/df_input_OneStrategySA.csv") %>%
  read_csv()
df_Input <- 
  df_Input_read %>% 
  replace_na(replace = list(epsilon=3952/100398, eta=0, theta=0, kappa=0,
                            w=0, r_LE=0, r_rmn=0, r_rcy=88260/96446,
                            r_drusA=77609/488046, r_rcy2=138996/311039))

df_result_tidy <- 
  df_Input_read %>% full_join(df_res_temp,by="strategy") %>% select(-c(eta:kappa)) %>% 
  pivot_longer(cols=c(epsilon:r_rcy2),names_to="param") %>% drop_na() %>% 
  select(param,value,everything(),-strategy)
  
df_result_tidy_1 <- df_result_tidy %>% 
  mutate(param=case_when(param=="epsilon" ~ "リユース率2",
                         param=="w" ~ "レンタル率",
                         param=="r_LE" ~ "使用期間延長割合",
                         param=="r_rmn" ~ "リマン率",
                         param=="r_rcy" ~ "リサイクル率1",
                         param=="r_rcy2" ~ "リサイクル率2",
                         param=="r_drusA" ~ "リユース率1"))
# plots <- list()
vars <- names(df_result_tidy)[3:5]
vars_conv <- c("TMR","処理残渣量","GHG排出量")
if (tgt_product=="デジタルカメラ"){
  scale_list <- c(1e-09,1e-06,1e-06)
  accuracy_list <- c(0.1,1,1)
  unit_list <- c("Mt-TMR","kt","kt-CO2 eq.")
} else{
  scale_list <- c(1e-09,1e-06,1e-09)
  accuracy_list <- c(1,1,1)
  unit_list <- c("Mt-TMR","kt","Mt-CO2 eq.")
}
# i<- 1
for (i in 1:length(vars)) {
  p <- 
    ggplot(df_result_tidy_1,
           aes_string(x="value",y=vars[i], colour="param",
                      linetype = "param")) +
    geom_line(linewidth=1.5) + 
    labs(x="パラメータの値",y=str_c(vars_conv[i],"（",unit_list[[i]],"）"),
         colour=NULL, linetype=NULL)+
    scale_y_continuous(labels = unit_format(unit = "", scale = scale_list[[i]],
                                            accuracy=accuracy_list[[i]])) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 25),
          plot.title = element_text(size = 25),
          plot.margin= unit(c(0.5, 1, 0.5, 0.5), "lines"),
          legend.title = element_blank(), 
          legend.key.size = unit(2,"cm"),
          legend.text = element_text(size=25)
    )
  ggsave(plot=p,
         filename=str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",
                        tgt_product,"_OneParamChange_",vars_conv[i],".jpg"),
         width=18, height=12, dpi="print")
}
