
for (tgt_product in product_list){
  KS_path <- str_c("Outputs/Results/",tgt_product,"/RSA/KSResTable/")
  df_KS_TMR_10 <- str_c(path,KS_path,tgt_product,"_KStest10_TMR.xlsx") %>% read_excel()
  df_KS_GHG_10 <- str_c(path,KS_path,tgt_product,"_KStest10_GHG.xlsx") %>% read_excel()
  df_KS_FD_10 <- str_c(path,KS_path,tgt_product,"_KStest10_FD.xlsx") %>% read_excel()
  df_KS_All_10 <- 
    df_KS_TMR_10 %>% rename(param=変数,p_TMR=p値, d_TMR=検定統計量d) %>% 
    full_join(df_KS_GHG_10 %>% rename(param=変数,p_GHG=p値, d_GHG=検定統計量d),by="param") %>% 
    full_join(df_KS_FD_10 %>% rename(param=変数,p_FD=p値, d_FD=検定統計量d),by="param")
  df_KS_TMR_90 <- str_c(path,KS_path,tgt_product,"_KStest90_TMR.xlsx") %>% read_excel()
  df_KS_GHG_90 <- str_c(path,KS_path,tgt_product,"_KStest90_GHG.xlsx") %>% read_excel()
  df_KS_FD_90 <- str_c(path,KS_path,tgt_product,"_KStest90_FD.xlsx") %>% read_excel()
  df_KS_All_90 <- 
    df_KS_TMR_90 %>% rename(param=変数,p_TMR=p値, d_TMR=検定統計量d) %>% 
    full_join(df_KS_GHG_90 %>% rename(param=変数,p_GHG=p値, d_GHG=検定統計量d),by="param") %>% 
    full_join(df_KS_FD_90 %>% rename(param=変数,p_FD=p値, d_FD=検定統計量d),by="param")
  
  # target_evaluator <- "TMR"
  for (target_evaluator in c("TMR","FD","GHG")){
    library(ggpattern)
    p <- df_KS_All_10 %>% rename_at(vars(p_TMR:d_FD), ~ str_c(.,"_10")) %>% 
      full_join(df_KS_All_90 %>% rename_at(vars(p_TMR:d_FD), ~ str_c(.,"_90")),by="param") %>% 
      pivot_longer(cols=-param) %>% 
      separate(col=name,
               into = c("dORp", "eval", "percent"),
               sep = "_",
               extra = "drop"
      ) %>% 
      mutate(percent=str_c("境界：上位",percent,"%"),
             param=factor(param,levels=c("リユース率1", "リユース率2","リマン率", "リサイクル率1","リサイクル率2", 
                                         "レンタル率", "使用期間延長割合","回収率", "リユース品使用期間比率",
                                         "レンタル期間","レンタル品使用頻度比率","戦略導入期間"))) %>% 
      filter(eval==target_evaluator) %>%
      pivot_wider(names_from=c(dORp),values_from=value) %>% 
      mutate(label=case_when(p>0.05 ~ "有意でない",
                             TRUE ~ "有意"),
             label=factor(label,levels=c("有意","有意でない"))) %>% rename(value=d) %>% 
      ggplot() + aes(x=factor(param),y=value,fill=percent,pattern=label)+ 
      geom_bar_pattern(stat = "identity", position = "dodge",colour="black",
                       pattern_fill = "black",
                       pattern_angle = 45,
                       pattern_density = 0.1,
                       # pattern_spacing = 0.025,
                       pattern_spacing = 0.01,
                       pattern_key_scale_factor = 0.6
      ) +
      scale_pattern_manual(values = c("有意でない" = "stripe", "有意" = "none")) +
      labs(x=NULL, y="KS検定統計量", fill=NULL)+
      guides(fill = guide_legend(override.aes = list(pattern = "none",colour="black"),),
             pattern =  guide_legend(override.aes = list(fill = "white",colour="black")),
             # pattern="none"
      )+
      theme(axis.text.x = element_text(size = 14,angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 15, hjust=0.5, vjust=1),
            legend.title = element_blank(), 
            legend.key.size = unit(1,"cm"), 
            legend.text = element_text(size=14)
      ) 
    # p
    ggsave(plot=p,filename=str_c(path,"Outputs/Results/",tgt_product,"/RSA/",tgt_product,
                                 "_KSResultBar_",target_evaluator,".jpg"),
           width=12.0,height=5.0,dpi="print")
  }
}


