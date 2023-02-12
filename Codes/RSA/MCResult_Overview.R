for (tgt_product in product_list){
  source("~/SFD_Simulator/Codes/RSA/Load_MCResult.R")
  # 評価指標の分布 -----------------------------------------------------------------
  ### 単一の分布
  df_result_filtered %>% select(TMR_sum,FD_sum,GHG_sum) %>% summary()
  
  histT <- df_result_filtered %>% 
    ggplot() + aes(x=TMR_sum) +
    geom_histogram(bins=50,color="white") + 
    labs(x="TMR（Mt-TMR）", y="試行数") +
    # scale_x_continuous(breaks=seq(-5.0e+9,8.0e+9,by=1.0e+09)) +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    )
  ggsave(plot=histT,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/MCOverview/",tgt_product,"_hist_TMR.jpg"),
         width=15 ,height=6,dpi="print")
  histF <- df_result_filtered %>% 
    ggplot() + aes(x=FD_sum) +
    geom_histogram(bins=50,color="white") + 
    labs(x="処理残渣量（kt）", y="試行数")+
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    )
  ggsave(plot=histF,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/MCOverview/",tgt_product,"_hist_FD.jpg"),
         width=15 ,height=6,dpi="print")
  histG <- df_result_filtered %>% 
    ggplot() + aes(x=GHG_sum) +
    geom_histogram(bins=50,color="white") + 
    # デジカメ
    # labs(x="GHG排出量（kt-CO2 eq.）", y="試行数")+
    # scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    # スマホ
    labs(x="GHG排出量（Mt-CO2 eq.）", y="試行数")+
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    ) 
  ggsave(plot=histG,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/MCOverview/",tgt_product,"_hist_GHG.jpg"),
         width=15 ,height=6,dpi="print")
  ### 2指標のプロット図
  
  p_FT <- df_result_filtered %>% 
    ggplot() + aes(x=FD_sum,y=TMR_sum) +
    geom_point(size=0.5) + 
    labs(x="処理残渣量（kt）", y="TMR（Mt-TMR）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    )
  p_GT <- df_result_filtered %>% 
    ggplot() + aes(x=GHG_sum,y=TMR_sum) +
    geom_point(size=0.5) + 
    labs(x="GHG排出量（Mt-CO2 eq.）", y="TMR（Mt-TMR）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    )
  p_FG <- df_result_filtered %>% 
    ggplot() + aes(x=FD_sum,y=GHG_sum) +
    geom_point(size=0.5) + 
    labs(x="処理残渣量（kt）", y="GHG排出量（Mt-CO2 eq.）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=1)) +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 20),
    )
  plots <- list()
  plots[[1]] <- p_FT
  plots[[2]] <- p_GT
  plots[[3]] <- p_FG
  
  ppp <- do.call("grid.arrange", c(plots, ncol=2))
  ggsave(plot=ppp,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/MCOverview/",tgt_product,"_plot_TFG.jpg"),
         width=14 ,height=12,dpi="print")
}