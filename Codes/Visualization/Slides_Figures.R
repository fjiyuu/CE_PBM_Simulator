for (tgt_product in product_list){
  source(str_c(path,"Codes/RSA/Load_MCResult.R"))
  # モンテカルロ結果の概要 -------------------------------------------------------------
  accT <- ifelse(tgt_product=="デジタルカメラ",0.1,1)
  accG <- ifelse(tgt_product=="デジタルカメラ",0.1,1)
  
  p_FT <- df_result_filtered %>% 
    ggplot() + aes(x=FD_sum,y=TMR_sum) +
    geom_point(size=0.5) + 
    labs(x="処理残渣量（kt）", y="TMR（Mt-TMR）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=accT)) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 30),
    )
  p_GT <- df_result_filtered %>% 
    ggplot() + aes(x=GHG_sum,y=TMR_sum) +
    geom_point(size=0.5) + 
    labs(x="GHG排出量（Mt-CO2 eq.）", y="TMR（Mt-TMR）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=accG)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=accT)) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 30),
          # axis.title.x = element_text(size = 30),
          # axis.title.y = element_blank(),
    )
  p_FG <- df_result_filtered %>% 
    ggplot() + aes(x=FD_sum,y=GHG_sum) +
    geom_point(size=0.5) + 
    labs(x="処理残渣量（kt）", y="GHG排出量（Mt-CO2 eq.）") +
    scale_x_continuous(labels = unit_format(unit = "", scale = 1e-6,accuracy=1)) +
    scale_y_continuous(labels = unit_format(unit = "", scale = 1e-9,accuracy=accG)) +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 30),
    )
  plots <- list()
  plots[[1]] <- p_FT
  plots[[2]] <- p_GT
  plots[[3]] <- p_FG
  
  ppp <- do.call("grid.arrange", c(plots, ncol=2))
  ggsave(plot=ppp,
         file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Presentation/Slides_",tgt_product,"_plot_TFG.jpg"),
         width=14 ,height=12,dpi="print")
}

# RSA解説 -------------------------------------------------------------------
tgt_product <- "デジタルカメラ"
source(str_c(path,"Codes/RSA/Load_MCResult.R"))
p1 <- df_result_filtered %>% arrange(TMR_sum) %>% 
  mutate(Index=row_number(),
         label=case_when(Index<=nrow(.)/5 ~ 1,
                         Index<=nrow(.)/5*2 ~ 2,
                         Index<=nrow(.)/5*3 ~ 3,
                         Index<=nrow(.)/5*4 ~ 4,
                         Index<=nrow(.)/5*5 ~ 5,
                         ),
         label=factor(label)) %>% 
  ggplot() + aes(x=cr_o,y=TMR_sum,color=label)+
  geom_point(size=0.5)+
  labs(x="パラメータ", y="出力の値") +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 25),
        legend.position = "none",
  )

p2 <- df_result_filtered %>% arrange(TMR_sum) %>% 
  mutate(Index=row_number(),
         label=case_when(Index<=nrow(.)/5 ~ 1,
                         Index<=nrow(.)/5*2 ~ 2,
                         Index<=nrow(.)/5*3 ~ 3,
                         Index<=nrow(.)/5*4 ~ 4,
                         Index<=nrow(.)/5*5 ~ 5,
         ),
         label=factor(label)) %>% 
  ggplot()+aes(x=cr_o,y=TMR_sum,color=label)+
  geom_line(stat="ecdf",linewidth=1.5) + 
  labs(x="パラメータ", y="CDF", fill=NULL, title="CDFプロット")+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 25),
        plot.title=element_blank(),
        legend.position = "none"
  )

ggsave(plot=p1,
       file=str_c(path,"Outputs/Others/Slides_手法説明_MappingCDF_1.jpg"),
       width=7,height=6,dpi="print")
ggsave(plot=p2,
       file=str_c(path,"Outputs/Others/Slides_手法説明_MappingCDF_2.jpg"),
       width=7,height=6,dpi="print")

