### 製品の選択
tgt_product <- "デジタルカメラ"
# tgt_product <- "スマートフォン"

df_result <- 
  str_c(path,"Outputs/Results/",tgt_product,
        "/BasicAnalysis/",tgt_product,"_df_result_TwoStrategies.csv") %>% 
  read_csv()
if (tgt_product=="デジタルカメラ"){
  y_intercept_temp <- 0.06
} else if(tgt_product=="スマートフォン"){
  y_intercept_temp <- 0.02
}

df_result_2 <- df_result %>% mutate(TMR_imp_rate=1-TMR_sum/TMR_sum[[1]],
                                    GHG_imp_rate=1-GHG_sum/GHG_sum[[1]],
                                    FD_imp_rate=1-FD_sum/FD_sum[[1]]) %>%
  mutate(strategy=case_when(strategy=="Recycle2_Reman" ~ "リサイクル2 × リマン",
                            strategy=="Recycle2_Reuse2" ~ "リサイクル2 × リユース2",
                            strategy=="Recycle2_Rental" ~ "リサイクル2 × レンタル",
                            strategy=="Recycle2_LE" ~ "リサイクル2 × 使用期間延長",
                            strategy=="Reman_Reuse2" ~ "リマン × リユース2",
                            strategy=="Reman_Rental" ~ "リマン × レンタル",
                            strategy=="Reman_LE" ~ "リマン × 使用期間延長",
                            strategy=="Reuse2_Rental" ~ "リユース2 × レンタル",
                            strategy=="Reuse2_LE" ~ "リユース2 × 使用期間延長",
                            strategy=="Rental_LE" ~ "レンタル × 使用期間延長",
                            strategy=="Reuse1_LE" ~ "リユース1 × 使用期間延長",
                            strategy=="Reuse1_Recycle2" ~ "リユース1 × リサイクル2",
                            strategy=="Reuse1_Reuse2" ~ "リユース1 × リユース2",
                            strategy=="Reuse1_Reman" ~ "リユース1 × リマン",
                            strategy=="Reuse1_Rental" ~ "リユース1 × レンタル",
                            strategy=="Recycle1_Reman" ~ "リサイクル1 × リマン",
                            strategy=="Recycle1_Reuse2" ~ "リサイクル1 × リユース2",
                            strategy=="Recycle1_Rental" ~ "リサイクル1 × レンタル",
                            strategy=="Recycle1_LE" ~ "リサイクル1 × 使用期間延長",
                            strategy=="Reuse1_Recycle1" ~ "リユース1 × リサイクル1",
                            strategy=="Recycle1_Recycle2" ~ "リサイクル1 × リサイクル2",
                            TRUE ~ strategy))

# GHG & TMR Plot -Combinations of 2 strategies- -----------------------------------------------------------

df_result_2 %>% 
  slice(-1) %>%
  ggplot() +
  geom_point(aes(x=TMR_imp_rate, y=GHG_imp_rate, colour=strategy),
             size = 3.0) +
  geom_vline(xintercept = y_intercept_temp,
             linetype = "dotted",
             color = "red",
             linewidth = 0.8) +
  scale_x_continuous(labels=percent_format(accuracy = 0.1)) +#,breaks=seq(0.0,0.1,by=0.02)
  scale_y_continuous(labels=percent_format(accuracy = 1)) + #,breaks=seq(0.0,0.1,by=0.02)
  theme(axis.text = element_text(size = 10),plot.title = element_text(size = 12),
        plot.margin= unit(c(0.5, 1, 0.5, 0.5), "lines"),
        legend.position="right",
        legend.margin = ggplot2::margin(0.5, 0, 0, 0.5, "cm")) +
  labs(x="TMR 改善率", y="GHG排出量 改善率", colour=NULL)
ggsave(filename=str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_TwoStrategies_ALL.jpg"),
       width=10.0,height=5.0,dpi="print")

# TMRだけ -------------------------------------------------------------------

df_result_2 %>% 
  slice(-1) %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(strategy, TMR_imp_rate), y=TMR_imp_rate),width=0.8) +
  # scale_y_continuous(labels=percent_format(accuracy = 1),breaks=seq(0.0,0.1,by=0.02)) + #デジカメ
  scale_y_continuous(labels=percent_format(accuracy = 0.1)) +
  geom_hline(yintercept = y_intercept_temp,
             linetype = "dotted",
             color = "red",
             linewidth = 0.8) +
  coord_flip() +
  labs(x="シナリオ", y="TMR 改善率", fill=NULL)+
  theme(axis.text = element_text(size = 10),plot.title = element_text(size = 12),
        plot.margin= unit(c(0.5, 1, 0.5, 0.5), "lines"))
ggsave(filename=str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_TwoStrategiesTMR.jpg"),
       width=18.0,height=12.0,units="cm")

# GHGだけ -------------------------------------------------------------------

p_GHG <- df_result_2 %>% 
  slice(-1) %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(strategy, GHG_imp_rate), y=GHG_imp_rate),width=0.8) +
  # scale_y_continuous(labels=percent_format(accuracy = 1),breaks=seq(-0.04,0.16,by=0.02)) +#デジカメ
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  coord_flip() +
  labs(x="シナリオ", y="GHG排出量 改善率", fill=NULL)+
  theme(axis.text = element_text(size = 10),plot.title = element_text(size = 12),
        plot.margin= unit(c(0.5, 1, 0.5, 0.5), "lines"))
ggsave(p_GHG,filename=str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_TwoStrategiesGHG.jpg"),
       width=18.0,height=12.0,units="cm")

# FDだけ --------------------------------------------------------------------

p_FD <- df_result_2 %>% 
  slice(-1) %>%
  ggplot() +
  geom_col(aes(x=fct_reorder(strategy, FD_imp_rate), y=FD_imp_rate),width=0.8) +
  # scale_y_continuous(labels=percent_format(accuracy = 1),breaks=seq(0.0,0.04,by=0.01)) +#デジカメ
  scale_y_continuous(labels=percent_format(accuracy = 1)) +
  coord_flip() +
  labs(x="シナリオ", y="処理残渣量 改善率", fill=NULL)+
  theme(axis.text = element_text(size = 10),plot.title = element_text(size = 12),
        plot.margin= unit(c(0.5, 1, 0.5, 0.5), "lines"))
ggsave(p_FD,filename=str_c(path,"Outputs/Results/",tgt_product,"/BasicAnalysis/",tgt_product,"_TwoStrategiesFD.jpg"),
       width=18.0,height=12.0,units="cm")


