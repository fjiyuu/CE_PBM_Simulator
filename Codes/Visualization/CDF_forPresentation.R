
for (tgt_product in product_list){
  source(str_c(path,"Codes/RSA/Load_MCResult.R"))
  for (target_evaluator in c("TMR","FD","GHG")){
    if(target_evaluator=="TMR"){
      print(target_evaluator)
      df_arranged <- df_result_filtered %>% arrange(TMR_sum)
    } else if(target_evaluator=="GHG"){
      print(target_evaluator)
      df_arranged <- df_result_filtered %>% arrange(GHG_sum)
    } else if(target_evaluator=="FD"){
      print(target_evaluator)
      df_arranged <- df_result_filtered %>% arrange(FD_sum)
    } else{
      print("ERROR")
    }
    df_res_label_10 <- df_arranged %>% 
      mutate(Index=row_number(),
             label=case_when(Index<=nrow(.)/10 ~ 1,
                             Index<=nrow(.)/10*2 ~ 2,
                             Index<=nrow(.)/10*3 ~ 3,
                             Index<=nrow(.)/10*4 ~ 4,
                             Index<=nrow(.)/10*5 ~ 5,
                             Index<=nrow(.)/10*6 ~ 6,
                             Index<=nrow(.)/10*7 ~ 7,
                             Index<=nrow(.)/10*8 ~ 8,
                             Index<=nrow(.)/10*9 ~ 9,
                             Index<=nrow(.)/10*10 ~ 10)) %>% 
      select(r_drusA_max,epsilon_max,r_rmn_max,r_rcy_max,r_rcy2_max,w_max,r_LE_max,
             cr_o,sr_B,Rental_period,Rir,CE_period,label)
    ### All vars
    col_list <- colnames(df_res_label_10 %>% select(-label))
    param_names <- c("リユース率1", "リユース率2","リマン率", "リサイクル率1","リサイクル率2", 
                     "レンタル率", "使用期間延長割合","回収率", "リユース品使用期間比率",
                     "レンタル期間","レンタル品使用頻度比率","戦略導入期間")
    plots <- list()
    for (i in 1:length(col_list)){
      plots[[i]] <- df_res_label_10 %>% 
        mutate(label=str_c("グループ",as.character(label))) %>% 
        mutate(label=factor(label,levels= str_c("グループ",c(1,2,3,4,5,6,7,8,9,10)))) %>% 
        ggplot()+aes_string(x=col_list[[i]],colour="label")+
        geom_line(stat="ecdf",linewidth=0.7) + 
        scale_colour_manual(name = 'label', guide = 'legend',
                            values = c("グループ1"="red", "グループ2"="coral2", "グループ3"="orange",
                                       "グループ4"="yellow","グループ5"="springgreen",
                                       "グループ6"="green4","グループ7"="turquoise",
                                       "グループ8"="dodgerblue","グループ9"="royalblue","グループ10"="purple"),
        )+
        labs(x=NULL, y="CDF", fill=NULL, title=param_names[[i]])+
        theme(axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.y = element_text(size = 14),
              plot.title = element_text(size = 15, hjust=0.5, vjust=1),
              plot.margin= unit(c(t=0.5, r=1, b=0.5, l=0.5), "lines"),
              # 0.5, 1, 2.0, 0.5
              legend.title = element_blank(), 
              legend.key.size = unit(1,"cm"), 
              legend.text = element_text(size=14),
              legend.position="none"
        ) 
    }
    if (target_evaluator=="GHG"){
      ppp <- ggarrange(plots[[1]],plots[[6]],plots[[7]], plots[[8]],  ncol=2,nrow=2,
                       common.legend=F
      )  
    } else if(target_evaluator=="TMR"){
      ppp <- ggarrange(plots[[1]],plots[[4]],plots[[5]], plots[[8]],  ncol=2,nrow=2,
                       common.legend=F
      )
    } else{
      ppp <- ggarrange(plots[[1]],plots[[4]],plots[[5]], plots[[8]],  ncol=2,nrow=2,
                       common.legend=F
      )
    }
    ggsave(ppp,
           file=str_c(path,"Outputs/Results/",tgt_product,"/RSA/Presentation/Slides_",tgt_product,"_10Category_",target_evaluator,"_Allvars.jpg"),
           width=7,height=5,dpi="print")
  }
}

# 凡例の図作成 ------------------------------------------------------------------
color_vec <- c("グループ1"="red", "グループ2"="coral2", "グループ3"="orange",
               "グループ4"="yellow","グループ5"="springgreen",
               "グループ6"="green4","グループ7"="turquoise",
               "グループ8"="dodgerblue","グループ9"="royalblue","グループ10"="purple")
p <- tibble(a=1,group=seq(1,10),col=color_vec) %>% 
  mutate(group=factor(str_c("グループ",group),levels= str_c("グループ",c(1,2,3,4,5,6,7,8,9,10)))) %>% 
  ggplot()+aes(x=a,fill=group)+
  geom_bar(position="stack") + 
  scale_fill_manual(
    values = color_vec,
  ) + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 15, hjust=0.5, vjust=1),
        plot.margin= unit(c(0, 0, 0, 0), "lines"),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks=element_blank(),
        plot.background=element_blank(),
  )
ggsave(plot=p,
       file=str_c(path,"Outputs/Others/10Group_Legends.jpg"),
       width=2,height=6,dpi="print")
