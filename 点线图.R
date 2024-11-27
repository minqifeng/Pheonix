#设置工作环境
rm(list = ls())
setwd("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\沉积物磷形态\\澜沧江论文\\min新整理\\数据\\重新分析\\环境因子点线图")

##加载R包
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggpubr) # 'ggplot2' Based Publication Ready Plots
library(readxl) 
library(tidyverse) 
##加载数据（随机编写，无实际意义）
df <- read_excel("环境因子.xlsx")
df$Group <- factor(df$Group, levels = c("Region I", "Region II", "Region III"))
df$Sites<- factor(df$Sites,levels = c("N01","N02","N03","N04","N05","N06","N07","N08","N09","N","L01","L02","L03","L04","L05","L06","L07","L","HD","MV","GG","XW","MW","DC","NZ","JH"))

# 使用pivot_longer函数将数据从宽格式转换为长格式
data <- 
  pivot_longer(df,-c(Sites, Group), names_to = "parameters", values_to = "value")  
data$parameters<- factor(data$parameters,levels = c("ORP","DO",	"TOC","TPP","Temp.","pH","Chl.a",	"SpCond",	"TD",	"DTP","FV",	"Turb."))


##绘图
p1 <- ggplot(data, aes(Sites,value))+
  #折线
  geom_line(aes(group=parameters), color="#156077",linewidth=2,alpha=1)+
  #散点
  geom_point(shape=21,size = 4,fill="white",color= "#4da0a0",stroke=2)+
  #分面
  facet_wrap(~parameters,scales = "free_y",nrow = 4,labeller = "label_both")+
  #主题相关设置
  labs(x= NULL, y= "Absolute quantity value", color = "black", shape = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,color="black"))
p1

##转置横纵坐标
p2 <- p1+coord_flip()+
  theme(legend.position = "none")
p2

##拼图
cowplot::plot_grid(p2,p1,ncol = 2,
                   rel_widths = c(0.72, 1))

