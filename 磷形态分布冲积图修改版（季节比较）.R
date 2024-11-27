# 加载需要的包，没用的话记得安装一下哟！
library(tidyverse)    # 提供用于数据处理和可视化的强大工具集，强推！
library(ggalluvial)   # 提供绘制流程图和河流图的功能
library(readxl)
# 加载数据
data18 <- read_excel("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\沉积物磷形态\\澜沧江论文\\min新整理\\数据\\重新分析\\磷形态冲积图\\沉积物磷形态（18+20）.xlsx",
                   sheet="2018")
data20<- read_excel("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\沉积物磷形态\\澜沧江论文\\min新整理\\数据\\重新分析\\磷形态冲积图\\沉积物磷形态（18+20）.xlsx",
                    sheet="2020")
head(data)



#R语言中的'plyr'包的ddply()函数，和常见的summarise()函数。
# library(plyr)
# zz<-dplyr(data16,c("site"),summarise,p1=mean(Ex_P,na.rm=TRUE),sd1=sd(Ex_P,na.rm=TRUE),p2=mean(Fe_P,na.rm=TRUE),sd2=sd(Fe_P,na.rm=TRUE),p3=mean(Al_P,na.rm=TRUE),sd3=sd(Al_P,na.rm=TRUE),p4=mean(Ca_P,na.rm=TRUE),sd4=sd(Ca_P,na.rm=TRUE),p5=mean(Res_P,na.rm=TRUE),sd5=sd(Res_P,na.rm=TRUE))
# zz

#dplyr 中使用 summarize 函数进行数据汇总时，通常要结合分组函数 group_by 一起使用
library(dplyr)
 #基于区域字段进行分组，求平均值
# data_area_mean_sd <- data %>%group_by(Area) %>% 
#   summarize(mean_Res_P = mean(Res_P),
#             mean_Fe_P = mean(Fe_P),
#             mean_Ex_P = mean(Ex_P),
#             mean_Ca_P = mean(Ca_P),
#             mean_Al_P = mean(Al_P),
#             mean_TIP = mean(TIP),
#             mean_Unbio_P = mean(Unbio_P),
#             mean_Bio_P = mean(Bio_P),
#             mean_Org_P = mean(Org_P),
#             mean_TP = mean(TP),
#             sd_Res_P = sd(Res_P),
#             sd_Fe_P = sd(Fe_P),
#             sd_Ex_P = sd(Ex_P),
#             sd_Ca_P = sd(Ca_P),
#             sd_Al_P = sd(Al_P),
#             sd_TIP = sd(TIP),
#             sd_Unbio_P = sd(Unbio_P),
#             sd_Bio_P = sd(Bio_P),
#             sd_Org_P = sd(Org_P),
#             sd_TP = sd(TP)
#   )

# write.csv(data_area_mean_sd,"不同区域磷形态描述统计.csv")



# # 按 site字段分组, 统计 lifeExp 的均值、对 pop 求和
# data_mean_sd <- data %>%group_by(site) %>% 
#   summarize(mean_Res_P = mean(Res_P),
#             mean_Fe_P = mean(Fe_P),
#             mean_Ex_P = mean(Ex_P),
#             mean_Ca_P = mean(Ca_P),
#             mean_Al_P = mean(Al_P),
#             mean_TIP = mean(TIP),
#             mean_Unbio_P = mean(Unbio_P),
#             mean_Bio_P = mean(Bio_P),
#             mean_Org_P = mean(Org_P),
#             mean_TP = mean(TP),
#             sd_Res_P = sd(Res_P),
#             sd_Fe_P = sd(Fe_P),
#             sd_Ex_P = sd(Ex_P),
#             sd_Ca_P = sd(Ca_P),
#             sd_Al_P = sd(Al_P),
#             sd_TIP = sd(TIP),
#             sd_Unbio_P = sd(Unbio_P),
#             sd_Bio_P = sd(Bio_P),
#             sd_Org_P = sd(Org_P),
#             sd_TP = sd(TP)
#             )%>%
#   arrange_at(2:14, desc) %>%
#   arrange(match(site, c(c('N01','N02','N03','N04','N05','N06','N07','N08','N09','L01','L02','L03','L04','L05','L06','L07','HD','MV','GG','XW','MW','DC','NZ','JH','GL'))))

data_mean18 <- data18[,c(1:11)] 
colnames(data_mean18 ) <- c('site','Res_P','Fe_P','Ex_P','Ca_P','Al_P','TIP','Unbio_P','Bio_P','Org_P','TP')

data_sd18 <- data18[,c(1,12:21)] 
colnames(data_sd18 ) <- c('site','Res_P','Fe_P','Ex_P','Ca_P','Al_P','TIP','Unbio_P','Bio_P','Org_P','TP')

data_mean20 <- data20[,c(1:11)] 
colnames(data_mean20 ) <- c('site','Res_P','Fe_P','Ex_P','Ca_P','Al_P','TIP','Unbio_P','Bio_P','Org_P','TP')

data_sd20 <- data20[,c(1,12:21)] 
colnames(data_sd20 ) <- c('site','Res_P','Fe_P','Ex_P','Ca_P','Al_P','TIP','Unbio_P','Bio_P','Org_P','TP')



#计算后输出数据框
# write.csv(data_mean_sd,file = "C:\\Users\\Lenovo\\Desktop\\沉积物磷形态平均值_标准差.csv")
# write.csv(data_mean_sd,file = "沉积物磷形态平均值_标准差.csv")
# write.csv(data_mean,file = "沉积物磷形态平均值.csv")
# write.csv(data_sd,file = "沉积物磷形态标准差.csv")

# 使用dplyr中的mutate函数为数据集添加一个ID列
# 使用pivot_longer函数将数据从宽格式转换为长格式
data_plot_Fracmean1816 <- 
data_mean18[,c(1:6)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_Fracmean1816)

data_plot_Fracsd1816 <- 
  data_sd18[,c(1:6)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_Fracsd1816)

data_plot18 <- cbind(data_plot_Fracmean1816,data_plot_Fracsd1816[,c(4)])


data_plot_Fracmean2016 <- 
  data_mean20[,c(1:6)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_Fracmean2016)

data_plot_Fracsd2016 <- 
  data_sd20[,c(1:6)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_Fracsd2016)

data_plot20 <- cbind(data_plot_Fracmean2016,data_plot_Fracsd2016[,c(4)])



data18pm <- 
  data_mean18[,c(1,7)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data18pm)

data18ps <- 
  data_sd18[,c(1,7)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data18ps)

data182 <- cbind(data18pm,data18ps[,c(4)])

data20pm <- 
  data_mean20[,c(1,7)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data20pm)

data20ps <- 
  data_sd20[,c(1,7)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data20ps)

data202 <- cbind(data20pm,data20ps[,c(4)])


#bio
data_plot_biomean_18 <- 
  data_mean18[,c(1,8:9)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_biomean_18)

data_plot_biosd_18 <- 
  data_sd18[,c(1,8:9)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_biosd_18)

data_plot180b <- cbind(data_plot_biomean_18,data_plot_biosd_18[,c(4)])



data_plot_biomean_20 <- 
  data_mean20[,c(1,8:9)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_biomean_20)

data_plot_biosd_20 <- 
  data_sd20[,c(1,8:9)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data_plot_biosd_20)

data_plot200b <- cbind(data_plot_biomean_20,data_plot_biosd_20[,c(4)])



data1811 <- 
  data_mean18[,c(1,10)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data1811)

data1812 <- 
  data_sd18[,c(1,10)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data1812)

data181b<- cbind(data1811,data1812[,c(4)])

data2011 <- 
  data_mean20[,c(1,10)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "value")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data2011)

data2012 <- 
  data_sd20[,c(1,10)] %>% 
  mutate(id = 1:n()) %>%  # 添加一个id列，以便于在ggplot中使用
  pivot_longer(-c(id, site), names_to = "Fractionation", values_to = "sd")   # 将数据从宽格式转换为长格式，并保存到data_plot数据框中
head(data2012)

data201b<- cbind(data2011,data2012[,c(4)])




col <- c("#0074b3","#982b2b","#459943",
         "#005496","#db6968")
col2<-c("#05609e","#ce4f55")


  
###### 绘制百分比堆叠连线柱形图#####
 


   data_plot18 %>% mutate(prop = round(value/sum(value)*100, 2), prop2 = round(sd/sum(value)*100, 2),.by = site)-> data_plot18           # 计算百分比
  data_plot18 %>% 
    group_by(id) %>% 
    mutate(new_col=cumsum(prop))-> data_plot18

  data_plot20 %>% mutate(prop = round(value/sum(value)*100, 2), prop2 = round(sd/sum(value)*100, 2),.by = site)-> data_plot20           # 计算百分比
  data_plot20 %>% 
    group_by(id) %>% 
    mutate(new_col=cumsum(prop)) -> data_plot20

  data_plot180b %>% mutate(prop = round(value/sum(value)*100, 2), prop2 = round(sd/sum(value)*100, 2),.by = site)-> data_plot180b           # 计算百分比
  data_plot180b %>% 
    group_by(id) %>% 
    mutate(new_col=cumsum(prop)) -> data_plot180b
  
  data_plot200b %>% mutate(prop = round(value/sum(value)*100, 2), prop2 = round(sd/sum(value)*100, 2),.by = site)-> data_plot200b           # 计算百分比
  data_plot200b %>% 
    group_by(id) %>% 
    mutate(new_col=cumsum(prop)) -> data_plot200b
  
  
# write.csv(data_plot,"磷形态百分比.csv")
# write.csv(data_plot2,"生物可利用磷百分比.csv") 
  
P181 <-   ggplot() + 
    annotate("rect", xmin = c(0,10.5,18.5,27.5), xmax = c(9.5,17.5,26.5,28.5),
             ymin = -5, ymax = 105,
             fill = c("grey89","grey89","grey64","grey89"), alpha =0.5)+#添加自定义矩形
    # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
    #          position = "fill")+
    geom_stratum(data=data_plot18,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
    geom_errorbar(data=data_plot18,aes(x=id,ymax=new_col+prop2,ymin=new_col-prop2),width=0.5,linewidth=1,position = position_dodge(0.9),color=c("black"))+# 绘制层级区域
    geom_flow(data=data_plot18,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
    geom_alluvium(data=data_plot18,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                  knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
    geom_line(data = data182,aes(x = id,y = value/8),color="skyblue",linewidth=3,alpha=0.8) +
    geom_errorbar(data=data182,aes(x=id,ymax=(value+sd)/8,ymin=(value-sd)/8),width=0.5,linewidth=1,position = position_dodge(0.9),color="skyblue")+
    geom_point(data = data182,aes(x = id , y = value/8 ),shape=21,color="skyblue",fill="white",size=5,stroke = 2,alpha=1)+
    scale_fill_manual(values = col) + 
    scale_color_manual(values = "skyblue") +# 设置填充颜色
    scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean18$site,name = NULL) +  # 设置x轴标签和刻度
    scale_y_continuous(expand = c(0, -0.5),name = 'Percentage',limit=c(-5,105),sec.axis = sec_axis( ~.*8, name = "TIP(mg/kg)") )+  # 设置y轴标签和刻度
    theme_minimal() + 
    theme(axis.ticks.length.x = unit(0.1,'cm'), 
          axis.ticks.length.y = unit(-0.1,'cm'), 
          axis.ticks.x = element_line(colour = "black",linewidth = 1),
          axis.ticks.y = element_line(colour = "black",linewidth = 1),
          axis.line.x = element_line(colour = "black",linewidth = 1),
          axis.line.y = element_line(colour = "black",linewidth = 1)
          )+    ## 设置刻度标签的粗细
    theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.title.x = element_text( size = 12),
          axis.title.y = element_text( size = 12))+
      theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
    theme(plot.background = element_rect(fill='white', color='white'), 
          plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
          panel.grid = element_blank()) 
    
  
P182 <-   ggplot() + 
  annotate("rect", xmin = c(0,10.5,18.5,27.5), xmax = c(9.5,17.5,26.5,28.5),
           ymin = -5, ymax = 105,
           fill = c("grey89","grey89","grey64","grey89"), alpha =0.5)+#添加自定义矩形
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=data_plot180b,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_errorbar(data=data_plot180b,aes(x=id,ymax=new_col+prop2,ymin=new_col-prop2),width=0.5,linewidth=1,position = position_dodge(0.9),color=c("black"))+# 绘制层级区域
  geom_flow(data=data_plot180b,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=data_plot180b,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  geom_line(data = data181b,aes(x = id,y = value/1.8),color="#c3d43c",linewidth=3,alpha=0.8) +
  geom_errorbar(data=data181b,aes(x=id,ymax=(value+sd)/1.8,ymin=(value-sd)/1.8),width=0.5,linewidth=1,position = position_dodge(0.9),color="#c3d43c")+
  geom_point(data = data181b,aes(x = id , y = value/1.8),shape=21,color="#c3d43c",fill="white",size=5,stroke = 2,alpha=1)+
  scale_fill_manual(values = col2) +  
  scale_color_manual(values = "#c3d43c") +
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean18$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(-5,105),sec.axis = sec_axis( ~.*1.5, name = "Org_P(mg/kg)") )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",linewidth = 1),
        axis.ticks.y = element_line(colour = "black",linewidth  = 1),
        axis.line.x = element_line(colour = "black",linewidth  = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())

P201 <-   ggplot() + 
  annotate("rect", xmin = c(0,10.5,18.5,27.5), xmax = c(9.5,17.5,26.5,28.5),
           ymin = -5, ymax = 105,
           fill = c("grey89","grey89","grey64","grey89"), alpha =0.5)+#添加自定义矩形
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=data_plot20,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_errorbar(data=data_plot20,aes(x=id,ymax=new_col+prop2,ymin=new_col-prop2),width=0.5,linewidth=1,position = position_dodge(0.9),color=c("black"))+# 绘制层级区域
  geom_flow(data=data_plot20,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=data_plot20,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  geom_line(data = data202,aes(x = id,y = value/8),color="skyblue",linewidth=3,alpha=0.8) +
  geom_errorbar(data=data202,aes(x=id,ymax=(value+sd)/8,ymin=(value-sd)/8),width=0.5,linewidth=1,position = position_dodge(0.9),color="skyblue")+
  geom_point(data = data202,aes(x = id , y = value/8 ),shape=21,color="skyblue",fill="white",size=5,stroke = 2,alpha=1)+
  scale_fill_manual(values = col) + 
  scale_color_manual(values = "skyblue") +# 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean20$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, -0.5),name = 'Percentage',limit=c(-5,105),sec.axis = sec_axis( ~.*8, name = "TIP(mg/kg)") )+  # 设置y轴标签和刻度
  theme_minimal() + 
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",linewidth = 1),
        axis.ticks.y = element_line(colour = "black",linewidth = 1),
        axis.line.x = element_line(colour = "black",linewidth = 1),
        axis.line.y = element_line(colour = "black",linewidth = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank()) 


P202 <-   ggplot() + 
  annotate("rect", xmin = c(0,10.5,18.5,27.5), xmax = c(9.5,17.5,26.5,28.5),
           ymin = -5, ymax = 105,
           fill = c("grey89","grey89","grey64","grey89"), alpha =0.5)+#添加自定义矩形
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=data_plot200b,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_errorbar(data=data_plot200b,aes(x=id,ymax=new_col+prop2,ymin=new_col-prop2),width=0.5,linewidth=1,position = position_dodge(0.9),color=c("black"))+# 绘制层级区域
  geom_flow(data=data_plot200b,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=data_plot200b,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  geom_line(data = data201b,aes(x = id,y = value/1.8),color="#c3d43c",linewidth=3,alpha=0.8) +
  geom_errorbar(data=data201b,aes(x=id,ymax=(value+sd)/1.8,ymin=(value-sd)/1.8),width=0.5,linewidth=1,position = position_dodge(0.9),color="#c3d43c")+
  geom_point(data = data201b,aes(x = id , y = value/1.8),shape=21,color="#c3d43c",fill="white",size=5,stroke = 2,alpha=1)+
  scale_fill_manual(values = col2) +  
  scale_color_manual(values = "#c3d43c") +
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean20$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(-5,105),sec.axis = sec_axis( ~.*1.5, name = "Org_P(mg/kg)") )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",linewidth = 1),
        axis.ticks.y = element_line(colour = "black",linewidth  = 1),
        axis.line.x = element_line(colour = "black",linewidth  = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())


library(patchwork)
P3+P4

# library(cowplot)
# plot_grid(P1,P2,ncol=1)
#12:18
p18 <- P181/P182
p20 <- P201/P202



library ("berryFunctions")


easydf1 = data_plot180b[1:34,]
easydf2 = data_plot180b[-1:-34,]

easydf21 = data_plot200b[1:34,]
easydf22 = data_plot200b[-1:-34,]
P1803 <-   ggplot() + 
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=easydf1,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_flow(data=easydf1,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=easydf1,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  scale_fill_manual(values = col2) +  
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean18$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(0,100) )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),
        axis.ticks.y = element_line(colour = "black",size = 1),
        axis.line.x = element_line(colour = "black",size = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())+
  geom_vline(xintercept =c(10),  # 添加垂直于x轴并且经c的直线
             yintercept = NULL,      # 添加一条垂直于y轴并且经过c的直线,yintercept = c(-Inf,+Inf)
             linetype = "dashed",
             color="red",
             linewidth=1)

P1804 <-   ggplot() + 
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=easydf2,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_flow(data=easydf2,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.3, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=easydf2,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.1, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  scale_fill_manual(values = col2) +  
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean18$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(0,100) )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),
        axis.ticks.y = element_line(colour = "black",size = 1),
        axis.line.x = element_line(colour = "black",size = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())+
  geom_vline(xintercept =c(27),  # 添加垂直于x轴并且经c的直线
             yintercept = NULL,      # 添加一条垂直于y轴并且经过c的直线,yintercept = c(-Inf,+Inf)
             linetype = "dashed",
             color="red",
             linewidth=1)



P2003 <-   ggplot() + 
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=easydf21,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_flow(data=easydf21,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.25, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=easydf21,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.5, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  scale_fill_manual(values = col2) +  
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean20$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(0,100) )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),
        axis.ticks.y = element_line(colour = "black",size = 1),
        axis.line.x = element_line(colour = "black",size = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())+
  geom_vline(xintercept =c(10),  # 添加垂直于x轴并且经c的直线
             yintercept = NULL,      # 添加一条垂直于y轴并且经过c的直线,yintercept = c(-Inf,+Inf)
             linetype = "dashed",
             color="red",
             linewidth=1)

P2004 <-   ggplot() + 
  # geom_bar(data=data_plot,aes(x=id, y=prop, fill = Fractionation),stat = "identity",
  #          position = "fill")+
  geom_stratum(data=easydf22,aes(x=id, y=prop, fill = Fractionation,stratum=Fractionation), color=NA, width = 0.65) +
  geom_flow(data=easydf22,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation), knot.pos = 0.3, width = 0.5, alpha=0.2) +   # 绘制流动连接线
  geom_alluvium(data=easydf22,aes(x=id, y=prop, fill=Fractionation,stratum=Fractionation,alluvium=Fractionation),                                           # 绘制所有uvium图层
                knot.pos = 0.1, color="red", linetype="dashed",width=0.7, linewidth=0.7, fill=NA, alpha=0.5) +
  scale_fill_manual(values = col2) +  
  # 设置填充颜色
  scale_x_continuous(expand = c(0, 0.1), breaks = 1:28, labels = data_mean20$site,name = NULL) +  # 设置x轴标签和刻度
  scale_y_continuous(expand = c(0, 0),name = 'Percentage',limit=c(0,100) )+  # 设置y轴标签和刻度
  theme_minimal() +
  theme(axis.ticks.length.x = unit(0.1,'cm'), 
        axis.ticks.length.y = unit(-0.1,'cm'), 
        axis.ticks.x = element_line(colour = "black",size = 1),
        axis.ticks.y = element_line(colour = "black",size = 1),
        axis.line.x = element_line(colour = "black",size = 1),
        axis.line.y = element_line(colour = "black",size = 1)
  )+    ## 设置刻度标签的粗细
  theme(axis.text.x = element_text(size = 12, angle=30,colour = "black",vjust = 0),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text( size = 12),
        axis.title.y = element_text( size = 12))+
  theme(legend.position = "right", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10))+ # 使用极简主题风格
  theme(plot.background = element_rect(fill='white', color='white'), 
        plot.margin = margin(0.5,2.5,0.5,2.5, unit = "cm"),# 设置绘图区背景色
        panel.grid = element_blank())+
  geom_vline(xintercept =c(27),  # 添加垂直于x轴并且经c的直线
             yintercept = NULL,      # 添加一条垂直于y轴并且经过c的直线,yintercept = c(-Inf,+Inf)
             linetype = "dashed",
             color="red",
             linewidth=1)





library(patchwork)
P1803/P2003
P1804/P2004
# library(cowplot)
# plot_grid(P1,P2,ncol=1)
p0 <- P1/P2

library(eoffice)
topptx(p0,filename="磷形态冲积图.pptx",height = 12,width = 6)