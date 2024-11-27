
##加载R包（没有安装相关包的同学可以先安装相应的R包）
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(psych) # Procedures for Psychological, Psychometric, and Personality Research
library(reshape2) # Flexibly Reshape Data: A Reboot of the Reshape Package
library(dplyr) # A Grammar of Data Manipulation
library(agricolae) # Statistical Procedures for Agricultural Research


setwd("E:/S研究材料与学术基础/M-Materials/黑臭水体治理与河湖生态修复/沉积物磷形态/澜沧江论文/min新整理/数据/重新分析/相关性热图")


#####内层热图——颜色与样本归一化值呈正相关
#加载数据
df3 <- read.table("df1.txt", sep="\t", header=T, check.names=F,row.names = 1)
df3$Sites <- rownames(df3)
group <- read.table("group.txt", sep="\t", header=T, check.names=F)
#合并分组与数据
df3 <- merge(df3,group,by="Sites")
rownames(df3) <- df3$Sites
df3 <- df3[-1]
#将宽数据转换为长数据
df4 <- melt(df3, id.vars = c("Group"))
##利用循环并采用多重比较法比较各组显著性
#初始化
variance<-aov(D4 ~ Group, data=df3)
MC <- LSD.test(variance,"Group", p.adj="BH")#p.adj="BH"
GB<- group_by(df3,Group)
sg<- MC$groups
sg$group <- rownames(sg)
#修改列名
colnames(sg)[2] <- "D4_label"
for (i in colnames(df3[2:33])) {
  variance<-aov(df3[,i] ~ Group, data=df3)
  MC <- LSD.test(variance,"Group", p.adj="BH")
  data <- MC$groups
  data$group <- rownames(data)
  colnames(data)[1:2]<-c(i,paste0(i,"_label"))
  sg<-merge(sg,data,by="group")
}
#将数据和字母分开储存
df_label <- sg[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65)]
df_data <- sg[c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64)]
rownames(df_data) <- df_data$group
#数据标准化（0-1）——根据个人数据选择方法处理
df_data2 <- as.data.frame(lapply(df_data[2:33], function(x) (x - min(x)) / (max(x) - min(x))))
df_data2$group <- df_data$group
#转换为长数据
df_label2 <- melt(df_label, id.vars = c("group"))
df_data3 <- melt(df_data2, id.vars = c("group"))
##增加空白间隔-通过转换为数值型x轴实现
#这里在As和Cu之间增加空白间隔并在首位增加间隔
#添加x列数据
df_data3$x <- rep(c(1:7,10:22,25:36),each=3)
df_label2$x <- rep(c(1:7,10:22,25:36),each=3)
#添加y列数据
df_data3$y <- rep(c(3:5),Time=32)
df_label2$y <- rep(c(3:5),Time=32)
##计算标签角度
number_of_bar <- 40
angle <-  90 - 360 * (df_label2$x-0.5) /(number_of_bar)
df_label2$angle<-ifelse(angle < -90, angle+180, angle)

##基于ggplot2绘图
ggplot()+
  #绘制热图
  geom_tile(data=df_data3,aes(x,y,fill=value),#x轴和y轴都需要用添加的数值型数据绘制
            color="grey",linewidth=0.6)+
  #自定义颜色
  scale_fill_gradientn(limit = c(0, 1), colors = c('white', '#fd5c63'))+
  #添加显著性标签
  geom_text(data=df_label2,aes(x,y,label = value))+#x轴和y轴都需要用添加的数值型数据绘制,angle=angle
  #转换为极坐标
  coord_polar()+
  #主题相关设置
  theme_void()+
  theme(panel.grid = element_blank(),
        axis.text.x=element_text(size = 11, color = "black"),
        legend.position = "top")+
  #去除轴标题并指定图例标题
  labs(x=NULL,y=NULL,fill="Normalize")+
  #自定义x轴标签，注意需要按照此前添加顺序指定
  scale_x_continuous(limits = c(0,40),
                     breaks = c(1:7,10:22,25:36),
                     labels = c('D4',	'D63',	'D125',	'D250',	'D500',	'D1000',	'D2000',	'Quartz',	'Illite',	'Albite',	'K-Feldspar',	'Chlorite',	'Calcite',	'Dolomite',	'Tremolite', 'Kaolinite',	'Koninchite',	'Pyrite',	'Feldspar',	'clay minerals',	'ORP',	'TOC',	'Temp.',	'pH',	'DO',	'TPP',  'Chl.a',	'SpCond',	'TD','DTP',	'FV',	'Turb.'))+
  #设置y轴范围
  #注意：这里的范围内层与外层图要一致且同时包含内外层，以便后续在AI中拼图
  scale_y_continuous(limits =  c(-0.5,15.5))+
  ## removing axes labels and ticks
  theme(
    axis.text.x = element_blank()
  )+
  #添加内层分组
  annotate("rect", xmin = 0.5, xmax = 7.5, ymin = 2.1, ymax = 2.4, fill="#00c4ff")+
  annotate("rect", xmin = 9.5, xmax = 22.5, ymin = 2.1, ymax = 2.4, fill="#11862f")+
  annotate("rect", xmin = 24.5, xmax = 36.5, ymin = 2.1, ymax = 2.4, fill="red")+
  #手动添加y轴标签
  annotate("text", x = 40, y = 3, label = "Region I", size=3.5, color = "black",angle=15)+
  annotate("text", x = 40 , y = 4, label = "Region II", size=3.5, color = "black",angle=15)+
  annotate("text", x = 40 , y = 5, label = "Region III", size=3.5, color = "black",angle=15)->p1
p1



#####外层热图——pearson相关性
#加载数据（随机编写，无实际意义）
df1 <- read.table("df1.txt", sep="\t", header=T, check.names=F)
df2 <- read.table("df2.txt", sep="\t", header=T, check.names=F)
#合并数据
data <- merge(df1,df2,by="Sites")
#将行名设置为样本名并删除多余列
rownames(data) <- data$Sites
data <- data[-1]
#计算相关性并提取R值与P值。校正选择为“none”或“BH”
cor<- corr.test(data, method="pearson",use="na.or.complete",adjust="BH")#校正
r.cor<-data.frame(cor$r)[1:32,33:41]
p.cor<-data.frame(cor$p)[1:32,33:41]
#将宽数据转换为长数据
r.cor$G <- rownames(r.cor)
df_r <- melt(r.cor, id.vars = c("G"), 
             measure.vars = c('Ex.P','Fe.P','Al.P','Ca.P','Res.P','TIP','Bio.P','Unbio.P','Org.P'))
#指定顺序
df_r$G <- factor(df_r$G,levels = c('D4',	'D63',	'D125',	'D250',	'D500',	'D1000',	'D2000',	'Quartz',	'Illite',	'Albite',	'K-Feldspar',	'Chlorite',	'Calcite',	'Dolomite',	'Tremolite', 'Kaolinite',	'Koninchite',	'Pyrite',	'Feldspar',	'clay minerals',	'ORP',	'TOC',	'Temp.',	'pH',	'DO',	'TPP',  'Chl.a',	'SpCond',	'TD','DTP',	'FV',	'Turb.'))
p.cor$G <- rownames(p.cor)
df_p <- melt(p.cor, id.vars = c("G"), 
             measure.vars =  c('Ex.P','Fe.P','Al.P','Ca.P','Res.P','TIP','Bio.P','Unbio.P','Org.P'))
#通过不同p值转换为*
df_p$sg <- ifelse(df_p$value>=0.05, "", ifelse(df_p$value<0.05&df_p$value>0.01,"*", 
                                               ifelse(df_p$value<=0.01&df_p$value>0.001,"**","***")))
##增加空白间隔-通过转换为数值型x轴实现
#这里在As和Cu之间增加空白间隔并在首位增加间隔
#添加x列数据
df_r$x <- c(1:7,10:22,25:36)
df_p$x <- c(1:7,10:22,25:36)
##将外层图片y轴设置为紧邻内层数据并间隔1
df_r$y <- rep(c(7:15),each=32)
df_p$y <- rep(c(7:15),each=32)
##计算标签角度
number_of_bar <- 40
angle <-  180 - 360 * (df_p$x-0.5) /(number_of_bar)
df_p$angle<-ifelse(angle < -90, angle+180, angle)

#基于ggplot2绘图
ggplot()+
  #热图
  geom_tile(data=df_r,aes(x,y,fill=value),#x轴和y轴都需要用添加的数值型数据绘制
            color="grey",linewidth=0.6)+
  #自定义颜色
  scale_fill_gradientn(limit = c(-1, 1), colors = c("#db6968", 'white',"#4d97cd"))+
  #显著性标签
  geom_text(data=df_p,aes(x,y,label = sg,angle=angle))+#x轴和y轴都需要用添加的数值型数据绘制,angle=angle
  #极坐标
  coord_polar()+
  #主题设置
  theme_void()+
  theme(panel.grid = element_blank(),
        axis.text.x=element_text(size = 11, color = "black"),
        legend.position = "top")+
  #去除轴标题并指定图例标题
  labs(x=NULL,y=NULL,fill="Pearson r")+
  #自定义x轴标签，注意需要按照此前添加顺序指定
  scale_x_continuous(limits = c(0,40),
                     breaks = c(1:7,10:22,25:36),
                     labels = c('D4',	'D63',	'D125',	'D250',	'D500',	'D1000',	'D2000',	'Quartz',	'Illite',	'Albite',	'K-Feldspar',	'Chlorite',	'Calcite',	'Dolomite',	'Tremolite', 'Kaolinite',	'Koninchite',	'Pyrite',	'Feldspar',	'clay minerals',	'ORP',	'TOC',	'Temp.',	'pH',	'DO',	'TPP',  'Chl.a',	'SpCond',	'TD','DTP',	'FV',	'Turb.'))+
  #设置y轴范围
  #注意：这里的范围内层与外层图要一致且同时包含内外层，以便后续在AI中拼图
  scale_y_continuous(limits = c(-0.5,15.5))+
  #自定义y轴标签
  annotate("text", x = 40 , y = 7, label = "Ex-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 8, label = "Fe-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 9, label = "Al-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 10, label = "Ca-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 11, label = "Res-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 12, label = "TIP", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 13, label = "Bio-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40 , y = 14, label = "Unbio-P", size=3, color = "black",angle=15)+
  annotate("text", x = 40, y = 15, label = "Org-P", size=3, color = "black",angle=15)->p2
p2


###由于ggplot2包中极坐标形式图形的拼图限制，这里需要将图形保存后在AI软件中拼接
##拼接方法：将图1选中平移到图2中心即可
library(patchwork)
p1+p2+
  plot_layout(guides = 'collect')
##最后在AI软件中拼图并对细节进行调整



######拓展——常规绘制并进行拼接
#内层图形
ggplot()+
  geom_tile(data=df_data3,aes(x,y,fill=value),
            color="grey",linewidth=0.6)+
  scale_fill_gradientn(limit = c(0, 1), colors = c('white', '#fd5c63'))+
  geom_text(data=df_label2,aes(x,y,label = value))+
  theme_void()+
  theme(panel.grid = element_blank(),
        axis.text.x=element_text(size = 11, color = "black"),
        legend.position = "top")+
  labs(x=NULL,y=NULL,fill="Normalize")+
   scale_x_continuous(limits = c(0,40),
                     breaks = c(1:7,10:22,25:36),
                     labels = c('D4',	'D63',	'D125',	'D250',	'D500',	'D1000',	'D2000',	'Quartz',	'Illite',	'Albite',	'K-Feldspar',	'Chlorite',	'Calcite',	'Dolomite',	'Tremolite', 'Kaolinite',	'Koninchite',	'Pyrite',	'Feldspar',	'clay minerals',	'ORP',	'TOC',	'Temp.',	'pH',	'DO',	'TPP',  'Chl.a',	'SpCond',	'TD','DTP',	'FV',	'Turb.'))+
  #设置y轴范围
  #注意：这里的范围内层与外层图要一致且同时包含内外层，以便后续在AI中拼图
  scale_y_continuous(limits =  c(-0.5,15.5))+
  theme(
    axis.text.x = element_text(size = 8, angle=60,hjust = 1)
  )+
  #添加内层分组
  annotate("rect", xmin = 0.5, xmax = 7.5, ymin = 2.1, ymax = 2.4, fill="#00c4ff")+
  annotate("rect", xmin = 9.5, xmax = 22.5, ymin = 2.1, ymax = 2.4, fill="#11862f")+
  annotate("rect", xmin = 24.5, xmax = 36.5, ymin = 2.1, ymax = 2.4, fill="red")+
  #手动添加y轴标签
  annotate("text", x = 40, y = 3, label = "Region I", size=3.5, color = "black",angle= 0,hjust=0)+
  annotate("text", x = 40 , y = 4, label = "Region II", size=3.5, color = "black",angle=0,hjust=0)+
  annotate("text", x = 40 , y = 5, label = "Region III", size=3.5, color = "black",angle=0,hjust=0)->p3
p3
#外层图形
ggplot()+
  geom_tile(data=df_r,aes(x,y,fill=value),
            color="grey",linewidth=0.6)+
  scale_fill_gradientn(limit = c(-1, 1), colors = c('#0099cc', 'white', '#ff9933'))+
  geom_text(data=df_p,aes(x,y,label = sg))+
  theme_void()+
  theme(panel.grid = element_blank(),
        legend.position = "top")+
  labs(x=NULL,y=NULL,fill="Pearson r")+
  #自定义x轴标签，注意需要按照此前添加顺序指定
  scale_x_continuous(limits = c(0,40),
                     breaks = c(1:7,10:22,25:36),
                     labels = c('D4',	'D63',	'D125',	'D250',	'D500',	'D1000',	'D2000',	'Quartz',	'Illite',	'Albite',	'K-Feldspar',	'Chlorite',	'Calcite',	'Dolomite',	'Tremolite', 'Kaolinite',	'Koninchite',	'Pyrite',	'Feldspar',	'clay minerals',	'ORP',	'TOC',	'Temp.',	'pH',	'DO',	'TPP',  'Chl.a',	'SpCond',	'TD','DTP',	'FV',	'Turb.'))+
  #设置y轴范围
  #注意：这里的范围内层与外层图要一致且同时包含内外层，以便后续在AI中拼图
  scale_y_continuous(limits = c(-0.5,15.5))+
  ## removing axes labels and ticks
  theme(
    axis.text.x = element_blank()
  )+
  #自定义y轴标签
  annotate("text", x = 40 , y = 7, label = "Ex-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 8, label = "Fe-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 9, label = "Al-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 10, label = "Ca-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 11, label = "Res-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 12, label = "TIP", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 13, label = "Bio-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40 , y = 14, label = "Unbio-P", size=3, color = "black",angle=0, hjust = 0)+
  annotate("text", x = 40, y = 15, label = "Org-P", size=3, color = "black",angle=0, hjust = 0)->p4
p4

library(patchwork)
p4/p3+
  plot_layout(guides = 'collect')
##拼图
library(aplot)
p3 %>% insert_top(p4, height = 0.8)
