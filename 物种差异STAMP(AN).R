## 导入数据
rm(list = ls())

#安装包
#library(openxlsx)
#library(tidyverse)
setwd("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\氮磷循环及微生物群落分析\\求索溪16s\\求索溪16S整理数据\\分析绘图\\物种丰度差异分析STAMP")
library(openxlsx)
library(tidyverse)
data = read.xlsx("Genus.xlsx",1,rowNames = TRUE)#读取功能预测数据；1为表格的第一个sheet；rowNames = TRUE为第一列为行名
group = read.xlsx("Genus.xlsx",2)#读取分组数据
data <- data.frame(apply(data,2,function(x) x/sum(x)),stringsAsFactors = FALSE) #计算丰度占比
data = data*100  #*100已表示百分号
data  =  data %>% filter(apply(data,1,mean) > 0.5)#筛选相对丰度大于0.5%的，可以根据自己要求改变
# write.csv(data,"丰度0.5%物种属水平.csv")

df <- read.csv("丰度0.5%物种属水平.csv")
row.names(data) <- df$Genus
data <- t(data)  #转置数据框
data1 <- data.frame(data,group$group2)#将分组信息加入
colnames(data1) <- c(colnames(data),"Group")
data1$Group <- as.factor(data1$Group)
str(data1)



## 进行t-test筛选差异显著的属，var.equal = FALSE即使用Welch t-test。
dfg = data1 %>% 
    select_if(is.numeric) %>%
    map_df(~ broom::tidy(t.test(. ~ Group,data = data1)), .id = 'var')
# dfg = data1 %>% 
#   select_if(is.numeric) %>%
#   map_df(~ broom::tidy(t.test(. ~ Group,data = data1,var.equal = FALSE)), .id = 'var')
dfg$p.value = p.adjust(dfg$p.value,"none")
dfg <- dfg[order(dfg$p.value, decreasing = F), ][1:10, ]#筛选p值后十

# dfg <- dfg %>% filter(p.value < 0.05)#筛选p值显著的物种

## wilcox：数据方差不齐

dfg <- data1 %>% 
    select_if(is.numeric) %>%
    map_df(~ broom::tidy(wilcox.test(. ~ Group,data = data1,exact = T,conf.int = TRUE, conf.level = 0.95)), .id = 'var')

dfg$p.value <- p.adjust(dfg$p.value,"none")
# dfg <- dfg[order(dfg$p.value, decreasing = F), ][1:10, ]#筛选p值后十
dfg <- dfg %>% filter(p.value < 0.05)

## 绘图数据构建
## 左侧条形图数据整理
#筛选差异显著的属
a = data1[,c(dfg$var,"Group")]
#将宽数据转化为长数据
b = a %>% gather(variable,value,-Group)
#计算平均值
c = b %>% group_by(variable,Group) %>%
  summarise(Mean = mean(value))
#下面的代码更简洁，用 %>%简化
abun.bar = data1[,c(dfg$var,"Group")] %>% 
    gather(variable,value,-Group) %>% #将宽数据转化为长数据
    group_by(variable,Group) %>% 
    summarise(Mean = mean(value))



## 右侧散点图
#筛选dfg结果中的信息
diff.mean <- dfg[,c("var","estimate","conf.low","conf.high","p.value")]
diff.mean$Group <- c(ifelse(diff.mean$estimate >0,levels(data1$Group)[1],
                            levels(data1$Group)[2]))#给数据添加分组，因为t检验是用的A组跟B组进行比较，所以estimate >0为A，否则为B
diff.mean <- diff.mean[order(diff.mean$estimate,decreasing = TRUE),]#排序，降序
# diff.mean <- diff.mean[order(abs(diff.mean$estimate),decreasing = TRUE),]#排序，降序,绝对值



## 左侧条形图
library(ggplot2)
col <-  c("#88c4e8","#db6968")
abun.bar$variable  =  factor(abun.bar$variable,levels = rev(diff.mean$var))
p1 = ggplot(abun.bar,aes(variable,Mean,fill = Group)) +
    scale_fill_manual(values=col)+#分组颜色填充
    scale_x_discrete(limits = levels(diff.mean$var)) +#指定坐标轴需要显示的的范围
    coord_flip() +#翻转XY轴
    xlab("") +#X轴标签
    ylab("Mean proportion (%)") +#y轴标签
    theme(panel.background = element_rect(fill = 'transparent'),#主题设定，
          panel.grid = element_blank(),#背景格子为空
          axis.ticks.length = unit(0.4,"lines"), #坐标轴刻度线长，正数向外
          axis.ticks = element_line(color='black'),#坐标轴刻度线颜色
          axis.line = element_line(colour = "black"),#坐标轴线颜色
          axis.title.x=element_text(colour='black', size=12,face = "bold"),#X轴文本设置
          axis.text=element_text(colour='black',size=10,face = "bold"),#坐标轴文本设定
          legend.title=element_blank(),#图例标题为空
          legend.text=element_text(size=12,face = "bold",colour = "black",
                                   margin = margin(r = 20)),#图例文本设定
          legend.position = "top",#图例位置，左下
          legend.direction = "horizontal",#图例方向，水平排列
          legend.key.width = unit(0.8,"cm"),#图例方块宽
          legend.key.height = unit(0.5,"cm"))#图例方块高

p1
for (i in 1:(nrow(diff.mean) - 1)) 
    p1 <- p1 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, 
                        fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
p1


p1 <-  p1+
     geom_bar(stat = "identity",position = "dodge",
           width = 0.7,colour = "black")+#柱状图柱子参数
     scale_y_continuous(expand = expansion(add = 0)) #对p1这个ggplot对象进行修改，将y轴的范围进行扩展，使得0点成为新的起始点
p1
## 右侧散点图
diff.mean$var = factor(diff.mean$var,levels = levels(abun.bar$variable))
diff.mean$p.value = as.numeric(diff.mean$p.value)
diff.mean$p.value = round(diff.mean$p.value,5)#保留3位小数
# diff.mean$p.value = as.character(diff.mean$p.value)
diff.mean$p.value = sprintf("%.5f",diff.mean$p.value)#将大数值，可将大数字转化为字符型
p2 = ggplot(diff.mean,aes(var,estimate,fill = Group)) +
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(),
          axis.ticks.length = unit(0.4,"lines"), 
          axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(colour='black', size=12,face = "bold"),
          axis.text=element_text(colour='black',size=10,face = "bold"),
          axis.text.y = element_blank(),
          legend.position = "none",
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 15,face = "bold",colour = "black",hjust = 0.5)) +
    scale_x_discrete(limits = levels(diff.mean$var)) +
    coord_flip() +
    xlab("") +
    ylab("Difference in mean proportions (%)") +
    labs(title="95% confidence intervals") 
p2
for (i in 1:(nrow(diff.mean) - 1)) 
    p2 <- p2 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf, 
                        fill = ifelse(i %% 2 == 0, 'white', 'gray95'))

p2 <- p2 +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(0), width = 0.3, linewidth = 0.6) +#误差线
    geom_point(shape = 21,size = 4) +#散点图参数
    scale_fill_manual(values=col) +#点颜色
    geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')#加虚线
    
p2   


diff.mean %>% mutate(label = if_else(p.value < 0.001,"***",
                       if_else(p.value <0.01,"**",
                               if_else(p.value<0.05,"*","ns"))),
                     p.value = as.character(p.value))  ->diff.mean
# diff.mean$merged <- paste(diff.mean$label,diff.mean$p.value, sep = " ")

p3 <- ggplot(diff.mean,aes(var,estimate,fill = Group)) +
    geom_text(aes(y = 0,x = var),label = diff.mean$label,
              hjust = 0,fontface = "bold",inherit.aes = FALSE,size = 3) +
    geom_text(aes(y = 0.15,x = var),label = diff.mean$p.value,
            hjust = 0,fontface = "bold",inherit.aes = FALSE,size = 3) +
    geom_text(aes(x = nrow(diff.mean)/2 +0.1,y = 0.7),label = "P-value (corrected)",
             srt = 90,fontface = "bold",size = 5) +
    coord_flip() +
    ylim(c(0,1)) +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) 

p3



## 图像拼接
library(patchwork)
p <- p1 + p2 + p3+ plot_layout(widths = c(4,6,2))
p

library(eoffice)
topptx(p,"物种差异AN.pptx",width=13.34,height=4.34)


   




