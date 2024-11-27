rm(list = ls())#清楚环境变量
setwd("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\氮磷循环及微生物群落分析\\求索溪16s\\求索溪16S整理数据\\分析绘图\\物种丰度circos")
#install.packages(circlize)
library(circlize)#引用包
library(tidyverse)#引用包

#方法一
library(openxlsx)#引用包
genus <- read.xlsx("taxa.table.xlsx",sheet=1,rowNames=T)

#方法二
library(readxl)#引用包
genus <- read_xlsx("taxa.table.xlsx",sheet=1)
genus <- as.data.frame(genus)
str(genus)
# 将第一列设为行名
rownames(genus) <- genus[,1]
# 删除原始的第一列数据
genus <- genus[, -1]



#计算整理前十菌属相对丰度表
genus$rowsum <- apply(genus,1,sum)
genus <- genus[order (genus$rowsum,decreasing=TRUE),]#对菌属进行降序排序
genus = genus[,-7]#删除求和列
#求物种相对丰度
df <- data.frame(apply(genus,2,function(x) x/sum(x)),stringsAsFactors = FALSE)

#按行求指定列平均值，并且把算好的平均值添加data1数据框，本数据已提前算好
# df$A <- apply(df[,1:3], 1, mean) 
# df$B <- apply(df[,4:6], 1, mean)
# df$C <- apply(df[,7:9], 1, mean)
# df$D <- apply(df[,10:12], 1, mean)
# df = df[,-(1:12)]

#之前已经按照每行的和进行过升序排列，所以可以直接取前10行
df1<- df[1:10,]/apply(df,2,sum)
df2 <- 1-apply(df1, 2, sum) #计算剩下物种的总丰度
#合并数据
df3 <- rbind(df1,df2)
rownames(df3)[11] = "Others"
data1 = as.matrix(df3)

# write.csv(data1,"前十属相对丰度.csv")
#进行和弦图基本绘制
?chordDiagram
chordDiagram(data1)
circos.clear()#清理当前对和弦图的参数设置
rownames(data1) <- c("Dechloromonas",	"Steroidobacteraceae",	"Anaerolineaceae",	"Luteolibacter",	"Pirellulaceae",	"Vicinamibacterales",	"SZB30",	"Thiobacillus",	"Candidatus_Competibacter",	"SJA_15","Other")

##预设links配色
col = c("#8DD3C7" ,"#BEBADA" ,'#009933',"#FF7F00",'#c5c5f2','#CC3366',
       "#FB8072","#F781BF" ,"#80B1D3","#A65628" ,'#660099')
grid_col = c("A1" ='#EF9A9A',"A2" ='#90CAF9',"A3"='#F3D32C',"N1"="#FCCDE5","N2" ="#7FFFD4","N3" ="#FFE4E1",
             "Dechloromonas" ="#BEBADA", "Steroidobacteraceae" ="#8DD3C7" , "Anaerolineaceae"= '#009933',
             "Luteolibacter "="#FF7F00" ,"Pirellulaceae"= '#c5c5f2',"Vicinamibacterales"= '#CC3366',
             "SZB30" = "#FB8072","Thiobacillus" ="#F781BF", "Candidatus_Competibacter" = "#80B1D3",
             "SJA_15" ="#A65628","Others"= '#660099')##预设置外圈颜色
library(RColorBrewer)



chordDiagram(data1,
             grid.col = grid.col, # 圆环的颜色
             directional = 1,      # 条带的方向。为1时，顺序从第一列到第二列，以此类推
             link.lwd = 1,         # 条带边框的宽度
             link.lty = 1         # 条带边框的类型
)



chordDiagram(data1,
             grid.col = grid.col, # 圆环的颜色
             link.border = "grey", # 条带边框的颜色
             transparency = 0.7,   # 透明度
             directional = 1,      # 条带的方向。为1时，顺序从第一列到第二列，以此类推
             link.lwd = 1,         # 条带边框的宽度
             link.lty = 1         # 条带边框的类型
)




#颜色设定
##颜色设定
color <- NULL
#为“to”列进行颜色赋值
color[c("A1","A2","A3","N1","N2","N3")] <- c("blue","red","yellow","green","pink",'#F3D32C')
#为“from”列进行颜色http://127.0.0.1:39401/graphics/plot_zoom_png?width=958&height=692赋值
color[rownames(data1)] <- c("#40A4D8","#33BEB7","#B2C224","#FECC2F","#FBA127",
                           "#F66320","#DB3937","#A463D7","#0C5BCE","grey","black")
#直接绘图。合并图
chordDiagram(data1, 
             grid.col = color, 
             grid.border=NULL,#边框颜色设置，设置为NULL则默认与填充色一致
             annotationTrack = c( "grid"), # 显示变量名称及圆环，"name","axis",
             transparency = 0.7,   # 透明度
             directional = -1,      # 条带的方向为1时，顺序从第一列到第二列，-1为反向，2为双向
             link.lwd = 1,         # 条带边框的宽度
             link.lty = 1,          #条带边框线条类型
             link.border = "grey", # 条带边框的颜色
             diffHeight = mm_h(3),#外圈和中间连线的间隔
             direction.type = c("diffHeight","arrows"), #线条是否带有箭头
             link.arr.type = "big.arrow",#箭头类型
) 

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[-1], CELL_META$sector.index,
              facing = "clockwise", niceFacing = T, adj = c(-1.2, 0), cex = 0.8)
  circos.axis(h = "top", labels.cex = 0.8,
              minor.ticks = 3,
              major.tick.length = mm_y(2),
              lwd=2,
              labels.niceFacing = T, labels.pos.adjust =T)
}, bg.border = NA)


legend(x=1,y=1,    ###位置 
       pch=20,    ###点形状
       legend=rownames(data1),
       title="Genus ",title.adj=0 ,  ##标题名字与位置，0为左对齐1为右对齐
       col=color[rownames(data1)] ,bty="n",   ##点配色,n为不加外边框，o为添加外边框
       cex=1,pt.cex=3,border="black",
       ncol = 1,xpd=T) ###一列，xpd表示图例是否可以超过图本身边界绘制，与画布par(mar=c())同用。



circos.clear()#清理当前对和弦图的参数设置
