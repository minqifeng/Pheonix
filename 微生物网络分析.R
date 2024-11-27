
#加载包
library(Hmisc)
library(igraph)
# library(psych)
# 加载数据

setwd("E:\\S研究材料与学术基础\\M-Materials\\黑臭水体治理与河湖生态修复\\氮磷循环及微生物群落分析\\求索溪16s\\求索溪16S整理数据\\分析绘图\\网络分析")


otu1<-read.csv("genus1.csv",row.names = 1)
head(otu1)
# 计算相对丰度
otu2 <- apply(otu1,2,function(x) x/sum(x)) # 对otu1中的每一列进行归一化处理
# 过滤低丰度

otu3 <- otu2[which(rowSums(otu2) >= 0.05), ] # 将丰度小于0.05的舍弃


GA1 <- otu3[,1:9]
GA2 <- otu3[,10:15]
GA3 <- otu3[,16:20]
GN1 <- otu3[,21:27]
GN2 <- otu3[,28:31]
GN3 <- otu3[,32:36]



#计算相关性系数；
sp.cor<- rcorr(t(GN3),type="spearman")
#提取r、p值矩阵；
r.cor<-sp.cor$r
p.cor<-sp.cor$P
#使用Benjamini-Hochberg("FDR-BH")法进行多重检验校正；
p.adj <- p.adjust(p.cor, method="BH")
#确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
r.cor[p.cor>=0.01|r.cor<0.6] = 0
#对角线处的1不计
diag(r.cor) <- 0
#使用邻接矩阵（即相关系数矩阵）创建网络；
A<-graph.adjacency(r.cor,weight=T,mode="undirected")
#去掉冗余的边（multiple edges、loop edges）；
A<-simplify(A)
#提取权重
df_weight = E(A)$weight
# 设定边的宽度，这里我们将相关系数与边宽进行关联
E(A)$width = abs(df_weight)*5
#加入OTU丰度信息
data <- rowSums(GN3)
data1=as.data.frame(data)
df_igraph_size = data1[V(A)$name,] # 筛选对应OTU属性
df_igraph_size2 = log10(df_igraph_size)#数据进行转换
V(A)$Abundance = df_igraph_size2
#加入物种信息,使用不同颜色表示
data2=read.csv("Genustax1.csv",row.names = 1)
df_igraph_col = data2[V(A)$name,]
V(A)$G = as.character(df_igraph_col)
#生成网络图的结点标签（OTU id）和degree属性；
V(A)$label <- V(A)$name
V(A)$degree <- degree(A)
#将网络图导出为"graphml"、"gml"格式，方便导入Gephi中使用；
write_graph(A, "N3.graphml", format="graphml")
write_graph(A, "N3.gml", format="gml")


df_degree<-data.frame(V(A)$G,V(A)$degree)
colnames(df_degree)<-c('group','Degree')
write.table (df_degree, file ="df_degree_N3.txt",sep ="\t", quote =F)





# 读取数据
df1 <- read.table("df_degree_A1.txt",header = T, check.names = F)
df2 <- read.table("df_degree_A3.txt",header = T, check.names = F)
df3 <- read.table("df_degree_N1.txt",header = T, check.names = F)
df4 <- read.table("df_degree_N3.txt",header = T, check.names = F)
#添加分组信息并合并数据

df1$G <- rep("A1",117)#根据数据行数进行修改
df2$G <- rep("A3",117)
df3$G <- rep("N1",117)#根据数据行数进行修改
df4$G <- rep("N3",117)
df <- rbind(df1,df2,df3,df4)

col<-c("#606f8a","#e8c559","#ea9c9d","#005496")


#展示连接数
p<-ggplot(df,aes(x=G,y=Degree,fill=G))+
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8)+#添加误差线,注意位置，放到最后则这条先不会被箱体覆盖
  geom_boxplot(aes(), 
               outlier.colour="white")+#异常点去除
  theme_bw()+
  theme(legend.position="none",#图例位置
        )+
  geom_jitter(size=3, width = 0.3,alpha=0.7,aes(color=G),shape=1,stroke = 1)+#添加抖动点
  geom_signif(comparisons = list(c("A1","A3"),c("N1","N3")), 
              map_signif_level = TRUE, #显示星号
              test = wilcox.test, y_position = c(20),
              size = 0.8)+
  labs(x=NULL)+
  annotate("rect", xmin=c(0,2.5), xmax=c(2.5,5),
           ymin=-Inf, ymax=+Inf,
           fill = c("grey","skyblue"), alpha =0.2)+ #添加自定义矩形
  geom_vline(xintercept =c(2.5),  # 添加垂直于x轴并且经c的直线
             yintercept = NULL,      # 添加一条垂直于y轴并且经过c的直线,yintercept = c(-Inf,+Inf)
             linetype = "dashed",
             color="red",
             linewidth=1)

p


# network property
# 边数量 The size of the graph (number of edges)
num.edges = length(E(igraph)) # length(curve_multiple(igraph))
num.edges
# 顶点数量 Order (number of vertices) of a graph
num.vertices = length(V(igraph))# length(diversity(igraph, weights = NULL, vids = V(igraph)))
num.vertices
# 连接数(connectance) 网络中物种之间实际发生的相互作用数之和（连接数之和）占总的潜在相互作用数（连接数）的比例，可以反映网络的复杂程度
connectance = edge_density(igraph,loops=FALSE)# 同 graph.density;loops如果为TRUE,允许自身环（self loops即A--A或B--B）的存在
connectance
# 平均度(Average degree)
average.degree = mean(igraph::degree(igraph))# 或者为2M/N,其中M 和N 分别表示网络的边数和节点数。
average.degree
# 平均路径长度(Average path length)
average.path.length = average.path.length(igraph) # 同mean_distance(igraph) # mean_distance calculates the average path length in a graph
average.path.length
# 直径(Diameter)
diameter = diameter(igraph, directed = FALSE, unconnected = TRUE, weights = NULL)
diameter
# 群连通度 edge connectivity / group adhesion
edge.connectivity = edge_connectivity(igraph)
edge.connectivity
# 聚集系数(Clustering coefficient)：分局域聚类系数和全局聚集系数，是反映网络中节点的紧密关系的参数，也称为传递性。整个网络的全局聚集系数C表征了整个网络的平均的“成簇性质”。
clustering.coefficient = transitivity(igraph) 
clustering.coefficient
no.clusters = no.clusters(igraph)
no.clusters
# 介数中心性(Betweenness centralization)
centralization.betweenness = centralization.betweenness(igraph)$centralization 
centralization.betweenness
# 度中心性(Degree centralization)
centralization.degree = centralization.degree(igraph)$centralization
centralization.degree





#####METHODS2#####
## 计算otu间的相关性,只保留在统计上显著的相关性
df_corr <- rcorr(t(otu3), type = 'spearman')
# 提取R、P值
df_corr_r = df_corr$r
df_corr_p = df_corr$P # 注意，这里P大写
# 使用BH法校正p值
df_p <- p.adjust(df_corr_p, method = 'BH')
# 计算相关性并进行显著性检验,只保留在统计上显著的相关性
# df_cor=corr.test(t(df3),use='pairwise',method='spearman',adjust='fdr',alpha=0.05)
# use='pairwise'表示仅考虑在相关性计算中没有缺失数据的行和列;adjust='fdr'表示使用FDR方法进行多重比较校正
# r_value<-df_cor$r
# p_value<-df_cor$p
# 确定物种间存在相互作用关系的阈值，将相关性r矩阵内不符合的数据转换为0
df_corr_r[df_corr_p>0.05|abs(df_corr_r)<0.6] = 0
# 构建带权无向图
df_igraph <- graph_from_adjacency_matrix(df_corr_r,mode="undirected",weighted=TRUE,diag=FALSE) # diag=FALSE表示矩阵的对角线元素不作为图的连边
# 提取出所有边的权重
df.weight=E(df_igraph)$weight
df.weight # 有负数值

## 线数据的相关设置
# 因为权重有正负值，所以采用E()和V()来定义线和点数据（如果权重均为正值，直接采用weighted=NULL即可）
df_edge=df.weight
# 设置线的颜色,权重大于0为红色，权重小于0为蓝色，权重等于0为灰色（也可以理解为相关性）
df_edge_color=ifelse(df_edge>0,"red",ifelse(df_edge<0,"blue","gray"))
E(df_igraph)$color=as.character(df_edge_color) # 转化为字符类型，并将其赋值给igraph对象中的线颜色属性
# 设定边的宽度
E(df_igraph)$width = abs(df.weight)*2.5 # 计算边权重的绝对值乘以一个系数

## 点数据的相关设置
# 节点的大小代表相应otu的丰度大小
data.spot <- rowSums(otu1)
data1=as.data.frame(data.spot) # 转化数据格式
df_igraph_size = data1[V(df_igraph)$name,] # 筛选对应OTU属性
df_igraph_size2 = log10(df_igraph_size)*2.5 # 数据进行转换，缩小范围
V(df_igraph)$size = df_igraph_size2 
# 设置节点的颜色代表不同otu
data.spot2=read.csv("Genustax.csv",row.names = 1) # 此处导入数据仅有各个otu对应的门分类单元
# 若分类中有“”，将其转换为"unassigned"
# data.spot2[data.spot2==""] <- "unassigned"
df_igraph_col = data.spot2[V(df_igraph)$name,] # 筛选出网图中节点对应的OTU分类信息
df_igraph_col2=as.factor(df_igraph_col) # 将数据转化为因子类型（仅能将一个对象转化为因子类型，所以输入的tax数据只保留了一个分类水平）
levels(df_igraph_col2) # 显示提取出6个分类单元
# 若是多个分类单元的数据（多列多个对象）
# df_igraph_col2<-data.frame(lapply(df_igraph_col,factor))
# df_igraph_col3<-df_igraph_col2[,2]
# levels(df_igraph_col3) # 提取出的结果一致
# 随机生成颜色
library(grDevices)
set.seed(123)
color.20<-sample(colors(),20) # 随机选取对应的6个颜色
color.20
color=c(color.20)
levels(df_igraph_col2) = color
V(df_igraph)$color = as.character(df_igraph_col2) # 将df_igraph_col2转化为字符类型，并将其赋值给igraph对象中的节点颜色属性

## 绘图
set.seed(33)
plot(df_igraph,main="Co-occurrence network",vertex.frame.color=NA, # 去掉节点的边框
     edge.lty=1, # 表示边的类型为实线
     edge.curved=TRUE, # 边的类型为曲线
     margin=c(0,0,0,0), vertex.label.cex=.7, # 节点标签大小
     vertex.label.dist=1, # 节点标签与节点的距离
     vertex.label.color='black',
     layout=layout_in_circle # 输入??igraph查看Layout algorithms的其他形式
)
# 设置图例
legend(x=-1.8,y=-0.2,levels(as.factor(df_igraph_col)),pch=21,col="black",pt.bg=color,cex = 0.8,box.lty=0, bg = "transparent")
# box.lty=0表示去掉图例的外边框；bg = "transparent"设置图例背景为透明



# 导出至Gephi，可以进行图像的修改
write.graph(df_igraph, 'all.net.graphml', format = 'graphml')





