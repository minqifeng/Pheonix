#示例数据集，详情加载 agridat 包后 ?lasrosas.corn
library(agridat)
library(ggplot2)
library(readxl)
library(mgcv)
library(patchwork)#拼图专用
##R 包 mgcv 执行加性模型
##单变量回归示例




#此处以一般加性模型为例，拟合玉米产量与施加氮肥浓度的平滑回归
#需要在 gam() 函数中指定自变量的局部平滑器类型，如 s() 的样条平滑、lo() 的 LOESS 平滑等
#详情 ?gam、?s、?lo 等

#以样条平滑为例，实现响应变量与自变量的局部平滑拟合


#库湾中层氮氩
mydata1 <- read_excel(file.choose(),sheet="A")#读取常规数据
mydata2 <- read_excel(file.choose(),sheet = "N")#读取常规数据
# gam_G <- gam(data=mydata,RA ~ s(SpCond), method = "REML")
# summary(gam_G)

p1 <- ggplot(mydata1, aes(Chla, S_NH4)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p2 <- ggplot(mydata1, aes(Chla, S_NO3)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p3 <- ggplot(mydata1, aes(Chla, S_TN)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p4 <- ggplot(mydata1, aes(Chla, W_NH4)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p5 <- ggplot(mydata1, aes(Chla, W_NO3)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p6 <- ggplot(mydata1, aes(Chla, W_TN)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()





p7 <- ggplot(mydata2, aes(Chla, S_NH4)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p8 <- ggplot(mydata2, aes(Chla, S_NO3)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p9 <- ggplot(mydata2, aes(Chla, S_TN)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p10 <- ggplot(mydata2, aes(Chla, W_NH4)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p11 <- ggplot(mydata2, aes(Chla, W_NO3)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


p12 <- ggplot(mydata2, aes(Chla, W_TN)) +
  geom_point(size=3,alpha=1,color="black") +
  geom_smooth(method = "gam", formula = y ~ s(x))+
  theme_bw()


(p1+p2+p3)/(p4+p5+p6)
(p7+p8+p9)/(p10+p11+p12)


p0 <- (p1+p2+p3+p4+p5+p6)/(p7+p8+p9+p10+p11+p12)

library(eoffice)#导出为PPT
topptx(p0,filename="AN对比叶绿素-氮形态加性回归.pptx")


