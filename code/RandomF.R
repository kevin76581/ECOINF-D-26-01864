options(max.print = 1000000)
#加载程序包
library(randomForest)
library(rfPermute)
library(ggplot2)
library(pdp)
#library(DALEX)
#library(tidymodels)
#library(DALEXtra)
#library(car)
#library(pdp)
#library(ALEPlot)
#library(A3)

#读取数据，训练模型
setwd("D:\\R\\China\\RF")
data <- read.table("data6.csv", header = TRUE, sep = ",", fileEncoding = "GBK")
data


y1 <- data[,1]
x1 <- data[,c(2:6)]
set.seed(1234)
forest <- randomForest(x1, y1, ntree = 2000, mtry = 2, nodesize = 1, 
                      importance = TRUE, na.action = na.roughfix)
forest  #查看训练效果
plot(forest)  #查看迭代结果


# 1. 提取OOB预测的残差
oob_pred <- forest$predicted  # 这是OOB样本的预测值
actual <- data$N_P          # 这是实际观测值

# 2. 计算RMSE和MAE
oob_rmse <- sqrt(mean((actual - oob_pred)^2, na.rm = TRUE))
oob_mae <- mean(abs(actual - oob_pred), na.rm = TRUE)

# 3. 打印结果
cat("OOB RMSE:", round(oob_rmse, 4), "\n")
cat("OOB MAE:", round(oob_mae, 4), "\n")




#重要性、p值及绘图
forest$importance  #重要性
varImpPlot(forest)  #可视化重要性
forest$rf$importance  #重要性（带p值模型）
forest$pval  #p值
plotImportance(forest)  #可视化重要性与p值

#部分依赖图（train指定绘图用的自变量）
partial(forest, pred.var = "H", plot = TRUE, rug = TRUE, 
        ice = T, center = T)
partial(forest, pred.var = "Tem2", plot = TRUE, rug = TRUE, 
        ice = T, center = T, train = x1[c(6:9,17:21,29:33),])

#使用自定义函数画部分依赖图(用之前跑一下 “部分依赖图（PDP）自定义.R” 里面定义函数的代码)
P<-pdp.c(object = forest, pred.var = "H", train = x1, center = T, inv.link = NULL, color = "grey", colorC = "black")+
  labs(x = "H", y = "Variation of N:P Ratio", size = 50)
pdp::partial(object = forest, pred.var = "H", train = x1, plot = F, rug = F, ice = T)
P+theme(axis.text.y = element_text(size = rel(2.3)), #y坐标字体大小
        axis.text.x = element_text(size = rel(2.3)), #x坐标字体大小
        axis.title.x = element_text(size = rel(2.5)), #x轴标题字体大小
        axis.title.y = element_text(size = rel(2.5))) #y轴标题字体大小)


par <-  partial(object = forest, pred.var = "H", train = x1, 
                plot = F, rug = F, ice = T, center = T)
#object，训练的模型
#pred.var，绘图变量的名称
#train，自变量数据（格式为data.frame）

par1 <- reshape2::dcast(par, par[,1]~par[,3], value.var = "yhat")

par1$mean <- rowMeans(par1[,2:(1+nrow(x1))])
#x1改成自变量数据
#第一列为图中横坐标数据
#mean为黑色线纵坐标
#其他为灰色线纵坐标