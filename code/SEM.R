library(semPlot)

#首先需要安装lavaan package
install.packages("lavaan")
library(lavaan)
# 加载数据集
setwd("D:\\R\\China\\R结构方程")
data <- read.table("China_2.csv", header = TRUE, sep = ",", fileEncoding = "GBK")
data



# 定义模型2-M0
model2 <- '
H ~ RF
TN ~ H + RF
TP ~ H + RF
NP ~ TN + TP
TLI ~ NP + TN + TP'

# 模型 M1：反馈模型（增加反向路径）
model2 <- '
H ~ RF
TN ~ H + RF + NP
TP ~ H + RF + NP
NP ~ TN + TP +TLI
TLI ~ NP + TN + TP'

# 模型 M2：因果反转模型（交换 H 与 TN 的顺序）
model2 <- '
H ~ RF + TP
TN ~ RF
TP ~ H + RF
NP ~ TN + TP
TLI ~ NP + TN + TP'

# 模型 M3：竞争中介模型（增加 RF 到 NP 的直接路径）
model2 <- '
H ~ RF
TN ~ H + RF
TP ~ H + RF
NP ~ TN + TP + RF
TLI ~ NP + TN + TP'




## 使用SEM函数拟合模型
fit <- sem(model2, data = data) 


summary(fit)
summary(fit, fit.measures=TRUE)
summary(fit, standardized = TRUE)
## 查看拟合结果

fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea","AIC","BIC"))
#简单评价标准:p>0.05,gfi>0.90，cfi>0.95,rmr越小越好,srmr<0.08,rmsea<0.05

#总结模型拟合结果
summary(fit)
#全路径图
semPaths(fit,"std",style="lisrel")
#将结果可视化
lavaan::lavInspect(fit,"est")









