
#讀檔案，看一下前六筆
#程式報表7.1
dta <- read.table('data/cesd.txt',header=TRUE)
str(dta)
head(dta)

#看題目間相關
#圖7.1
require(corrplot)
corrplot(cor(dta), method="ellipse", type="lower", 
 col=gray.colors(200,start = 1, end = 0),tl.col="black") 

#利用 moment套件看看偏態與峰度
#程式報表7.2
require(moments)
skkur <- cbind(skewness(dta), kurtosis(dta))
dimnames(skkur)[[2]] <- c('偏態','峰度')
round(skkur,3)


#lavaan 是 R 平台上 SEM 的 package
require(lavaan)

#CESD 的四因素模型
#寫法是 因素 =~ 測量指標
CESD.M1 <- '
憂鬱情感=~悲傷+恐懼+寂寞+痛哭+煩惱+失敗+悶悶不樂
身體症狀=~困擾+費力+不專心+睡眠+寡言+無力+胃口不好
人際困擾=~不友善+不喜歡
正向情感=~良好+樂趣+快樂+希望'

#跑 CFA，要註明模型 CESD.M1 與資料名稱 dta
fit <- cfa(CESD.M1, data=dta,std.lv=TRUE)

#輸出結果
#程式報表7.3, 7.4
summary(fit, fit.measures=TRUE)

#定義分量表與分量表題號
scale1 <- c(1,1:7)
scale2 <- c(2,8:14)
scale3 <- c(3,15:16)
scale4 <- c(4,17:20)

#定義如何計算構念信度、題目信度與平均變異抽取量
my_reli <- function(w){
 x <- w[-1]
 y <- w[1]
 L <- fit@Model@GLIST$lambda[x,y]
 P <- diag(fit@Model@GLIST$psi)[y]
 E <- diag(fit@Model@GLIST$theta)[x]
 reli <- sum(L)^2*P/(sum(L)^2*P+sum(E))
 itemreli <- L*L*P/(L*L*P+E)
 AVE <- mean(itemreli)
 show(my_reli <- list(reli=reli,itemreli=itemreli,AVE=AVE))
}

my_reli(scale1)

scale <- list(scale1,scale2,scale3,scale4)
lapply(scale,my_reli)


#繪製路徑圖
require(semPlot)
semPaths(fit,style='lisrel','std',nCharNodes=8,nCharEdges=3)

## 驗證CESD 四因素模型，發現適合度需改善，擬利用修改指標
## 修正模型。
## 由於考慮做交叉驗證（cross-validation），把資料切成兩個樣
## 本，一個拿來做調校樣本（calibration sample），一個拿來做
## 交叉驗證樣本。

#讀檔
dat <-read.table('data/cesd.txt',header=T)


#隨機分成兩組、分別是 dat0(調校樣本)與 dat1(交叉驗證樣本) 
#set.seed 是隨機種子，可以自由設定數值
#固定隨機種子可以讓每次分組相同，分析相同分組資料
#如果沒用這個指令，每次執行時，隨機分組都會不同
set.seed(20140826)
id0 <- sample(dim(dat)[1], dim(dat)[1]/2)
dat0 <- dat[id0,]
dat1 <- dat[-id0,]


#原始模型
require(lavaan)
CESD.M1 <- '
憂鬱情感=~悲傷+恐懼+寂寞+痛哭+煩惱+失敗+悶悶不樂
身體症狀=~困擾+費力+不專心+睡眠+寡言+無力+胃口不好
人際困擾=~不友善+不喜歡
正向情感=~良好+樂趣+快樂+希望'

#顯示修改指標。
#程式報表7.5
fit0 <- cfa(CESD.M1, data=dat0, std.lv=TRUE)
summary(fit0, fit.measures=TRUE)
modindices(fit0)

#列出前十個
modi <-modindices(fit0)
modi[order(-modi$mi),][1:10,]


#修改模型，加入「悶悶不樂~~胃口不好」，即兩者測量誤差間共變
CESD.M1 <- '
憂鬱情緒=~悲傷+恐懼+寂寞+痛哭+煩惱+失敗+悶悶不樂
身體問題=~困擾+費力+不專心+睡眠+寡言+無力+胃口不好
人際困擾=~不友善+不喜歡
正向情緒=~良好+樂趣+快樂+希望
悶悶不樂~~胃口不好'

#配適看看。
fit0 <- cfa(CESD.M1, data=dat0, std.lv=TRUE)
summary(fit0, fit.measures=TRUE)

#在調校樣本下，繼續看看原始模型哪些地方可能需要改
#直到讓模型與資料配適
modindices(fit0)


#修改模型，加入「悶悶不樂~~寡言」，即兩者測量誤差間共變
CESD.M1 <- '
憂鬱情緒=~悲傷+恐懼+寂寞+痛哭+煩惱+失敗+悶悶不樂
身體問題=~困擾+費力+不專心+睡眠+寡言+無力+胃口不好
人際困擾=~不友善+不喜歡+失敗
正向情緒=~良好+樂趣+快樂+希望
悶悶不樂~~胃口不好
悶悶不樂~~寡言'
fit0 <- cfa(CESD.M1, data=dat0, std.lv=TRUE)
summary(fit0, fit.measures=TRUE)


#修改結束，將模型用在交互驗證樣本。
fit <- cfa(CESD.M1, data=dat1, std.lv=TRUE)
summary(fit, fit.measures=TRUE)

