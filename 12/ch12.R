## 在Rstudio，Tools -> Global Options -> General -> Default Text Encoding -> UTF-8
## Opening with encoding -> BIG5
## 系統語系要設定為台灣正體中文
Sys.setlocale(category = "LC_ALL", locale = "cht")

## Function for check package installed from stackoverflow.com
pkgTest <- function(x)
{
    if (!require(x,character.only = TRUE))
    {
        install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
}

#讀資料

dta <- read.table('http://myweb.ncku.edu.tw/~cpcheng/Rbook/12/data/Addiction.txt', header = T)



#選進學校老師可以觀察的變項

dta <- dta[, 1:8]



#顯示前六筆，看資料結構

#程式報表12.1

head(dta)

str(dta)



#程式報表12.2

summary(dta)



#程式報表12.3

#計算每個變項各水準次數

t(apply(dta,  2, table))



#計算每個變項各水準百分比

show(dtap <- prop.table(t(apply(dta,  2, table)), 1))



#載入lattice套件，準備畫圖
pkgTest("lattice")
require(lattice)



#繪製各變項百分比

#圖12.1

dotplot(dtap[order(dtap[, 1]), ], pch = c(1, 20), grid = T,

  ylab = '變項', xlab = '各變項兩類別對應百分比',

  auto.key = list(column = 2, pch=c(1,20),text = c('低_女_否', '高_男_是')))





#程式報表12.4

#看看成癮與性別的關係

with(dta, table(成癮, 性別))



#換成比率

with(dta, prop.table(table(成癮, 性別), 2))



#檢定

with(dta, chisq.test(table(成癮, 性別)))



#算成癮與其他變項卡方檢定 p 值

with(dta, sapply(2:8, function(x){ c(names(dta[, 2:8])[x-1],

   round(chisq.test(table(dta[, 1], dta[, x]))$p.val, 6))}))



#計算每個變項各類別的成癮、不成癮比率

dta_p <- with(dta, sapply(2:8, function(x){

              prop.table(table(dta[, x], dta[, 1]), 1) } ))

              

#只取成癮比率

dta_p <- t( dta_p[3:4, ])



#放進變項名稱

rownames(dta_p) <- names(dta[, -1])

 

#以變項中各類別成癮比率差異（效果量），排列變項

d_p <- dta_p[order(abs(dta_p[, 1] - dta_p[, 2])), ]



#載入 reshape 套件，改變資料排列
pkgTest("reshape2")
require(reshape2)

d_p <- melt(d_p)



#命名三個欄位，看一下資料

names(d_p) <- c('變項', '類別', '比率')

#程式報表12.5

d_p



#畫圖

#圖12.2

dotplot(變項 ~ 比率, data = d_p,  xlim = c(.1, .9),

  xlab = '比率', ylab = '變項', main = '不同類別成癮比率',

  panel = function(x, y){

    panel.xyplot(x, y, pch = 16)

    panel.abline(v = .5, col = 8, lty = 2)

    panel.segments(d_p[d_p$類別 == 2, '比率'], as.numeric(y), 

                   d_p[d_p$類別 == 1, '比率'], as.numeric(y), lty = 3)

   }) 









#重排成癮變項，確認有興趣的情境（是）排在後面

dta$成癮 <- ordered(dta$成癮, levels = c('否', '是'))



#將資料切兩半，一個用來訓練，一個用來測試
## Machine learning
set.seed(20150214)

n <- dim(dta)[1]

nh <- sample(1:n, floor(n/2))

dta_trn <- dta[nh, ] 

dta_tst <- dta[-nh, ]



#載入 rpart 套件
pkgTest("rpart")
library(rpart)



#決策樹分析 will all the variables as predictors

#程式報表12.6, 12.7

summary(rslt_trn <- rpart(成癮 ~ ., data = dta_trn, 

           control = rpart.control(cp = .001, minsplit = 50, minbucket = 20) ) )



#載入rpart.plot套件
pkgTest("rpart.plot")
library(rpart.plot) 



#把結果畫出來比較清晰

#圖12.3

prp(rslt_trn, type = 2, extra = 7, left = F, nn = T)







#試剪決策樹，圖示跟文字輸出

#程式報表12.8

printcp(rslt_trn)

#圖12.4

plotcp(rslt_trn)

 

#剪了，並畫出來

#圖12.5

show(rslt_trn_p <- prune(rslt_trn, cp = .0045))

prp(rslt_trn_p, type = 2, extra = 7, left = F, nn = T)







#交互驗證。比較兩個樣本，不同修剪時正確率

trn_cp <- printcp(rslt_trn)[, 'CP']



# extract the dimension of cp to be used a lot later

n_cp <- length(trn_cp) - 1



pc <- matrix(NA, nrow = n_cp, ncol = 3)



for ( i in 1:n_cp ) {

  pc[i, 1]  <- (trn_cp[i] + trn_cp[i+1]) / 2

  rslt_trn_p <- prune(rslt_trn, cp = pc[i, 1])

  t0 <- table(dta_trn$成癮, predict(rslt_trn_p, newdata = dta_trn, type = 'class'))

  pc[i, 2] <- (t0[1, 1] + t0[2, 2])/ sum(t0)

  t0 <- table(dta_tst$成癮, predict(rslt_trn_p, newdata = dta_tst, type = 'class'))

  pc[i, 3] <- (t0[1, 1] + t0[2, 2])/sum(t0)

}



#看結果

#程式報表12.9

colnames(pc) <- c('CP', '訓練樣本正確率', '測試樣本正確率')

print(pc <- round(pc, 4))



#畫圖 

#圖12.6

plot(x = 1:n_cp, y = pc[, 2], xlab = '複雜性參數值', ylab = '分類正確率', type = 'p', 

     axes = FALSE, ylim = range(pc[, -1]) + IQR(pc[, -1]) * c(-1.5, 1.5))

points(x = 1:n_cp, y = pc[, 3], pch = 16)

axis(1, at = 1:n_cp, labels = pc[, 1])

axis(2)

axis(4)

grid()

legend('topleft', legend = c('訓練', '測試'), pch = c(1, 16), bty = 'n') 

box() 

###