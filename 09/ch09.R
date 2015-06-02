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

#資料檢視

#載進 heplots 套件，引用其中的資料
pkgTest("heplots")
require(heplots)



#看看說明檔

?VocabGrowth



#資料依序是在8,9,10,11年級時的測量值

data(VocabGrowth)

dta <- VocabGrowth



#前36筆是男生，後28筆是女生

dta$id <- c(1:64)

dta$gender <- factor(c(rep(1,36),rep(2,28)))



#檢視資料格式與前六筆

#程式報表9.1

head(dta)

str(dta)





#載入reshape2套件，更動資料排列形狀
pkgTest("reshape2")
require(reshape2)

dtal <- melt(dta, variable.name ='gradename',  value.name = 'score',id=c('id','gender'))



#把年製造出來，看一下目前資料

dtal$grade <- as.numeric(substr(dtal$gradename, 6, 7))

#程式報表9.2

head(dtal)



#看四波的平均數與變異數，男女有點差異

#程式報表9.3

aggregate(score ~ grade+gender, data = dtal, mean)

aggregate(score ~ grade+gender, data = dtal, sd)



#準備繪圖，載入ggplot2，記錄下目前配色主題
pkgTest("ggplot2")
require(ggplot2)

old <- theme_set(theme_bw())



#繪製不同性別四波圖

#先稍稍調整一下圖，免得彼此壓到

#圖9.1

pd <- position_dodge(width = .2)

ggplot(data = dtal, aes(x = grade, y = score, shape = gender)) +

 stat_summary(fun.data = 'mean_cl_boot', size = 1, position = pd) +

 stat_summary(fun.y = mean, geom = 'line', aes(group = gender), position = pd) +

 guides(shape = guide_legend(title = '', reverse = TRUE)) +

 labs(x = '年級', y = '字彙分數') +

 theme(legend.position = c(.9, .9))





#先選擇男性分析

dtal_M <- subset(dtal, gender == 1)



#利用 moment套件看看偏態與峰度

#程式報表9.4
pkgTest("moments")
require(moments)

aggregate(score ~ grade, data = dtal_M, skewness)

aggregate(score ~ grade, data = dtal_M, kurtosis)



#載進 car，看四波的直方圖

#圖9.2
pkgTest("car")
require(car)

densityPlot(score ~ gradename, data = dtal_M, xlab = '分數',ylab='機率',adjust=2)



#看看個別資料，畫上迴歸線並配上區間

#圖9.3

ggplot(data = dtal_M, aes(x = grade, y = score)) +

 geom_line(aes(group = id), linetype = 'dotted') +

 stat_summary(fun.data = 'mean_cl_boot') +

 stat_smooth(method = 'loess') +

 labs(x = '年級', y = '字彙分數')



#個別迴歸線 

#圖9.4

ggplot(data = dtal_M, aes(x = grade, y = score)) +

 stat_smooth(aes(group = id), method = 'lm', se = F, color = 'gray') +

 geom_point(color = 'gray') +

 labs(x = '年級', y = '字彙分數')



#將年級置中

dtal_M$grade_c <- scale(dtal_M$grade, scale = F)



#載入nlme，準備做每個人的迴歸線
pkgTest("nlme")
require(nlme)

m1 <- lmList(score ~ grade_c | id, data = dtal_M)



#抓抓看離群值。這個功能來自car套件

#圖9.5

dataEllipse(coef(m1)[, 1], coef(m1)[, 2], levels = c(.68, .95), pch = 19,

            col = 'black', id.n = 2, xlab = '截距估計值', 

            ylab = '斜率估計值')







#潛在成長模型可以視為因素分析的子模型，我們以 lavaan 進行分析
pkgTest("lavaan")
library(lavaan)



#先選擇男性分析

dta_M <- subset(dta, gender == 1)



#先試試看線性模型

growth1 <- 'intercept =~ 1*grade8+1*grade9+1*grade10+1*grade11
slope =~ 0*grade8+1*grade9+2*grade10+3*grade11'  ## Erase invalid `\n`

#程式報表9.5, 9.6

rslt1 <- growth(model = growth1, data = dta_M)

summary(rslt1, fit.measures = T)





#試試看二次模型

growth2 <- 'intercept =~ 1*grade8+1*grade9+1*grade10+1*grade11
linear =~ 0*grade8+1*grade9+2*grade10+3*grade11
qudratic =~ 0*grade8+1*grade9+4*grade10+9*grade11' ## Erase invalid `\n`



#程式報表9.7

rslt2 <- growth(model = growth2, data = dta_M)

summary(rslt2, fit.measures = T)

anova(rslt1, rslt2)





#包含形狀因素的潛在成長模型，形狀不固定

growth3 <- 'intercept =~ 1*grade8+1*grade9+1*grade10+1*grade11
shape =~ 0*grade8+grade9+grade10+1*grade11'  ## Erase invalid `\n`



#程式報表9.8

rslt3 <- growth(model = growth3, data = dta_M)

summary(rslt3, fit.measures=T)



rslt <- c(rslt1,rslt2,rslt3)

lapply(rslt, function(x) fitMeasures(x,c('chisq','df','pvalue','rmsea','srmr','tli','cfi','aic')))
## Make xtable in R markdown




##包括共變量的潛在成長模型

#我們選擇二次模型

#進一步看看性別對截距與形狀的影響。

#先看看女孩資料配適情形。

dta_F <- subset(dta, gender == 2)



rslt22 <- growth(model = growth2, data=dta_F)

summary(rslt22, fit.measures = T)



#將性別變成 0,1 的虛擬變項

dta$g <- ifelse(dta$gender ==1,1,0)



#包含形狀參數的潛在成長模型，讓性別影響截距與形狀參數。

growth4 <- 'intercept =~ 1*grade8+1*grade9+1*grade10+1*grade11
linear =~ 0*grade8+1*grade9+2*grade10+3*grade11
qudratic =~ 0*grade8+1*grade9+4*grade10+9*grade11
intercept~g
linear~g
qudratic~g'  ## Erase invalid `\n`



#程式報表9.9

rslt4 <- growth(model = growth4, data = dta, fixed.x = T)

summary(rslt4, fit.measures = T)





#修改模型，把不顯著的linear, quadratic變異數拿掉

growth5 <- 'intercept =~ 1*grade8+1*grade9+1*grade10+1*grade11
linear =~ 0*grade8+1*grade9+2*grade10+3*grade11
qudratic =~ 0*grade8+1*grade9+4*grade10+9*grade11
intercept~g
linear~g
qudratic~g
linear~~0*linear
qudratic~~0*qudratic'  ## Erase invalid `\n`



#程式報表9.10

rslt5 <- growth(model = growth5, data = dta, fixed.x = T)

summary(rslt5, fit.measures = T)

anova(rslt4,rslt5)





#比較實際資料與預測值平均

#計算實際平均數

dest <- dreal <- aggregate(score ~ grade+gender, data = dtal, mean)

dreal$type <- c(rep('實際男',4),rep('實際女',4))

g <- dreal$gender==1

t <- (dreal$grade-8)



#計算預測值

est <- (1.156+.003*g)+(1.714-.530*g)*t+(-.329+.195*g)*t^2

dest$type <- c(rep('預測男',4),rep('預測女',4))

dest$score <- est

dall <- rbind(dreal,dest)



#畫圖

#圖9.6

xyplot(score ~ grade, data = dall, group = type,

       lty = c(2,2,1,1), type = 'l',

       grid = T, xlab = '年級', ylab = '字彙分數平均', 

       auto.key = list(columns = 4),

       panel = function(...){ 

         panel.xyplot(...) 

       }) 



#放回ggplot2舊主題

theme_set(old)

