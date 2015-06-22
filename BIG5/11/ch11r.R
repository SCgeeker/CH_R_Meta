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

dta <- read.table('data/married.txt', header = T)



#顯示前六筆，看資料結構

#程式報表11.1

head(dta)

#程式報表11.2

str(dta)

summary(dta)



#重排教育各水準

dta$教育 <- factor(dta$教育, levels = c('國小以下', '國中', '高中',

                                              '專科', '大學以上'))



#重排婚姻，由多到少

dta$婚姻 <- factor(dta$婚姻, levels = c('有偶', '未婚', '離婚', '喪偶'))



#將年齡換成年次

dta$年次 <- (102-dta$年齡)



#將婚姻狀況分為未婚與結過婚

dta$結過婚 <- ifelse(dta$婚姻 == '未婚', 0, 1)



#計算每個變項組別各百分比 

#程式報表11.3

apply(dta, 2, function(x) table(x)/dim(dta)[1])





#單一變項與結過婚與否的列聯表

#程式報表11.4

ftable(dta, row.vars = '教育', col.vars = '結過婚')

ftable(dta, row.vars = '性別', col.vars = '結過婚')

ftable(dta, row.vars = '年次', col.vars = '結過婚')



#兩個變項與結過婚與否的列聯表

#程式報表11.5

ftable(dta, row.vars = c('教育', '性別'), col.vars = '結過婚')

ftable(dta, row.vars = c('教育', '年次'), col.vars = '結過婚')

ftable(dta, row.vars = c('性別', '年次'), col.vars = '結過婚')



#三個變項與結過婚與否的列聯表，也可以換成比率

ftable(dta, row.vars = c(1, 2, 5), col.vars = '結過婚')

prop.table(ftable(dta, row.vars=c(1, 2, 5), col.vars = '結過婚'), 1)



#換種方式看

#程式報表11.6

ftable(dta, row.vars = c(1, 2), col.vars = c('年次','結過婚'))

ftable(xtabs(結過婚 ~ 性別 + 教育 + as.factor(年次), data = dta))



#準備要畫圖，載入ggplot2
pkgTest("ggplot2")
require(ggplot2)



#圖示不同年次、性別與教育程度的結過婚比率，加上信賴區間

#圖11.1

ggplot(data = dta, aes(x = 年次, y = 結過婚, shape = 性別, color = 性別)) +

  stat_summary(fun.data = mean_se, size = 1) +

  facet_grid(. ~ 教育 ) +

  labs(x = '年次', y = '結過婚百分比') +

  theme(legend.position = c (.95, .1))







#簡單邏輯迴歸

#程式報表11.7

summary(m1 <- glm(結過婚 ~ 年次, data = dta, family = binomial) )



#勝算比的信賴區間 

#程式報表11.8

exp(cbind(coef=coef(m1), confint(m1))) 



#邏輯迴歸，加入性別與教育程度

summary(m_full <- glm(結過婚 ~ 年次 * 性別 * 教育 , data = dta, family = binomial))



#看看那個效果可刪除

drop1(m_full, test = 'Chisq')

summary(m_drop1 <- update(m_full, ~ . - 年次:性別:教育) )



#邏輯迴歸模型診斷，將預測值與實際觀測值放在一起

#圖11.2

ggplot(data = dta, aes(x = 年次, y = 結過婚, shape = 性別, color = 性別)) +

  stat_summary(fun.data = mean_se, size = 1) +

  stat_summary(aes(y = fitted(m_drop1)), fun.y = mean, geom = 'line', size = 1) +

  facet_grid(. ~ 教育) +

  scale_x_continuous(breaks = seq(30, 70, by = 5)) +

  labs(x= '年次', y = '結過婚百分比') +

  theme(legend.position= c (.9, .1))





#修改邏輯迴歸，加入分散參數。載入 dispmod 套件，預備處理此事
pkgTest("dispmod")
require(dispmod)



#將資料整理成 dispmod 要的形式

dta_f <- data.frame(ftable(dta, row.vars = c(1, 2, 5), col.vars = '結過婚'))

dta_f1 <- subset(dta_f , 結過婚 == 1)

dta_f1$Tot <- dta_f[1:70, 5] + dta_f[71:140, 5]

dta_f1 <- dta_f1[,-4]



#將年次設定為連續變項

dta_f1$年次 <- as.numeric(as.vector(dta_f1$年次))



#看看資料

#程式報表11.9

head(dta_f1)



#以整理過資料重新分析，確認與前面相同

summary(m_last <- glm(cbind(Freq, Tot-Freq) ~ 年次 * 性別 * 教育 - 年次:性別:教育, 

                    data = dta_f1, family = binomial))



#加入分散參數

#程式報表11.10, 11.11

summary(m_lastd <- glm.binomial.disp(m_last))



#看看分散參數估計值

m_lastd$dispersion



#看看模型是否改善

anova(m_last, m_lastd)



#準備再一次模型診斷，收集資料，畫圖

#圖11.3

dta_mlastd <- data.frame(dta_f1, phat = fitted(m_lastd))

ggplot(data = dta_mlastd, aes(x = 年次, y = Freq/Tot,  color = 性別, shape = 性別)) +

  geom_point(size = 3) +

  stat_smooth(aes(x = 年次, y = phat), method = 'loess', se = F, size = 1) +

  facet_grid(. ~ 教育) +

  scale_x_continuous(breaks = seq(30, 70, by = 5)) +

  labs(x= '年次', y = '結過婚百分比') +

  theme(legend.position = 'NONE' )

  

#準備繪圖。為了繪製方便，模型中去掉截距

summary(m_lastd0 <- update(m_lastd, . ~ . - 1))
pkgTest("coefplot")
require(coefplot)



#繪製估計值

#圖11.4

coefplot(m_lastd0) + labs(x = '估計值', y = '變項', title = '') 





 

##多分類邏輯迴歸

#先看看年代與婚姻列聯表

#程式報表11.12

with(dta, prop.table(table(年次, 婚姻), 1))



#載入 mlogit 套件，準備執行多分類邏輯迴歸
pkgTest("mlogit")
require(mlogit)



#將資料轉成 mlogit 需要格式

dta_w <- mlogit.data(dta, shape = 'wide', choice = '婚姻')



#看看資料

#程式報表11.13

head(dta_w)



#只展示年代效果

#程式報表11.14

summary(ml0 <- mlogit(婚姻 ~ 0 | 年次, data = dta_w ) )



#勝算比信賴區間

cbind(b_hat = exp(coef(ml0)), b_ci = exp(confint(ml0)))



#準備畫圖，先得到預測值

newdta <- with(dta, data.frame(

               年次 = rep(seq(from = 39.5, to = 69.5, length = 7), 4),

               婚姻 = factor(rep(c('有偶', '未婚', '離婚', '喪偶'), each = 7) ) ) )

 

newdta_w <- mlogit.data(newdta, shape = 'wide', choice = '婚姻')

ml0_phat <- as.data.frame(predict(ml0, newdta_w)[1:7, ])



#需轉換資料形式，載入 reshape 套件
pkgTest("reshape2")
require(reshape2)



#製造年次婚姻列聯表、將預測值轉成需要格式、彙整在一起

obs_p <- data.frame(with(dta, prop.table(table(年次, 婚姻), 1)))

dta_op <- data.frame(obs_p, melt(ml0_phat))



#繪圖

#圖11.5

ggplot(data = dta_op, aes(x = 年次, y = Freq, shape = 婚姻)) +

 geom_point(size = 3) + 

 stat_smooth(aes(x = 年次, y = value, group = 婚姻), method = 'loess', se = F, size = 1) +

 labs(x = '年次', y = '分類百分比') +

 theme(legend.position = c(.92, .88))