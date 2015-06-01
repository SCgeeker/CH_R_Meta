## 在Rstudio，Tools -> Global Options -> General -> Default Text Encoding -> UTF-8
## 系統語系要設定為台灣正體中文
Sys.setlocale(category = "LC_ALL", locale = "cht")

##讀取檔案、提取資料與製造變項

#這是一般 TXT 檔，檔頭有變項名稱

#資料來自於 NHIS 2010 調查,取 1955 年以前出生者（55歲以上）

#視力困難、聽力困難、移動困難與溝通困難變項，分數越高越有困難

## 預設資料編碼為CP950(BIG5)
dta <- read.table(file = 'data/Quality_of_Life.txt', header = TRUE, encoding = "CP950")

#dta 的類型是資料框架（data frame）

class(dta)

       

#看看dta維度

dim(dta)



#利用names看看變項名稱

names(dta)

#看前六筆

#程式報表1.1

head(dta)







#看看第一列第一欄對應資料，這是類別資料

dta[1, 1]



#看看第九列，這是資料框架

#看看第一欄，這「不是」資料框架，是類別

dta[9, ]

dta[,1]



#前者是類別變項，後者是資料框架

dta[,'教育']

dta['教育']



#還是資料框架

dta[5:7, c('視力', '聽力')]



#檢視資料結構

#程式報表1.2

str(dta)



#看看類別變項的屬性，這是五個水準類別變項

attributes(dta[, '教育'])



#利用table指令看看類別變項分佈

#程式報表1.3

table(dta[, '教育'])



#將類別變項轉成數值變項，再利用table

#程式報表1.3

table(as.numeric(dta[, '教育']))









#指令summary是常用指令，可以用在不同物件上

#指令summary作用結果視作用物件而定

#這是用在資料框架上

#程式報表1.4

summary(dta['教育'])



#這是用在類別變項上

#程式報表1.4

summary(dta[, '教育'])



#第一個是數值，第二個是資料框架

#指令t用來轉置矩陣，資料框架被轉成矩陣了

class(dta[, '視力'])

class(dta['視力'])

class(t(dta['視力']))



#製造平均數兩種寫法

#很直覺，不過如果有遺漏值就麻煩了

dta$功能 <-(dta$視力 + dta$聽力 + dta$移動 + dta$溝通)/4

                          

#有遺漏值會警告我們

tail(dta$功能 <- rowMeans(dta[, 4:7]))







###基本統計量（示範應用函數）

#看看資料基本統計

#程式報表1.5

summary(dta)



#計算健康情形標準差

#利用 with 會讓指令比較好寫好讀

sd(dta$功能)

with(dta, sd(功能))



#計算偏態

#先安裝再載入套件 moments

#注意，安裝只需要一次，但載入則是每次

install.packages('moments', dependencies = TRUE)

require(moments)

skewness(dta$功能)



#一次計算所有平均數

sapply(dta[, -c(1:3)], mean)



#定義一個函數，可以同時算出平均數、標準差、偏態與峰度

#一次計算多個變項平均數、標準差、偏態與峰度

#程式報表1.6

my_summary <- function(x) {

 require(moments)

 funs <- c(mean, sd, skewness, kurtosis)

 sapply(funs, function(f) f(x, na.rm = TRUE))

}

sapply(dta[, c(4:8)], my_summary)







#計算不同教育程度健康功能標準差（兩種寫法）

tapply(dta$功能, dta$年齡, sd)

with(dta, tapply(功能, 年齡, sd))



#把不同性別、教育者平均數算出來（兩種寫法）

#第二種寫法把輸出放到 hlth 物件了，沒有輸出到螢幕

#程式報表1.7

with(dta, aggregate(dta[, 4:8], by = list(性別, 年齡), FUN = mean))

hlth <- aggregate(cbind(視力, 聽力, 移動, 溝通, 功能) ~ 性別 + 年齡, 

                       data = dta, mean)

                       

#把 hlth 的前兩欄命名

names(hlth)[1:2] <- c('性別', '年齡')



#不同性別與年齡的健康功能圖

#圖1.1

with(hlth, dotchart(t(功能), group = 性別, labels = 年齡, xlab = '功能'))



#依移動排序，可以看到男性移動困難退化的較慢

#程式報表1.8

hlth[order(hlth$移動),c(1:2,5)]







###繪圖顯示（示範資料重新結構）



#先將教育程度水準重排

dta$教育 <- factor(dta$教育, levels = c('小學', '初中', '高中', '專科', '大學'))



#不同教育水準健康功能盒鬚圖

#把資料改成水平，更容易比較

#這是直接針對原始資料，而非統計量畫圖

#圖1.2

plot(功能 ~ 教育, data = dta, horizontal = T)



#加上格線，有助比較

grid()





#載入 lattice，準備畫圖

#取出四個要畫圖的變項

require(lattice)

v4 <- dta[, 4:7]



#把變項年齡與性別上的直方圖記下來

p <- lapply(names(v4), function(x) histogram( ~ v4[x] | 年齡 + 性別, data = dta,

            xlab = x )) 

                            

#利用 gridExtra 排一下四張圖

#圖1.3

library(gridExtra)

grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2, ncol = 2)







#載進 ggplot2，準備畫圖

#Hmisc提供 ggplot2 額外功能

require(ggplot2)

require(Hmisc)



#調整圖形，避免重疊

pd <- position_dodge(width = .3)



#繪製不同年齡生理健康的平均數

#加上利用拔靴法得到的平均數信賴區間

#圖1.4

ggplot(data = dta, aes(x = 年齡, y = 功能, color = 性別) ) +

 stat_summary(fun.data = 'mean_cl_boot', position = pd) +

 facet_grid(教育 ~ . ) +

 labs(x = '年齡', y = '功能分數') +

 coord_cartesian(ylim = range(dta['功能'])) +

 coord_flip() +

 scale_color_manual(values = c('black', 'gray'), 

                    guide = guide_legend(title = '性別', reverse = TRUE)) +

 theme_bw() +

 theme(legend.justification = c(1, 0), legend.position = c(1, 0))



 

 

#載入reshape2、把資料由寬形變長形

require(reshape2)

dtal <- melt(dta[, -8], variable.name = '功能項目', value.name = '分數')



#看看資料

#程式報表1.9

head(dtal)



#不同年齡性別各功能項目平均數

#圖1.5

ggplot(data = dtal, aes(x = 年齡, y = 分數, 

       shape = reorder(功能項目, 分數, mean),  group = reorder(功能項目, 分數))) +

 stat_summary(fun.data = mean_se, geom = 'errorbar', position = pd) +

 stat_summary(fun.y = mean, geom = 'point', position = pd, size = 3) +

 stat_summary(fun.y = mean, geom = 'line', position = pd, linetype = 'dotted') +

 facet_grid( . ~  性別) +

 labs(x = '年齡', y = '分數') +

 scale_shape_manual(values = c(2, 8, 1, 6), 

                    guide = guide_legend(title = '功能項目', reverse = TRUE)) +

 theme_bw() +

 theme(legend.justification = c(0, 1), legend.position = c(0, 1))



#不同性別在功能上平均數檢定（兩種寫法）

#程式報表1.10

t.test(功能 ~ 性別, data = dta)

with(dta, t.test(功能 ~ 性別))

          

