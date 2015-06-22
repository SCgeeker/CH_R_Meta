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

#讀進資料

dta <- read.table("data/TMTmeta.txt", header=T)



#顯示前六筆資料

#程式報表10.1

head(dta)



#載入 RISmed 檢索資料庫
pkgTest("RISmed")
require(RISmed)

tally <- array()

keyword <- 'Terror Management Theory'

x <- 1

for (i in 2001:2013) {

 r <- EUtilsSummary(keyword, type='esearch', db='pubmed', mindate=i, maxdate=i)

 tally[x] <- QueryCount(r)

 x <- x + 1

}

names(tally) <- 2001:2013

#圖10.1

barplot(tally, las=2, main="PubMed 查詢 TMT 文獻數")





#看看效果量的描述性統計

summary(dta$ES)



#效果量直方圖

#圖10.2
pkgTest("lattice")
library(lattice)

histogram(~ES, xlab="研究所得效果（相關）",ylab="相對頻率",xlim=c(-1.0, 1.1), data=dta)



#載入整合分析軟體 metafor
pkgTest("metafor")
require(metafor)



#利用相關與樣本數計算相關的抽樣變異，並放進原先資料

dta$vi <- escalc(measure="COR", ri=dta$ES, ni=dta$N)$vi



#抽取277筆中的31筆，依區域與 ES 排序，繪製森林圖，標示區域

set.seed(201408)

dta31 <- dta[sample(277,31),]

dta31 <- dta31[with(dta31, order(Region, ES)), ]

#圖10.3

forest(dta31$ES, dta31$vi, slab = dta31$Region)





#計算所有研究的平均效果

#程式報表10.2

summary(rslt <- rma(yi=ES, vi=vi, data=dta))



#檢查出版偏誤，先繪製 funnelplot

#圖10.4

funnel(rslt)



#程式報表10.3

ranktest(rslt)

regtest(rslt)



#整合調節分析

#先排除區域不明與第四組研究群體資料

dta <- subset(dta,  subset=(Region != 99 & Group < 4))

dta$Region <- factor(dta$Region)

dta$Group <- factor(dta$Group)



#檢查區域效果。先排除區域不明與第四組研究群體資料

#程式報表10.4

summary(rsltReg <- rma(yi=ES, vi=vi, mods= ~ Region, data=dta))                       



#每組抽取10筆，依 Group 與 ES 排序，繪製森林圖，以 Group 標示

set.seed(2014)

dta1 <- dta[dta$Group==1,][sample(145,10),]

dta2 <- dta[dta$Group==2,][sample(36,10),]

dta3 <- dta[dta$Group==3,][sample(93,10),]

dta30 <- rbind(dta1,dta2,dta3)

dta30 <- dta30[with(dta30, order(Group, ES)), ]

#圖10.5

forest(dta30$ES, dta30$vi,annotate=F,slab = dta30$Group)



#檢查研究群體效果

summary(rsltGrp <- rma(yi=ES, vi=vi, mods= ~ Group, data=dta))



#同時檢驗區域與研究群體效果

#程式報表10.5

summary(rsltGrpReg <- rma(yi=ES, vi=vi, mods= ~ Group+Region, data=dta))



