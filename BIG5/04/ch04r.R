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


#讀檔案，這是一般的文字檔，可以用 notepad 開啟

## 預設資料編碼為CP950(BIG5)
dta <- read.table("data/bully.txt", header = TRUE, encoding="CP950")



#看一下資料結構、前六筆、基本統計量

#程式報表4.1

head(dta)

str(dta)

summary(dta)



#選取青少女與青少男資料，分別稱做 dta_f、dta_m

dta_f <- subset(dta, 性別 == '女')

dta_m <- subset(dta, 性別 != '女')



#看一下變項間關聯

#圖4.2
pkgTest("GGally")
require(GGally)

ggpairs(dta_f[,-1], axisLabels= 'internal')



#Baron & kenny（1986） 的四步驟，以青少女（dta_f）示範

#程式報表4.2

summary(lm(憂鬱~指數+年齡,data=dta_f))

summary(m2<-lm(被霸凌~指數+年齡,data=dta_f))

summary(lm(憂鬱~被霸凌+年齡,data=dta_f))

summary(m4<-lm(憂鬱~指數+被霸凌+年齡,data=dta_f))



#把結果記下來，等一下要用

res_f <- lapply(list(m2, m4), summary)



#部分殘差圖，先設定一張圖分成三小圖

#圖4.3

par(mfrow = c(1, 3))

termplot(m4, partial.resid = T, smooth = panel.smooth) 





#Sobel test

#擷取回迴歸係數與標準誤

a <- c(Est = res_f[[1]]$coef['指數', 'Estimate'], 

       SE = res_f[[1]]$coef['指數', 'Std. Error'])

b <- c(Est = res_f[[2]]$coef['被霸凌', 'Estimate'], 

       SE = res_f[[2]]$coef['被霸凌', 'Std. Error'])



#計算中介效果與標準誤，並進行檢驗

ab <- a['Est'] * b['Est']

abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)

c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))





#利用拔靴法計算中介效果信賴區間

#先載入 alr3 套件，用來協助拔靴法
pkgTest("alr3")
require(alr3)



#記得兩邊的隨機種子要設定成一樣 

set.seed(2014)

beta4_bt <- bootCase(m4, B = 1001)

set.seed(2014)

beta2_bt <- bootCase(m2, B = 1001)



#擷取中介效果

ab_bt <- beta4_bt[,3] * beta2_bt[,2]

c("Bootstrap SD" = sd(ab_bt), quantile(ab_bt, c(.025, .975)))





#如果沒有共變量，可以用MBESS套件
pkgTest("MBESS")
require(MBESS)

mediation(dv=dta$憂鬱, x=dta$指數, mediator=dta$被霸凌,

  bootstrap = TRUE, B = 1001)

#中介效果與總效果圖

#圖4.4

mediation.effect.plot(dv=dta$憂鬱, x=dta$指數, mediator=dta$被霸凌, 

                      legend.loc=NA, ylab = '憂鬱', xlab = '被霸凌')





#調節效果

#驗證性別對指數到被霸凌影響力的調節效果

m1_fl <- lm(被霸凌 ~ 指數 + 年齡 + 性別 + 指數:性別, data = dta)

m1_rd <- update(m1_fl, . ~ . - 指數:性別)

anova(m1_rd, m1_fl)

summary(m1_fl)$r.sq - summary(m1_rd)$r.sq



#驗證性別對指數到憂鬱影響力的調節效果

m4_fl <-lm(憂鬱 ~ 指數 + 被霸凌 + 年齡 + 性別 + 指數:性別, 

            data = dta)           

m4_rd <- update(m4_fl, . ~ . - 指數:性別) 

anova(m4_rd, m4_fl)

summary(m4_fl)$r.sq - summary(m4_rd)$r.sq



#驗證性別對被霸凌到憂鬱影響力的調節效果

m4_fl2 <- update(m4_rd, . ~ . + 被霸凌:性別)

anova(m4_rd, m4_fl2)

summary(m4_fl2)$r.sq - summary(m4_rd)$r.sq



#計算相對解釋量差異 

100*(summary(m4_fl2)$r.sq - summary(m4_rd)$r.sq)/summary(m4_fl2)$r.sq



#呈現迴歸結果

#程式報表4.3

summary(m4_fl2)



#呈現變項效果，載入coefplot套件
pkgTest("coefplot")
require(coefplot)



#記下設定

old <- theme_set(theme_bw())



#去除截距、把效果畫出來

#圖4.5

coefplot(update(m4_fl2, . ~ . - 1)) + 

 labs(x = '估計值', y = '迴歸參數', title = '') 

 



#利用fortify指令把資料加進結果中，方便畫圖

m4_fy <- fortify(m4_fl2)



#交互作用圖

#圖4.6

ggplot(data = m4_fy, aes(x = 被霸凌, y = .fitted, shape = 性別, color = 性別 )) + 

 geom_point(aes(x = 被霸凌, y = 憂鬱, shape = 性別))+

 stat_smooth(method = 'lm', size = 1) +

 scale_x_continuous(breaks = 0:12) +

 labs(x = '被霸凌', y = '憂鬱') +

 theme(legend.position = c(.8, .1))

 

#回覆色彩主題

theme_set(old)



#檢驗調節效果

#此套件不支援中文，把變項換成英文
pkgTest("pequod")
require(pequod)

dtaeng <- dta

names(dtaeng) <- c('gender','dep','bully','BMI','age')

dtaeng$gender <- as.numeric(dtaeng$gender=="男")

summary(rslt <- lmres(dep~BMI+age+bully*gender, data=dtaeng))

#計算與檢驗簡單斜率

#程式報表4.4

summary(sl<-simpleSlope(rslt,pred="bully",mod1="gender",coded="gender"))



#也可以畫圖

#圖4.7

PlotSlope(sl)



