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

#資料來自於 TIMSS 2011 年台灣資料

#讀檔案
## 預設資料編碼為CP950(BIG5)
dta <- read.table(file = "data/TIMSS2011TW.txt", header = TRUE, encoding="CP950")



#看資料結構與前六筆

#程式報表3.1

str(dta)

head(dta)



#看資料基本統計

#程式報表3.2

summary(dta)







#載進 ggplot2 準備畫圖
pkgTest("ggplot2")
require(ggplot2)



#底下的圖都用黑白配色（theme_bw），先記下目前的設定

#最後要記得改回來

old <- theme_set(theme_bw())



#看不同性別數學分數的盒鬚圖

#圖3.1

ggplot(data = dta, aes(x = 性別, y = 數學)) +

 geom_boxplot() + coord_flip() +

 labs( y = '分數', x = '性別', title = '數學分數盒鬚圖')



#看信賴區間

with(dta, tapply(數學, 性別,

     function(x) c(mean(x) + c(-2, 2) * sd(x)/sqrt(length(x)))))



#以t檢定比較不同性別的數學差異、預設作法會做 Welch 校正

#程式報表3.3

t.test(數學 ~ 性別, data = dta)



#這才是一般假設變異數同值下的 t 檢定

#程式報表3.4

t.test(數學 ~ 性別, data = dta, var.equal = TRUE)







#看不同父母教育背景者的數學成績差異

#先把父母教育各個水準順序定下來

dta$父母教育 <- factor(dta$父母教育, levels = c('國小以下', '初中', '高中',

                                                '專科', '大學以上'))

#看不同父母教育程度下的數學分數平均數

with(dta, tapply(數學, 父母教育, mean) )



#圖示不同父母教育程度下的數學分數平均數，加上信賴區間

#圖3.2

ggplot(data = dta, aes(x = 父母教育, y = 數學)) +

  stat_summary(fun.data = 'mean_cl_boot', size = 1) +

  scale_y_continuous(breaks = seq(500, 660, by = 20)) +

  geom_hline(yintercept = mean(dta$數學) , linetype = 'dotted') +

  labs(x = '父母教育', y = '數學平均分數') +

  coord_flip()

  

#檢定 

#程式報表3.5

anova(m1 <- lm(數學 ~ 父母教育, data = dta))

summary(m1)$r.squared







#父母教育的效果或許會是教育資源造成的，畫圖看看

#圖3.3

ggplot(data = dta, aes(group = 父母教育, y = 數學, x = 教育資源)) +

  stat_smooth(method = 'lm', se = F) +

  stat_smooth(aes(group = 父母教育, y = 數學, x = 教育資源), method = 'lm', se = F) + 

  facet_grid( . ~  父母教育) +

  labs(x = '教育資源', y = '數學分數')



#把教育資源加進模型

#程式報表3.6

anova(m2 <- update(m1, . ~ . + 教育資源, data = dta))



#或許不是父母教育而是教育資源造成，這邊只考慮教育資源

#程式報表3.7

anova(m3 <- update(m2, . ~ . - 父母教育,  data = dta))



#將結果放在一個list中，等一下比較方便抓結果

res_lm <- lapply(list(m1, m2, m3), summary)



#比較在控制教育資源下，父母教育的效果

#程式報表3.8

(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq

anova(m3, m2)



#比較在控制父母教育下，教育資源的效果

(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq

anova(m1, m2)



#畫效果
pkgTest("coefplot")
require(coefplot)

coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = 數學分數')



#將截距去除，畫更易懂起來

#圖3.4

m2 <- lm(數學 ~ 父母教育+教育資源- 1, data = dta)

coefplot(m2, xlab = '估計值', ylab = '迴歸變項', title = '反應變項 = 數學分數')







#把資料與迴歸分析的預測值、殘差與影響度放進資料

fit_m2 <- data.frame(dta[, c(2, 12, 13)], fitted = fitted(m2), resid = resid(m2),

                     infl = influence(m2)$hat )



#疊合真實觀測值預測值的直方圖，依父母教育

#圖3.5

ggplot(data = fit_m2, aes(x = 數學, group = 父母教育 )) +

 stat_density(geom = 'path', position = 'identity') +

 stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +

 geom_vline(xintercept = c(with(dta, tapply(數學,父母教育, mean))), linetype = 'dotted')+

 facet_grid(父母教育 ~ .) +

 scale_x_continuous(breaks = seq(200, 900, by = 100))+

 labs(x = '數學分數', y = '機率密度')



#看殘差分配，依父母教育，檢視常態與變異數同質假設

#圖3.6

ggplot(data = fit_m2, aes(x = scale(resid)), group = 父母教育 ) +

 stat_density(geom = 'path', position = 'identity', aes(linetype = 父母教育)) +

 scale_linetype_manual(values = 5:1) +

 guides(linetype = guide_legend(reverse = TRUE)) +

 labs(x = '標準化殘差', y = '機率密度') +

 theme(legend.position = c(.15, .8))



#看看殘差的 Q-Q 圖，依父母教育。檢視常態假設

#圖3.7
pkgTest("lattice")
require(lattice)

qqmath(~ scale(resid) | 父母教育, data = fit_m2, type = c('p', 'g', 'r'),

       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),

       pch = '.', cex = 2)



#畫預測值與殘差的散佈圖，檢查線性與等分散假設

#圖3.8
pkgTest("MASS")
require(MASS)

ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = 父母教育 )) +

  geom_point(pch = 20, size = 1) +

  stat_smooth(method = 'rlm', se = F) +

  facet_grid(父母教育 ~ .) +

  labs(x = '數學預測值', y = '標準化殘差')



#呈現影響值（影響估計結果過大的值）與標準化殘差

#圖3.9

ggplot(data = fit_m2, aes(x = infl, y = scale(resid), group = 父母教育)) +

 geom_text(aes(label = rownames(fit_m2)), cex = 2) +

 geom_hline(yintercept = 0, linetype = 'dotted') +

 facet_grid(父母教育 ~ .) +

 labs(x = '影響值', y = '標準化殘差')



#看看影響值

summary(influence(m2)$hat)



#改回舊的配色，不然 R 就黑白下去了

theme_set(old)







#底下要呈現多個連續解釋變項時的情形

#看看個人變項的可能效果，把跟數學有關的部分取出來

dta_math <- dta[, c('數學', '數學興趣', '數學評價', '數學投入')]



#看看基本統計量

colMeans(dta_math)



#呈現兩兩散佈圖

#圖3.10
pkgTest("heplots")
require(heplots)

scatterplotMatrix(~ 數學 + 數學興趣 + 數學評價 + 數學投入, data= dta_math,

  pch = '.', cex = 3, smooth = FALSE, reg.line = FALSE, ellipse = TRUE,

  diagonal = 'none', lower.panel = NULL)



#載入corrplot 套件，以圖形顯示相關大小

#圖3.11
pkgTest("corrplot")
require(corrplot)

corrplot(cor(dta_math), method = 'ellipse', order = 'hclust', addrect = 4,

         type = 'upper', tl.pos = 'tp')

corrplot(cor(dta_math), add = TRUE, type = 'lower', method = 'number',

         order = 'hclust', col = 'black', diag = FALSE, tl.pos = 'n', cl.pos = 'n')



#放進三個解釋變項

#程式報表3.9

summary(m4 <- lm(數學 ~ 數學興趣 + 數學評價 + 數學投入, data = dta_math))



#看效果

#圖3.12

coefplot(m4, predictors = c('數學興趣', '數學評價', '數學投入'),

 xlab = '估計值', ylab = '迴歸變項(去除截距)', title = '反應變項是數學分數')



#看效果

#圖3.13
pkgTest("effects")
require(effects)

plot(allEffects(m4), main = '', ylim = c(550, 670), grid = T)



#載入 lm.beta套件，計算標準化迴歸係數

#程式報表3.10
pkgTest("lm.beta")
library(lm.beta)

summary(lm.beta(m4))



#看看控制數學興趣與數學評價後，數學投入的效果

summary(m5 <- update(m4, . ~ . - 數學投入 , data = dta_math))

anova(m5, m4)

