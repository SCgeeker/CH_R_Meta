
#讀檔案，資料來自於 TIMSS 2011 年
dta <- read.table(file = "data/TIMSSmath.txt", header = TRUE)

#檢視資料結構
#程式報表2.2
str(dta)

#看看前六筆
#程式報表2.1
head(dta)

#看看資料基本統計#
#程式報表2.3
summary(dta)

#載進 lattice，準備畫圖。
require(lattice)

#看看數學分數的直方圖
#圖2.1
histogram(~ 數學, data = dta, xlab = '數學分數', ylab='機率',type = "density")


##連續變項間關係
#把學科分數取出來
dta_scores <- dta[, c('數學', '化學', '地科', '生物', '物理')]

#兩兩變項畫散佈圖
#圖2.2
pairs(dta_scores, pch = '.', upper.panel = panel.smooth, lower.panel = NULL, 
      col = 'gray')

#利用 lattice 的 splom 指令重畫兩兩變項散佈圖，算是進階版
#圖2.3
splom(~ dta_scores, cex = 0.1, pch = '.', axis.text.cex = 0.5, 
      type = c('p', 'r', 'g'))

#數學與物理分數相關
round(cor(dta$數學,dta$物理), 3)

#所有學科分數相關
#程式報表2.4
round(cor(dta_scores), 3)

#檢定相關是否顯著，也可以看到信賴區間
cor.test( ~ 數學 + 物理, data = dta_scores)

#載進 Hmist，一次檢定所有相關
#程式報表2.5
require(Hmisc)
rcorr(as.matrix(dta_scores), type="pearson")

#檢驗數學與物理、數學與生物何者相關高
#程式報表2.6
require(cocor)
cocor(~數學 + 物理 | 數學 + 生物, dta)
cocor(~數學 + 物理 | 地科 + 生物, dta)


##連續變項與類別變項間的關係
##兩個類別
#看看家裡有無電腦跟數學間的關係
#圖2.4
densityplot(~ 數學, groups = 電腦, data = dta, xlab = '數學分數', lty = c(1,2),
  plot.points = F, type = "g", , main = '電腦 (無 = 虛線, 有 = 實線)')

#也可用QQ圖比較
#圖2.5
qq(電腦 ~ 數學, data = dta, type = c('p', 'g'), pch = '.', aspect = 1, 
   xlab = '數學分數 (有電腦)', ylab = '數學分數 (無電腦)')

#看看有無電腦的學生數學平均與數標差
aggregate(數學 ~ 電腦, data = dta, FUN = mean)
aggregate(數學 ~ 電腦, data = dta, FUN = sd)


##多個類別
#看看不同國家學生的資料分數直方圖
#圖2.6
histogram(~ 數學 | 國家, data = dta, xlab = '數學分數', ylab='機率',
          type = 'density', layout = c(4, 1))

#以盒鬚圖看不同國家學生的差異
#圖2.7
bwplot(國家 ~ 數學, data = dta, xlab = '數學分數',
  panel = function(x, y, ...) {
    panel.bwplot(x, y, ...)
    panel.grid(v = -1, h = 0, ...) })

#看看不同國家的學生數學平均與數標差
aggregate(數學 ~ 國家, data = dta, FUN = mean)
aggregate(數學 ~ 國家, data = dta, FUN = sd)

#看看不同國家間，物理與數學間的關係是否類似
#圖2.8
xyplot(物理 ~ 數學 |  國家, data = dta, xlab = '數學分數', ylab = '物理分數',
       type = c("g", "p", "r"), cex = 0.1, layout = c(4, 1))

##連續變項與兩個類別變項間的關係
#看看不同國家、有無電腦的學生數學平均與對應的平均數標準誤
#程式報表2.7
show(m0 <- aggregate(數學 ~ 國家 + 電腦, data = dta, FUN = mean))
show(m1 <- aggregate(數學 ~ 國家 + 電腦, data = dta, function(x) sd(x)/sqrt(length(x))))

#把資料集中，並重排國家
m <- data.frame(m0, l = m0$數學 - m1$數學, u = m0$數學 + m1$數學)
m$國家 <- factor(m$國家, levels=c('日本', '香港', '台灣', '韓國'))

#載入 latticeExtra 套件
require(latticeExtra)

#在圖中加入了誤差
#圖2.9
segplot( 國家 ~ l + u | 電腦, data = m, centers = 數學, 
        draw.bands = F, xlab = '數學平均分數', ylab = '國家',
        main = '電腦', layout = c(1, 2),
        segments.fun = panel.arrows, ends = "both", angle = 90, length = 1, 
        unit = "mm")
