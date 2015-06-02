
#讀取檔案
dta <- read.table("data/Rosenberg.txt", header = T)

#看前六筆、以及資料結構
#程式報表8.1, 8.2
head(dta)
str(dta)         

#為了示範處理二元資料，將資料轉成兩點。
dta_b <- data.frame(ifelse(dta[, -1] > 3, 1, 0))

#計算「正確率」（答「同意」的百分比）
colMeans(dta_b) 

#依正確率重排變項
dta_b <- dta_b[, order(colMeans(dta_b))]

#計算總分（答「同意」題數）
dta_b$ts <- apply(dta_b, 1, sum)

#預計以總分-正確率繪圖
#先製造一個 6x6 個矩陣，轉成資料框架
pm <- data.frame(matrix(NA,6,6))
                                                       
#不同總分時各題答對率
for(i in 0:5) {
    pm[(i+1), ] <- sapply(subset(dta_b, ts == i), mean) }

#加上變項名字
names(pm) <- names(dta_b)

#轉成長形
pmL <- reshape(pm, varying=list(1:5), v.name = 'PC', direction='long', 
               times = names(dta_b[, -6]),  timevar = '題目')

#畫圖，先載入ggplot2
#圖8.1
require(ggplot2)
ggplot(data = pmL, aes(x = ts, y = PC, shape = 題目)) + 
  geom_point(size = 3) +
  geom_line() +
  labs(x = '總分', y = '同意比率') +
  theme_bw() +
  theme(legend.position = c(.8, .2))


#載入ltm套件，進行 Rasch 分析
require(ltm)

#Rasch 分析不需要總分，將之移除
dta_b <- dta_b[, -6]

#ltm 仍內建古典測驗理論的題目分析
#程式報表8.3
descript(dta_b)

# Rasch分析
#程式報表8.4
summary(mb <- rasch(dta_b))

#題目特徵曲線
#圖8.1
plot(mb, xlab = '自尊量度', ylab = '機率', main = '題目特徵曲線',
     lwd = 1.5, legend = TRUE, cx = 'right', col = 'black', 
     lty = c(2, 1, 2, 1, 2), pch = c(0, 16, 4, 15, 1))
grid()

#題目資訊曲線
#圖8.3
plot(mb, type = 'IIC', xlab = '自尊量度', ylab = '資訊', main = '題目資訊曲線',
     legend = TRUE, lwd = 1.5, cx = 'topright', col = 'black', 
     lty = c(2, 1, 2, 1, 2), pch = c(0, 16, 4, 15, 1))
grid()

#測驗資訊曲線
#圖8.4
plot(mb, type = 'IIC', items = 0, xlab = '自尊量度', ylab = '資訊', 
     main = '測驗資訊曲線', lwd = 1.5)
grid()

#適合度檢定
#程式報表8.5
GoF.rasch(mb, B = 1000)

#去除「滿意」後適合度檢定
mb2 <- rasch(dta_b[,-2])
GoF.rasch(mb2,B=1000)

#利用 eRm 補充其他指標、檢定以及 DIF 檢測
#程式報表8.6
require(eRm)
summary(mb1 <- RM(dta_b))

#列印個人能力參數，總分與個人能力對照表
#程式報表8.7
show(mb1p <- person.parameter(mb1))

#人-題圖（person-item map）
#圖8.5
plotPImap(mb1, sorted = TRUE, main = '人-題圖', latdim = '自尊量度',
          pplabel = '人\n參數\n分配')

#outfit MSQ 看起來還好
#程式報表8.8
itemfit(mb1p)

#繪製 infit 圖
#圖8.6
plotPWmap(mb1, mainitem = '題圖', latdim = '自尊量度', tlab = 'Infit t-統計值')



#性別的差異試題功能分析
#程式報表8.9
summary(mb1_lrt <- LRtest(mb1, splitcr = dta$性別))

#繪製不同性別題目特徵圖，並加上校正後的信賴區間
#圖8.7
plotDIF(mb1_lrt, gamma = (1 - (0.05/10)),main = '題目性別差異信賴區間')
abline(v = 0, lty = 2, col = 'lightgray')



#某些版本的R可能無法將HH套件所需的某些套件自動載入
#因此我們需手動載入這些未順利載入的套件
require(grid)
require(lattice)
require(latticeExtra)
require(HH)

#以圖形呈現點量表的情形
#圖8.8
dta_t <- data.frame(sapply(dta[, -1], table))
likert(t(dta_t), as.percent = T, positive.order = T, main = '各題反應')

#要分析的資料，不包括第一個變項性別
dta_lkt <- dta[, -1]

#利用平均數重新排序變項
dta_lkt <- dta_lkt[, order(colMeans(dta_lkt))]

#繪圖
#圖8.9
require(reshape2)
dtaL <- melt(dta_lkt, variable.name = '題目', value.name = '分數')
ggplot(data = dtaL, aes(x = 分數, group = 題目)) + 
 geom_histogram(aes(y = ..density.. ), binwidth = .5, fill = 'gray') +
 stat_function(fun = dnorm, args = list(mean = mean(dtaL$分數), sd = sd(dtaL$分數)))+
 facet_wrap( ~ 題目) +
 labs(x = '分數', y = '機率密度') +
 theme_bw() 
 
#多序類資料的Graded response model分析
#程式報表8.10
summary(grm0 <- grm(dta_lkt, constrained = TRUE))

#把五題畫在一起
#圖8.10
par(mfrow = c(2, 3))

# Item characteristic curves
plot(grm0, lwd = 1.2, legend = TRUE, cx = 'right', col = gray(0:6/6), 
     cex.main = 0.7, xlab = '自尊量度', ylab = '机率')

#關掉圖形裝置
dev.off()

## Item Information Curves 
plot(grm0, type = 'IIC', legend = TRUE, cx = 'topright', lwd = 1.2,
     xlab = '自尊量度', ylab = '資訊', main = '項目資訊曲線', lty = 1:6,
     col = gray(0:6/6))
grid()

## Test Information Function 
plot(grm0, type = 'IIC', items = 0, lwd = 1.5,
     xlab = '自尊量度', ylab = '資訊', main = '量表資訊曲線')
grid()

#預設的 GRM，不要求區辨力相同
summary(grm1 <- grm(dta_lkt))

#不要求區辨度相同的二參數模型表現較佳
anova(grm0, grm1)

