
#載入mlmRev套件
library(mlmRev)

#載進資料 Chem97、檢視套件提供的資訊、看資料結構
data(Chem97)
?Chem97
str(Chem97)
#程式報表5.1
head(Chem97)

#去掉第六與第八個變項，只將需要的資料留下，並重新命名
#原始文章分析是以 GCSE 預測 Chem，我們預計倒過來做
dta <- Chem97[, -c(6, 8)]
names(dta) <- c("區域", "學校", "學生", "化學", "性別", "總成績")
#程式報表5.1
head(dta)

#先看描述統計量
#程式報表5.2
sapply(dta[, c('化學', '總成績')], summary)
sapply(dta[, c('化學', '總成績')], sd)

#計算各校化學與總成績平均分數，記錄成資料檔，留待後用
dta_m <- aggregate(cbind(化學, 總成績, as.numeric(性別) - 1) ~ 學校, 
                   data = dta, mean)
names(dta_m) <- c('學校', '化學平均', '總成績平均', '女性比率')
#程式報表5.3
head(dta_m)

#載進 lattice，準備畫圖
library(lattice)

#以總成績與化學原始分數，繪製直方圖
p1 <- histogram(~ 總成績, data = dta, type = 'density', xlab = '總成績',
                ylab = '機率密度')
p2 <- histogram(~ 化學, data = dta, type = 'density', xlab = '化學分數', 
                ylab = '機率密度')

#以學校平均分數，繪製直方圖
p3 <- histogram(~ 總成績平均, data = dta_m, type = 'density', 
          xlab = '各校總成績平均', ylab = '機率密度')
p4 <- histogram(~ 化學平均, data = dta_m, type = 'density', 
          xlab = '各校化學平均分數', ylab = '機率密度')

#計算學校標準差，存檔留後用
dta_sd <- aggregate(cbind(化學, 總成績) ~ 學校, data = dta, sd)
names(dta_sd)[2:3] <- c('化學標準差', '總成績標準差')

#以學校標準差，繪製直方圖。
p5 <- histogram(~ 總成績標準差, data = dta_sd, type = 'density',
          xlab = '各校總成績標準差', ylab = '機率密度')
          
p6 <- histogram(~ 化學標準差, data = dta_sd, type = 'density',
          xlab = '各校化學分數標準差', ylab = '機率密度')
          
#載入gridExtra，把六張放一起
#圖 5.1
require(gridExtra)
grid.arrange(p6, p5, p4, p3, p2, p1, as.table = T)


#看看化學與總成績相關，以學生層次分數計算是 .662
cor(dta[, c('化學', '總成績')])

#以學校層次分數計算生態相關（ecological correlation），為 .698
##程式報表5.4
cor(dta_m[, -1])

#畫看兩者間關聯
#圖5.2
plot(dta[, 4], dta[, 6], type = 'n', xlab ='化學分數', ylab = '總成績', 
     asp = 1)
grid()
#學生
points(dta[, 4], dta[, 6], pch = '.', cex = 2)
abline(lm(dta[,6] ~ dta[, 4]))
#學校
points(dta_m[, 2], dta_m[, 3], cex = 0.5, col = 'grey') 
abline(lm(dta_m[,3] ~ dta_m[,2]), col = 'grey')
#對角線
abline(0, 1, lty = 3, col = 'blue')

#看看各校與各區域以化學預測總分時的截距與斜率
#請注意原始文章的預測變項與解釋變項跟此所作是相反的。
#載入 ggplot2 套件，準備畫圖。
require(ggplot2)

# 記錄下原始配色
old <- theme_set(theme_bw())

#各校以化學預測總分時的截距與斜率
#圖5.3
ggplot(data = dta, aes(x = 化學, y = 總成績, group = 學校))+
 stat_smooth(method = 'lm', se = F, color = 'lightgray') +
 geom_point(size = 1) +
 stat_smooth(aes(group = 1), method= 'lm', se = F, color = 'black') +
 labs(x = '化學分數', y = '總成績', title = '學校')
 
#各區域以化學預測總分時的截距與斜率
#圖5.4
ggplot(data = dta, aes(x = 化學, y = 總成績, group = 區域))+
 stat_smooth(method = 'lm', se = F, color = 'lightgray') +
 geom_point(size = 1) +
 stat_smooth(aes(group = 1), method = 'lm', se = F, color = 'black') +
 labs(x = '化學分數', y = '總成績' , title = '區域') 
 
# 將變項以總分均置中，亦即，減去總平均。
dta$化學置中 <- scale(dta$化學, scale = F)
 
#選取25個學校與區域 
set.seed(1225)
ns25 <- sample(levels(dta$學校), 25)
set.seed(1225)
nr25 <- sample(levels(dta$區域), 25)

#重看一次各校以化學預測總分時的截距與斜率
#圖5.5
ggplot(data = dta[dta$學校 %in% ns25, ], aes(x = 化學置中, y = 總成績, color = 性別))+
  geom_point(size = 1) +
  stat_smooth(method = 'lm', se = F) +
  facet_wrap( ~ 學校 )

#重看一次各區域以化學預測總分時的截距與斜率
#圖5.6
ggplot(data = dta[dta$區域 %in% nr25, ], aes(x = 化學置中, y = 總成績, color = 性別))+
  geom_point(size = 1) +
  stat_smooth(method = 'lm', se = F) +
  facet_wrap( ~ 區域 )
  


#載入 lme4 套件，用來分析多層次資料
library(lme4)

# 將變項以總分均置中，亦即，減去總平均。
dta$化學置中 <- scale(dta$化學, scale = F)

#先以完整模型嘗試
#程式報表5.5
summary(m0 <- lmer(總成績 ~ 化學置中 + 性別 + 化學置中:性別 + 
                  ( 1 | 學校 ) + ( 1 | 區域 ), data = dta ) )       

# 試著去除區域的隨機效果，並看看去除隨機效果是否顯著。
m1 <- update(m0, . ~ . - ( 1 | 區域 ) )
#程式報表5.6
anova(m0, m1)

#去除交互作用項
#程式報表5.7
drop1(m0, test = 'Chisq')

summary(m0)

#載入 coefplot2 套件，繪製固定效果
install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R")
require(coefplot2)
coefplot2(m0)

#抽取變異成分，計算學校與區域可以解釋部分（ICCs）
print(vc <- VarCorr(m0), comp = 'Variance' )
vc <- as.data.frame(vc)
vc[vc$grp=='學校', 'vcov']/ sum(vc$vcov)
vc[vc$grp=='區域', 'vcov']/ sum(vc$vcov)
1 - (vc[vc$grp=='Residual', 'vcov']/ sum(vc$vcov))


#學校與區域層次的隨機效果QQ圖，檢查隨機效果是否呈常態
qq_r21 <- qqmath(~ ranef(m0, varCond = T)$學校, type = c('p', 'g', 'r'), pch = '.',
                xlab = '常態分位數', ylab = '學校隨機效果分位數')
qq_r22 <- qqmath(~ ranef(m0, varCond = T)$區域, type = c('p', 'g', 'r'), pch = '.',
                xlab = '常態分位數', ylab = '區域隨機效果分位數')

#殘差的QQ圖，檢驗殘差是否呈常態
qq_r0 <- qqmath(~ resid(m0, scale = T), type = c('p', 'g', 'r'), pch = '.',
                xlab = '標準化常態分位數', ylab = '標準化殘差')
                
#預測值對殘差圖
m0_f <- fortify(m0)
r_m0 <- ggplot(data = m0_f, aes(x = .fitted, y = .scresid)) + 
          geom_point(pch = '.') +
          stat_smooth(method = 'loess', se = F) +
          labs( x = '預測值', y = '標準化殘差')

#把圖放在一起呈現
#圖5.7
require(gridExtra)
grid.arrange(qq_r21, qq_r22, qq_r0, r_m0, nrow = 2, ncol = 2)

 


#顯示複雜的多層次模型，包括隨機斜率效果、學校層次的預測變項以及跨層次交互作用
#先要將不同層次資料併在一起
dta <- merge(dta,dta_m, by="學校") 

rslt <- lmer(總成績 ~ 化學置中 + 性別 + 化學置中:性別 +
        女性比率 + 女性比率:性別+
        (1+化學置中+性別|學校) + (1|區域), data=dta)
summary(rslt)
