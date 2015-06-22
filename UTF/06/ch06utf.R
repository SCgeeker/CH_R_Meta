
#讀進資料
dta <- read.table("data/prenatal1.txt", header = T)

#以str看一下資料結構、看一下最後六題，確認樣本數是500
#程式報表6.1
str(dta)
tail(dta)

#計算每題遺漏比率，500是樣本數
show(apply(apply(dta, 2, is.na), 2, sum)/500)

#看看各題四個選項分配
show(dta_tbl <- apply(dta, 2,  table)/500)

#為畫圖準備，載入reshape2，將表格資料由寬形換成長形
require(reshape2)
dtal_tbl <- melt(dta_tbl)

#命名長形資料變項名
names(dtal_tbl) <- c("選項", "題目", "比率")

#製造題目編號
dtal_tbl$題目編號 <- rep(1:35, rep(4,35))

#更動選項顯示，利用plyr套件
require(plyr)
dtal_tbl$選項 <- mapvalues(dtal_tbl$選項, from = c(1,2,3,4), to = c("一點也不","稍微如此","有些如此","的確如此"))
dtal_tbl$選項 <- ordered(dtal_tbl$選項,c("一點也不","稍微如此","有些如此","的確如此"))

#看一下資料
#程式報表6.2
head(dtal_tbl)

#準備畫圖，更改並記錄下目前配色主題
require(ggplot2)
old <- theme_set(theme_bw())

#分量表一：前15題
#圖6.1
ggplot(data = subset(dtal_tbl, 題目編號 < 16), aes(x = reorder(題目, 比率, max), 
       y = 比率, ymin = 0.25, ymax = 比率, group = 題目)) +
 geom_pointrange() +
 geom_hline(yintercept = 0.25, linetype = "dotted") +
 facet_grid(. ~ 選項) +
 coord_flip() +
 labs(x = "題目", y = "選項比率", title = "母職認同分量表") 
 
#分量表二：16-25題
ggplot(data = subset(dtal_tbl, 題目編號 > 15 & 題目編號 < 26),  aes(x = reorder(題目, 比率, max), 
       y = 比率, ymin = 0.25, ymax = 比率, group = 題目)) +
 geom_pointrange() +
 geom_hline(yintercept = 0.25, linetype = "dotted") +
 facet_grid(. ~ 選項) +
 coord_flip() +
 labs(x = "題目", y = "選項比率", title = "母親接受分量表") 
  
#分量表三：26-35題
ggplot(data = subset(dtal_tbl, 題目編號 > 25),  aes(x = reorder(題目, 比率, max), 
       y = 比率, ymin = 0.25, ymax = 比率, group = 題目)) +
 geom_pointrange() +
 geom_hline(yintercept = 0.25, linetype = "dotted") +
 facet_grid(. ~ 選項) +
 coord_flip() +
 labs(x = "題目", y = "選項比率", title = "先生接受分量表") 

#回復配色主題
theme_set(old)

#定義一個函數，可以同時
#計算題目的平均數、標準差、偏態與峰度
my_summary <- function(x) {
 require(moments)
 funs <- c(mean, sd, skewness, kurtosis)
 sapply(funs, function(f) f(x, na.rm = TRUE))
}

#一次算完所有題目前四級動差，並存成資料檔
#程式報表6.3
dta_desc <- apply(dta, 2, my_summary)
rownames(dta_desc) <- c("平均", "標準差", "偏態", "峰度")
rslt1 <- as.data.frame(t(dta_desc))
round(rslt1,3)

#準備畫圖，改成長形資料
dtal_desc <- melt(dta_desc)
names(dtal_desc)[1:2] <- c("動差", "題目")
head(dtal_desc)

#換配色主題，並把舊的存起來
old <- theme_set(theme_minimal())

#繪製所有題目平均數
ggplot(data = subset(dtal_desc, 動差 == "平均"), 
       aes(x = reorder(題目, value, max), y = value, group = 動差)) +
 geom_point(size = 3)+
 geom_hline(yintercept = mean(t(dta_desc["平均",])) + 
            c(-1.5, 0, 1.5) * sd(t(dta_desc["平均",])), linetype = "dashed") +
 coord_flip() +
 labs(x = "題目", y = "平均") 
 
#繪製所有題目標準差
#圖6.2
ggplot(data = subset(dtal_desc, 動差 == "標準差"), 
       aes(x = reorder(題目, value, max), y = value, group = 動差)) +
 geom_point(size = 3)+
 geom_hline(yintercept = mean(t(dta_desc["標準差",])) + 
            c(-1.5, 0, 1.5) * sd(t(dta_desc["標準差",])), linetype = "dashed") +
 coord_flip() +
 labs(x = "題目", y = "標準差") 

#繪製所有題目偏態
ggplot(data = subset(dtal_desc, 動差 == "偏態"), 
       aes(x = reorder(題目, value, max), y = value, group = 動差)) +
 geom_point(size = 3)+
 geom_hline(yintercept = mean(t(dta_desc["偏態",])) + 
            c(-1.5, 0, 1.5) * sd(t(dta_desc["偏態",])), linetype = "dashed") +
 coord_flip() +
 labs(x = "題目", y = "偏態") 

 
#繪製所有題目峰度
ggplot(data = subset(dtal_desc, 動差 == "峰度"), 
       aes(x = reorder(題目, value, max), y = value, group = 動差)) +
 geom_point(size = 3)+
 geom_hline(yintercept = mean(t(dta_desc["峰度",])) + 
            c(-1.5, 0, 1.5) * sd(t(dta_desc["峰度",])), linetype = "dashed") +
 coord_flip() +
 labs(x = "題目", y = "峰度") 

#恢復配色主題
theme_set(old)


#計算區辨度。以總分為準，選取低分組與高分組，比較各題在兩組上的差異。
dta$tot <- apply(dta, 1, sum)
dta$grp <- NA
dta$grp[rank(dta$tot) < 500*.27] <- "L"
dta$grp[rank(dta$tot) > 500*.73] <- "H"
dta$grp <- factor(dta$grp)

#算高低分組平均數
dtam <- aggregate(dta[,1:35], by=list(dta$grp), mean)

#第一欄沒有用，刪掉
dtam <- t(dtam[,-1])

#t檢定
item_t <- sapply(dta[,1:35], function(x) t.test(x ~ dta$grp)$statistic)

#將計算結果存於新資料框架rslt2中
rslt2 <- data.frame(Item=rownames(dtam),m.l=dtam[,2], m.h=dtam[,1], m.dif=dtam[,1]-dtam[,2], t.stat=item_t)

#畫出t檢定結果
#圖6.3
ggplot(data = rslt2, aes(x=reorder(Item, t.stat, max), y=t.stat)) +
 geom_point() +
 geom_hline(yintercept = 2, linetype="dashed") +
 coord_flip() +
 labs(x = "題目", y = "t檢定值") +
 theme_bw()

#整理資料、命名欄位並四捨五入取至小數點後第3位
#程式報表6.4
rslt2 <- rslt2[,-1]
names(rslt2) <- c('低分組平均','高分組平均','差異','t檢定')
round(rslt2,3)



#利用psychometrics套件，計算題目與總分相關
require(psychometric)
itotr <- item.exam(dta[, 1:35], discrim = TRUE)

#將資料轉換為包含三個資料框架的列
ldta <- list(x = dta[, 1:15], y = dta[,16:25], z = dta[, 26:35])

#計算題目與分量表總分相關
isubr <- lapply(ldta, item.exam, discrim = TRUE)

#整理分析結果並存檔
rslt3 <- t(rbind(itotr$Item.Tot.woi, 
           c(isubr$x$Item.Tot.woi, isubr$y$Item.Tot.woi, isubr$z$Item.Tot.woi) ) )

#呈現第三部分
rslt3 <- data.frame(rslt3)
names(rslt3) <- c('題目總分相關','題目分量表相關')
row.names(rslt3) <- names(dta[,1:35])
round(rslt3, 3)

#題目信度
isubrel <- c(isubr$x$Item.Rel.woi, isubr$y$Item.Rel.woi, isubr$z$Item.Rel.woi)

#卸下psychometrics套件
detach("package:psychometric", unload = TRUE)

#載入psych，看總量表與分量表的 Cronbach alpha
require(psych)

#題目刪除後全量表信度變化
itotalpha <- alpha(dta[, 1:35])$alpha.drop[,'raw_alpha']

#題目刪除後分量表信度變化
isubalpha <- lapply(ldta, alpha)
ialphad <- c(isubalpha$x$alpha.drop[,'raw_alpha'],
             isubalpha$y$alpha.drop[,'raw_alpha'],
             isubalpha$z$alpha.drop[,'raw_alpha'])

#把分析結果集中
rslt4 <- as.data.frame(t(rbind(itotalpha, ialphad, isubrel)))
names(rslt4) <- c('總量表信度（刪題）', '分量表信度（刪題）', '題目信度')

#加上題目名字、顯示三位
#程式報表6.5
row.names(rslt4) <- names(dta[,1:35])
round(rslt4, 3)


#因素分析
#根據量表設計理念，因素數為 3
#程式報表6.6
require(psych)
print.psych(fa(dta[, 1:35], nfactor = 3, fm = "pa", rotate = "promax"), cut = .3)

#平行分析探索因素數
#圖6.4
fa.parallel(dta[, 1:35], fa = "pc", show.legend = FALSE)

#設定因素數是4，看看因素結構
#程式報表6.7
print.psych(fa(dta[, 1:35], nfactor = 4, fm = "pa", rotate = "promax"), cut = .3)


#選題後，以另一個樣本驗證
dta2 <- read.table("data/prenatal2.txt", header = TRUE)

#平行分析看看因素數
#圖6.5
fa.parallel(dta2, fa = "pc", show.legend = FALSE)

#設定因素數是3、因素分析結果
#程式報表6.8, 6.9
print.psych(fa(dta2, fm = "pa", nfactor = 3, rotate = "promax"), cut = .3)

#定義分量表
ldta2 <- list(x = dta2[, 1:3], y = dta2[, 4:11], z = dta2[, 12:16])

#分量表信度
lapply(ldta2, alpha)

#總量表信度
#程式報表6.10
require(psych)
omega(dta2, nfactor = 3)

