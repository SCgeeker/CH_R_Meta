## �bRstudio�ATools -> Global Options -> General -> Default Text Encoding -> UTF-8
## Opening with encoding -> BIG5
## �t�λy�t�n�]�w���x�W���餤��
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


#Ū�i���

dta <- read.table("data/prenatal1.txt", header = T)



#�Hstr�ݤ@�U��Ƶ��c�B�ݤ@�U�̫᤻�D�A�T�{�˥��ƬO500

#�{������6.1

str(dta)

tail(dta)



#�p��C�D��|��v�A500�O�˥���

show(apply(apply(dta, 2, is.na), 2, sum)/500)



#�ݬݦU�D�|�ӿﶵ���t

show(dta_tbl <- apply(dta, 2,  table)/500)



#���e�ϷǳơA���Jreshape2�A�N�����ƥѼe�δ�������
pkgTest("reshape2")
require(reshape2)

dtal_tbl <- melt(dta_tbl)



#�R�W���θ���ܶ��W

names(dtal_tbl) <- c("�ﶵ", "�D��", "��v")



#�s�y�D�ؽs��

dtal_tbl$�D�ؽs�� <- rep(1:35, rep(4,35))



#��ʿﶵ��ܡA�Q��plyr�M��
pkgTest("plyr")
require(plyr)

dtal_tbl$�ﶵ <- mapvalues(dtal_tbl$�ﶵ, from = c(1,2,3,4), to = c("�@�I�]��","�y�L�p��","���Ǧp��","���T�p��"))

dtal_tbl$�ﶵ <- ordered(dtal_tbl$�ﶵ,c("�@�I�]��","�y�L�p��","���Ǧp��","���T�p��"))



#�ݤ@�U���

#�{������6.2

head(dtal_tbl)



#�ǳƵe�ϡA���ðO���U�ثe�t��D�D
pkgTest("ggplot2")
require(ggplot2)

old <- theme_set(theme_bw())



#���q���@�G�e15�D

#��6.1

ggplot(data = subset(dtal_tbl, �D�ؽs�� < 16), aes(x = reorder(�D��, ��v, max), 

       y = ��v, ymin = 0.25, ymax = ��v, group = �D��)) +

 geom_pointrange() +

 geom_hline(yintercept = 0.25, linetype = "dotted") +

 facet_grid(. ~ �ﶵ) +

 coord_flip() +

 labs(x = "�D��", y = "�ﶵ��v", title = "��¾�{�P���q��") 

 

#���q���G�G16-25�D

ggplot(data = subset(dtal_tbl, �D�ؽs�� > 15 & �D�ؽs�� < 26),  aes(x = reorder(�D��, ��v, max), 

       y = ��v, ymin = 0.25, ymax = ��v, group = �D��)) +

 geom_pointrange() +

 geom_hline(yintercept = 0.25, linetype = "dotted") +

 facet_grid(. ~ �ﶵ) +

 coord_flip() +

 labs(x = "�D��", y = "�ﶵ��v", title = "���˱������q��") 

  

#���q���T�G26-35�D

ggplot(data = subset(dtal_tbl, �D�ؽs�� > 25),  aes(x = reorder(�D��, ��v, max), 

       y = ��v, ymin = 0.25, ymax = ��v, group = �D��)) +

 geom_pointrange() +

 geom_hline(yintercept = 0.25, linetype = "dotted") +

 facet_grid(. ~ �ﶵ) +

 coord_flip() +

 labs(x = "�D��", y = "�ﶵ��v", title = "���ͱ������q��") 



#�^�_�t��D�D

theme_set(old)



#�w�q�@�Ө�ơA�i�H�P��

#�p���D�ت������ơB�зǮt�B���A�P�p��

my_summary <- function(x) {

 pkgTest("moments");require(moments)

 funs <- c(mean, sd, skewness, kurtosis)

 sapply(funs, function(f) f(x, na.rm = TRUE))

}



#�@���⧹�Ҧ��D�ثe�|�Űʮt�A�æs�������

#�{������6.3

dta_desc <- apply(dta, 2, my_summary)

rownames(dta_desc) <- c("����", "�зǮt", "���A", "�p��")

rslt1 <- as.data.frame(t(dta_desc))

round(rslt1,3)



#�ǳƵe�ϡA�令���θ��

dtal_desc <- melt(dta_desc)

names(dtal_desc)[1:2] <- c("�ʮt", "�D��")

head(dtal_desc)



#���t��D�D�A�ç��ª��s�_��

old <- theme_set(theme_minimal())



#ø�s�Ҧ��D�إ�����

ggplot(data = subset(dtal_desc, �ʮt == "����"), 

       aes(x = reorder(�D��, value, max), y = value, group = �ʮt)) +

 geom_point(size = 3)+

 geom_hline(yintercept = mean(t(dta_desc["����",])) + 

            c(-1.5, 0, 1.5) * sd(t(dta_desc["����",])), linetype = "dashed") +

 coord_flip() +

 labs(x = "�D��", y = "����") 

 

#ø�s�Ҧ��D�ؼзǮt

#��6.2

ggplot(data = subset(dtal_desc, �ʮt == "�зǮt"), 

       aes(x = reorder(�D��, value, max), y = value, group = �ʮt)) +

 geom_point(size = 3)+

 geom_hline(yintercept = mean(t(dta_desc["�зǮt",])) + 

            c(-1.5, 0, 1.5) * sd(t(dta_desc["�зǮt",])), linetype = "dashed") +

 coord_flip() +

 labs(x = "�D��", y = "�зǮt") 



#ø�s�Ҧ��D�ذ��A

ggplot(data = subset(dtal_desc, �ʮt == "���A"), 

       aes(x = reorder(�D��, value, max), y = value, group = �ʮt)) +

 geom_point(size = 3)+

 geom_hline(yintercept = mean(t(dta_desc["���A",])) + 

            c(-1.5, 0, 1.5) * sd(t(dta_desc["���A",])), linetype = "dashed") +

 coord_flip() +

 labs(x = "�D��", y = "���A") 



 

#ø�s�Ҧ��D�خp��

ggplot(data = subset(dtal_desc, �ʮt == "�p��"), 

       aes(x = reorder(�D��, value, max), y = value, group = �ʮt)) +

 geom_point(size = 3)+

 geom_hline(yintercept = mean(t(dta_desc["�p��",])) + 

            c(-1.5, 0, 1.5) * sd(t(dta_desc["�p��",])), linetype = "dashed") +

 coord_flip() +

 labs(x = "�D��", y = "�p��") 



#��_�t��D�D

theme_set(old)





#�p��Ͽ�סC�H�`�����ǡA����C���ջP�����աA����U�D�b��դW���t���C

dta$tot <- apply(dta, 1, sum)

dta$grp <- NA

dta$grp[rank(dta$tot) < 500*.27] <- "L"

dta$grp[rank(dta$tot) > 500*.73] <- "H"

dta$grp <- factor(dta$grp)



#�Ⱚ�C���ե�����

dtam <- aggregate(dta[,1:35], by=list(dta$grp), mean)



#�Ĥ@��S���ΡA�R��

dtam <- t(dtam[,-1])



#t�˩w

item_t <- sapply(dta[,1:35], function(x) t.test(x ~ dta$grp)$statistic)



#�N�p�⵲�G�s��s��Ʈج[rslt2��

rslt2 <- data.frame(Item=rownames(dtam),m.l=dtam[,2], m.h=dtam[,1], m.dif=dtam[,1]-dtam[,2], t.stat=item_t)



#�e�Xt�˩w���G

#��6.3

ggplot(data = rslt2, aes(x=reorder(Item, t.stat, max), y=t.stat)) +

 geom_point() +

 geom_hline(yintercept = 2, linetype="dashed") +

 coord_flip() +

 labs(x = "�D��", y = "t�˩w��") +

 theme_bw()



#��z��ơB�R�W���å|�ˤ��J���ܤp���I���3��

#�{������6.4

rslt2 <- rslt2[,-1]

names(rslt2) <- c('�C���ե���','�����ե���','�t��','t�˩w')

round(rslt2,3)







#�Q��psychometrics�M��A�p���D�ػP�`������
pkgTest("psychometric")
require(psychometric)

itotr <- item.exam(dta[, 1:35], discrim = TRUE)



#�N����ഫ���]�t�T�Ӹ�Ʈج[���C

ldta <- list(x = dta[, 1:15], y = dta[,16:25], z = dta[, 26:35])



#�p���D�ػP���q���`������

isubr <- lapply(ldta, item.exam, discrim = TRUE)



#��z���R���G�æs��

rslt3 <- t(rbind(itotr$Item.Tot.woi, 

           c(isubr$x$Item.Tot.woi, isubr$y$Item.Tot.woi, isubr$z$Item.Tot.woi) ) )



#�e�{�ĤT����

rslt3 <- data.frame(rslt3)

names(rslt3) <- c('�D���`������','�D�ؤ��q������')

row.names(rslt3) <- names(dta[,1:35])

round(rslt3, 3)



#�D�ثH��

isubrel <- c(isubr$x$Item.Rel.woi, isubr$y$Item.Rel.woi, isubr$z$Item.Rel.woi)



#���Upsychometrics�M��

detach("package:psychometric", unload = TRUE)



#���Jpsych�A���`�q���P���q���� Cronbach alpha
pkgTest("psych")
require(psych)



#�D�اR������q���H���ܤ�

itotalpha <- alpha(dta[, 1:35])$alpha.drop[,'raw_alpha']



#�D�اR������q���H���ܤ�

isubalpha <- lapply(ldta, alpha)

ialphad <- c(isubalpha$x$alpha.drop[,'raw_alpha'],

             isubalpha$y$alpha.drop[,'raw_alpha'],

             isubalpha$z$alpha.drop[,'raw_alpha'])



#����R���G����

rslt4 <- as.data.frame(t(rbind(itotalpha, ialphad, isubrel)))

names(rslt4) <- c('�`�q���H�ס]�R�D�^', '���q���H�ס]�R�D�^', '�D�ثH��')



#�[�W�D�ئW�r�B��ܤT��

#�{������6.5

row.names(rslt4) <- names(dta[,1:35])

round(rslt4, 3)





#�]�����R

#�ھڶq���]�p�z���A�]���Ƭ� 3

#�{������6.6
pkgTest("psych")
require(psych)

print.psych(fa(dta[, 1:35], nfactor = 3, fm = "pa", rotate = "promax"), cut = .3)



#������R�����]����

#��6.4

fa.parallel(dta[, 1:35], fa = "pc", show.legend = FALSE)



#�]�w�]���ƬO4�A�ݬݦ]�����c

#�{������6.7

print.psych(fa(dta[, 1:35], nfactor = 4, fm = "pa", rotate = "promax"), cut = .3)





#���D��A�H�t�@�Ӽ˥�����

dta2 <- read.table("data/prenatal3.txt", header = TRUE) ## Corrected Filename 


#������R�ݬݦ]����

#��6.5

fa.parallel(dta2, fa = "pc", show.legend = FALSE)



#�]�w�]���ƬO3�B�]�����R���G

#�{������6.8, 6.9

print.psych(fa(dta2, fm = "pa", nfactor = 3, rotate = "promax"), cut = .3)



#�w�q���q��

ldta2 <- list(x = dta2[, 1:3], y = dta2[, 4:11], z = dta2[, 12:16])



#���q���H��

lapply(ldta2, alpha)



#�`�q���H��

#�{������6.10
pkgTest("psych")
require(psych)

omega(dta2, nfactor = 3)


