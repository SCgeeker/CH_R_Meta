#Ū�i���
dta <- read.table("data/TMTmeta.txt", header=T)

#��ܫe�������
#�{������10.1
head(dta)

#���J RISmed �˯���Ʈw
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
#��10.1
barplot(tally, las=2, main="PubMed �d�� TMT ���m��")


#�ݬݮĪG�q���y�z�ʲέp
summary(dta$ES)

#�ĪG�q�����
#��10.2
library(lattice)
histogram(~ES, xlab="��s�ұo�ĪG�]�����^",ylab="�۹��W�v",xlim=c(-1.0, 1.1), data=dta)

#���J��X���R�n�� metafor
require(metafor)

#�Q�ά����P�˥��ƭp�����������ܲ��A�é�i������
dta$vi <- escalc(measure="COR", ri=dta$ES, ni=dta$N)$vi

#���277������31���A�̰ϰ�P ES �ƧǡAø�s�˪L�ϡA�Хܰϰ�
set.seed(201408)
dta31 <- dta[sample(277,31),]
dta31 <- dta31[with(dta31, order(Region, ES)), ]
#��10.3
forest(dta31$ES, dta31$vi, slab = dta31$Region)


#�p��Ҧ���s�������ĪG
#�{������10.2
summary(rslt <- rma(yi=ES, vi=vi, data=dta))

#�ˬd�X�����~�A��ø�s funnelplot
#��10.4
funnel(rslt)

#�{������10.3
ranktest(rslt)
regtest(rslt)

#��X�ո`���R
#���ư��ϰ줣���P�ĥ|�լ�s�s����
dta <- subset(dta,  subset=(Region != 99 & Group < 4))
dta$Region <- factor(dta$Region)
dta$Group <- factor(dta$Group)

#�ˬd�ϰ�ĪG�C���ư��ϰ줣���P�ĥ|�լ�s�s����
#�{������10.4
summary(rsltReg <- rma(yi=ES, vi=vi, mods= ~ Region, data=dta))                       

#�C�թ��10���A�� Group �P ES �ƧǡAø�s�˪L�ϡA�H Group �Х�
set.seed(2014)
dta1 <- dta[dta$Group==1,][sample(145,10),]
dta2 <- dta[dta$Group==2,][sample(36,10),]
dta3 <- dta[dta$Group==3,][sample(93,10),]
dta30 <- rbind(dta1,dta2,dta3)
dta30 <- dta30[with(dta30, order(Group, ES)), ]
#��10.5
forest(dta30$ES, dta30$vi,annotate=F,slab = dta30$Group)

#�ˬd��s�s��ĪG
summary(rsltGrp <- rma(yi=ES, vi=vi, mods= ~ Group, data=dta))

#�P������ϰ�P��s�s��ĪG
#�{������10.5
summary(rsltGrpReg <- rma(yi=ES, vi=vi, mods= ~ Group+Region, data=dta))
