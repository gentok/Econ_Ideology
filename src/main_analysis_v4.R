#' ---
#' title: "本調査の分析"
#' author: "加藤言人"
#' date: "2019年12月26日"
#' output:
#'  pdf_document: 
#'   latex_engine: xelatex 
#' documentclass: bxjsarticle
#' classoption: xelatex,ja=standard
#' geometry: no
#' mainfont: Meiryo
#' monofont: Meiryo
#' ---

#+ echo=FALSE
# このドキュメントはCP932で書かれています。
# This document is encoded in CP932 (Shift-JIS)

#+ echo=FALSE
## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## Used Packages
# devtools::install_github("gentok/estvis")
library(estvis)
library(readr);library(knitr);library(ggplot2)
require(grid);library(gridExtra);library(haven)
require(sandwich); require(lmtest)

#+ message=FALSE
## Read Data
d <- readRDS("../data_public/main_data_v3.rds")
#d <- read_dta("../data_public/main_data_v1.dta", encoding="UTF-8")
nrow(d)

## Drop Respondents
d <- d[which(d$satisficer==0),] # Not Satisficers
d <- d[which(d$surveytime<=90),] # Took too long
nrow(d)

dtmp <- d[complete.cases(d[,c("knall","fem","age","lvlen","ownh",
  "edu3","wk","mar","cld")]),]
nrow(dtmp)

#'
#' # 争点イデオロギー指標の作成（因子分析）
#'

# Issue Ideology 
require(psych)
mydata <- d[,c("issue_1",
               "issue_2",
               "issue_3",
               "issue_4",
               "issue_5",
               "issue_6",
               "issue_7",
               "issue_8",
               "issue_9",
               "issue_10",
               "issue_11",
               "issue_12",
               "issue_13",
               "issue_14",
               "issue_15")]
mydata <- apply(mydata, 2, function(k){k[is.na(k)]<- 0; k})

# Scree-plot (Aiming at two vectors)
plot(eigen(cor(mydata))$values, type="b",
     ylab="Eigen Value",xlab="")

# with promax rotation (two factors)
fit <- fa(mydata, 2, fm="ml", 
          rotate="promax",
          scores="Bartlett")
print(fit, digits=3, cutoff=.1, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=colnames(mydata),cex=.7) # add variable names

require(ggrepel)
load <- as.data.frame(load)
load$vn <- c("自衛隊の拡充",
             "集団的自衛権の行使",
             "在日米軍の維持",
             "国防軍の組織",
             "憲法改正要件の緩和",
             "首相の公式靖国参拝",
             "財政出動の実施",
             "公共事業の実施",
             "TPPへの参加",
             "増税で社会福祉充実",
             "移民受け入れの推進",
             "外国人参政権の付与",
             "夫婦別姓の合法化",
             "同性婚の合法化",
             "原発の再稼働")

p <- 
  ggplot(load, aes(x=ML1,y=ML2)) + 
  geom_hline(yintercept=0,color="gray30",linetype=2) +
  geom_vline(xintercept=0,color="gray30",linetype=2) +
  geom_point() + 
  geom_text_repel(aes(label=vn)) + 
  labs(title="争点態度イデオロギーの因子分析（因子負荷量）",
       x="外交安全保障イデオロギー（第1因子）",
       y="権利機会平等イデオロギー（第2因子）",
       caption="\n※因子負荷量の推定にはプロマックス回転と最尤法を用いた．因子スコアはBartlett法を用いて算出した．") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.5),
        axis.text = element_text(size=10))
p

#+ eval=FALSE
ggsave("../out/v4_score_ide_iss.png", p, width=7, height=5.5)

#+
# Defense Ideology (supposedly)
d$ide_iss_1 <- fit$scores[,1]
hist(d$ide_iss_1)
# Welfare/Economic Ideology (supposedly)
d$ide_iss_2 <- -fit$scores[,2]
hist(d$ide_iss_2)

#'
#' # イデオロギー指標間の相関
#' 

require(xtable)

ctab <- cor(d[,c("ide_self","ide_psup","ide_iss_1","ide_iss_2")])
ctab[upper.tri(ctab)] <- NA
colnames(ctab) <- rownames(ctab) <- c("自己申告","政党支持","外交安全保障","権利機会平等")
print(xtable(ctab, digits=3,caption="イデオロギー指標間の相関"), 
      caption.placement="top")

#'
#' # 記述統計
#'
#'
#' ## 従属変数：金融緩和政策選好
#' 

tab <- table(d$easing_opi)/sum(table(d$easing_opi))
tab <- data.frame(prop = as.numeric(tab),
                  names = c("反対\n(-3)","-2","-1",
                            "どちらともいえない\n(0)",
                            "1","2","賛成\n(3)"))
tab$names <- factor(tab$names, levels=tab$names)

p <- ggplot(tab, aes(x=names,y=prop)) + 
  geom_bar(stat="identity") + 
  ylab("回答割合") + xlab(NULL) + 
  #ggtitle("金融緩和に対する意見の回答分布") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/v4_dvdist.png", p, width=7, height=5)

#'
#' ## 独立変数：イデオロギー
#' 
#' ### 自己申告イデオロギー
#'
tab <- table(d$ide_self)/sum(table(d$ide_self))
tab <- data.frame(prop = as.numeric(tab),
                  names = c("左派/\nリベラル\n(-3)","-2","-1",
                            "中立\n(0)","1","2","右派/\n保守\n(3)"))
tab$names <- factor(tab$names, levels=tab$names)

p1 <- ggplot(tab, aes(x=names,y=prop)) + 
  geom_bar(stat="identity") + 
  ylab(NULL) + xlab(NULL) + 
  ggtitle("自己申告\nイデオロギー\n（度数分布）") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p1

#' 
#' ### 争点態度イデオロギー
#' 

p2_1 <- ggplot(d, aes(x=ide_iss_1,y=..count../sum(..count..))) + 
  geom_histogram(bins=10,color="white") +
  ylab(NULL) + xlab(NULL) + 
  ggtitle("外交安全保障\nイデオロギー\n（ヒストグラム）") + 
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     limits=c(-3,3),
                     labels=c("左派\n(-3)\n","-2","-1","0","1","2","右派\n(3)\n")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p2_1

p2_2 <- ggplot(d, aes(x=ide_iss_2,y=..count../sum(..count..))) + 
  geom_histogram(bins=10,color="white") +
  ylab(NULL) + xlab(NULL) + 
  ggtitle("権利機会平等\nイデオロギー\n（ヒストグラム）") + 
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     limits=c(-3,3),
                     labels=c("左派\n(-3)\n","-2","-1","0","1","2","右派\n(3)\n")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p2_2

#'
#' ### 政党支持イデオロギー
#' 
tab <- table(d$ide_psup)/sum(table(d$ide_psup))
tab <- data.frame(prop = as.numeric(tab),
                  names = c("左派\n政党支持\n(-1)","無党派\nその他\n(0)",
                            "右派\n政党支持\n(1)"))
tab$names <- factor(tab$names, levels=tab$names)

p3 <- ggplot(tab, aes(x=names,y=prop)) + 
  geom_bar(stat="identity") + 
  ylab(NULL) + xlab(NULL) + 
  ggtitle("政党支持\nイデオロギー\n（度数分布）") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p3

#+
ggplot() + theme_void()
p <- arrangeGrob(p1,p3,p2_1,p2_2, nrow=1, left="回答割合")
grid.draw(p)

#+ eval=FALSE
ggsave("../out/v4_idedist123.png", p, width=9.5, height=4)

#'
#' ### 自己申告イデオロギーと争点態度イデオロギーの相関
#' 

p12cor_1 <- ggplot(d, aes(x=ide_self,y=ide_iss_1)) + 
  geom_jitter(alpha=0.6, width=0.2, height=0.2,size=2) + 
  ylab("外交安全保障イデオロギー") + 
  xlab("自己申告イデオロギー") + 
  annotate("text", x=-2.5, y=3.2, size=4.5,
           label=paste("r =",round(cor(d$ide_self,d$ide_iss_1),3))) + 
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     labels=c("左派/\nリベラル\n(-3)","-2","-1",
                              "中立\n(0)","1","2","右派/\n保守\n(3)")) + 
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     #limits=c(-3,3),
                     labels=c("左派(-3)","-2","-1","0","1","2","右派(3)")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=12, face="bold"))
p12cor_1

p12cor_2 <- ggplot(d, aes(x=ide_self,y=ide_iss_2)) + 
  geom_jitter(alpha=0.6, width=0.2, height=0.2,size=2) + 
  ylab("権利機会平等イデオロギー") + 
  xlab("自己申告イデオロギー") + 
  annotate("text", x=-2.5, y=3.5, size=4.5,
           label=paste("r =",round(cor(d$ide_self,d$ide_iss_2),3))) + 
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     labels=c("左派/\nリベラル\n(-3)","-2","-1",
                              "中立\n(0)","1","2","右派/\n保守\n(3)")) + 
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3),
                     #limits=c(-3,3),
                     labels=c("左派(-3)","-2","-1","0","1","2","右派(3)")) +
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.title = element_text(size=12, face="bold"),
        axis.text = element_text(size=12, face="bold"))
p12cor_2

#+
ggplot() + theme_void()
p12cor <- arrangeGrob(p12cor_1 + xlab(NULL) + ylab(NULL) + ggtitle("外交安全保障"),
                 p12cor_2 + xlab(NULL) + ylab(NULL) + ggtitle("権利機会平等"), 
                 nrow=1, left="争点態度イデオロギー",
                 bottom="自己申告イデオロギー")
grid.draw(p12cor)

#+ eval=FALSE, echo=FALSE
ggsave("../out/v4_idecor.png", p12cor, width=8, height=5.5)

#'
#' # 実験群比較
#'
#' ## 準備

# 統制変数
ctl <- formula( ~ .+ knall + fem + age + lvlen + ownh + 
                  as.factor(edu3) + wk + mar + cld)

# 予測値計算用データ
preddata <- data.frame(g_easing_N = seq(0,4,1))
preddata$knall = median(d$knall,na.rm=TRUE)
preddata$fem = median(d$fem, na.rm=TRUE)
preddata$age = median(d$age, na.rm=TRUE)
preddata$lvlen = median(d$lvlen, na.rm=TRUE)
preddata$ownh = median(d$ownh, na.rm=TRUE)
preddata$edu3 = median(as.numeric(d$edu3)-1, na.rm=TRUE)
preddata$wk = median(d$wk, na.rm=TRUE)
preddata$mar = median(d$mar, na.rm=TRUE)
preddata$cld = median(d$cld, na.rm=TRUE)[1]

# 変数名
vn <- c("（定数項）",
        "1.経済成長",
        "2.経済成長＆貧困削減",
        "3.経済成長＆学者賛成",
        "4.経済成長＆貧困＆学者",
        "政治知識","性別（女性）",
        "年齢","居住年数","持ち家",
        "教育：短大／高専／専門学校",
        "教育：大卒以上",
        "就労","婚姻","子ども")
vnx <- c(vn[1:5],"イデオロギー",vn[6:15],
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆学者",
         "イデオロギー×4.成長＆貧困＆学者",
         "イデオロギー",
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆学者",
         "イデオロギー×4.成長＆貧困＆学者",
         "イデオロギー",
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆学者",
         "イデオロギー×4.成長＆貧困＆学者")
vnx2 <- c(vn,vnx[c(6,17:30)])

#'
#' ## 自己申告イデオロギー条件付け
#'

## Main Models
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4"))*ide_self,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m1h3a <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4"))*ide_self,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m1h3b <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_self,ctl), data=d)
m1 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

## Marginal Effect
m1_mg <- simu_interact(m1,"ide_self",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                       vcov.est = "robust")
m1_mg90 <- simu_interact(m1,"ide_self",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                           level.ci = 0.90, vcov.est = "robust")

#'
#' ## 争点態度イデオロギー条件付け
#' 
#' ### 外交安全保障イデオロギー   
#' 

## Main Models
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4"))*ide_iss_1,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3a_1 <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4"))*ide_iss_1,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3b_1 <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_iss_1,ctl), data=d)
m2_1 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

## Marginal Effect
m2_1_mg <- simu_interact(m2_1,"ide_iss_1",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                         vcov.est = "robust")
m2_1_mg90 <- simu_interact(m2_1,"ide_iss_1",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                           level.ci = 0.90, vcov.est = "robust")

#'
#' ### 権利機会平等イデオロギー
#'

## Main Models
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4"))*ide_iss_2,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3a_2 <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4"))*ide_iss_2,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3b_2 <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_iss_2,ctl), data=d)
m2_2 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

## Marginal Effect
m2_2_mg <- simu_interact(m2_2,"ide_iss_2",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                         vcov.est = "robust")
m2_2_mg90 <- simu_interact(m2_2,"ide_iss_2",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                           level.ci = 0.90, vcov.est = "robust")

#'
#' ## 政党支持イデオロギー条件付け
#'

mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4"))*ide_psup,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m3h3a <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4"))*ide_psup,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m3h3b <- mx_easing
mx_easing <- lm(update(easing_opi ~ 
                         as.factor(g_easing_N)*ide_psup
                         # as.factor(g_easing_N)*left_psup +
                         # as.factor(g_easing_N)*right_psup
                         ,ctl), data=d)
m3 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

## Marginal Effect
m3_mg <- simu_interact(m3,"ide_psup",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                       vcov.est = "robust")
m3_mg90 <- simu_interact(m3,"ide_psup",moveprof=data.frame(g_easing_N=seq(0,4,1)),
                           level.ci = 0.90, vcov.est = "robust")

#'
#' ## 条件付け効果のプロット
#'

mgdt_m1 <- cbind(m1_mg$profile,m1_mg$predsum,m1_mg90$predsum[,c(4,5)],type="自己申告")
mgdt_m3 <- cbind(m1_mg$profile,m3_mg$predsum,m3_mg90$predsum[,c(4,5)],type="政党支持")
mgdt_m2_1 <- cbind(m1_mg$profile,m2_1_mg$predsum,m2_1_mg90$predsum[,c(4,5)],type="外交安全保障")
mgdt_m2_2 <- cbind(m1_mg$profile,m2_2_mg$predsum,m2_2_mg90$predsum[,c(4,5)],type="権利機会平等")

mgdt <- rbind(mgdt_m1,mgdt_m3,mgdt_m2_1,mgdt_m2_2)
names(mgdt)[1:2] <- c("tr","ide")
names(mgdt)[grep("CI",names(mgdt))] <- c("lci95","uci95","lci90","uci90")
# mgdt$tr <- c("統制群",
#              "1.経済成長",
#              "2.経済成長&貧困削減",
#              "3.経済成長&学者賛成",
#              "4.経済成長&貧困削減&学者賛成")
mgdt$tr <- factor(mgdt$tr, levels=unique(mgdt$tr))
mgdt$type <- factor(mgdt$type, levels=unique(mgdt$type))

mgdt$ptest <- ifelse(mgdt$lci95*mgdt$uci95>0,"p<.05",
                      ifelse(mgdt$lci90*mgdt$uci90>0,"p<.10","n.s.(p>=.10)"))
mgdt$ptest <- factor(mgdt$ptest, levels=c("p<.05","p<.10","n.s.(p>=.10)"))

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(mgdt, aes(x=tr, y=Mean)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_point(aes(color=ptest, shape=ptest), size=2) + 
  geom_errorbar(aes(ymin=lci95, ymax=uci95, color=ptest), width=0.3) + 
  geom_errorbar(aes(ymin=lci90, ymax=uci90, color=ptest), width=0, size=1) + 
  #coord_flip() + 
  facet_grid(~type) +
  scale_color_manual(name="", values=c("red2","darkorange2","gray50")) + 
  scale_shape_discrete(name="") + 
  # ggtitle("金融緩和実験 \n(政党支持イデオロギーによる条件付け)") +
  #ggtitle("政党支持イデオロギーと金融緩和選好") +
  ylab("イデオロギーの条件付き係数＋９５％信頼区間\n（太線は９０％信頼区間）") + 
  xlab("実験群\n\n0:統制群; 1.経済成長; 2.経済成長&貧困削減; 3.経済成長&学者賛成; 4.経済成長&貧困削減&学者賛成") + 
  labs(subtitle="イデオロギー指標",
       caption="分析結果の詳細については表１を参照．") + 
  theme_bw() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,-0), "cm"),
        panel.grid = element_line(color=NA),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.x = element_text(color="black", size=11),
        axis.text.y = element_text(color="black"),
        legend.position = "bottom")
p

#+ eval=FALSE
ggsave("../out/v4_expres.png", p, width=8, height=5)

#'
#' ## 図２：仮説検証用プロット
#'

# H1
h1cf <- rbind(coeftest(m1, vcovHC(m1, "HC1"))[17,],
              coeftest(m3, vcovHC(m3, "HC1"))[17,],
              coeftest(m2_1, vcovHC(m2_1, "HC1"))[17,],
              coeftest(m2_2, vcovHC(m2_2, "HC1"))[17,])

# H2
h2cf <- rbind(coeftest(m1h3a, vcovHC(m1h3a, "HC1"))[18,],
               coeftest(m3h3a, vcovHC(m3h3a, "HC1"))[18,],
               coeftest(m2h3a_1, vcovHC(m2h3a_1, "HC1"))[18,],
               coeftest(m2h3a_2, vcovHC(m2h3a_2, "HC1"))[18,])


# H3A
h3acf <- rbind(coeftest(m1h3a, vcovHC(m1h3a, "HC1"))[19,],
               coeftest(m3h3a, vcovHC(m3h3a, "HC1"))[19,],
               coeftest(m2h3a_1, vcovHC(m2h3a_1, "HC1"))[19,],
               coeftest(m2h3a_2, vcovHC(m2h3a_2, "HC1"))[19,])

# H3B
h3bcf <- rbind(coeftest(m1h3b, vcovHC(m1h3b, "HC1"))[20,],
               coeftest(m3h3b, vcovHC(m3h3b, "HC1"))[20,],
               coeftest(m2h3b_1, vcovHC(m2h3b_1, "HC1"))[20,],
               coeftest(m2h3b_2, vcovHC(m2h3b_2, "HC1"))[20,])

htest <- as.data.frame(rbind(h1cf,h2cf,h3acf,h3bcf))
names(htest) <- c("est","se","tval","pval")
htest$hyp <- rep(c("H1:1.経済成長\nv.s.統制群",
                   "H2:2.経済成長＆貧困削減\nv.s.1.経済成長",
                   "H3:3.経済成長＆学者賛成\nv.s.1.経済成長",
                   "H3:4.成長＆貧困＆学者\nv.s.2.経済成長＆貧困削減"),
                 each=4)
htest$hyp <- factor(htest$hyp, levels=rev(unique(htest$hyp)))
htest$ms <- rep(c("自己申告","政党支持","外交安全保障","権利機会平等"),4)
htest$ms <- factor(htest$ms, levels=unique(htest$ms))

htest$l90CI <- htest$est - htest$se*qnorm(0.95)
htest$u90CI <- htest$est + htest$se*qnorm(0.95)
htest$l95CI <- htest$est - htest$se*qnorm(0.975)
htest$u95CI <- htest$est + htest$se*qnorm(0.975)

htest$ptest <- ifelse(htest$pval<0.05,"p<.05",
                      ifelse(htest$pval<0.10,"p<.10","n.s.(p>=.10)"))
htest$ptest <- factor(htest$ptest, levels=c("p<.05","p<.10","n.s.(p>=.10)"))

p <- ggplot(htest, aes(x=hyp)) + 
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=l95CI,ymax=u95CI,
                    color=ptest),width=0.1) + 
  geom_errorbar(aes(ymin=l90CI,ymax=u90CI,
                    color=ptest),width=0,size=0.8) + 
  geom_point(aes(y=est,color=ptest,shape=ptest),size=2) + 
  scale_y_continuous(breaks=c(-0.3,0,0.3)) + 
  scale_color_manual(name="", values=c("red2","darkorange2","gray50")) + 
  scale_shape_discrete(name="") + 
  facet_grid(.~ms) +
  xlab(NULL) + ylab("イデオロギー交差項の係数＋９５％信頼区間\n（太線は９０％信頼区間）") + 
  labs(subtitle="イデオロギー指標",
       caption="分析結果の詳細については表１を参照．") + 
  coord_flip() + theme_bw() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,-0), "cm"),
        panel.grid = element_line(color=NA),
        plot.subtitle = element_text(hjust=0.5),
        axis.text.y = element_text(color="black"),
        legend.position = "bottom")
p

#+ eval=FALSE
ggsave("../out/v4_expres_htest.png", p, width=8, height=5)


#'
#' ## 表のエクスポート
#'

table_coef(list(m1,m3,m2_1,m2_2), vcov.est="robust", 
           single.row=FALSE, 
           custom.variable.names = c(vnx,vnx[26:30]),
           order.variable = c(1:6,17:35,7:16),
           m.names = c("自己申告","政党支持","外交安全保障","権利機会平等"),
           caption="イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）",
           label="idetab", dcolumn = TRUE,
           custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
           format = "tex", file.name = "../out/v4_idetab")

table_coef(list(m1,m3,m2_1,m2_2), vcov.est="robust", 
           single.row=FALSE, 
           drop.intercept = TRUE,
           drop.variable.names = c("knall","fem","age","lvlen","ownh",
                                    "as.factor(edu3)1","as.factor(edu3)2",
                                    "wk","mar","cld"),
           custom.variable.names = c(vnx[-c(1,7:16)],vnx[26:30]),
           m.names = c("自己申告","政党支持","外交安全保障","権利機会平等"),
           caption="イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）",
           label="idetab", dcolumn = TRUE,
           custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．定数項・統制変数の係数はオンライン付録参照．",
           format = "tex", file.name = "../out/v4_idetab_short")


#+ eval=FALSE, echo=FALSE
# Exporting HTML & PDF File
# In R Studio
# rmarkdown::render('main_analysis_v4.R', 'pdf_document', encoding = 'CP932')
# rmarkdown::render('main_analysis_v4.R', rmarkdown::github_document(toc=TRUE),
#                   clean=FALSE, encoding = 'CP932')
# tmp <- list.files(getwd())
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(getwd(),"/",tmp[i]))
# In Terminal, run:
# Rscript -e "rmarkdown::render('main_analysis.R', 'html_document', encoding = 'CP932')"
