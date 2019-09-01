#' ---
#' title: "本調査の分析"
#' author: "加藤言人"
#' date: "2019年8月31日"
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
d <- readRDS("../data_public/main_data_v1.rds")
#d <- read_dta("../data_public/main_data_v1.dta", encoding="UTF-8")
nrow(d)

## Drop Respondents (Currently NULL)
d <- d[which(d$satisficer==0),] # Not Satisficers
d <- d[which(d$surveytime<=90),] # Took too long
#d <- d[which(d$check_fail==0),] # Not Failing Manipulation Check
nrow(d)

dtmp <- d[complete.cases(d[,c("knall","fem","age","lvlen","ownh",
  "edu3","wk","mar","cld")]),]
nrow(dtmp)

table(dtmp$g_easing)

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
ggsave("../out/dvdist.png", p, width=7, height=5)

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
p <- arrangeGrob(p1,p3, nrow=1, left="回答割合")
grid.draw(p)

#+ eval=FALSE
ggsave("../out/idedist13.png", p, width=8, height=4.5)

#+
ggplot() + theme_void()
p <- arrangeGrob(p1,p3,p2_1,p2_2, nrow=1, left="回答割合")
grid.draw(p)

#+ eval=FALSE
ggsave("../out/idedist123.png", p, width=9.5, height=4)

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
ggsave("../out/idecor.png", p12cor, width=8, height=5.5)

#'
#' # 実験群比較
#'
#' ## 準備

# 統制変数
ctl <- formula( ~ .+ knall + fem + age + lvlen + ownh + 
                  as.factor(edu3) + wk + mar + cld)

# 予測値計算用データ
preddata <- data.frame(g_easing_N = seq(0,5,1))
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
        "3.経済成長＆格差縮小", 
        "4.経済成長＆学者賛成",
        "5.経済成長＆貧困削減＆学者賛成",
        "政治知識","性別（女性）",
        "年齢","居住年数","持ち家",
        "教育：短大／高専／専門学校",
        "教育：大卒以上",
        "就労","婚姻","子ども")
vnx <- c(vn[1:6],"イデオロギー",vn[7:16],
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆格差",
         "イデオロギー×4.成長＆学者",
         "イデオロギー×5.成長＆貧困＆学者",
         "イデオロギー",
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆格差",
         "イデオロギー×4.成長＆学者",
         "イデオロギー×5.成長＆貧困＆学者",
         "イデオロギー",
         "イデオロギー×1.成長",
         "イデオロギー×2.成長＆貧困",
         "イデオロギー×3.成長＆格差",
         "イデオロギー×4.成長＆学者",
         "イデオロギー×5.成長＆貧困＆学者")
vnx2 <- c(vn,vnx[c(7,18:34)])


#'
#' ## 実験刺激の直接効果
#'

#+ echo=FALSE
m_easing <- lm(update(easing_opi ~ as.factor(g_easing_N),ctl), data=d)
m0 <- m_easing

table_coef(list(m0), vcov.est="robust", 
           single.row=TRUE, custom.variable.names = vn,
           m.names = "基本モデル", dcolumn = TRUE,
           caption="実験情報刺激が金融緩和選好に与える効果（重回帰分析）",
           custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
           label="basetab",
           format = "tex", file.name = "../out/basetab")

coeftest(m_easing, vcov.=vcovHC(m_easing,"HC1"))

pr <- simu_pred(m_easing, profile=preddata, vcov.est = "robust")

pr_easing <- data.frame(t = names(table(d$g_easing)),
                        t_jp = c("統制群",
                                 "経済成長",
                                 "経済成長&\n貧困削減",
                                 "経済成長&\n格差縮小", 
                                 "経済成長&\n学者賛成",
                                 "経済成長&\n貧困削減&\n学者賛成"),
                        pr = pr$predsum$Mean,
                        loCI = pr$predsum$lowerCI,
                        upCI = pr$predsum$upperCI)
pr_easing$t_jp <- factor(pr_easing$t_jp, levels=pr_easing$t_jp)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(pr_easing, aes(x=t_jp,y=pr)) + 
  geom_point(size=2) + 
  geom_errorbar(aes(ymin=loCI, ymax=upCI), width=0.2) +
  ggtitle("金融緩和実験") +
  ylab("平均値(95%信頼区間)") + xlab(NULL) +　
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/expres.png", p, width=8, height=5)

#'
#' ## 自己申告イデオロギー条件付け
#'

mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4","5"))*ide_self,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m1h3a <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4","5"))*ide_self,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m1h3b <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_self,ctl), data=d)
m1 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

preddatax <- rbind(preddata,preddata,preddata)
preddatax$ide_self <- rep(c(-2,0,2), each=6)
pr <- simu_pred(mx_easing, profile=preddatax, vcov.est = "robust")

prx_easing <- data.frame(t = factor(rep(pr_easing$t_jp,3),levels=pr_easing$t_jp),
                         Ideology = factor(rep(c("左派(-2)","中立(0)","右派(2)"), each=6),
                                           levels=c("左派(-2)","中立(0)","右派(2)")),
                         pr = pr$predsum$Mean,
                         loCI = pr$predsum$lowerCI,
                         upCI = pr$predsum$upperCI)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(prx_easing, aes(x=t, y=pr)) + 
  geom_point(aes(shape=Ideology), position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=loCI, ymax=upCI, linetype=Ideology), 
                position = position_dodge(width = 0.5), width=0.2) + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") + 
  # ggtitle("金融緩和実験 \n(自己申告イデオロギーによる条件付け)") +
  ggtitle("自己申告イデオロギーと金融緩和選好") +
  ylab("平均値(95%信頼区間)") + xlab(NULL) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/expres_self.png", p, width=8, height=5)

#'
#' ## 争点態度イデオロギー条件付け
#'

mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4","5"))*ide_iss_1,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3a_1 <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4","5"))*ide_iss_1,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3b_1 <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_iss_1,ctl), data=d)
m2_1 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

preddatax <- rbind(preddata,preddata,preddata)
preddatax$ide_iss_1 <- rep(c(-2,0,2), each=6)
pr <- simu_pred(mx_easing, profile=preddatax, vcov.est = "robust")

prx_easing <- data.frame(t = factor(rep(pr_easing$t_jp,3),levels=pr_easing$t_jp),
                         Ideology = factor(rep(c("左派(-2)","中立(0)","右派(2)"), each=6),
                                           levels=c("左派(-2)","中立(0)","右派(2)")),
                         pr = pr$predsum$Mean,
                         loCI = pr$predsum$lowerCI,
                         upCI = pr$predsum$upperCI)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(prx_easing, aes(x=t, y=pr)) + 
  geom_point(aes(shape=Ideology), position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=loCI, ymax=upCI, linetype=Ideology), 
                position = position_dodge(width = 0.5), width=0.2) + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") + 
  # ggtitle("金融緩和実験 \n(争点態度イデオロギーによる条件付け)") +
  ggtitle("外交安全保障イデオロギーと金融緩和選好") +
  ylab("平均値(95%信頼区間)") + xlab(NULL) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/expres_iss_1.png", p, width=8, height=5)

#+
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4","5"))*ide_iss_2,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3a_2 <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4","5"))*ide_iss_2,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m2h3b_2 <- mx_easing
mx_easing <- lm(update(easing_opi ~ as.factor(g_easing_N)*ide_iss_2,ctl), data=d)
m2_2 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

preddatax <- rbind(preddata,preddata,preddata)
preddatax$ide_iss_2 <- rep(c(-2,0,2), each=6)
pr <- simu_pred(mx_easing, profile=preddatax, vcov.est = "robust")

prx_easing <- data.frame(t = factor(rep(pr_easing$t_jp,3),levels=pr_easing$t_jp),
                         Ideology = factor(rep(c("左派(-2)","中立(0)","右派(2)"), each=6),
                                           levels=c("左派(-2)","中立(0)","右派(2)")),
                         pr = pr$predsum$Mean,
                         loCI = pr$predsum$lowerCI,
                         upCI = pr$predsum$upperCI)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(prx_easing, aes(x=t, y=pr)) + 
  geom_point(aes(shape=Ideology), position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=loCI, ymax=upCI, linetype=Ideology), 
                position = position_dodge(width = 0.5), width=0.2) + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") + 
  # ggtitle("金融緩和実験 \n(争点態度イデオロギーによる条件付け)") +
  ggtitle("権利機会平等イデオロギーと金融緩和選好") +
  ylab("平均値(95%信頼区間)") + xlab(NULL) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/expres_iss_2.png", p, width=8, height=5)

#'
#' ## 政党支持イデオロギー条件付け
#'

mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("1","0","2","3","4","5"))*ide_psup,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m3h3a <- mx_easing
mx_easing <- lm(update(easing_opi ~ factor(g_easing_N,levels=c("2","0","1","3","4","5"))*ide_psup,ctl), data=d)
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))
m3h3b <- mx_easing
mx_easing <- lm(update(easing_opi ~ 
                         as.factor(g_easing_N)*ide_psup
                         # as.factor(g_easing_N)*left_psup +
                         # as.factor(g_easing_N)*right_psup
                         ,ctl), data=d)
m3 <- mx_easing
coeftest(mx_easing, vcovHC(mx_easing, "HC1"))

preddatax <- rbind(preddata,preddata,preddata)
preddatax$ide_psup <- rep(c(-1,0,1), each=6)
preddatax$left_psup = rep(c(1,0,0), each=6)
preddatax$right_psup = rep(c(0,0,1), each=6)
pr <- simu_pred(mx_easing, profile=preddatax, vcov.est = "robust")

prx_easing <- data.frame(t = factor(rep(pr_easing$t_jp,3),levels=pr_easing$t_jp),
                         Ideology = factor(rep(c("左派政党支持(-1)",
                                                 "無党派(0)",
                                                 "右派政党支持(1)"), each=6),
                                           levels=c("左派政党支持(-1)",
                                                    "無党派(0)",
                                                    "右派政党支持(1)")),
                         pr = pr$predsum$Mean,
                         loCI = pr$predsum$lowerCI,
                         upCI = pr$predsum$upperCI)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=FALSE
p <- ggplot(prx_easing, aes(x=t, y=pr)) + 
  geom_point(aes(shape=Ideology), position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin=loCI, ymax=upCI, linetype=Ideology), 
                position = position_dodge(width = 0.5), width=0.2) + 
  scale_linetype_discrete(name="") + 
  scale_shape_discrete(name="") + 
  # ggtitle("金融緩和実験 \n(政党支持イデオロギーによる条件付け)") +
  ggtitle("政党支持イデオロギーと金融緩和選好") +
  ylab("平均値(95%信頼区間)") + xlab(NULL) + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size=12, face="bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12, face="bold"))
p

#+ eval=FALSE
ggsave("../out/expres_psup.png", p, width=8, height=5)

#'
#' ## 図２：仮説検証用プロット
#'

# H1
h1cf <- rbind(coeftest(m1, vcovHC(m1, "HC1"))[18,],
              coeftest(m3, vcovHC(m3, "HC1"))[18,],
              coeftest(m2_1, vcovHC(m2_1, "HC1"))[18,],
              coeftest(m2_2, vcovHC(m2_2, "HC1"))[18,])

# H2A
h2acf <- rbind(coeftest(m1h3a, vcovHC(m1h3a, "HC1"))[19,],
               coeftest(m3h3a, vcovHC(m3h3a, "HC1"))[19,],
               coeftest(m2h3a_1, vcovHC(m2h3a_1, "HC1"))[19,],
               coeftest(m2h3a_2, vcovHC(m2h3a_2, "HC1"))[19,])

# H2B
h2bcf <- rbind(coeftest(m1h3a, vcovHC(m1h3a, "HC1"))[20,],
               coeftest(m3h3a, vcovHC(m3h3a, "HC1"))[20,],
               coeftest(m2h3a_1, vcovHC(m2h3a_1, "HC1"))[20,],
               coeftest(m2h3a_2, vcovHC(m2h3a_2, "HC1"))[20,])

# H3A
h3acf <- rbind(coeftest(m1h3a, vcovHC(m1h3a, "HC1"))[21,],
               coeftest(m3h3a, vcovHC(m3h3a, "HC1"))[21,],
               coeftest(m2h3a_1, vcovHC(m2h3a_1, "HC1"))[21,],
               coeftest(m2h3a_2, vcovHC(m2h3a_2, "HC1"))[21,])

# H3B
h3bcf <- rbind(coeftest(m1h3b, vcovHC(m1h3b, "HC1"))[22,],
               coeftest(m3h3b, vcovHC(m3h3b, "HC1"))[22,],
               coeftest(m2h3b_1, vcovHC(m2h3b_1, "HC1"))[22,],
               coeftest(m2h3b_2, vcovHC(m2h3b_2, "HC1"))[22,])

htest <- as.data.frame(rbind(h1cf,h2acf,h2bcf,h3acf,h3bcf))
names(htest) <- c("est","se","tval","pval")
htest$hyp <- rep(c("H1:1.経済成長\nv.s.統制群",
                   "H2A:2.経済成長＆貧困削減\nv.s.1.経済成長",
                   "H2B:3.経済成長＆格差縮小\nv.s.1.経済成長",
                   "H3:4.経済成長＆学者賛成\nv.s.1.経済成長",
                   "H3:5.経済成長＆貧困削減＆学者賛成\nv.s.2.経済成長＆貧困削減"),
                 each=4)
htest$hyp <- factor(htest$hyp, levels=rev(unique(htest$hyp)))
htest$ms <- rep(c("自己申告","政党支持","外交安全保障","権利機会平等"),5)
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
ggsave("../out/expres_htest.png", p, width=8, height=5)


#'
#' ## 表のエクスポート
#'

# table_coef(list(m1,m3), vcov.est="robust", 
#            single.row=TRUE, custom.variable.names = vnx[1:28],
#            m.names = c("自己申告","政党支持"),
#            caption="自己申告・政党支持イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）",
#            label="idetab", dcolumn = TRUE,
#            custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
#            order.variable = c(1:7,18:28,8:17),
#            format = "tex", file.name = "../out/idetab_13")
# 
# table_coef(list(m2_1,m2_2), vcov.est="robust", 
#            single.row=TRUE, custom.variable.names = vnx[1:28],
#            m.names = c("外交安全保障","権利機会平等"),
#            caption="争点態度イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）",
#            label="idetab_2", dcolumn = TRUE,
#            custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
#            order.variable = c(1:7,18:28,8:17),
#            format = "tex", file.name = "../out/idetab_2")

table_coef(list(m1,m3,m2_1,m2_2), vcov.est="robust", 
           single.row=FALSE, custom.variable.names = c(vnx,vnx[29:34]),
           m.names = c("自己申告","政党支持","外交安全保障","権利機会平等"),
           caption="イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）",
           label="idetab", dcolumn = TRUE,
           custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
           order.variable = c(1:7,18:40,8:17),
           format = "tex", file.name = "../out/idetab")

# tmpvn <- unique(c(names(coef(m1h3a)),names(coef(m3h3a)),names(coef(m2h3a_1)),names(coef(m2h3a_2))))
# table_coef(list(m1h3a,m3h3a,m2h3a_1,m2h3a_2), vcov.est="robust", 
#            single.row=FALSE, 
#            custom.variable.names = rep("イデオロギーX成長＆学者",4),
#            drop.variable.names = tmpvn[-c(21,27,33,39)],
#            m.names = c("自己申告","政党支持","外交安全保障","権利機会平等"),
#            caption="イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）。仮説３の検証として、実験群４と１を比較。",
#            label="idetab_h3a", dcolumn = TRUE,
#            custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
#            format = "tex", file.name = "../out/idetab_h3a")
# 
# tmpvn <- unique(c(names(coef(m1h3b)),names(coef(m3h3b)),names(coef(m2h3b_1)),names(coef(m2h3b_2))))
# table_coef(list(m1h3b,m3h3b,m2h3b_1,m2h3b_2), vcov.est="robust", 
#            single.row=FALSE, 
#            custom.variable.names = rep("イデオロギーX成長＆貧困＆学者",4),
#            drop.variable.names = tmpvn[-c(22,28,34,40)],
#            m.names = c("自己申告","政党支持","外交安全保障","権利機会平等"),
#            caption="イデオロギーと金融緩和選好の関係に実験情報刺激が与える効果（重回帰分析）。仮説３の検証として、実験群５と２を比較。",
#            label="idetab_h3b", dcolumn = TRUE,
#            custom.footnote = "最小二乗法による重回帰分析、ロバスト標準誤差使用．",
#            format = "tex", file.name = "../out/idetab_h3b")

#+ eval=FALSE, echo=FALSE
# Exporting HTML & PDF File
# In R Studio
# rmarkdown::render('main_analysis.R', 'pdf_document', encoding = 'CP932')
# rmarkdown::render('main_analysis.R', rmarkdown::github_document(toc=TRUE),
#                   clean=FALSE, encoding = 'CP932')
# tmp <- list.files(getwd())
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(getwd(),"/",tmp[i]))
# In Terminal, run:
# Rscript -e "rmarkdown::render('main_analysis.R', 'html_document', encoding = 'CP932')"
