#' ---
#' title: "東大朝日調査を用いた金融緩和選好とイデオロギーの関係に関する分析"
#' author: "加藤言人"
#' date: "2020年5月21日"
#' ---
#' 
#' # データの読み込み  
#' 

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## データの読み込み
dloc <- "http://www.masaki.j.u-tokyo.ac.jp/utas/2014_2016UTASV20161004.csv"
d <- read.csv(dloc, fileEncoding = "SHIFT-JIS")

#'
#' # 変数の作成
#'

## イデオロギー
d$ide <- d$W2Q21
d$ide[d$W2Q21==99] <- 5
d$ide <- d$ide - 5
table(d$ide)

## 金融緩和
d$easing <- d$W2Q15
d$easing[d$W2Q15==99] <- 3
d$easing <- 6 - d$easing
table(d$easing)

## 安倍内閣支持
d$abesup <- d$W2Q12
d$abesup[d$W2Q12==99] <- 3
d$abesup <- 6 - d$abesup
table(d$abesup)

d$abesup5 <- NA
d$abesup5[d$abesup==1] <- "よくやっている\nとは思わない"
d$abesup5[d$abesup==2] <- "どちらかと言えば\nよくやっている\nとは思わない"
d$abesup5[d$abesup==3] <- "どちらとも\n言えない\n／無回答"
d$abesup5[d$abesup==4] <- "どちらかと言えば\nよくやっている\nと思う"
d$abesup5[d$abesup==5] <- "よくやっている\nと思う"
d$abesup5 <- factor(d$abesup5, levels=c("よくやっている\nとは思わない",
                                        "どちらかと言えば\nよくやっている\nとは思わない",
                                        "どちらとも\n言えない\n／無回答",
                                        "どちらかと言えば\nよくやっている\nと思う",
                                        "よくやっている\nと思う"))

# Demographies
# 性別(女性)
d$fem <- ifelse(d$W1F1==2, 1, ifelse(d$W1F1==3,NA,0))
table(d$fem)
# 年齢
d$age_20s <- ifelse(d$W1F2==1, 1, ifelse(d$W1F2==99,NA,0))
d$age_30s <- ifelse(d$W1F2==2, 1, ifelse(d$W1F2==99,NA,0))
d$age_40s <- ifelse(d$W1F2==3, 1, ifelse(d$W1F2==99,NA,0))
d$age_50s <- ifelse(d$W1F2==4, 1, ifelse(d$W1F2==99,NA,0))
d$age_60s <- ifelse(d$W1F2==5, 1, ifelse(d$W1F2==99,NA,0))
# d$age_70s <- ifelse(d$W1F2==6, 1, ifelse(d$W1F2==99,NA,0))
# 学歴
table(d$W1F3FA)
d$W1F3[which(d$W1F3FA%in%c("尋常高等小学校","高等小学校",
                           "小学校もまともに行けなかった"))] <- 1
d$W1F3[which(d$W1F3FA%in%c("旧女学校","旧制女学校",
                           "S23年・04年制の高等女学校卆",
                           "実浅女学校","女学校","女子高",
                           "女子高","専門学校中退",
                           "大学中退","転勤のため大学中退"))] <- 2
d$W1F3[which(d$W1F3FA=="高専")] <- 4
#d$edu_el_hi <- ifelse(d$W1F3%in%c(1,2), 1, ifelse(d$W1F3%in%c(7,99),NA,0))
d$edu_vo_ju <- ifelse(d$W1F3%in%c(3,4), 1, ifelse(d$W1F3%in%c(7,99),NA,0))
d$edu_un_gr <- ifelse(d$W1F3%in%c(5,6), 1, ifelse(d$W1F3%in%c(7,99),NA,0))
# table(d$edu_el_hi)
table(d$edu_vo_ju)
table(d$edu_un_gr)

#'
#' # 安倍首相支持を統制した場合の重回帰分析(表A1)
#'

# devtools::install_github("gentok/estvis")
library(estvis)

m1 <- lm(easing ~ ide + abesup, data=d)
summary(m1)

m2 <- lm(easing ~ ide + abesup + fem + 
           age_20s + age_30s + age_40s + age_50s + age_60s + 
           edu_vo_ju + edu_un_gr, data=d)
summary(m2)

table_coef(list(m1,m2), single.row = TRUE, 
           file.name="../out/utas16_evidence_ols.tex",
           custom.variable.names = c("(定数項)","自己申告イデオロギー",
                                     "安倍首相の業績評価","性別(女性)",
                                     "20代(vs.70代以上)","30代(vs.70代以上)",
                                     "40代(vs.70代以上)","50代(vs.70代以上)",
                                     "60代(vs.70代以上)",
                                     "短期大学・専門学校卒(vs.高卒以下)",
                                     "大学・大学院卒(vs.高卒以下)"),
           dcolumn = TRUE, fontsize = "scriptsize", float.pos = "ht!!",
           caption = "2016年東大朝日調査を用いた金融緩和政策に対する評価の決定要因に関する重回帰分析",
           label="utas16_olstab")
tmp <- readLines("../out/utas16_evidence_ols.tex")
# tmp <- iconv(tmp, from="SHIFT-JIS", to="UTF-8")
writeLines(tmp, "../out/utas16_evidence_ols.tex", useBytes = TRUE)

#'
#' # プロット（図1）
#' 

dplot <- d[which(!is.na(d$abesup5)),]

library(ggplot2)

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=TRUE
p <- ggplot(dplot, aes(x=ide,y=easing)) + 
  geom_jitter(width=0.1, color="gray50") + 
  geom_smooth(method='lm', color="black", fill="orange", alpha=0.8) + 
  facet_grid(.~abesup5) + 
  xlab("自己申告イデオロギー(11段階)") + ylab("日本銀行の金融緩和策評価") + 
  scale_x_continuous(breaks=c(-5,0,5),labels=c("　最も\n　左","中間／\n無回答","最も　\n右　")) + 
  scale_y_continuous(breaks=c(1,2,3,4,5),labels=c("評価しない",
                                                  "どちらかといえば\n評価しない",
                                                  "どちらともいえない/\n無回答",
                                                  "どちらかといえば\n評価する",
                                                  "評価する")) + 
  labs(caption=paste("データは東京大学谷口研究室・朝日新聞社共同調査有権者調査(2016年参院選)を使用。",
                     "安倍首相の業績評価は問12(Ｗ2Ｑ12)、金融緩和政策の評価は問15(Ｗ2Ｑ15)、イデオロギーは問21(Ｗ2Ｑ21)を参照。",
                     "灰色の点は散布図(見やすくするためジッタを適用）、直線とその上下の塗りつぶしは線形回帰直線と95％信頼区間を示す。",
                     sep="\n"),
       subtitle = "安倍首相の業績評価") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(hjust=0.5))
p

#+ eval=FALSE
ggsave("../out/utas16_evidence.png", p, width=8, height=5)
# ggsave("../out/utas16_evidence.pdf", p, width=8, height=5, family="Japan1")

#+ fig.width=8, fig.height=5, dev="png", dpi=300, echo=TRUE
p <- ggplot(dplot, aes(x=ide,y=easing)) + 
  geom_jitter(width=0.1, color="gray60") + 
  geom_smooth(method='lm', color="black", fill="gray30", alpha=0.8) + 
  facet_grid(.~abesup5) + 
  xlab("自己申告イデオロギー(11段階)") + ylab("日本銀行の金融緩和策評価") + 
  scale_x_continuous(breaks=c(-5,0,5),labels=c("　最も\n　左","中間／\n無回答","最も　\n右　")) + 
  scale_y_continuous(breaks=c(1,2,3,4,5),labels=c("評価しない",
                                                  "どちらかといえば\n評価しない",
                                                  "どちらともいえない/\n無回答",
                                                  "どちらかといえば\n評価する",
                                                  "評価する")) + 
  labs(caption=paste("データは東京大学谷口研究室・朝日新聞社共同調査有権者調査(2016年参院選)を使用。",
                     "安倍首相の業績評価は問12(Ｗ2Ｑ12)、金融緩和政策の評価は問15(Ｗ2Ｑ15)、イデオロギーは問21(Ｗ2Ｑ21)を参照。",
                     "灰色の点は散布図(見やすくするためジッタを適用）、直線とその上下の塗りつぶしは線形回帰直線と95％信頼区間を示す。",
                     sep="\n"),
       subtitle = "安倍首相の業績評価") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(hjust=0.5))
p

#+ eval=FALSE
ggsave("../out/utas16_evidence_gray.png", p, width=8, height=5)
# ggsave("../out/utas16_evidence_gray.pdf", p, width=8, height=5, family="Japan1")

#+ eval=FALSE, echo=FALSE
# Exporting HTML & PDF File
# In R Studio
# rmarkdown::render('utas2016_evidence_v6.R', rmarkdown::github_document(toc=TRUE),
#                   clean=FALSE)
# tmp <- list.files(getwd())
# tmp <- tmp[grep("\\.spin\\.R$|\\.spin\\.Rmd$|\\.utf8\\.md$|\\.knit\\.md$|\\.tex",tmp)]
# for (i in 1:length(tmp)) file.remove(paste0(getwd(),"/",tmp[i]))
# In Terminal, run:
# Rscript -e "rmarkdown::render('utas2016_evidence_v6.R', 'html_document')"
