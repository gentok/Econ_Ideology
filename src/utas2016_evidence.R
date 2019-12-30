## 東大朝日調査による金融緩和選好とイデオロギーの関係に関する分析

## Set Working Directory to the current directory 
## (If using RStudio, can be set automatically)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Clear Workspace
rm(list=ls())

## データの読み込み
dloc <- "http://www.masaki.j.u-tokyo.ac.jp/utas/2014_2016UTASV20161004.csv"
d <- read.csv(dloc)

## イデオロギー
d$ide <- d$W2Q21
d$ide[d$W2Q21==99] <- 5
d$ide <- d$ide - 5
table(d$ide)

d$ide3 <- NA
d$ide3[d$ide<=-1] <- -1
d$ide3[d$ide==0] <- 0
d$ide3[d$ide>=1] <- 1
table(d$ide3)

## 金融緩和
d$easing <- d$W2Q15
d$easing[d$W2Q15==99] <- 3
d$easing <- 5 - d$easing
table(d$easing)

## 安倍内閣支持
d$abesup <- d$W1Q8
d$abesup[d$W1Q8==99] <- 3
d$abesup <- 5 - d$abesup
table(d$abesup)

d$abesup3 <- NA
d$abesup3[d$abesup<2] <- "評価しない"
d$abesup3[d$abesup==2] <- "どちらでもない"
d$abesup3[d$abesup>2] <- "評価する"
d$abesup3 <- factor(d$abesup3, levels=c("評価しない","どちらでもない","評価する"))

# プロット
library(ggplot2)

p <- ggplot(d, aes(x=ide,y=easing)) + 
  geom_jitter(width=0.1, color="gray50") + 
  geom_smooth(method='lm', color="black", fill="orange", alpha=0.8) + 
  facet_grid(.~abesup3) + 
  xlab("自己申告イデオロギー(11段階)") + ylab("日本銀行の金融緩和策評価") + 
  scale_x_continuous(breaks=c(-5,0,5),labels=c("　最も左","中間／無回答","最も右　")) + 
  scale_y_continuous(breaks=c(0,1,2,3,4),labels=c("評価しない",
                                              "どちらかといえば\n評価しない",
                                              "どちらともいえない/\n無回答",
                                              "どちらかといえば\n評価する",
                                              "評価する")) + 
  labs(caption="データは東京大学谷口研究室・朝日新聞社共同調査有権者調査(2016年参院選)を使用。\n灰色の点は散布図(見やすくするためジッタを適用）、直線とその上下の塗りつぶしは線形回帰直線と95％信頼区間を示す。",
       subtitle = "安倍内閣の業績評価") + 
  theme_bw() + 
  theme(plot.subtitle = element_text(hjust=0.5))
p

#+ eval=FALSE
ggsave("../out/utas16_evidence.png", p, width=8, height=5)

# 安倍内閣支持を統制した場合の分析

#devtools::install_github("gentok/estvis")
library(estvis)

m <- lm(easing ~ ide + abesup, data=d)
summary(m)

s <- simu_pred(m, profile = data.frame(ide=seq(-5,5,1)),abesup=0)
plot_simu(s, name.x = "ide")

