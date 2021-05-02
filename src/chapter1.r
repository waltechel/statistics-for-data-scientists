# packages needed for chapter 1
library(dplyr)
library(tidyr)
library(ggplot2)
library(vioplot)
library(ascii)
library(corrplot)
library(descr)

getwd()

# Import the datasets needed for chapter 1
PSDS_PATH <- file.path(getwd())
PSDS_PATH
# PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')
# dir.create(file.path(PSDS_PATH, 'figures'))

# dataSet Making
state <- read.csv(file.path(PSDS_PATH, 'data', 'state.csv'))
dfw <- read.csv(file.path(PSDS_PATH, 'data', 'dfw_airline.csv'))
sp500_px <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'))
sp500_sym <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_sym.csv'), stringsAsFactors = FALSE)
kc_tax <- read.csv(file.path(PSDS_PATH, 'data', 'kc_tax.csv'))
lc_loans <- read.csv(file.path(PSDS_PATH, 'data', 'lc_loans.csv'))
airline_stats <- read.csv(file.path(PSDS_PATH, 'data', 'airline_stats.csv'), stringsAsFactors = FALSE)
airline_stats$airline <- ordered(airline_stats$airline, levels=c('Alaska', 'American', 'Jet Blue', 'Delta', 'United', 'Southwest'))

## Code to create state table
head(state)
state_asc <- state
state_asc
state_asc$Population
state_asc$Population <- formatC(state_asc$Population, format="d", digits=0, big.mark=",")
state_asc$Population
head(state_asc)
ascii(state_asc[1:8,], 
      digits=c(0, 0,1), 
      align=c("l", "l", "r", "r"), 
      caption="A few rows of the +data.frame state+ of population and murder rate by state.")

## Code snippet 1.1
## R을 이용해 인구의 평균, 절사평균, 중간값을 계산하였다.
mean(state$Population)
mean(state$Population, trim = 0.1)
median(state$Population)

## Code snippet 1.2
library("matrixStats")
## 주마다 다른 인구를 보기 위해 가중평균을 사용하였다.
print("살인률의 평균")
mean(state$Murder.Rate)
print("살인률의 가중평균")
weighted.mean(state$Murder.Rate, w = state$Population)
weightedMean(state$Murder.Rate, w = state$Population)
weightedMedian(state$Murder.Rate, w = state$Population)

## Code snippet 1.3
## 다음은 미국 각 주의 인구와 살인 비율을 담고 있는 데이터 집합에서 첫 몇 행을 보여준다.
head(state)
sd(state$Population)
iqr(state$Population) ## 사분위범위
mad(state$Population) ## 중위절대편차

## Code snippet 1.4
## 백분위수를 활용해서 데이터의 흩어진 정도를 파악한다.
quantile(state$Murder.Rate)
quantile(state$Murder.Rate, p=c(0.05, 0.25, 0.5, 0.75, 0.95))

## Code to create PercentileTable
ascii(
  quantile(state$Murder.Rate, p=c(0.05, 0.25, 0.5, 0.75, 0.95)),
  include.rownames=FALSE, 
  include.colnames=TRUE, 
  digits=rep(2,5), 
  align=rep("r", 5), 
  caption="Percentiles of murder rate by state.")

## Code snippet 1.5
## R을 이용해서  주별 인구 분포를 나타내는 상자그림이다.
boxplot(state$Population/1000000, ylab="Population (millions)")

## Code for Figure 2
png(filename=file.path(PSDS_PATH, "figures", "psds_0102.png"), width = 3, height=4, units='in', res=300)
par(mar=c(0,4,0,0)+.1)
boxplot(state$Population/1000000, ylab="Population (millions)")
dev.off()

## Code snippet 1.6
## 주별 인구의 도수분포표
breaks <- seq(from=min(state$Population), to=max(state$Population), length=11)
breaks
pop_freq <- cut(state$Population, breaks=breaks, right=TRUE, include.lowest = TRUE)
pop_freq
state$PopFreq <- pop_freq
head(state)
table(pop_freq)

## Code for FreqTable
state_abb <- state %>%
  arrange(Population) %>%
  group_by(PopFreq) %>%
  summarize(state = paste(Abbreviation, collapse=","), .drop=FALSE) %>%
  complete(PopFreq, fill=list(state='')) %>%
  select(state) 
head(state_abb)
state_abb <- unlist(state_abb)
head(state_abb)

breaks
lower_br <- formatC(breaks[1:10], format="d", digits=0, big.mark=",")
lower_br
upper_br <- formatC(c(breaks[2:10]-1, breaks[11]), format="d", digits=0, big.mark=",")
upper_br

pop_table <- data.frame("BinNumber"=1:10,
                        "BinRange"=paste(lower_br, upper_br, sep="-"),
                        "Count"=as.numeric(table(pop_freq)),
                        "States"=state_abb)
head(pop_table)
pop_table$BinNumber
pop_table$BinRange
pop_table$Count
pop_table$States
head(pop_table)

ascii(pop_table, 
      include.rownames=FALSE, 
      digits=c(0, 0, 0, 0), 
      align=c("l", "r", "r", "l"), 
      caption="A frequency table of population by state.")

## Code snippet 1.7
## 히스토그램, x축에는 구간을 표시하고, y축에는 해당 구간별 데이터의 개수를 표시한다.
hist(state$Population, breaks=breaks)

## Code for Figure 3
png(filename=file.path(PSDS_PATH, "figures", "psds_0103.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
pop_hist <- hist(state$Population, breaks=breaks,
                 xlab="Population", main="")
dev.off()

## Code snippet 1.8
## 주별 살인율의 밀도
hist(state$Murder.Rate, freq=FALSE )
lines(density(state$Murder.Rate), lwd=3, col="blue")

## Code for Figure 4
png(filename=file.path(PSDS_PATH, "figures", "psds_0104.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
hist(state$Murder.Rate, freq=FALSE, xlab="Murder Rate (per 100,000)", main="" )
lines(density(state$Murder.Rate), lwd=3, col="blue")
dev.off()

## Code for AirportDelays
ascii(
  100*as.matrix(dfw/sum(dfw)),
  include.rownames=FALSE, include.colnames=TRUE, digits=rep(2,5), align=rep("r", 5), 
  caption="Percentage of delays by cause at Dallas-Ft. Worth airport.")


## Code for figure 5
png(filename=file.path(PSDS_PATH, "figures", "psds_0105.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4, 4, 0, 1) + .1)
barplot(as.matrix(dfw)/6, cex.axis = 0.8, cex.names = 0.7)
dev.off()


## Code for CorrTable (Table 1.7)
telecom <- sp500_px[, sp500_sym[sp500_sym$sector=="telecommunications_services", 'symbol']]
telecom
head(telecom)
telecom <- telecom[row.names(telecom)>"2012-07-01", ]
telecom
telecom_cor <- cor(telecom)
ascii(telecom_cor, digits=c( 3,3,3,3,3), align=c("l", "r", "r", "r", "r", "r"), caption="Correlation between telecommunication stock returns.",
      include.rownames = TRUE, include.colnames = TRUE)

## Code snippet 1.10
## ETF는 S&P 500(SPY)와 다우 존스 지수(DIA)와 높은 상관성을 가진다.
head(sp500_px[,1:6])
row.names(sp500_px)
# sp500_px[row.names(sp500_px)>"2012-07-01",1:6]
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01", 
                 sp500_sym[sp500_sym$sector=="etf", 'symbol']]
corrplot(cor(etfs), method = "ellipse")

## Code for figure 6
png(filename=file.path(PSDS_PATH, "figures", "psds_0106.png"), width = 4, height=4, units='in', res=300)
etfs <- sp500_px[row.names(sp500_px)>"2012-07-01", sp500_sym[sp500_sym$sector=="etf", 'symbol']]
library(corrplot)
corrplot(cor(etfs), method = "ellipse")
dev.off()

## Code snippet 1.11
# 두 개 주식간의 상관관계를 나타낸다.
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

## Code for Figure 7
png(filename=file.path(PSDS_PATH, "figures", "psds_0107.png"),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,1)+.1)
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ", cex=.8)
abline(h=0, v=0, col="grey")
dev.off()

## Code snippet 1.12
## 너무 비싸거나 너무 작거나 너무 큰 것들은 제외했다.
kc_tax0 <- subset(kc_tax, 
                  TaxAssessedValue < 750000 & SqFtTotLiving>100 & SqFtTotLiving<3500)
nrow(kc_tax0)

## Code snippet 1.13
## 두 군데 밀집한 곳이 존재한다.
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")

## Code for figure 8
png(filename=file.path(PSDS_PATH, "figures", "psds_0108.png"),  width = 4, height=4, units='in', res=300)
ggplot(kc_tax0, (aes(x=SqFtTotLiving, y=TaxAssessedValue))) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient(low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value")
dev.off()

## Code snippet 1.14
## 등고선에서도 두 군데의 봉이 있다.
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point( alpha=0.1) + 
  geom_density2d(colour="white") + 
  labs(x="Finished Square Feet", y="Tax Assessed Value")

## Code for figure 9
png(filename=file.path(PSDS_PATH, "psds_0109.png"),  width = 4, height=4, units='in', res=300)
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point(colour="blue", alpha=0.1) + 
  geom_density2d(colour="white") + 
  labs(x="Finished Square Feet", y="Tax Assessed Value")
dev.off()


## Code snippet 1.15
## 분할표 : 두 범주형 변수를 요약하는 효과적인 방법
## 개인대출 등급과 대출 결과를 나타내는 분할표이다.
## 대출 등급과 상황에 대한 분할표
## Code for CrossTabs
x_tab <- CrossTable(lc_loans$grade, lc_loans$status, 
                    prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE)

tots <- cbind(row.names(x_tab$tab), format(cbind(x_tab$tab, x_tab$rs)))
props <- cbind("", format(cbind(x_tab$prop.row, x_tab$rs/x_tab$gt), digits=1))
c_tot <- c("Total", format(c(x_tab$cs, x_tab$gt)))

asc_tab <- matrix(nrow=nrow(tots)*2+1, ncol=ncol(tots))
colnames(asc_tab) <- c("Grade", colnames(x_tab$tab), "Total")
idx <- seq(1, nrow(asc_tab)-1, by=2)
asc_tab[idx,] <- tots
asc_tab[idx+1,] <- props
asc_tab[nrow(asc_tab), ] <- c_tot

ascii(asc_tab,  align=c("l", "r", "r", "r", "r"), include.rownames = FALSE, include.colnames = TRUE)


#########################################################################################

## Code snippet 1.16
## 항공사별 비행 지연 정도를 비교한 그래프
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))

## Code for figure 10
png(filename=file.path(PSDS_PATH, "figures", "psds_0110.png"), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50), cex.axis=.6,
        ylab="Daily % of Delayed Flights")

dev.off()

## Code snippet 1.17
## 바이올린 도표 : 상자그림을 보완한 형태로, Y축을 따라 밀도추정 결과를 동시에 시각화한 그래프
ggplot(data=airline_stats, aes(airline, pct_carrier_delay))  + 
  ylim(0, 50) + 
  geom_violin() +
  labs(x="", y="Daily % of Delayed Flights")

## Code for figure 11
png(filename=file.path(PSDS_PATH, "figures", "psds_0111.png"), width = 4, height=4, units='in', res=300)

ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) + 
  ylim(0, 50) + 
  geom_violin(draw_quantiles = c(.25, .5, .75), linetype=2) +
  geom_violin(fill=NA, size=1.1) +
  theme_bw() + 
  labs(x="", y="% of Delayed Flights")

dev.off()

## Code snippet 1.18
## 어떤 우편번호에서의 평가액이 다른 두 군데보다 훨씬 더 높다는 것을 알게된다.
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
         aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="white", high="black") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")

## Code for figure 12
png(filename=file.path(PSDS_PATH, "figures", "psds_0112.png"), width = 5, height=4, units='in', res=300)

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="gray95", high="blue") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")
dev.off()

