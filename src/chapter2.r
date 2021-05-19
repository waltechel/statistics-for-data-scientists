# packages needed for chapter 2
library(dplyr)
library(tidyr)
library(ggplot2)
library(vioplot)
library(ascii)
library(corrplot)
library(descr)
library(ggplot2)

rm(list = ls())

PSDS_PATH <- file.path(getwd())
PSDS_PATH

# 데이터 불러오기
loans_income <- read.csv(file.path(PSDS_PATH, 'data', 'loans_income.csv'))[,1]
sp500_px <- read.csv(file.path(PSDS_PATH, 'data', 'sp500_px.csv'))

head(loans_income)
head(sp500_px)

# 정규분포 그림 그리는 부분
x <- seq(from=-3, to=3, length=300)
x
gauss <- dnorm(x)
gauss

par(mar=c(3, 3, 0, 0)+.1)
# 그림 그리기
plot(x, gauss, type="l", col='blue', xlab='', ylab='', axes=FALSE)
# 색 칠하기
polygon(x, gauss, col='blue')

# 정규분포 그림 만들기
png(filename=file.path(PSDS_PATH, 'figures', 'normal_density.png'),  width = 4, height=5, units='in', res=300)
par(mar=c(3, 3, 0, 0)+.1)
plot(x, gauss, type="l", col='blue', xlab='', ylab='', axes=FALSE)
polygon(x, gauss, col='blue')
dev.off()

# 히스토그램 그림 그리는 부분
norm_samp <- rnorm(100)
par(mar=c(3, 3, 0, 0)+.1)
hist(norm_samp, axes=FALSE, col='red', main='')

# 히스토그램 그림 만들기
png(filename=file.path(PSDS_PATH, 'figures', 'samp_hist.png'), width = 200, height = 250)
norm_samp <- rnorm(100)
par(mar=c(3, 3, 0, 0)+.1)
hist(norm_samp, axes=FALSE, col='red', main='')
dev.off()

## Code snippet 2.1
# 부트스트랩 : 현재 있는 표본에서 추가적으로 표본을 복원추출하고 각 표본에 대한 통계량과 모델을 다시 계산하는 것
# 부트스트랩 표본 : 관측 데이터 집합으로부터 얻은 복원추출 표본

# 부트스트랩 알고리즘
# 1. 샘플 값을 뽑아서 기록하고 제자리에 놓는다.
# 2. 샘플의 크기만큼 N 번 반복한다.
# 3. 재표본추출된 값의 평균을 기록한다.
# 4. 이 과정을 R번 반복한다.

# R 패키지 boot는 이런 여러 단계를 하나의 함수로 제공한다.
library(boot)
stat_fun <- function(x, idx) median(x[idx])
# 사람들의 소득 데이터에 부트스트랩을 적용
boot_obj <- boot(loans_income, R = 1000, statistic=stat_fun)
summary(boot_obj)
# boot_obj를 봤을 때 bias는 -82.9944
boot_obj

# 2.3. 통계학에서의 표본분포
# 중심극한정리 : 모집단이 정규분포가 아니더라도 표본크기가 충분하고 
# 데이터가 정규성을 크게 이탈하지 않는 경우
# 여러 표본에서 추출한 평균은 종모양의 정규곡선을 따른다.
# take a simple random sample
samp_data <- data.frame(income=sample(loans_income, 1000), 
                        type='data_dist')
head(samp_data)
# take a sample of means of 5 values 5개 값의 평균으로 이뤄진 표본을 취한다
samp_mean_05 <- data.frame(
  income = tapply(sample(loans_income, 1000*5), 
                  rep(1:1000, rep(5, 1000)), FUN=mean),
  type = 'mean_of_5')
# take a sample of means of 20 values 20개 값의 평균으로 이뤄진 표본을 취한다.
samp_mean_20 <- data.frame(
  income = tapply(sample(loans_income, 1000*20), 
                  rep(1:1000, rep(20, 1000)), FUN=mean),
  type = 'mean_of_20')
# data.frame 바인딩 후 factor로 형변환
# bind the data.frames and convert type to a factor
income <- rbind(samp_data, samp_mean_05, samp_mean_20)
income$type = factor(income$type, 
                     levels=c('data_dist', 'mean_of_5', 'mean_of_20'),
                     labels=c('Data', 'Mean of 5', 'Mean of 20'))

# 히스토그램 그리기
# plot the histograms
ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +
  facet_grid(type ~ .)

## Code for Figure 6
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0206.png'),  width = 3, height=4, units='in', res=300)
ggplot(income, aes(x=income)) +
  geom_histogram(bins=40) +
  facet_grid(type ~ .) +
  theme_bw()
dev.off()


# 표준정규분포와 QQ 그림
# QQ 그림은 표본이 정규분포에 얼마나 가까운지를 시각적으로 판별하는 데 사용된다.
norm_samp <- rnorm(100)
norm_samp
par(mar=c(3, 3, 0, 0)+.1)
qqnorm(norm_samp, main='', xlab='', ylab='')
abline(a=0, b=1, col='grey')

## Code for Figure 11
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0211.png'),  width = 4, height=4, units='in', res=300)
norm_samp <- rnorm(100)
par(mar=c(3, 3, 0, 0)+.1)
qqnorm(norm_samp, main='', xlab='', ylab='')
abline(a=0, b=1, col='grey')
dev.off()

# 긴 꼬리 분포
# 낮은 값의 점들은 대각선보다 훨씬 낮고 높은 값은 대각선보다 훨씬 위에 위치한다.
# 데이터가 정규분포를 따른다고 하더라도 예상되는 값보다 훨씬 더 많은 극단값을 관찰할 가능성이 있음을 보여준다.
# 나심 탈레브, <블랙 스완>

## Code for Figure 12
par(mar=c(3, 3, 0, 0)+.1)
nflx <- sp500_px[,'NFLX']
head(nflx)
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx, main='', xlab='', ylab='')
abline(a=0, b=1, col='grey')

## Code for Figure 12
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0212.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(3, 3, 0, 0)+.1)
nflx <- sp500_px[,'NFLX']
?diff
nflx <- diff(log(nflx[nflx>0]))
qqnorm(nflx, main='', xlab='', ylab='')
abline(a=0, b=1, col='grey')
dev.off()

