library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ascii)
library(lubridate)
library(splines)
library(mgcv)

getwd()
PSDS_PATH <- file.path(getwd())
PSDS_PATH

## Import datasets needed for chapter 4
# PSDS_PATH <- file.path('~', 'statistics-for-data-scientists')

# 4.0 회귀분석에 필요한 데이터셋
lung <- read.csv(file.path(PSDS_PATH, 'data', 'LungDisease.csv'))

zhvi <- read.csv(file.path(PSDS_PATH, 'data', 'County_Zhvi_AllHomes.csv'))
zhvi <- unlist(zhvi[13,-(1:5)])
dates <- parse_date_time(paste(substr(names(zhvi), start=2, stop=8), "01", sep="."), "Ymd")
zhvi <- data.frame(ym=dates, zhvi_px=zhvi, row.names = NULL) %>%
  mutate(zhvi_idx=zhvi/last(zhvi))

house <- read.csv(file.path(PSDS_PATH, 'data', 'house_sales.csv'), sep='\t')
# house <- house[house$ZipCode > 0, ]
# write.table(house, file.path(PSDS_PATH, 'data', 'house_sales.csv'), sep='\t')
head(house, 2)

# 4.1. 단순선형회귀
# 잔차 : 관측값과 적합값의 차이
# 최소 제곱 : 잔차의 제곱합을 최소화하여 회귀를 피팅하는 바업

# 4.1.1. 회귀식
# Y = b0 + b1 * X 일 때 b0가 절편(상수), b1은 X의 기울기(계수)

## Code for Figure 1
# 다음 그래프는 노동자들이 먼지에 노출된 연수와 폐활량을 표시한 것이다.
# png(filename=file.path(PSDS_PATH, 'figures', 'psds_0401.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")
# dev.off()

## Code snippet 4.1
# 단순선형회귀는 예측변수 Exposure에 대한 함수로 응답변수 PEFR을 예측하기 위한 가장 최선의 직선을 찾는다.
model <- lm(PEFR ~ Exposure, data=lung)
summary(model)

## Code for figure 2
# 다음 그래프는 절편 b0가 424.583이고, 노동자가 먼지에 노출된 연수가 0일 떄 예측되는 PEFR이다.
# png(filename=file.path(PSDS_PATH, 'figures', 'psds_0402.png'), width = 350, height = 350)
par(mar=c(4,4,0,0)+.1)

plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR", ylim=c(300,450), type="n", xaxs="i")
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)
text(x=.3, y=model$coefficients[1], labels=expression("b"[0]),  adj=0, cex=1.5)
x <- c(7.5, 17.5)
y <- predict(model, newdata=data.frame(Exposure=x))
segments(x[1], y[2], x[2], y[2] , col="red", lwd=2, lty=2)
segments(x[1], y[1], x[1], y[2] , col="red", lwd=2, lty=2)
text(x[1], mean(y), labels=expression(Delta~Y), pos=2, cex=1.5)
text(mean(x), y[2], labels=expression(Delta~X), pos=1, cex=1.5)
text(mean(x), 400, labels=expression(b[1] == frac(Delta ~ Y, Delta ~ X)), cex=1.5)
# dev.off()

# 4.1.2. 적합값과 잔차
# 적합값 : 예측값을 지칭하는 말로, 보통 햇hat y^ 으로 나타낸다.
# 잔차 : 원래 값에서 예측한 값을 빼서 구한다.

## Code snippet 4.2
# 예측값을 구하는 predict
fitted <- predict(model)
# 잔차를 구하는 residuals
resid <- residuals(model)
shapiro.test(resid)
plot(model)
var.test(resid)
hist(resid)
model

## Code for figure 3
# png(filename=file.path(PSDS_PATH, 'figures', 'psds_0403.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)

library(dplyr)
lung1 <- lung %>%
  mutate(Fitted=fitted,
         positive = PEFR>Fitted) %>%
  group_by(Exposure, positive) %>%
  summarize(PEFR_max = max(PEFR), 
            PEFR_min = min(PEFR),
            Fitted = first(Fitted)) %>%
  ungroup() %>%
  mutate(PEFR = ifelse(positive, PEFR_max, PEFR_min)) %>%
  arrange(Exposure)

plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")
# 회귀선
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=2)
# 폐활량에 대한 회귀선으로부터 얻은 잔차
segments(lung1$Exposure, lung1$PEFR, lung1$Exposure, lung1$Fitted, col="red", lty=3)
# dev.off()

# 4.1.3. 최소제곱
# 실무에서 회귀선은 잔차들을 제곱한 값들의 합인 잔차제곱합(RSS : residual sum of squares)을 최소화하는 선이다
# 다시말해 Y = b0 + b1 * X 일 때 b0가 절편(상수), b1은 X의 기울기(계수) 라면
# b0, b1은 잔차제곱합을 최소화하는 값이다.

# par(mar=c(4,4,0,0)+.1)

# plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")

# 4.2. 다중선형회귀
# 4.2.1. 킹 카운티 주택 정보 예제
## Code snippet 4.3
head(house[, c("AdjSalePrice", "SqFtTotLiving", "SqFtLot", "Bathrooms", 
               "Bedrooms", "BldgGrade")])

## Code snippet 4.4
# 이런 변수들로부터 판매 금액을 예측한다.
# na.omit 옵션은 모델을 만들 때 결측값이 있는 레코드를 삭제하는 옵션이다.
house_lm <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,  
               data=house, na.action=na.omit)

## Code snippet 4.5
house_lm
# 주택에 1제곱피트를 추가하면 예상 가격은 2.288e+02 만큼 커진다
2.288e+02

# 4.2.2. 모형 평가
# RMSE : 제곱근 평균제곱오차, 예측된 Y^ 값의 평균제곱오차의 제곱근
# RSE : 잔차 표준오차, RMSE 가 n으로 나눈다면 RSE 는 n - p - 1(자유도) 로 나눈 것이다.
# summary에 RSE가 나온다. 
# Multiple R-squared는 모델 데이터의 변동률을 측정한다.
## Code snippet 4.6
summary(house_lm)

# 4.2.4. 모형 선택 및 단계적 회귀
# 오컴의 면도날 : 모든 것이 동일한 조건에서는 복잡한 모델보다 단순한 모델을 우선 사용한다. 
# 변수를 추가하면 항상 RMSE는 감소하고 R^2 는 증가한다.
# AIC = 2P + nlog(RSS / N) , 이므로 P(변수의 개수) 가 많아질 때마다 2P만큼의 불이익을 얻게 된다.
## Code snippet 4.7
house_full <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                   Bedrooms + BldgGrade + PropertyType + NbrLivingUnits + 
                   SqFtFinBasement + YrBuilt + YrRenovated + NewConstruction,
                 data=house, na.action=na.omit)

## Code snippet 4.8
library(MASS)
step_lm <- stepAIC(house_full, direction="both")
step_lm
# stepAIC를 이용해서 가장 좋은 formula를 발견하였다.
formula <- AdjSalePrice ~ SqFtTotLiving + Bathrooms + Bedrooms + BldgGrade + 
  PropertyType + SqFtFinBasement + YrBuilt

# 4.2.5. 가중회귀
# 오래된 매매 정보일수록 최근 정보보다는 신뢰하기가 어렵다.
# WeightedRegression
## Code snippet 4.9
# Weight 열이 가중치로 작용되며, 2005년보다 얼마나 차이가 있는지
# 2005 -> 0, 2020 -> 15점을 가중치로 제공
house$Year = year(house$DocumentDate)
house$Weight = house$Year - 2005
unique(house$Weight)

## Code snippet 4.10
house_wt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, weight=Weight, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_wt=house_wt$coefficients), digits=3)
house_nwt <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade,
               data=house, na.action=na.omit)
round(cbind(house_lm=house_lm$coefficients, 
            house_nwt=house_nwt$coefficients), digits=3)


# Factor Variables
## Code snippet 4.11
# 4.4. 회귀에서의 요인변수
# 요인변수 : 이진 값으로 나타낼 수 있는 이산값
# 예 ) 대출 목적 : 자동차, 결혼, 주식, 학업
# 다음은 주거 형태에 따른 요인변수
# Levels: Multiplex Single Family Townhouse
head(house[, 'PropertyType'])

## Code snippet 4.12
# 더미변수를 이용해서 표현함
prop_type_dummies <- model.matrix(~PropertyType -1, data=house)
head(prop_type_dummies)

## Code snippet 4.13
# 회귀분석 결과에서도 PropertyTypeSingle Family PropertyTypeTownhouse 가 나타났다
# Multiplex가 나타나지 않는 이유는 P개의 수준을 갖는 요인변수에서 P - 1개의 값을 알게 되면 나머지 하나를
# 자연스럽게 알 수 있기 때문이다. 따라서 다중공선성 오류를 발생시키지 않게 하기 위해 두 개의 요인 변수값
# 만 취하게 되었다.
lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
     Bedrooms +  BldgGrade + PropertyType, data=house)

# 4.4.2. 다수의 수준을 갖는 요인변수들
# 요인변수의 수준이 너무 높을 때(답이 너무 많을 때) 요인변수의 그룹을 묶는 것이다.
## Code snippet 4.14
# 82개의 우편번호가 있다. 우편번호는 주택 가격에 대한 위치의 효과를 볼 수 있는 매우 중요한 변수이므로 
# 제외할 수 없는데, 이 경우 82개의 우편번호를 회귀 결과의 잔찻값의 중간값으로 5개의 그룹으로 통합한다.
dim(table(house$ZipCode))

## Code snippet 4.15)
zip_groups <- house %>%
  mutate(resid = residuals(house_lm)) %>%
  group_by(ZipCode) %>%
  summarize(med_resid = median(resid),
            cnt = n()) %>%
  # sort the zip codes by the median residual
  arrange(med_resid) %>%
  mutate(cum_cnt = cumsum(cnt),
         ZipGroup = factor(ntile(cum_cnt, 5)))


zip_groups <- data.frame(zip_groups)
head(zip_groups)
head(merge(house, zip_groups, all.x = T), 2)
head(merge(house, zip_groups, all.x = T)[, c(1, 28)], 2)
head(merge(house, zip_groups, all.x = T)[, c(1, 28)], 2)
# house <- house %>% left_join(select(zip_groups, ZipCode, ZipGroup), by='ZipCode')

# correlated variables
# Code snippet 4.15
step_lm$coefficients

# Code snippet 4.16
update(step_lm, . ~ . -SqFtTotLiving - SqFtFinBasement - Bathrooms)

#  ConfoundingVariables
## Code snippet 4.17
lm(AdjSalePrice ~  SqFtTotLiving + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType + ZipGroup,
   data=house, na.action=na.omit)


#  Interactions
## Code snippet 4.18
lm(AdjSalePrice ~  SqFtTotLiving*ZipGroup + SqFtLot + 
     Bathrooms + Bedrooms + 
     BldgGrade + PropertyType,
   data=house, na.action=na.omit)

head(model.matrix(~C(PropertyType, sum) , data=house))


# outlier anaysis
## Code snippet 4.19
house_98105 <- house[house$ZipCode == 98105,]
lm_98105 <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + Bathrooms + 
                 Bedrooms + BldgGrade, data=house_98105)

## Code snippet 4.20
sresid <- rstandard(lm_98105)
idx <- order(sresid, decreasing=FALSE)
sresid[idx[1]]
resid(lm_98105)[idx[1]]

## Code snippet 4.21
house_98105[idx[1], c('AdjSalePrice', 'SqFtTotLiving', 'SqFtLot',
                      'Bathrooms', 'Bedrooms', 'BldgGrade')]

# Figure 4-5: Influential data point in regression
seed <- 11
set.seed(seed)
x <- rnorm(25)
y <- -x/5 + rnorm(25)
x[1] <- 8
y[1] <- 8

png(filename=file.path(PSDS_PATH, 'figures', 'psds_0405.png'),  width = 4, height=4, units='in', res=300)
par(mar=c(3,3,0,0)+.1)
plot(x, y, xlab='', ylab='', pch=16)
model <- lm(y~x)
abline(a=model$coefficients[1], b=model$coefficients[2], col="blue", lwd=3)
model <- lm(y[-1]~x[-1])
abline(a=model$coefficients[1], b=model$coefficients[2], col="red", lwd=3, lty=2)
dev.off()

# influential observations
## Code snippet 4.22
std_resid <- rstandard(lm_98105)
cooks_D <- cooks.distance(lm_98105)
hat_values <- hatvalues(lm_98105)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)

## Code for Figure 4-6
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0406.png'), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
plot(hat_values, std_resid, cex=10*sqrt(cooks_D))
abline(h=c(-2.5, 2.5), lty=2)
dev.off()


## Table 4-2

lm_98105_inf <- lm(AdjSalePrice ~ SqFtTotLiving + SqFtLot + 
                     Bathrooms +  Bedrooms + BldgGrade,
                   subset=cooks_D<.08, data=house_98105)

df <- data.frame(lm_98105$coefficients,
                 lm_98105_inf$coefficients)
names(df) <- c('Original', 'Influential Removed')
ascii((df),
      include.rownames=TRUE, include.colnames=TRUE, header=TRUE,
      digits=rep(0, 3), align=c("l", "r", "r") ,
      caption="Comparison of regression coefficients with the full data and with influential data removed")

## heteroskedasticity
## Code snippet 4.23
df <- data.frame(
  resid = residuals(lm_98105),
  pred = predict(lm_98105))
ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth() 

## Code for figure 4-7
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0407.png'), width = 4, height=4, units='in', res=300)

ggplot(df, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth() +
  theme_bw() 


dev.off()

## Code for figure 4-8
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0408.png'), width = 4, height=4, units='in', res=300)
par(mar=c(4,4,0,0)+.1)
hist(std_resid, main='')
dev.off()


## partial residuals plot
## Code snippet 4.24
terms <- predict(lm_98105, type='terms')
partial_resid <- resid(lm_98105) + terms

## Code snippet 4.25
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  

## Code for figure 4-9
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0409.png'),  width = 4, height=4, units='in', res=300)

df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 'SqFtTotLiving'],
                 PartialResid = partial_resid[, 'SqFtTotLiving'])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  theme_bw() +
  geom_line(aes(SqFtTotLiving, Terms)) 

dev.off()


## Code snippet 4.26

lm(AdjSalePrice ~ poly(SqFtTotLiving, 2) + SqFtLot +
   BldgGrade + Bathrooms +  Bedrooms, 
   data=house_98105)


lm_poly <- lm(AdjSalePrice ~  poly(SqFtTotLiving, 2) + SqFtLot + 
                BldgGrade +  Bathrooms +  Bedrooms,
              data=house_98105)
terms <- predict(lm_poly, type='terms')
partial_resid <- resid(lm_poly) + terms

## Code for Figure 4-10
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0410.png'), width = 4, height=4, units='in', res=300)

df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 1],
                 PartialResid = partial_resid[, 1])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))+
  theme_bw()

dev.off()


## Code snippet 4.27
knots <- quantile(house_98105$SqFtTotLiving, p=c(.25, .5, .75))
lm_spline <- lm(AdjSalePrice ~ bs(SqFtTotLiving, knots=knots, degree=3) +  SqFtLot +  
                  Bathrooms + Bedrooms + BldgGrade,  data=house_98105)


terms1 <- predict(lm_spline, type='terms')
partial_resid1 <- resid(lm_spline) + terms

## Code for Figure 4-12
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0412.png'), width = 4, height=4, units='in', res=300)

df1 <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms1[, 1],
                 PartialResid = partial_resid1[, 1])
ggplot(df1, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))+
  theme_bw()

dev.off()

## Code snippet 4.27
lm_gam <- gam(AdjSalePrice ~ s(SqFtTotLiving) + SqFtLot + 
                Bathrooms +  Bedrooms + BldgGrade, 
              data=house_98105)
terms <- predict.gam(lm_gam, type='terms')
partial_resid <- resid(lm_gam) + terms

## Code for Figure 4-13
png(filename=file.path(PSDS_PATH, 'figures', 'psds_0413.png'), width = 4, height=4, units='in', res=300)
df <- data.frame(SqFtTotLiving = house_98105[, 'SqFtTotLiving'],
                 Terms = terms[, 5],
                 PartialResid = partial_resid[, 5])
ggplot(df, aes(SqFtTotLiving, PartialResid)) +
  geom_point(shape=1) + scale_shape(solid = FALSE) +
  geom_smooth(linetype=2) + 
  geom_line(aes(SqFtTotLiving, Terms))  +
  theme_bw()
dev.off()



