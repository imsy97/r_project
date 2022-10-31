install.packages("readxl")
library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)

mydata<-read_excel("C:/R/R/dataset/steamplayeruser.xlsx",
                   col_names=T,col_types="guess",na="NA")

################## 1. 데이터 전처리 ##################
mydata$Game_Name<-as.factor(mydata$Game_Name)
# 결측치 대체 #
mydata$Gain<-ifelse(is.na(mydata$Gain),0.355,mydata$Gain)
mydata$Percent_Gain<-ifelse(is.na(mydata$Percent_Gain),0.0012,mydata$Percent_Gain)

# 게임의 종류가 너무 많아서 2012년부터 데이터가 존재하는 게임
# 3개를 뽑아서 3개의 게임에 대한 시계열 분석을 하겠다.Globalstrike, Dota2, TeamFortress
GlobalStrike<-subset(mydata, subset=(mydata$Game_Name=='Counter Strike: Global Offensive'))
Dota2<-subset(mydata,subset=(mydata$Game_Name=='Dota 2'))
TeamFortress<-subset(mydata,subset=(mydata$Game_Name=='Team Fortress 2'))

# Month_Year을 정렬한다. (과거-->미래)
GlobalStrike<-GlobalStrike[order(as.Date(GlobalStrike$Month_Year,format="%Y-%m-%d")),]
Dota2<-Dota2[order(as.Date(Dota2$Month_Year,format="%Y-%m-%d")),]
TeamFortress<-TeamFortress[order(as.Date(TeamFortress$Month_Year,format="%Y-%m-%d")),]

#Avg_players만 예측하고자 나머지 열을 제거한다.
globalstrike<-subset(GlobalStrike,select= -c(Month_Year,Game_Name,Gain, Percent_Gain,Peak_Players))
dota2<-subset(Dota2, select = -c(Month_Year,Game_Name,Gain,Percent_Gain, Peak_Players))
teamfortress<-subset(TeamFortress, select = -c(Month_Year,Game_Name,Gain,Percent_Gain,Peak_Players))

# 세 데이터프레임을 합친다
final<-cbind(globalstrike,dota2,teamfortress)
#이름변환
colnames(final)<-c("GlobalStrike","Dota2","TeamFortress")
final<-as.matrix(final)
################ 2. 시계열 분석 시작 ###############
ts_new<-ts(final,start=c(2012,7),frequency=12) #월 단위를 만들어준다
ts_globalstrike <- ts(globalstrike, start=c(2012,7), frequency=12)
ts_dota2 <- ts(dota2, start=c(2012,7), frequency = 12)
ts_teamfortress <- ts(teamfortress, start=c(2012,7), frequency = 12)
autoplot(ts_new,ylab="avg_players",main="평균게임유저수동향")
# 세데이터 모두 계절성은 없어 보인다.

# 각 게임의 계절성을 구체적으로 확인해본다.
ggseasonplot(ts_globalstrike, year.labels = TRUE, xlab = 'Year', ylab = 'global strike 평균유저수') + labs(title = 'globalstrike 게임 유저수 동향')
ggseasonplot(ts_dota2, year.labels = TRUE, xlab = 'Year', ylab = 'dota2 평균유저수') + labs(title = 'dota2 게임 유저수 동향')
ggseasonplot(ts_teamfortress, year.labels = TRUE, xlab = 'Year', ylab = 'teamfortress 평균유저수') + labs(title = 'teamfortress 게임 유저수 동향')

# 데이터를 분해해본다.(승법모형 가정)--> 계절성이 있다면 추세는 남기고 계절성만 없앤다.
decompose(ts_globalstrike, type = 'multiplicative') %>% autoplot()
globalstrike_decompose = decompose(ts_globalstrike, type = 'multiplicative')
decompose(ts_dota2, type='multiplicative') %>% autoplot()
dota2_decompose = decompose(ts_dota2, type='multiplicative')
decompose(ts_teamfortress, type = 'multiplicative') %>% autoplot()
teamfortress_decompose = decompose(ts_teamfortress, type = 'multiplicative')
globalstrike_decompose %>% names()

# 계절 성분들만 없앤다.
globalstrike_no_seasonal = ts_globalstrike / globalstrike_decompose$seasonal
dota2_no_seasonal = ts_dota2 / dota2_decompose$seasonal
teamfortress_no_seasonal = ts_teamfortress / teamfortress_decompose$seasonal

globalstrike_no_season_plot = autoplot(globalstrike_no_seasonal) + ggtitle('Multiplicative model without seasonal effects')
dota2_no_season_plot = autoplot(dota2_no_seasonal) + ggtitle('Multiplicative model without seasonal effects')
teamfortress_no_season_plot = autoplot(teamfortress_no_seasonal) + ggtitle('Multiplicative model without seasonal effects')

# 데이터를 이동평균한 값을 통해 추세를 좀더 부드럽게 도출해내자
library(TTR)
global_sma12 = SMA(globalstrike_no_seasonal, n = 12)
dota2_sma12 = SMA(dota2_no_seasonal, n = 12)
teamfortress_sma12 = SMA(teamfortress_no_seasonal, n = 12)
plot.ts(global_sma12)
plot.ts(dota2_sma12)
plot.ts(teamfortress_sma12)


# 먼저 자기상관관계를 확인한다.
ggAcf(global_sma12, lag=48) # globalstrike는 추세가 확실히 존재한다. 
ggAcf(dota2_sma12, lag=48)
ggAcf(teamfortress_sma12, lag=48)
# 부분 자기상관계수를 확인한다.
ggPacf(global_sma12, lag=48)
ggPacf(dota2_sma12, lag=48)
ggPacf(teamfortress_sma12, lag=48)
pacf(ts_new) # pacf는 안정되어 있다.

# 차분을 통해 정상화 시킨다.
global_diff1 = diff(globalstrike_no_seasonal, differences = 1)
plot.ts(globalstrike_no_seasonal)
plot.ts(global_diff1) # 1차 차분
dota2_diff1 = diff(dota2_no_seasonal, differences = 1)
plot.ts(dota2_no_seasonal)
plot.ts(dota2_diff1)
teamfortress_diff1 = diff(teamfortress_no_seasonal, differences = 1)
plot.ts(teamfortress_no_seasonal)
plot.ts(teamfortress_diff1)
# 1차 차분만으로도 어느정도 정상성의 패턴이 보인다.

# 다시 한번 acf와 pacf를 확인한다.
ggAcf(global_diff1, lag=48) # MA(2)
ggAcf(dota2_diff1, lag=48) 
ggAcf(teamfortress_diff1, lag=48)
# 부분 자기상관계수를 확인한다.
ggPacf(global_diff1, lag=48) # AR(1)
ggPacf(dota2_diff1, lag=48) # 절단값이 애매하다. 
ggPacf(teamfortress_diff1, lag=48)
# 모두 정상성을 만족한다고 봐도 무방하겠다.
# globalstrike : ARIMA(1,1,2)
# dota2 : 애매해서 auto.arima()를 활용해보자.
# teamfortress : auto.arima()를 활용해보자.

# auto.arima() 로 확인
auto.arima(ts_globalstrike) # ARIMA(0,1,1)
auto.arima(ts_dota2) # ARIMA(0,2,1)(1,0,1)[12]
auto.arima(ts_teamfortress) # ARIMA(0,1,1)(0,0,1)[12]

#예측
global_arima = Arima(ts_globalstrike, order=c(1,1,2)) # 차분 통해 확인한 값 적용 
global_arima


# globalstrike 최종 예측
globalstrike_fit = arima(ts_globalstrike, order=c(0,1,1))
globalstrike_fcast = forecast(globalstrike_fit, h = 12)
autoplot(globalstrike_fcast) + ggtitle("GlobalStrike의 1년 예측값")

# dota2 최종 예측
dota2_fit = arima(ts_dota2, order=c(0,2,1), seasonal = c(1,0,1))
dota2_fcast = forecast(dota2_fit, h = 12)
autoplot(dota2_fcast) + ggtitle("dota2의 1년 예측값")

# TeamFortress 최종 예측
teamfortress_fit = arima(ts_teamfortress, order=c(0,1,1), seasonal = c(0,0,1))
teamfortress_fcast = forecast(teamfortress_fit, h = 12)
autoplot(teamfortress_fcast) + ggtitle("teamfortress의 1년 예측값")


#모델의 정확도를 수치값으로 확인한다.
accuracy(globalstrike_fit)
accuracy(dota2_fit)
accuracy(teamfortress_fit)




