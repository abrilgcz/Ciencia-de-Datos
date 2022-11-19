# SARIMA (Seasonal Auto Regressive Integrated Moving Average)
# p = # AR (autorregresivo)
# d = # diferencias (X(t) - X(t-1)) Integrated
# q = # MA (promedio móvil)
# P = # AR estacional aparece sar1 -> X_{t-12}, sar2 -> X_{t-24}
# D = # diferencias estacionales (X_t - X_{t-12})
# Q = # MA estacional
# S = # períodos por año
# ARIMA(p, d, q)(P, D, Q)[S] en auto.arima
# en sarima es el mismito orden

library(forecast) #trae auto.arima
library(astsa) #es la que ajusta y predice bien

#ROBOS
robos <- read.csv("C:\\Users\\abril\\Downloads\\robo_meses.csv")

robos <- ts(robos$Casos, freq = 12,start=c(2015,1)) #lo haces serie de tiempo
auto_model <- auto.arima(robos)
auto_model # ver qué modelo salió

fit_model <- sarima(robos, 1, 1, 0, 2, 0, 0, 12)

sarima.for(robos, 12, 1, 1, 0, 2, 0, 0, 12)
# data son los datos que salen de ts(...)


#FEMINICIDIO
feminicidio <- read.csv("C:\\Users\\abril\\Downloads\\feminicidio.csv",encoding = 'UTF-8')

feminicidio <-ts(feminicidio$Casos, freq = 12,start=c(2015,1))
auto_model <- auto.arima(feminicidio)
auto_model

#no tiene estacionalidad, usamos una función diferente
pronostico<- forecast(auto_model,12,level=95)

plot(pronostico, main="Pronóstico de Feminicidios",xlab='Año',ylab="Casos")

#ABUSO SEXUAL
abuso <- read.csv("C:\\Users\\abril\\Downloads\\abuso.csv",encoding = 'UTF-8')
abuso <- ts(abuso$Casos, freq = 12,start=c(2015,1))
auto_model <- auto.arima(abuso)
auto_model 

fit_model <- sarima(abuso, 1, 1, 2, 1, 0, 1, 12)

sarima.for(abuso, 12, 1, 1, 2, 1, 0, 1, 12)

#NARCOMENUDEO
narco <- read.csv("C:\\Users\\abril\\Downloads\\narco.csv",encoding = 'UTF-8')
narco <- ts(narco$Casos, freq = 12,start=c(2015,1))
auto_model <- auto.arima(narco)
auto_model 

fit_model <- sarima(narco, 0, 1, 1, 1, 0, 1, 12)

sarima.for(narco, 12, 0, 1, 1, 1, 0, 1, 12)


#HOMICIDIO
homicidio <- read.csv("C:\\Users\\abril\\Downloads\\homicidio.csv",encoding = 'UTF-8')
homicidio <- ts(homicidio$Casos, freq = 12,start=c(2015,1))
auto_model <- auto.arima(homicidio)
auto_model 

fit_model <- sarima(homicidio, 0, 1, 1, 0, 0, 2, 12)

sarima.for(homicidio, 12, 0, 1, 1, 0, 0, 2, 12)

#EXTORSIÓN

extorsion <- read.csv("C:\\Users\\abril\\Downloads\\extorsion.csv",encoding = 'UTF-8')
extorsion <- ts(extorsion$Casos, freq = 12,start=c(2015,1))
auto_model <- auto.arima(extorsion)
auto_model 

fit_model <- sarima(extorsion, 0, 1, 1, 1, 0, 0, 12)

sarima.for(extorsion, 12, 0, 1, 1, 1, 0, 0, 12)

