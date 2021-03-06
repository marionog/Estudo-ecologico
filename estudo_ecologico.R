### ESTUDO ECOL�GICO - COBERTURA DE MAMOGRAFIA
### ECOLOGIC STUDY - MAMOGRAPHIC COVERAGE
# por regi�es de sa�de do Brasil
# for health regions of Brazil

# ler arquivo de mapa com proje��o (formato shape)
# Read shapefiles with projections
library(rgdal)
regsaude <- readOGR(dsn = ".", layer = "mama")

## 1 - AN�LISE EXPLORAT�RIA DE DADOS
## 1 - EXPLORATORY DATA ANALYSIS

names(regsaude@data)
str(regsaude@data)

# Resumo num�rico
# Numeric summary
summary(regsaude)
sapply(regsaude@data,sd)

# Resumo num�rico por regi�es do Brasil
# Numeric summary by regions of Brazil
summary(regsaude[regsaude$REG_NAME=="Regi�o Sudeste",])
summary(regsaude[regsaude$REG_NAME=="Regi�o Centro-Oeste",])
summary(regsaude[regsaude$REG_NAME=="Regi�o Nordeste",])
summary(regsaude[regsaude$REG_NAME=="Regi�o Norte",])
summary(regsaude[regsaude$REG_NAME=="Regi�o Sul",])
sapply((regsaude@data[regsaude$REG_NAME=="Regi�o Sudeste",]),sd)
sapply((regsaude@data[regsaude$REG_NAME=="Regi�o Centro-Oeste",]),sd)
sapply((regsaude@data[regsaude$REG_NAME=="Regi�o Nordeste",]),sd)
sapply((regsaude@data[regsaude$REG_NAME=="Regi�o Norte",]),sd)
sapply((regsaude@data[regsaude$REG_NAME=="Regi�o Sul",]),sd)

## 2 - AN�LISE EXPLORAT�RIA DE DADOS ESPACIAIS
## 2 - EXPLORATORY SPATIAL DATA ANALYSIS

library(spdep)

# Para mostrar o mapa
# Showing the map

par(mar=c(1,1,1,1)+0.1, mfrow=c(1,1))
plot(regsaude, main="Regi�es de sa�de do Brasil",col="gray")

# Moran I global - spatial auto-correlation with permutation test
moran.mc(regsaude$razmam10a11,w.list,nsim=999)
moran.mc(regsaude$popg2010,w.list,nsim=999)
moran.mc(regsaude$pibpc,w.list,nsim=999)
moran.mc(regsaude$escmedia,w.list,nsim=999)
moran.mc(regsaude$GINI2010,w.list,nsim=999)
moran.mc(regsaude$razurb,w.list,nsim=999)
moran.mc(regsaude$razmed,w.list,nsim=999)
moran.mc(regsaude$cobpsf,w.list,nsim=999)
moran.mc(regsaude$razrad10,w.list,nsim=999)
moran.mc(regsaude$razapmam10,w.list,nsim=999)
moran.mc(regsaude$mampmam10,w.list,nsim=999)

# Lee L test - bivariate spatial correlation with permutation test
lee.mc(regsaude$razmam10a11,regsaude$popg2010,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$pibpc,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$escmedia,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$GINI2010,w.list,nsim=999,alternative="less")
lee.mc(regsaude$razmam10a11,regsaude$razurb,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$razmed,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$cobpsf,w.list,nsim=999,alternative="less")
lee.mc(regsaude$razmam10a11,regsaude$razrad10,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$razapmam10,w.list,nsim=999)
lee.mc(regsaude$razmam10a11,regsaude$mampmam10,w.list,nsim=999)

## Mapas tem�ticos
## Thematic maps

pal.gray <- gray(5:1/6)
library(classInt)

# desfecho
# outcome

par(mar=c(3,3,3,3), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$razmam10a11, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="Mamography screening coverage %")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.05-6.08)","[6.08-15.62)","[15.62-28.26)","[28.26-41.07)","[41.07-84.15)"),title="")
plot(regiao,add=T)

# exposi��es
# exposures

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$GINI2010, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="a) Gini index")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.38-0.47)","[0.47-0.49)","[0.49-0.52)","[0.52-0.55)","[0.55-0.71)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$popg2010, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="b) Population (x 1000)")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[21.47-137.19)","[137.19-209.35)","[209.35-291.687)","[291.68-474.41)","[471.41-11253.5)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$razurb, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="c) Urbanization rate %")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[40.00-60.00)","[60.00-70.80)","[70.80-82.00)","[82.00-91.00)","[91.00-100.00)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$pibpc, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="d) GDP per capita R$ (x 1000)")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[3.15-5.91)","[5.91-10.82)","[10.82-15.37)","[15.37-20.70)","[20.70-59.22)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$escmedia, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="e) Secundary/higher education %")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[22.54-35.73)","[35.73-43.43","[43.43-49.19)","[49.19-54.89)","[54.89-70.75)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$cobpsf, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="f) ESF coverage %")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[13.01-59.83)","[59.83-78.61)","[78.61-91.65)","[91.65-100.00)","[100.00-100.00)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$razmed, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="g) Physician ratio / 1000")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.12-0.48)","[0.48-0.71","[0.71-1.01)","[1.01-1.43)","[1.43-3.64)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$razrad10, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="h) Radiologists ratio / 100000")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.00-1.00)","[1.00-2.00)","[2.00-3.00)","[3.00-5.00)","[5.00-20.00)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$razapmam10, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="i) Mammography machines ratio / 10000")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.00-0.53)","[0.53-0.91)","[0.91-1.37)","[1.37-1.81)","[1.81-7.70)"),title="")
plot(regiao,add=T)

par(mar=c(6,6,6,6), mfrow=c(1,1))
classes_qt <- classIntervals(regsaude$mampmam10, n=5, style = "quantile", rtimes = 1)
cols.gray <- findColours(classes_qt, pal.gray)
plot(regsaude,col=cols.gray,border=NA, main="j) Mammograms per machine (x 100)")
legend("bottomleft",cex=0.8,fill=attr(cols.gray,"palette"),bty="n",
       legend=c("[0.00-0.36)","[0.36-4.39)","[4.39-7.83)","[7.83-11.34)","[11.34-52.06)"),title="")
plot(regiao,add=T)


## MODELOS DE REGRESS�O
## REGRESSION MODELS

# 1 Modelos simples
# 1 Simple models

m.pop <- lm(razmam10a11~log(popg2010), data=regsaude)
summary(m.pop)

m.pib <- lm(razmam10a11~pibpc, data=regsaude)
summary(m.pib)

m.gini <- lm(razmam10a11~GINI2010, data=regsaude)
summary(m.gini)

m.urb <- lm(razmam10a11~razurb, data=regsaude)
summary(m.urb)

m.psf <- lm(razmam10a11~cobpsf, data=regsaude)
summary(m.psf)

m.rad <- lm(razmam10a11~razrad10, data=regsaude)
summary(m.rad)

m.ap <- lm(razmam10a11~razapmam10, data=regsaude)
summary(m.ap)

m.mampmam <- lm(razmam10a11~mampmam10, data=regsaude)
summary(m.mampmam)

regsaude$REG_NAME <- relevel(regsaude$REG_NAME,ref="Regi�o Sudeste")
m.reg <- lm(razmam10a11~REG_NAME, data=regsaude)
summary(m.reg)

### 2 Modelos m�ltiplos
### 2 Multiple models

## etapa 1 - bloco socioecon�mico
## step 1 - socioeconomic block

# sem ajustes espaciais
# without spatial adjusting
m.socio <- lm(razmam10a11~log(popg2010)+pibpc+GINI2010+razurb, data=regsaude)
summary(m.socio)
AIC(m.socio)
res.socio <- resid(m.socio)
library(lmtest)
bptest(m.socio)
lm.morantest(m.socio,w.list,alternative="two.sided")
lm.LMtests(m.socio,w.list,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
library(car)
crPlots(m.socio)

# com ajuste para depend�ncia espacial (modelo SAR)
# adjusting for spatial dependence (SAR model)
m.socio <- lagsarlm(razmam10a11~log(popg2010)+pibpc+GINI2010+razurb, data=regsaude,w.list)
summary(m.socio)
summary(impacts(m.socio, listw=w.list,R=100),zstats=TRUE)
res.socio <- resid(m.socio)
bptest.sarlm(m.socio)

# ajuste para depend�ncia e heterogeneidade espacial (modelo SAR e inclus�o de vari�vel indicadora de grandes regi�es)
# adjusting for spatial dependence and heterogeneity (SAR model with inclusion of variable indicating great regions)
m.socio <- lagsarlm(razmam10a11~REG_NAME+log(popg2010)+pibpc+GINI2010+razurb, data=regsaude,w.list)
summary(m.socio)
summary(impacts(m.socio, listw=w.list,R=100),zstats=TRUE)
res.socio <- resid(m.socio)
bptest.sarlm(m.socio)

# regimes espaciais
# spatial regimes
m.regime <- lagsarlm(razmam10a11 ~ 0 + (one+log(popg2010)+pibpc+GINI2010+razurb):(REG_NAME),
                     data=regsaude,w.list)
summary(m.regime)
summary(impacts(m.regime, listw=w.list,R=100),zstats=TRUE)

# teste de Chow espacial
# spatial Chow test
spatialchow.test <- function(rest,unrest)
{
  lrest <- rest$LL
  lunrest <- unrest$LL
  k <- rest$parameters - 2
  spchow <- - 2.0 * (lrest - lunrest)
  pchow <- pchisq(spchow,k,lower.tail=FALSE)
  list(spchow,pchow,k)
}

spatialchow.test(m.socio,m.regime)

## etapa 2 - bloco socioecon�mico + bloco assistencial
## step 2 - socioeconomic block + healthcare block

# sem ajustes espaciais
# without spatial adjusting
m.care <- lm(razmam10a11~log(popg2010)+pibpc+GINI2010+razurb+cobpsf+razrad10+razapmam10, data=regsaude)
summary(m.care)
AIC(m.care)
res.care <- resid(m.care)
bptest(m.care)
lm.morantest(m.care,w.list,alternative="two.sided")
lm.LMtests(m.care,w.list,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
crPlots(m.care)

# com ajuste para depend�ncia espacial (modelo SAR)
# adjusting for spatial dependence (SAR model)
m.care <- lagsarlm(razmam10a11~log(popg2010)+pibpc+GINI2010+razurb+cobpsf+razrad10+razapmam10, data=regsaude,w.list)
summary(m.care)
summary(impacts(m.care, listw=w.list,R=100),zstats=TRUE)
res.care <- resid(m.care)
bptest.sarlm(m.care)

# ajuste para depend�ncia e heterogeneidade espacial (modelo SAR e inclus�o de vari�vel indicadora de grandes regi�es)
# adjusting for spatial dependence and heterogeneity (SAR model with inclusion of variable indicating great regions)
m.care <- lagsarlm(razmam10a11~REG_NAME+log(popg2010)+pibpc+GINI2010+razurb+cobpsf+razrad10+razapmam10, data=regsaude,w.list)
summary(m.care)
summary(impacts(m.care, listw=w.list,R=100),zstats=TRUE)
res.care <- resid(m.care)
bptest.sarlm(m.care)

# regimes espaciais
# spatial regimes
m.regime <- lagsarlm(razmam10a11 ~ 0 +
                       (one+log(popg2010)+pibpc+GINI2010+razurb+cobpsf+razrad10+razapmam10):
                       (REG_NAME), data=regsaude,w.list)
summary(m.regime)
summary(impacts(m.regime, listw=w.list,R=100),zstats=TRUE)
spatialchow.test(m.care,m.regime)

## etapa 3 - blocos socioecon�mico, assistencial e de utiliza��o de sevi�os
## step 3 - socioeconomic, healthcare and service utilization blocks

# sem ajustes espaciais
# without spatial adjusting
m.blocos <- lm(razmam10a11 ~ log(popg2010)+pibpc+GINI2010+razurb+
                 cobpsf+razrad10+razapmam10+
                 mampmam10, data=regsaude)
summary(m.blocos)
AIC(m.blocos)
res.blocos <- resid(m.blocos)
bptest(m.blocos)
lm.moran.blocos <- lm.morantest(m.blocos,w.list,alternative="two.sided")
lm.moran.blocos
lagrange.blocos <- lm.LMtests(m.blocos,w.list,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
lagrange.blocos
crPlots(m.blocos)

# com ajuste para depend�ncia espacial (modelo SAR)
# adjusting for spatial dependence (SAR model)
m.sar <- lagsarlm(razmam10a11~log(popg2010)+pibpc+GINI2010+razurb+
                    cobpsf+razrad10+razapmam10+
                    mampmam10, data=regsaude,w.list)
summary(m.sar)
summary(impacts(m.sar, listw=w.list,R=100),zstats=TRUE)
res.sar <- resid(m.sar)
bptest.sarlm(m.sar)

# ajuste para depend�ncia e heterogeneidade espacial (modelo SAR e inclus�o de vari�vel indicadora de grandes regi�es)
# adjusting for spatial dependence and heterogeneity (SAR model with inclusion of variable indicating great regions)
m.sar <- lagsarlm(razmam10a11~REG_NAME+log(popg2010)+pibpc+GINI2010+razurb+
                    cobpsf+razrad10+razapmam10+
                    mampmam10, data=regsaude,w.list)
summary(m.sar)
summary(impacts(m.sar, listw=w.list,R=100),zstats=TRUE)
res.sar <- resid(m.sar)
bptest.sarlm(m.sar)

# regimes espaciais
# spatial regimes
m.regime <- lagsarlm(razmam10a11 ~ 0 +
                       (one+log(popg2010)+pibpc+GINI2010+razurb+cobpsf+
                          razrad10+razapmam10+mampmam10):
                       (REG_NAME), data=regsaude,w.list)
summary(m.regime)
summary(impacts(m.regime, listw=w.list,R=100),zstats=TRUE)
spatialchow.test(m.sar,m.regime)
