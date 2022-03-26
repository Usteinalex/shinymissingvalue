bd<-read.table('D:\\UBS\\MASTER 2\\Taaf\\Ulco_lisic\\Acompleter.csv', sep=",", header=TRUE)
View(bd)
#describe(bd)
summary(bd)
install.packages("visdat")
library(visdat)
vis_miss(bd)
X11()
ts.plot(bd$diat)
ts.plot(bd$turb)
ts.plot(bd$sal)
#bd$diat[is.na(bd$diat)] <- median(bd$diat,na.rm = T)
# AZ TTP
###### Scenario de completion
# La methode de remplacement par la moyenne
install.packages("zoo")
library(zoo)
bd$diat <- na.aggregate(bd$diat, FUN = mean)
bd$turb <- na.aggregate(bd$turb, FUN = mean)
bd$sal <- na.aggregate(bd$sal, FUN = mean)
#df_miss$y <- na.aggregate(df_miss$y, FUN = mean)

# Repetition de la dernière valeur observée
bd$diat<-na.locf(bd$diat)
bd$turb<-na.locf(bd$turb)
bd$sal<- na.locf(bd$sal,fromLast = TRUE)

## interpolation entre point

# interpolation linéaire
z_lin <- na.approx(z)
diat_lin <- na.approx(bd$diat)
turb_lin <- na.approx(bd$turb)
sal_lin <- na.approx(bd$sal)
# interpolation spline
z_spline <- na.spline(z)
diat_spline <- na.spline(bd$diat)
turb_spline <- na.spline(bd$turb)
sal_spline <- na.spline(bd$sal)

data_z <- data.frame(x =1:length(bd$diat) , z = bd$diat,z_spline = diat_spline)
# plot
library(dplyr)
library(plotly)
p <- plot_ly(data_z, x = ~x) %>%
  #add_trace(y = ~z_lin, name = "linéaire", type = "scatter", mode = "lines") %>%
  add_trace(y = ~z_spline, name = "spline", type = "scatter", mode = "lines") %>%
  add_trace(y = ~z, name = "z", type = "scatter", mode = "lines+markers")
X11()
p

#### répétition d’une séquence
# La fonction na.StructTS() du package {zoo} permet de reproduire la 
# saisonnalité de la série en utilisant le filtre saisonnier de Kalman
zout <- na.StructTS(z)
bd$diat<-na.StructTS(bd$diat)
bd$turb<-na.StructTS(trub)
bd$sal<- na.StructTS(sal)
