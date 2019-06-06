#####Ejercicio 2 de la tarea 2 de Computo Estadistico
##### Regresion Bootstrap

#Introducimos los datos
pubs<-c(35,99,163,214,326,344,454,1054)
mortality<-c(957,2990,2017,4025,5997,5007,7012,6022)

#Generamos un dataframe con los datos
x<-as.data.frame(cbind(pubs,mortality))

#Generamos nuestro modelo
model1 = lm(mortality ~., data=x)
summary(model1)
#Obtenemos los coeficientes del modelo
betahat = coef(model1)  

#Fijamos una semilla para permitir reproducibilidad
set.seed(100) 

#Aqui vamos a almacenar los vectores de coeficientes de regresion en cada fila 
bstar = NULL
n = length(x$mortality)
#Numero de remuestreos a hacer
B=1000   
for(draw in 1:B){
  Dstar = x[sample(1:n, size=n, replace=T),]
  #Generamos el modelo con los datos obtenidos por el remuestreo
  model=lm(mortality ~., data=Dstar)    
  bstar=rbind(bstar, coef(model))    
}

#Generamos los nuevos coeficientes a partir de los obtenidos por bootstrap
boost.coef<-apply(bstar, 2, mean)

# INTERVALOS DE CONFIANZA (para el numero de pubs) 
#Para el modelo de regresion lineal
confint(model1)

# Para el parametro obtenido por bootstrap
#Ordenamos los valores obtenidos por bootstrap para el parametro del numero de pubs
sorted.param<-sort(bstar[,2])

# Calculamos los limites inferior y superior para establecer los CI
bounds<-c(B*(0.05/2), B*(1-(0.05/2)))

#Calculamos los CI por percentiles
boost.ci<-c(sorted.param[bounds[1]], sorted.param[bounds[2]])

#Graficamos los resultados
plot(pubs, mortality, ylim=c(0,8000), xlim=c(0,1200), 
     xlab="Number of pubs", ylab="Deaths per annum", 
     col="tomato1", pch = 16, cex = 1.3, main="OLS vs Boostrap Regression")
abline(betahat[1], betahat[2], col="red3")
abline(boost.coef[1], boost.coef[2], col="royalblue4")
legend(750, 2000, legend=c("Regresion lineal", "Boostrap Regression"), 
       col=c("red3", "royalblue4"), lty=1)
