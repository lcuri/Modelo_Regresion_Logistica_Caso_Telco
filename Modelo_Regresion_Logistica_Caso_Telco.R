#                                             MODELO DE REGRESION LOGISTICA - CASO CHURN TELCO
#                                                                                                       By Luis Curi

## 1. PREPARACION DEL ENTORNO 
##################################################################################################################################
#situar directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##Limpiar el entorno
rm(list = ls())

##llamar a la base
telco<-read.csv("telcoChurn.csv")

#################################################################################################################################
## 2. EXPLORACION DE LOS DATOS
#################################################################################################################################
  #a. Explorar los datos
str(telco)
summary(telco)

library(funModeling)
df_status(telco)

  # B.verificar la existencia de outliers

    ##apply aplica una funcion a un df. function crea la funcion, 1 = aplica sobre filas,2 = aplica sobre columnas
apply(telco, 2,function(x){sum(is.na(x))})
    ##md.pattern plotea los outliers por columna (se recomienda rotar el nombre de las columnas)
mice::md.pattern(telco, rotate.names = TRUE)
    ##missmap tambien sirve para ver outliers, te ubica aprox en que observacion se encuentra (por variable)
library(Amelia)
missmap(telco, col = c("black","white"))

################################################################################################################################
## 3. CONVERSION Y ADECUACION DE DATOS
#####################################################################################################################
   #Se me solicita que es necesario que la variable tenure(tenencia) tenga rangos definidos las cuales se me brinda

   ## creamos la funcion para la columna tenure
group_tenure<- function(tenure)
{
  if(tenure >= 0 && tenure <= 6){return("0 a 6 meses")}
  else if(tenure > 6 && tenure <= 12){return("6 a 12 meses")}
  else if(tenure > 12 && tenure <= 24){return("12 a 24 meses")}
  else if(tenure > 24 && tenure <= 36){return("24 a 26 meses")}
  else if(tenure > 36 && tenure <= 48){return("36 a 48 meses")}
  else if(tenure > 48 && tenure <= 62){return("48 a 62 meses")}
  else if(tenure > 62){return("mayor a 62 meses")}
}
  ##Aplicamos la funcion para la columna Tenure (sapply usado para aplica la funcion a una sola columna) a diferencia de apply que aplica 
  ## la funcion a más de una columna o fila
telco$tenure_intervals <- sapply(telco$tenure ,group_tenure)

  ##Identificamos las variables con caracteres no uniformizadas
 apply(telco[,c(2,4,5,7,8,9,10,11,12,13,14,15,16,17,18,21)], 2, function(x) {unique(x)}) 
  ## aplicamos la uniformizacion
telco$MultipleLines[telco$MultipleLines =="No phone service"]<-"No"
telco$OnlineSecurity[telco$OnlineSecurity=="No internet service"]<-"No"
telco$OnlineBackup[telco$OnlineBackup=="No internet service"]<-"No"
telco$DeviceProtection[telco$DeviceProtection=="No internet service"]<-"No"
telco$TechSupport[telco$TechSupport=="No internet service"]<-"No"
telco$StreamingTV[telco$StreamingTV=="No internet service"]<-"No"
telco$StreamingMovies[telco$StreamingMovies=="No internet service"]<-"No"

  #convertir datos tipo caracter a factor 
str(telco)
  ## Aplicamos un for i a las columnas que necesitan pasar de caracter a factor
for (i in c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,21,22))
{
  telco[,i]<-as.factor(telco[,i])
}
  #verificamos lo realizadpo
str(telco)

  ##quitamos las columnas que no necesitamos
telco_1 <- telco[,c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
  ##ordenamos la columnas 
telco_1 <- telco_1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,19)]

  #renombramos la columnas para entenderlas mejor
names(telco_1)<-c("genero","Jubilado","partner","dependientes","servicioTelefono","multiplesLineas",
                "tipoInternet","servicioSeguridad","servicioBackups","servicioProteccionDispositivo",
                "servicioSoporteTecnico","servicioStreaming","servicioPeliculas","tipoContrato",
                "Factura_papel","metodoPago","cargosMes","totalCargos","permanencia","churn")

  ##como la cantidad de valores perdidos es minimo, eliminamos
telco_1<-na.omit(telco_1)

#verificamos lo realizado
df_status(telco_1)

##########################################################################################################
## 4. ANALISIS EXPLORATORIO
##########################################################################################################
library(funModeling) ##usado para realizar auditoria de datos(calidad de datos)
  ##Estadisticos basicos y presencia de NA y ceros
summary(telco_1) #resumen estadistico basico
df_status(telco_1) #cantidad de ceros y NA
plot_num(telco_1) #comportamiento de datos
  ##graficos de variables cuantitativos
boxplot(telco_1[,c(2,17,18)], las = 2)
  ##graficas de variables cualitativos
par(mfrow = c(1:2))
for (i in c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,20,21))
{
  barplot(table(telco_1[,i]),col=c("dodgerblue4","burlywood"), main = paste("grafico de", names(telco_1[i])))
}
  ##grafica de la variable predictora
par(mfrow = c(1:2))
barplot(prop.table(table(telco_1$churn)),col=c("dodgerblue4","burlywood"), 
        main = paste("grafico de", names(telco_1[20])))

  #Normalizacion de datos 
library(clusterSim)
  #evaluo como se comportan mis datos una vez normalizados
telco_norm <- data.Normalization(telco_1[,c(17,18)], type = "n4", normalization = "column")
boxplot(telco_norm)
  #Evaluacion de outliers
  #vamos a ver la presencia de outliers en la data 
telco_1$outlier=FALSE ##creo una columna nueva con valores Falso
for (i in 1:ncol(telco_1)-1) #for i considerando todas la columnas menos la columna donde se almacena el resultado
{
  columna = telco_1[,i] #parametro para evaluar columna por columna
  if (is.numeric(columna)) #si la columna es numerica
  {
    media = mean(columna)  #entonces calcula su promedio
    desviacion = sd(columna) #calcula su desviacion standar
    telco_1$outlier = (telco_1$outlier | columna>(media+3*desviacion) | columna<(media-3*desviacion)) 
    ## si el dato es mayor o menor a la media + 3 desviaciones standar es considerado outlier.
    ##se una de las 2 condiciones es verdadero, entonces el valor en la columna será TRUE (existencia de outlier)
    ## de lo contrario FALSE (operaciones booleanas)
  }
}
  #contamos la cantidad de outliers que tenemos
table(telco_1$outlier) #no tenemos outliers

  #quitamos la columna evaluada por que ya nos sirve
telco_1$outlier<-NULL
  #con los datos listos para aplicar el modelo podemos ahora si, ejecutar el modelo

## El area especializada recomienda crear una nueva variable: con el objetivo de que el modelo tenga mas informacion
for(i in 1:nrow(telco_1)){
  telco_1$numServiciosContratados[i]<-sum(ifelse(telco_1$servicioSeguridad[i]=="Yes",1,0),
                                          ifelse(telco_1$servicioBackups[i]=="Yes",1,0),
                                          ifelse(telco_1$servicioProteccionDispositivo[i]=="Yes",1,0),
                                          ifelse(telco_1$servicioSoporteTecnico[i]=="Yes",1,0),
                                          ifelse(telco_1$servicioStreaming[i]=="Yes",1,0),
                                          ifelse(telco_1$servicioPeliculas[i]=="Yes",1,0))
}

################################################################################################################
## 5. BALANCEO DE DATOS
################################################################################################################
  ## Corroborar si la variable predictora no sufre de desbalance de categorias
table(telco_1$churn)
prop.table(table(telco_1$churn))

  ##podemos ver que la proporcion de categorias es mayor al 5% para la categoria minoritaria(yes>26%)
  ##No se requiere usar ninguna tecnica de balanceo de datos
################################################################################################################
## 6. SEPARAR DATA TEST Y ENTRENAMIENTO
################################################################################################################
library(caret)
# Muestreamos los datos: 70 training - 30 test
indice = createDataPartition(telco_1$churn, p = 0.8, times = 1, list=FALSE)
datosTrain = telco_1[ indice,]
datosTest = telco_1[-indice,]

##verificamos el correcto balanceo de las particiones
table(datosTrain$churn)
table(datosTest$churn)

################################################################################################################
## 7. SELECCION DE VARIABLES
################################################################################################################

  #Selección de variables: Usamos el algoritmo de boruta para la selección de variables(usado para random forest, pero su fin es seleccion de variables)
  #
#install.packages("Boruta")
library(Boruta)
boruta_output <- Boruta(churn ~ ., data=na.omit(datosTrain), doTrace=2)  # aplicacion del algoritmo - omite NA,realiza 2 iteraciones
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  #extraer variables confirmadas y tentativas
print(boruta_signif)  # mostrar las variables
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plotear la importancia de variables
boruta_signif

                  ##podemos observar que la unica variable que es irrelevante para el modelo es "genero"


importancia <- data.frame(importancia = apply((data.frame(boruta_output$ImpHistory)),2,mean)) #ordenar las variables segun importancia(como en el plot)
decision <- data.frame(decision = boruta_output$finalDecision) #guardar en el df desicion, las variables y sus resultados

final.boruta <- TentativeRoughFix(boruta_output) #seleccion final
print(final.boruta) #indican la cantidad de variables confirmadas y las variables que no se deben incluir

boruta.df <- attStats(final.boruta) #mostrar estadisticas importantes de la desicion
print(boruta.df)
                    ##podemos observar que la unica variable que es irrelevante para el modelo es "gender"

  
################################################################################################################
## 8. APLICACION DEL MODELO: 
################################################################################################################

set.seed(123) #colocar una semilla

  ##aplicacion del modelo

# Logistic regression model
#   (1) TRAIN: "estudiando" para el examen
modelo <- glm(churn ~ Jubilado+partner+dependientes+servicioTelefono+multiplesLineas+tipoInternet+servicioSeguridad+
                servicioBackups+servicioProteccionDispositivo+servicioSoporteTecnico+servicioStreaming+
                servicioPeliculas+tipoContrato+Factura_papel+metodoPago+cargosMes+totalCargos+
                permanencia+numServiciosContratados,
              family=binomial(link="logit"),
              data=datosTrain)
print(summary(modelo))

    ##Podemos observar que la variable creada: num servicios contratados era relevante en la seleccion de variables,
    ##pero al aplicar el algoritmo, genero NA, esto se debe que a pesar de ser relevante, el modelo no lo considera 
    ##por que existe multicolinealidad, ya qu solo es una suma de 3 variables. En consecuencia: no sirve para el modelo


###################################################################################################################
## 9. EVALUACION DEL MODELO
##################################################################################################################


    ##evaluo mi modelo con la data de test
predicciones <- predict(modelo,newdata=datosTest,type="response")
predicciones

   ##Si la prediccion es mayor al 50% entonces es yes(se fue), y no(permanece)
datosTest$predichos <- as.numeric(predicciones > 0.5) #para fines comerciales está informacion puede ser usada para proponer
                                                   ##ofertas en funcion a su probabilidad de fuga (50%,70%, 90%)

  
   ## convertir los boobleanos de los valores reales para construir la matriz de confusion
datosTest$predichos[datosTest$predichos=="0"] <- "No"
datosTest$predichos[datosTest$predichos=="1"] <- "Yes"
   ##contruir la tabla para la matriz : valores reales vs valores predichos
tabla_matriz<-table(datosTest$churn,datosTest$predichos)
  
   ##Construccion de la matriz de confiusión
acc_1<-confusionMatrix(tabla_matriz, positive = "Yes") #usamos la funcion confusiomatrix e indicamos cual es el valor positivo para 
                                                       #que la matriz se contruya en base a ello
acc_1

###############################################################################################################
## 10. CALCULO DE LA CURVA ROCR
###############################################################################################################
library("ROCR")
pred_1<-prediction(predicciones,datosTest$churn)
perf_1<-performance(pred_1,"tpr","fpr")
auc_1<-100*as.numeric(performance(pred_1, "auc")@y.values)
auc_1

##plotear el AUC
plot(perf_1, type="o", main= paste("Área bajo la curva = ",round(auc_1),'%'),col="blue")
abline(a=0,b=1,color="red")

   ##GINI
gini_1<- 2*(auc_1-50)
gini_1

  ##f1 SCORE
acc_1$byClass[7]

###############################################################################################################
## 11. EVALUACION DEL GRADO DE IMPORTANCIA DE LAS VARIABLES SEGUN EL MODELO
##############################################################################################################

   # a cuanto  contribuye cada variable a clasificar o predecir la variable target?
library(caret)
imp <- varImp(modelo) #Varimp calcula la importancia de variabilidad para modelos de clasificacion o regresion
arrange(imp, -Overall)

  # Ordenamos las variables por importancia al modelo
imp_list <- rownames(imp)[order(imp$Overall, decreasing=TRUE)] #ordenamos en funcion del overall 
pesoVariables <- as.data.frame(imp_list)
pesoVariables

