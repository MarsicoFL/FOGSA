
# Cargando librerias 
#Primero cargamos las librerias necesarias. En caso de no estar instalados los paquetes se preguntará automáticamente si se quieren instalar. De no darse esto último quitar el # de las lineas install.packages y correr el código.
#install.packages("forrel")
library(forrel)
#install.packages("plotly")
library(plotly)

## Cargando el pedigrí
#En esta linea se cargará el pedigrí desde el archivo ".fam". El mismo es un archivo de familias con una base de datos de frecuencias alélicas y un pedigrí cargado en el módulo DVI. Entre los símbolos $ se encuentra el nombre del pedigrí y se pide cargar el de referencia (recordar que en el módulo de DVI se arman dos por cada grupo familiar en función de las hipótesis que se contrastan). Al final se hace el plot.
#Nota: el archivo que cargamos acá es Ejemplo.fam, es importante que se encuentre en la misma carpeta donde se encuentra este script.
x = readFam("Ejemplo.fam", useDVI = T, verbose = FALSE)$Referencia$`Reference pedigree`
x = relabel(x, new = c(1:11)) #relabeling the name of the family members
plot(x, shaded = typedMembers(x))

## Estimación del IBD (Identical by descent)
#Acá calculamos el IBD entre todos los pares de individuos que componen el pedigrí. Aquellos sin genótipar apareceran en U-N. Los códigos son: MZ gemelos monocigoticos; PO padres e hijos; H medios hermanos; U tio sobrino; G abuelo; S hermano; FC primos hermanos; U-N no relacionados.
k = IBDestimate(x)
showInTriangle(k, labels = TRUE)

## Power plot
#Graficamos el poder estadístico (Poder de Exclusión y Poder de Inclusión). Una de las opciones es simular (condicionalmente) como se modificaría agregando nuevos individuos genotipados. Para seleccionar a quienes se debe modificar la sección sel = list("seleccionado1"; "seleccionado2"; etc). 
sel = list("5", c("5", "6"),"8","11")
#sel = list("5")
simData = MPPsims(x, nProfiles = 1, selections = sel, lrSims = 1, thresholdIP = 10000, seed = 123, missing = "9")
powerPlot(simData, type = 1)


## Cálculo de los errores y distribuciones
#Dado la demora en simular muchos casos partimos del output del punto anterior (archivo sim.csv). El mismo posee dos columnas: Ejemplo_TP indica los valores simulados cuando POI es MP; Ejemplo_RP indica aquellos obtenidos cuando POI no es MP. 
sim <- read.delim("sim.csv")
attach(sim)
TPED = Ejemplo_TP
RPED = Ejemplo_RP
nsimul = 10000

## Estadística descriptiva
#Analizamos las medidas de tendencia central, etc.
summary(TPED) 
summary(RPED) 

## Distribuciones de Log10(LR)
#Generamos los plots de las distribución del LR bajo las dos hipótesis planteadas.
plot(density(log10(as.numeric(unlist(TPED)))), xlab= "log10 (LR)", ylab= "Densidad", 
     col=1, lwd=2, main="", xlim=c(min(log10(RPED))-1,max(log10(TPED)+1)), ylim=c(0,0.7))
points(density(log10(RPED)), col=2, lty=1, lwd=2, type= "l")
  legend("topright", c("H1: POI es MP", "H2: POI no es MP"), 
       lwd=2,lty=1:1, col=1:2)

## Calculo de falsos positivos (FPR) y falsos negativos (FNR)
#Cada valor de LR posee un FPR y FNR asociado. Por lo tanto tomando valores de 1 a 10000 cálculamos estos dos parámetros.

ValoresLR = seq(1, 10000, length.out=10000) 
FPs = 0
FNs = 0
for(i in 1:10000) { FPs[i] = sum(RPED > ValoresLR[i]); #False positives.
                    FNs[i] = sum(TPED < ValoresLR[i])} #False negatives.

## Gráfico del umbral.
#Considerando los dos parámetros antes mencionados generamos un gráfico donde se indica el FNR y FPR para cada LR tomado como umbral. Esto permite dar cuenta de la probabilidad de cometer errores que tenemos al seleccionar un determinado umbral.

Datos = data.frame(x = ValoresLR, y= FPs/nsimul, z= FNs/nsimul, w=FPs)
plot(x = FNs/nsimul, y=FPs/nsimul, lwd=2, ylab= "FPR", xlab = "FNR", type = "l")
p <- plot_ly(
  Datos, x = Datos$z, y = Datos$y,
  # Hover text:
  text = ~paste("LR: ", Datos$x,
          "<br>E(FP) :", Datos$y*nsimul),
  color = Datos$z, size = 1
)
p

## Selección de LR umbral (DT).
#Definimos al origen de coordenadas (0,0) como punto óptimo del gráfico de decisión dado que allí FPR y FNR son iguales a cero. De esta manera calculamos la distancia al punto óptimo para cada LR en el gráfico de decisión. Importante: notar que en este momento de puede dar un peso a los errores, esto intenta modelar cuan abiertos estamos a cada tipo de error (FP o FN). 

weight = 10 #The differential weight between false positives and PMI.
Dis = sqrt(((FNs/nsimul))^2+((weight*FPs/nsimul)^2)) #Weighted Euclidean Distance
Tabla = data.frame(x=ValoresLR, y= Dis)
LRdt = which.min(Tabla$y)
plot(x=ValoresLR, y=Dis, lwd=2, ylab= "Distance" ,xlab= "LRs" , type= "l", 
     xlim= c(0,100))

## LRdt
#Obtenemos el DT, valor umbral óptimo, considerando aquel que minimice la distancia al origen de coordenadas. 
LRdt = which.min(Tabla$y)
A = "Taking into account the weighted euclidean distance, the LRdt value is: " 
print(c(A, LRdt))

## Coeficiente de correlación de Matthews (CCM)
#Estimamos el CCM (rφ) que no es más que el coeficiente de correlación de pearson en el caso especial de analizar casos en donde hay decisiones binarias (entre dos hipótesis). Un CCM igual a 1 quiere decir que nunca nos confundimos, igual a -1 nos confundimos siempre y 0 indica que nos confundimos igual que el azar en nuestra toma de decisiones con los criterios elegidos.

LRmin = LRdt - 5
LRup = LRdt + 30
ValoresLR = seq(LRdt, LRup, length.out=31) 
MCC = 0
for (i in 1:31) { FPs = 0; FNs = 0; TNs = 0;TPs = 0;
                  FPs[i] = (sum(RPED > ValoresLR[i]))/nsimul;
                  FNs[i] = (sum(TPED < ValoresLR[i]))/nsimul;
                  TPs[i] = (sum(TPED > ValoresLR[i]))/nsimul;
                  TNs[i] = (sum(RPED < ValoresLR[i]))/nsimul; 
MCC[i] = (TPs[i]*TNs[i]-FPs[i]*FNs[i])/(sqrt(TPs[i]+FPs[i])*sqrt(TPs[i]+FNs[i])
                                      *sqrt(TNs[i]+FPs[i])*sqrt(TNs[i]+FNs[i]))
                                                          }
plot(ValoresLR, MCC, lwd=2, ylab= "Matthews Corr" ,xlab= "LRs", 
     type= "l", xlim= c(LRdt,LRup), ylim = c(min(MCC),max(MCC)))
