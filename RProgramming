######################     PARA EJECUTAR PULSAMOS CTRL+R (WINDOWS) O CMD+R (MAC)                                    ########

# Dónde estamos
getwd()

# Poner bien el directorio de trabajo. Por consola o cambiar dir desde el menú. 
# El directorio de trabajo tiene que ser el mismo que dónde tenemos el script
setwd()

#################     INSTALAR Y CARGAR PAQUETES (no siempre necesario, ESTO ES UN EJEMPLO)                    ########
install.packages("UsingR") 
install.packages(c("devtools","pacman"))
library(UsingR)
library(devtools)
library(pacman)
library(ggplot2)

##################    MANUAL
help.start()
help(hist)

######################################################################################################################
###############################                R COMO CALCULADORA
1+1
2+3*4
3^2
exp(1)
sqrt(10)
pi
2*pi*6378 #circunferencia de la Tierra

######################################################################################################################
###############################               VARIABLES
# Numeric: contienen números con decimales
# Booleanos: TRUE or FALSE
# Strings: secuencias de caracteres
x<-1
y<-3
z<-4
x*y*z
x*y*Z #Hay distinción entre mayúsculas y minúsculas!
This.Year<-2023 #pueden incluir puntos, guiones bajos pero nunca "-". Y nunca deben empezar por números

#######################################################################################################################
###############################               VECTORES
# Series de números
# Diferentes maneras de crearlos y operar con ellos
rep(1,10)
seq(2,6)
seq(4,20,by=4)
x<-c(2,0,0,4)
y<-c(1,9,9,9)
x+y
x*4
sqrt(x)

# Navegar con ellos
x[1]
x[-1]
x[1]<-3;x
x[-1]<-5;x
y<9
y[4]=1
y<9
y[y<9]=2
y

###########################################################################################################################
############################                    DATA FRAMES
# Son agrupaciones de vectores
# Es de lo más fácil de manejar en R
bp<-read.table("bp.txt",header=T)
bp
bp["WEIGHT"]
bp[,2]
bp$WEIGHT
bp$WEIGHT[2]

bp[,-2] #aquí no mostramos parte de los datos
###########################################################################################################################
###############################             	LISTAS
# Colecciones de variables
# Se parecen mucho a las data frames

# Tenemos unos genes asociados a tres grupos
geneset1<-c(1,4)
geneset2<-c(2,5,7,8)
geneset3<-c(1,5,6)

# Almacenamos los datos en una lista
genesets<-vector("list",3)
genesets[[1]]<-geneset1
genesets[[2]]<-geneset2
genesets[[3]]<-geneset3
genesets

# Ahora le ponemos nombre a los grupos
names(genesets)=paste0("set",1:3)
genesets

# Ahora nombramos a los genes
genenames=paste0("gene",1:10)
genesets[[1]]=genenames[geneset1]
genesets[[2]]=genenames[geneset2]
genesets[[3]]=genenames[geneset3]
genesets

# Extraer elementos de la lista
genesets[1]
genesets[[1]]
genesets[[1]][1]
unlist(genesets[1])[1]
length(genesets)
length(genesets[[2]])

# Crear una lista vacía
lista_vacia<-list()
# Ahora con tres elementos
lista_vacia<-vector("list",length=3)
# Ahora la llenamos con un bucle
for(j in 1:3){
lista_vacia[[j]] <- c(1,2,3*j)
}
lista_vacia

# Añadir elementos. Para eso el data frame es lo mismo que la lista
lista_vacia[[4]] <- data.frame(x=c(8,5,3),y=c(7,9,1))
lista_vacia

# Eliminar elementos
lista_vacia[[2]]<-NULL
#lista_vacia[-2] #equivalente
lista_vacia

######################################################################################################################
#############################           IF ELSE IFELSE
if(4 > 5) {
  "Verdadero"
} else {
  "Falso"
}

num <- 1:8

ifelse(num %% 2 == 0, "Par", "Non")

######################################################################################################################
#############################            LOOPS

# For
dado <- 1:6
for(cara in dado) {
  print(cara ^ 2)
}

mi_vector<-NULL
for(cara in dado) {
  mi_vector[cara] <- cara ^ 2
}
mi_vector

# While
umbral <- 5
valor <- 0

while(valor < umbral) {
  print("Todavía no.")
  valor <- valor + 1
}

while(1 < 2) {
  print("Presiona ESC para detener")
}

######################################################################################################################
##########################             BREAK Y NEXT
for(i in 1:10) {
  if(i == 3) {
    break
  }
  print(i)
}

numero <- 20
while(numero > 5) {
  if(numero == 15) {
    break
  }
  numero <- numero - 1
}
numero

for(i in 1:4) {
  if(i == 3) {
    next
  }
  print(i)
}


######################################################################################################################
#####################                     FUNCIONES

area_cuad <- function(lado1, lado2) {
  lado1 * lado2
}
area_cuad(lado1 = 4, lado2 = 6)
area_cuad(lado1 = 36, lado2 = 36)
area_cuad(lado1 = 14)
area_cuad(128, 64)

area_prisma <- function(arista1, arista2, arista3) {
  area_cuad(arista1, arista2) * arista3
}
area_prisma(3, 6, 9)

crear_histograma <- function(datos, nombre) {
  media <- mean(datos)
  desv_est <- sd(datos)
  
  hist(datos, main = nombre, xlab = "Datos", ylab = "Frecuencia", col = "gold")
  abline(v = media, col = "red")
  abline(v = media + (desv_est * c(1, -1)), col = "blue")
}

ingreso <- rnorm(1500, mean = 15000, sd = 4500)

# Resultado
ingreso[1:10]
crear_histograma(ingreso, "Ingreso")

peso <- rnorm(75, mean = 60, sd = 15)
crear_histograma(peso, "Peso")

#######################################################################################################################
##################################       APPLY y  LAPPLY

# APPLY apply(x,margin,fun) #en Margin, 1 es filas y 2 columnas. Fun es la funcion que como requisito siempre tiene que admitir funciones

mi_df <- data.frame(v1 = 1:3, v2 = 4:6)
mi_df
# Coerción a matriz
mi_matriz <- as.matrix(mi_df)

# Verificamos que sea matriz
is.matrix(mi_matriz)
class(mi_matriz)
mi_matriz

apply(mi_matriz,1,mean)
apply(mi_matriz,2,sum)

# LAPPLY vale para listas lapply(x,fun)
trees[1:5,] #esta por defecto en R
lapply(trees,mean)
arboles <- lapply(X = trees, FUN = mean)
class(arboles)

# Podemos usar lapply en lugar de un bucle
mi_vector <- 6:12
resultado <- NULL
posicion <- 1
for(numero in mi_vector) {
  resultado[posicion] <- sqrt(numero)
  posicion <- posicion + 1
}
resultado

resultado <- NULL
resultado <- lapply(mi_vector, sqrt)
resultado
as.numeric(resultado)

########################################################################################################################
####################                     IMPORTAR Y EXPORTAR

# Descargar datos https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Diagnostic%29 (diagnosticos de cancer para IA)
download.file(
  url = "https://raw.githubusercontent.com/jboscomendoza/r-principiantes-bookdown/master/datos/breast-cancer-wis.data", 
  dest = "breast-cancer-wis.data"
)
bcancer <- read.table(file = "breast-cancer-wis.data")
head(bcancer)
bcancer <- read.table(file = "breast-cancer-wis.data", header = FALSE, sep = ",")
head(bcancer)
nombres <- c("id", "clump_t", "u_csize", "u_cshape", "m_adh", "spcs", "b_nuc","b_chr", "n_nuc", "mit", "class")
bcancer <- read.table(file = "breast-cancer-wis.data", header = FALSE, sep = ",",col.names = nombres)
head(bcancer)
class(bcancer)

# Exportar datos
write.table(x = iris, file = "iris.txt", sep = ",", row.names = FALSE, col.names = TRUE)
iris_txt <- read.table(file = "iris.txt", header = TRUE, sep = ",")
head(iris_txt)
write.csv(x = iris, file = "iris.csv", row.names = FALSE) 
iris_csv <- read.csv("iris.csv")
head(iris_csv)

#######################################################################################################################
##############################           GRAFICOS
#### Para los colores que valen http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

download.file(
  url = "https://raw.githubusercontent.com/jboscomendoza/r-principiantes-bookdown/master/datos/bank.csv", 
  destfile = "bank.csv"
  )
readLines("bank.csv", n = 4) 
banco <- read.csv(file = "bank.csv", sep = ";")
head(banco)
dim(banco)
lapply(banco,class)
summary(banco)

# Histograma
hist(x=banco$age)
hist(x = banco$age, main = "Histograma de Edad", 
     xlab = "Edad", ylab = "Frecuencia")
hist(x = banco$age, main = "Histograma de Edad", 
     xlab = "Edad", ylab = "Frecuencia",
     col = "purple")
hist(x = banco$duration, main = "Histograma de Duration", 
     xlab = "Duration", ylab = "Frecuencia",
     col = "ivory")

# Plot 
plot(x = banco$education)
plot(x = banco$education, main = "Gráfica de Educacíón",
     xlab = "Nivel educativo", ylab = "Frecuencia", 
     col = c("royalblue", "seagreen", "purple", "grey"))

# Barplot
table(banco$education)
tab_banco <- table(banco$loan, banco$education)
tab_banco
barplot(tab_banco)
prop.table(tab_banco, margin = 1)
prop.table(tab_banco, margin = 2)
prop.table(tab_banco)
ptab_banco <- prop.table(tab_banco, margin = 2)
barplot(ptab_banco)
barplot(ptab_banco,  main = "Préstamos por nivel educativo",
     xlab = "Nivel educativo", ylab = "Proporción", 
     col = c("royalblue", "grey"))
unique(banco$loan)
barplot(ptab_banco,  main = "Préstamos por nivel educativo",
     xlab = "Nivel educativo", ylab = "Proporción", 
     col = c("royalblue", "grey"))
legend(x = "topright", legend = c("No", "Yes"), fill = c("royalblue", "grey"), 
       title = "Loan")

# Diagrama de dispersion
plot(x = iris$Petal.Length, y = iris$Petal.Width, col = iris$Species, 
     main = "Iris - Pétalo", xlab = "Largo", ylab = "Ancho")
legend(x = "topleft", legend = c("Setosa", "Versicolor", "Virginica"), 
       fill = c("black", "red", "green"), title = "Especie")

# Boxplot
#formula: Para esta función las fórmulas tienen el 
#formato y ~ x, donde x es el nombre de la variable continua a graficar, y la x es la variable que usaremos como agrupación.
boxplot(x = banco$age)
boxplot(formula = age ~ education, data =  banco)

######################################################################################################################

##################         UNA SOLA MUESTRA                                 ###############################################
            
# Suponemos que estamos estudiando la expresión de 10 genes

x<-c(3,6,3,6,7,1,4,7,4,4)
x
class(x)
x[1]
x[7]
x[0]
x[3:7]

# Para poner secuencias:
3:7
seq(3,7,1)

# Poner posiciones de la 1,3 y de la 5 a la 8
c(1,3,5:8)
x[c(1,3,5:8)]

# Saber qué datos cumplen una condición
x >= 4
x[x>=4] #solo nos devuelve los que dan TRUE
x[x>6]
x[x>4&x<=6]
y<-x[x>4&x<=6]
y
x[x<=4|x>6]
x[x==4]

# En qué posición tenemos ciertos valores que cumplen una condición
which(x==4)

# Ordenar un vector
sort(x) # Orden creciente
sort(x,index.return=TRUE) # Para sabe rqué posición ocupaban esos valores en el vector original
sort(x,decreasing=TRUE,index.return=TRUE) # Orden decreciente
x  # ¿Nos guarda el orden? Para eso hay que almacenarlo en una variable
z <- sort(x)
z

######################            EJERCICIO 1

# Introduce los siguientes datos:
x<-c(9,5,6,5,2,8,3,2,4,9,8,6,6,11,6,6,5,5,4,3,9,8,6,7,1,5,6,3,4,3,3,4,4,4,3,3,3,5,4,7,2,5,7,2,5,3,3,6,8,4,6,2,4,4,7,5,7,7,7,4,6,6,6,0,5,3,4,5,4,2,3,6,4,7,2,4,10,8,8,3,3,6,4,6,4,3,7,2,5,5,5,4,5,9,5,8,8,5,4,3)
x
#1.- ¿Cual es el valor medio, la varianza y la desviación estándar de estos valores?
mean(x)
var(x)
sd(x)

#2.- ¿Cuántos de los valores son 3?
length(x[x==3])
y<-x[x==3]
length(y)

#3.- ¿Cuál es el rango intercuartílico? Diferencia entre Q3 - Q1 (aprox el 50% observaciones). A más grande más dispersión
# El primer cuartil (Q1) que es el valor que separa el 25% inferior de los datos del 75% superior.
# El segundo cuartil (Q2), también conocido como mediana, que es el valor que separa el 50% inferior del 50% superior de los datos.
# El tercer cuartil (Q3) que es el valor que separa el 75% inferior del 25% superior de los datos.
IQR(x)
x
# formula Lp=(n+1)*(p/100)
y<-sort(x)#primero ordenamos los datos
y
n<-length(y)
n
q1 <- y[(n+1)*(25/100)];q1
q3 <- y[(n+1)*(75/100)];q3
q3-q1
#otra forma
q1<-quantile(x,c(0.25),type=6);q1
q3<-quantile(x,c(0.75),type=6);q3
q3-q1
#la mediana es lo mismo que el q2
mediana<-median(x)
mediana
q2<-quantile(x,c(0.50),type=6);q2

#4.- Ordena el vector y calcula las sumas acumuladas de los valores ordenados con la función base::cumsum
y<-sort(x)
suma_acumulada<-cumsum(y)
suma_acumulada

#5.- Obtén el mínimo y el máximo
min(x)
max(x)
summary(x)

##########################    EJERCICIO 2
#Generar valores aleatorios con una distribución normal de media 45 y sd de 2.3. 23489 valores a obtener.
x<-rnorm(23489,mean=45,sd=2.3)
x

#1.- ¿Cuántas valores de x son mayores o iguales de 39.4?
val<-x[x>=39.4]
length(val)

#2.- ¿Cuántos valores de x son estrictamente menores que 46?
length(x[x<46])

#3.- ¿Cuántos valores de x son estrictamente menores que 46 y mayores o iguales de 39.4?
length(x[x<46&x>=39.4])

#4.- ¿Cuántos valores son tales que su parte entera (por defecto) es igual a 40? Se puede utilizar también la función base::floor
length(x[x==40]) #¿qué pasa aquí?
help(floor)
z<-floor(x)
z
length(z[z==40])

######################################################################################################################
############################                  VARIAS MUESTRAS                              ###########################

# Suponemos que tenemos la expresión de 10 genes en 4 muestras. Distribución de Poisson. Lambda es la tasa promedio
set.seed(123)
(x=matrix(rpois(10*4,lambda=5),nrow=10))
class(x)

# Podemos añadir nombres  las columnas y filas
colnames(x)=c("m1","m2","m3","m4")
# colnames(x)=paste0("m",1:4)
rownames(x)<-paste0("gene",1:10)
x

# Buscar en la matriz
x[6,2]
x["gene6","m2"]
x[,2]
x[,"m2"]

#################################              EJERCICIO 3
# Crear una matrix con valores de distribución normal, media de 34 y sd de 3.21.
x<-matrix(rnorm(2345*122,mean=34,sd=3.21),nrow=2345)
x

#1.- ¿Cuántas columnas tiene la matriz?
ncol(x)

#2.- ¿Qué columna tiene la media más alta? Utiliza las funciones base::apply y base::which.max
colMeans(x)
y<-apply(x,2,mean) #el 2 indica columnas
which.max(y)
which.max(colMeans(x))

#3.- ¿Qué fila tiene la media más pequeña? Utiliza las funciones base::apply y base::which.min
rowMeans(x)
which.min(rowMeans(x))
y<-apply(x,1,mean)
which.min(y)

#4.- ¿Qué fila tiene el menor rango intercuartílico? Utiliza las funciones base::apply y base::which.min
which.min(apply(x,1,IQR))

#5.- ¿En cuántas ocasiones la primera columna es mayor o igual que la segunda columna de la matriz
y<-x[,1]>=x[,2]
length(y)

######################################################################################################################
#################################         DATOS GOLUB                           ######################################
# Los datos golub son datos de expresión de genes que se emplearon en "Molecular classification of cancer: class 
#discovery and class prediction by gene expression monitoring. Science 1999.
# Son 3051 genes de 38 pacientes con leucemia. 27 de esos pacientes tienen una leucemia linfoblástica aguda (ALL) y el resto (11) leucemia mielode aguda (AML).
# El tipo de tumor está indicado en el vector golub.cl donde ALL es 0 y AML es 1. Los nombres de los genes son golub.gnames
# Las filas son los genes y las columnas las muestras

if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("multtest")

library("multtest")
data(golub)
summary(golub)
head(golub)
class(golub)
golub.cl
class(golub.cl)
nrow(golub);ncol(golub);dim(golub) #dim es para las dimensiones

# Vamos a crear un factor para ALL y AML. Facilita el procesado de los datos
golub.fac <- factor(golub.cl,levels=0:1,labels=c("ALL","AML"))
golub.fac
table(golub.fac) #Para ver cuántas tenemos de cada uno
summary(golub.fac)

# Creamos nuestro primer diagrama de barras
library(ggplot2) #en caso de no haberla cargado antes. Viene con el paquete pacman
primer_df <- data.frame(golub.fac)
ggplot(primer_df,aes(x=golub.fac))+geom_bar()
dev.off()
# ¿Qué pasa si sustituimos golub.fac por golub.cl?
primer_df <- data.frame(golub.cl)
ggplot(primer_df,aes(x=golub.cl))+geom_bar()
dev.off()

# Ver expresión de un gen en una muestra y en todas. Representarlo en un gráfico.
golub[2000,12]
golub[2000,]
#calculamos los ejes x e y
muestra<-1:ncol(golub)
y2000<-golub[2000,]
df<-data.frame(muestra,y2000)
ggplot(df,aes(x=muestra,y=y2000))+geom_point()
dev.off()

# Ver expresiones de los pacientes ALL y AML para el gen 2000
golub[2000,golub.fac=="ALL"]
golub[2000,golub.fac=="AML"]
df <- data.frame(muestra,y2000,tipo=golub.fac)
ggplot(df,aes(x=muestra,y=y2000,color=tipo))+geom_point()
dev.off()

# Otra opción es representar las muestras AML y ALL por separado
df<-data.frame(muestra,y2000,tipo=golub.fac)
ggplot(df,aes(x=muestra,y=y2000,color=tipo))+geom_point()+xlab("Número de muestra")+ylab("Gen en fila 2000")+facet_grid(tipo~.)
dev.off()

# Otra opción es colocar todo horizontal
df<-data.frame(muestra,y2000,tipo=golub.fac)
ggplot(df,aes(x=muestra,y=y2000,color=tipo))+geom_point()+xlab("Número de muestra")+ylab("Gen en fila 2000")+facet_grid(.~tipo)
dev.off()

# ¿Qué gen corresponde la fila 2000?
golub.gnames[2000]
class(golub.gnames)
dim(golub.gnames)
golub.gnames[2000,]
golub.gnames[2000,2]
#identificador de affymetrix
golub.gnames[2000,3]
rownames(golub)=golub.gnames[,3] #nombramos las filas con el identificador de Affy
colnames(golub)=golub.fac #de columna ponemos el tipo de leucemia
summary(golub)
head(golub)

# Ahora vamos a hacer otras gráficas. En eje x medidas de localización de ALL, en y lo mismo pero del AML
mean(golub[2000,])
median(golub[2000,])
# vemos que hay mucha asimetría porque no coinciden los valores
golub.ALL <- golub[,golub.fac == "ALL"]
golub.AML <- golub[,golub.fac == "AML"]
ALL.mean <- apply(golub.ALL,1,mean)
AML.mean <- apply(golub.AML,1,mean)
ALL.median <- apply(golub.ALL,1,median)
AML.median <- apply(golub.AML,1,median)
df<-data.frame(x0=ALL.mean,y0=AML.mean)
p=ggplot(df,aes(x=x0,y=y0))+geom_point()
p+xlab("Medias ALL")+ylab("Medias AML")
dev.off()
# vamos a poner un sombreado que ayude a distinguir dónde hay más densidad de puntos
df<-data.frame(x0=ALL.mean,y0=AML.mean)
ggplot(df,aes(x=x0,y=y0))+geom_point(alpha=.25)+xlab("Medias ALL")+ylab("Medias AML")
dev.off()
#ahora representamos la mediana
df<-data.frame(x0=ALL.median,y0=AML.median)
ggplot(df,aes(x=x0,y=y0))+geom_point(alpha=.25)+xlab("Medianas ALL")+ylab("Medianas AML")
dev.off()

################                     EJERCICIO 4
#1.- Calcular para cada gen la expresión media sin considerar el tipo de cancer
apply(golub,1,mean)

#2.- Calcular para cada gen la desviación estándar
apply(golub,1,sd)

#3.- Representar gráficamente un dibujo que, para cada gen, nos muestre en abscisas la expresión media y en ordenadas la desviación estándar
expresion_media <- apply(golub,1,mean)
desv_stdr <- apply(golub,1,sd)
df<-data.frame(x=expresion_media,y=desv_stdr)
ggplot(df,aes(x=expresion_media,y=desv_stdr))+geom_point(alpha=.25)
dev.off()

#4.- Utilizando la función grDevices::png guardar el dibujo anterior en un fichero externo
getwd()
#"C:/Users/irene/Documents/UAX/Asignaturas/Biomedicina/Bioinformatica/2023-2024/R/Golub.png"
png("C:/Users/irene/Documents/UAX/Asignaturas/Biomedicina/Bioinformatica/2023-2024/R/Golub.png")
ggplot(df,aes(x=expresion_media,y=desv_stdr))+geom_point(alpha=.25)
dev.off()

#5.- Determinar la desviación estándar de las expresiones de cada gen
desvSD<-apply(golub,1,sd)
desvSD

#6.- Representar un histograma de estas desviaciones estándar
plot(hist(desvSD))

#3.- Calcular el percentil de orden 0.9 (Q0.9) de las desviaciones calculadas en el punto anterior. 
desvSD.sort<-sort(desvSD)
n<-length(desvSD.sort)
n
q09<-desvSD.sort[(n+1)*(90/100)]
q09

#4.- Utilizando la función abline añadir al dibujo del apartado 2 una línea vertical cuya abscisa coincida con el percentil 0.9
abline(v=q09,col="red")
dev.off()
#5.- Seleccionar aquellos genes cuya desviación estándar sea mayor que el valor Q0.9
genes.sdAlta<-desvSD[desvSD>q09]
genes.sdAlta


