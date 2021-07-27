library(DBI)
library(VIM)
library(tidyverse)
library(DMwR)
library(fastDummies)
library(missForest)

##Conectando a MYSQL##
bd2 <- DBI::dbConnect(RMySQL::MySQL(),
                      dbname = "",
                      host = "database-1.chtu56j8cqsz.us-east-1.rds.amazonaws.com",
                      user = "admin",
                      password = "Peru123.",
                      Port     = 3306)

dbGetQuery(bd2, 'USE TAREA_UPC')

DATA <- dbGetQuery(bd2, 'Select * from train
UNION 
SELECT A.PassengerId,B.Survived,A.Pclass, A.`Name`, A.Sex, A.Age, A.SibSp, A.Parch, A.Ticket, A.Fare, A.Cabin, A.Embarked FROM test A
INNER JOIN gender_submission B on A.PassengerId=B.PassengerId')

dbDisconnect(bd2)

##Cantidad de Datos##
ndatos <- nrow(DATA)
ndatos

##Convirtiendo Información##
DATA$PassengerId<-as.integer(DATA$PassengerId)
DATA$Survived<-as.factor(DATA$Survived)
DATA$Pclass<-as.integer(DATA$Pclass)
DATA$Age<-as.integer(DATA$Age)
DATA$SibSp<-as.integer(DATA$SibSp)
DATA$Parch<-as.integer(DATA$Parch)
DATA$Fare<-as.integer(DATA$Fare)
DATA$Sex<-as.factor(DATA$Sex)
str(DATA)

## Mostrar en qué columnas tienen valores perdidos
cidx_perd <- which(colSums(is.na(DATA))!=0)
cidx_perd

## Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(DATA[,cidx_perd]))
nperdidos

## Porcentaje de valores perdidos en las columnas
pperdidos <- 100*nperdidos/ndatos
pperdidos

#La variable Cabin, tiene el 77% de datos nulos, podemos eliminarlos
DATA$Cabin <- NULL

#Para el análisis podemos eliminar la variable Passengerld y Name
DATA$PassengerId<-NULL
DATA$Name<-NULL

## Gráfico de agregación: "aggregation plot"
agreg <- aggr(DATA, numbers=TRUE)
agreg
summary(agreg)
aggr(DATA, numbers=TRUE, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

##Parallel boxplots##
matrixplot(DATA)
VIM::pbox(DATA[c(4,8,9)], pos=1) 

### Prueba t de medias
t.test(Fare ~ is.na(Age), data=DATA)

DATA <- dummy_cols(DATA, select_columns = 'Sex')
head(DATA)

### Imputación usando k-Vecinos más cercanos ######
df_iknn <- knnImputation(DATA[c(1,2,4,5,6,8,10,11)], k=5)
df_iknn2 <- VIM::kNN(data=DATA[c(1,2,4,5,6,8,10,11)], variable=c("Age","Fare"), k=5)

aggr(df_iknn2, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))

which(colSums(is.na(df_iknn))!=0)

###Imputación usando Random Forest##

set.seed(2019) #Semilla
DATA_IMPUTADA <- missForest(DATA[c(1,2,4,5,6,8,10,11)],maxiter = 100)
DATA_IMPUTADA<-DATA_IMPUTADA$ximp
which(colSums(is.na(DATA_IMPUTADA))!=0)

head(DATA_IMPUTADA)
