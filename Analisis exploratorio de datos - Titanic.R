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
                      password = rstudioapi::askForPassword("Database password"),
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

## EDA
library(gmodels)



# Explorar los primeros 10 datos
head(df, 10)  

# Revisar la estructura de los datos
str(df)   

# Resumen estadístico descriptivo
summary(df)

##Convirtiendo Información##

DATA$PassengerId<-as.integer(DATA$PassengerId)
DATA$Survived<-as.factor(DATA$Survived)
#DATA$Pclass<-as.factor(DATA$Pclass)
DATA$Pclass<-as.integer(DATA$Pclass)
DATA$Age<-as.integer(DATA$Age)
DATA$SibSp<-as.integer(DATA$SibSp)
DATA$Parch<-as.integer(DATA$Parch)
DATA$Fare<-as.integer(DATA$Fare)
DATA$Sex<-as.factor(DATA$Sex)
str(DATA)
df = DATA

frec <- table(df$Age) 
prop <- prop.table(table(df$Age))*100      # Porcentaje
edad <- t(rbind(frec, prop))
edad

# obtener matriz de sexo por clase de ticket
CrossTable(x=df$Sex, y=df$Pclass)

# obtener matriz de puerto de embarque por clase de ticket
CrossTable(x=df$Embarked, y=df$Pclass)

# Gráfico de barras de género por clase de pasajero por proporción de clientes
barplot(t(tabla2), col=2:4, beside = TRUE,
        xlab="Sexo",
        ylab="Proporción de Clientes",
        main="Distribución de personas por género y clase de ticket")
legend("topleft",legend=levels(as.factor(df$Pclass)),col=2:4,
       pch=15,title="Clase de pasaje")

# Distribución de la edad de los pasajeros
hist(DATA$Age, prob=TRUE, main = "Pasajeros por edad", xlab = "Edad")
lines(density(DATA$Age, na.rm = T))

# Distribución del precio del viaje de los pasajeros
hist(DATA$Fare, prob=TRUE)
lines(density(DATA$Fare, na.rm = T))

# Revisar la edad por clase de ticket
boxplot(df$Age ~ df$Pclass,
        xlab="Clase de ticket",ylab="Edad",
        main="Edad vs Clase de ticket")

# Edad vs precio de viaje
plot(x = df$Age, y = df$Fare, col= df$Survived)
legend(x = "topleft", legend = c("No", "Yes"), fill = c("Black", "Red")
       , title = "Sobrevivió?")


# Conteo de pasajeros por edad, por género y por flag de sobrevivió
ggplot(df, aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex)

# Conteo de pasajeros por edad, por clase de ticket y por flag de sobrevivió
ggplot(df, aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Pclass)

# Conteo de pasajeros por edad, por clase de ticket y por flag de sobrevivió
ggplot(df, aes(Pclass, fill = factor(Survived))) + 
  geom_histogram() + 
  facet_grid(.~Sex)
