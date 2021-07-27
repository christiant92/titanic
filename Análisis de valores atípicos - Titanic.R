library(cluster)
library(MASS)
library(DBI)

bd <- DBI::dbConnect(RMySQL::MySQL(),
                     dbname = "",
                     host = "database-1.chtu56j8cqsz.us-east-1.rds.amazonaws.com",
                     user = "admin",
                     password = rstudioapi::askForPassword("Database password"),
                     Port     = 3306)

dbGetQuery(bd, 'USE TAREA_UPC')

df <- dbGetQuery(bd, 'Select * from train
UNION 
SELECT A.PassengerId,B.Survived,A.Pclass, A.`Name`, A.Sex, A.Age, A.SibSp, A.Parch, A.Ticket, A.Fare, A.Cabin, A.Embarked FROM test A
INNER JOIN gender_submission B on A.PassengerId=B.PassengerId')

dbDisconnect(bd2)

head(df, 10)    # Primeros 10 datos
str(df)         # Estructura de los datos
summary(df)

# Convirtiendo Datos
df$PassengerId<-as.integer(df$PassengerId)
df$Survived<-as.integer(df$Survived)
df$Pclass<-as.integer(df$Pclass)
df$Age<-as.integer(df$Age)
df$SibSp<-as.integer(df$SibSp)
df$Parch<-as.integer(df$Parch)
df$Fare<-as.integer(df$Fare)
df$Sex<-as.factor(df$Sex)
str(df)

# Seleccionando Columnas de Interés
df2 <- df[c(2,3,6,7,8,10)]
str(df2)

# Omite NA -- Validar con grupo
df2 <- na.omit(df2)
str(df2)
summary(df2)

# Distancia de Mahalanobis
dm2 <- mahalanobis(df2, colMeans(df2), cov(df2))
barplot(dm2, main="Mahalanobis")
which.max(dm2)

# Punto de Corte 
#p <- 1-0.001
#dof = ncol(df2)
#k <- (qchisq(p, dof))
k <- 35 # Experimentación visual
idx_outliers <- which(dm2 > k)
idx_outliers
df2[idx_outliers,]         # Registros con valores atípicos
nrow(df2[idx_outliers,])

# Gráfico de Ojiva
plot(sort(dm2), ppoints(nrow(df2)), xlab="DM al cuadrado ordenada", 
     ylab="Probabilidad Acumulada")
abline(v = k, col = "red")

# QQ-plot:
dof <- ncol(df2)
x <- qchisq(ppoints(nrow(df2)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot para"~~{chi^2}[nu==6]))
abline(0, 1, col="red")

idx_outliers <- which(dm2 > k)
idx_outliers


# Uso de estimador robusto: MCD (determinante de covarianza mínima)

# Porcentaje de valores que utilizará MCD
q = 0.85
# Calcula media y covarianza robusta usando MCD
mcd <- cov.mcd(df2, quantile.used=nrow(df2)*q)
dm2 <- mahalanobis(df2, mcd$center, mcd$cov)

# Punto de Corte 
#p <- 1-0.001
#k <- (qchisq(p, dof))
k <- 35 # Experimentación visual
idx_outliers <- which(dm2 > k)
idx_outliers
df2[idx_outliers,]         # Registros con valores atípicos
nrow(df2[idx_outliers,])

# Ojiva
plot(sort(dm2), ppoints(nrow(df2)), xlab="DM al cuadrado ordenada",
     ylab="Probabilidad Acumulada")
abline(v = k, col = "red")

# QQ-plot
x <- qchisq(ppoints(nrow(df2)), dof)
y <- dm2
qqplot(x, y, main=expression("Q-Q plot for"~~{chi^2}[nu==6]))
abline(0, 1, col="red")

idx_outliers <- which(dm2 > k)
idx_outliers

df2[idx_outliers,]

# Clusters (PAM)
pamDf = pam(df2, 15, stand=TRUE)     # número de clústers
pamDf$clusinfo                      # Ver qué clúster tiene menos elementos
df2[pamDf$clustering==12,]
