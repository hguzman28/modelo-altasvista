library(data.table)
library(stringr)
library(gdata)
library(lubridate)
library(sampling)
library(randomForest)
library(C50)
library(rpart)
library(nnet)
library(gbm)
library(pROC)
library(rvest)
library(xml2)

######################################################################################################################

Vexp <- grep("TRUE",names(x) %in% c("Mesa","Zona.de.la.mesa","GFestivos","Origen","Reserva de grupo","Restaurante2","Hora","PAX","Tiempo_Reserva",
									"Mes_Ir","Dia_Mes_Ir","Dia_Semana_Ir","Mes_Res","Dia_Mes_Res","Dia_Semana_Res",
									"Reservas_Antes","Reservas_Antes_Rest","Antiguedad","Antiguedad_Incumplimiento","Hora.añadida"))
									
Vobj <- grep("TRUE",names(x) %in% "Incumple")

	n <- table(x[,Vobj,with=FALSE])*0.6
	set.seed(010214)
	Muestra <- sampling:::strata(x, stratanames=c("Incumple"),size=c(round(min(n)),round(min(n))),method="srswor")

	train <- x[Muestra$ID_unit,] ## Muetra de Entrenamiento
	test <- x[-Muestra$ID_unit,]
	
	train <- data.frame(train)
	test <- data.frame(test)
	

	gc(reset=TRUE)
	set.seed(2148396)
	Mod_RF <-randomForest(train[,Vexp],train[,Vobj], ntree=1333,doBest=TRUE,nodesize=1733,classwt=c(1,0.96),mtry = 6)
	gc(reset=TRUE)
	varImpPlot(Mod_RF)
	
	gc(reset=TRUE)
	
	p <- predict(Mod_RF,train[,Vexp])
	train <- cbind(train,P_RF=p)
	#Mtrain_EMP[[1]] <- round(prop.table(table(train[,Vobj],train$P_RF),1),2)
	round(prop.table(table(train[,Vobj],train$P_RF),1),2)
	
	
 
	gc(reset=TRUE)
 
	p <- predict(Mod_RF,test[,Vexp])
	test <- cbind(test,P_RF=p)
	#Mtest_EMP[[1]] <- round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	
	gc(reset=TRUE)
	
	train <- train[,-ncol(train),]
	test <- test[,-ncol(test),]

