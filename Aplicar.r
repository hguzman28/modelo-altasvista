Calificar <- function(FI="2023-09-23",FF="2023-09-23",Parametro=0.63){

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

	load("E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Modelo_Final.RData")
	load("E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Funciones.RData")
	
	x <- Datos(Fecha_Ini=FI,Fecha_Fin=FF)
	
	x <- Prep_Data(x)
	
	p <- predict(Mod_RF,x,type="prob")[,2]
	x <- cbind(x,P_RF2=p)
	x[,P_RF:=ifelse(P_RF2> Parametro,1,0)]
	#Mtest_EMP[[1]] <- round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	#round(prop.table(table(y$Incumple,y$P_RF),1),2)
	
	fwrite(x,"E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Resultados.csv",bom=TRUE)
	x
}


