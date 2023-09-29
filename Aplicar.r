library(rvest)
library(data.table)
library(lubridate)
library(randomForest)
library(mongolite)


Calificar <- function(FI="2023-09-01",FF="2023-09-01",Parametro=0.63){


	load("./Modelo_Final.RData")
	load("./Funciones.r")
	
	x <- Datos(Fecha_Ini=FI,Fecha_Fin=FF)
	
	x <- Prep_Data(x)
	
	x <- x[Fecha==FI,]
	
	p <- predict(Mod_RF,x,type="prob")[,2]
	x <- cbind(x,P_RF2=p)
	x[,P_RF:=ifelse(P_RF2> Parametro,1,0)]
	#Mtest_EMP[[1]] <- round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	#round(prop.table(table(x$Incumple,x$P_RF),1),2)
	
	fwrite(x,"./Resultados.csv",bom=TRUE)

		# Conectarse a MongoDB
	mongo_url <- "mongodb+srv://youneed:grVpxuOAVcJTkKhd@cluster0.3fthc83.mongodb.net/altasvisas?retryWrites=true&w=majority"
	conn <- mongo(collection = "resultados", url = mongo_url)
	
	# Guardar el dataframe en MongoDB
	conn$insert(x)
	
	# Cerrar la conexión con MongoDB
	conn$disconnect()
	x



}

Calificar(FI="2023-09-01",FF="2023-09-01",Parametro=0.63)