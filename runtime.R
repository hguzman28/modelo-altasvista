library(rvest)
library(data.table)
library(lubridate)
library(randomForest)
library(mongolite)


parity  <- function()
{
	print("Inicia calificar")
	load("/lambda/Modelo_Final.RData")
	# load("./Funciones.RData")
	source("/lambda/Funciones.r")
	
	x <- Datos(Fecha_Ini="2023-09-14",Fecha_Fin="2023-09-14")
	
	x <- Prep_Data(x)
	
	p <- predict(Mod_RF,x,type="prob")[,2]
	x <- cbind(x,P_RF2=p)
	x[,P_RF:=ifelse(P_RF2> 0.65,1,0)]

	
	# fwrite(x,"./Resultados.csv",bom=TRUE)
	x
	print(x)

	# Conectarse a MongoDB
	mongo_url <- "mongodb+srv://youneed:grVpxuOAVcJTkKhd@cluster0.3fthc83.mongodb.net/altasvisas?retryWrites=true&w=majority"
	conn <- mongo(collection = "resultados", url = mongo_url)
	
	# Guardar el dataframe en MongoDB
	conn$insert(x)
	
	# Cerrar la conexión con MongoDB
	conn$disconnect()
	x
}



# base <- data.frame(documento =345674534,EDAD=35,I1_T=1575.43,I5_T=1,I6_T=242,Efectivo=1575.43,Tarjeta.Credito=0,I9_T=4,ingresos=0)

# calificar.flujo3L(base=base,ID=1234)
lambdr::start_lambda()