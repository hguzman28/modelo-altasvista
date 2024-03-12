library(data.table)
library(stringr)
library(gdata)
library(lubridate)
library(randomForest)
library(rvest)
library(xml2)
library(mongolite)


parity  <- function()
{
	print("Inicia calificar")
	load("/lambda/Modelo_Final.RData")
	# load("/lambda/Funciones.RData")
	source("/lambda/Funciones.r")

	fecha_actual <- format(Sys.Date(), "%Y-%m-%d")
	print(fecha_actual)

	x <- Datos(Fecha_Ini=fecha_actual,Fecha_Fin=fecha_actual)

	print("Data")
	
	x <- Prep_Data(x)
	print("Prep_Data")
	
	x <- x[Fecha==fecha_actual,]
	print("predict")
	p <- predict(Mod_RF,x,type="prob")[,2]
	x <- cbind(x,P_RF2=p)
	x[,P_RF:=ifelse(P_RF2> 0.63,1,0)]
	#Mtest_EMP[[1]] <- round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	#round(prop.table(table(x$Incumple,x$P_RF),1),2)
	
	library(mongolite)
	connection_string = 'mongodb+srv://UserAltasVistas:YNAltasVistas@cluster-altas-vistas.bwxiixl.mongodb.net/altasvisas?retryWrites=true&w=majority'
	trips_collection = mongo(collection="resultados", db="altasvisas", url=connection_string)
	
	criterio <- paste0('{"Fecha": "',fecha_actual,'"}')
	y <- data.table(trips_collection$find(query = criterio))
	
	`%notin%` <- Negate(`%in%`)
	
	# crear llave para filtrar
	y[ , key := paste0(Nombre_Completo, Hora)]
	x[ , key := paste0(Nombre_Completo, Hora)]

	x <- x[key %notin% y$key,]
	
	if(nrow(x)>0){
	
		library(mongolite)
		connection_string = 'mongodb+srv://UserAltasVistas:YNAltasVistas@cluster-altas-vistas.bwxiixl.mongodb.net/altasvisas?retryWrites=true&w=majority'
		trips_collection = mongo(collection="resultados", db="altasvisas", url=connection_string)
		trips_collection$insert(x)
		
	}
	print("head")
	head(x)
        print("### table ###")
	table(x$P_RF)
	

    # Puedes devolver un valor dummy si es necesario
    return("Proceso completado exitosamente")
}



# base <- data.frame(documento =345674534,EDAD=35,I1_T=1575.43,I5_T=1,I6_T=242,Efectivo=1575.43,Tarjeta.Credito=0,I9_T=4,ingresos=0)

# calificar.flujo3L(base=base,ID=1234)
lambdr::start_lambda()
