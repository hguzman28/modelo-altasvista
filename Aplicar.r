fecha_actual <- as.character(Sys.Date())
print(fecha_actual)

Calificar <- function(FI=fecha_actual,FF=fecha_actual,Parametro=0.63){

	library(data.table)
	library(stringr)
	library(gdata)
	library(lubridate)
	library(randomForest)
	library(rvest)
	library(xml2)
	library(mongolite)

	load("./Modelo_Final.RData")
	load("./Funciones.RData")
	
	x <- Datos(Fecha_Ini=FI,Fecha_Fin=FF)
	# x <- Datos(Fecha_Ini=fecha_actual,Fecha_Fin=fecha_actual)
	
	x <- Prep_Data(x)
	
	x <- x[Fecha==FI,]
	# x <- x[Fecha==fecha_actual,]

	p <- predict(Mod_RF,x,type="prob")[,2]
	x <- cbind(x,P_RF2=p)
	x[,P_RF:=ifelse(P_RF2> Parametro,1,0)]
	# x[,P_RF:=ifelse(P_RF2> 0.63,1,0)]
	#Mtest_EMP[[1]] <- round(prop.table(table(test[,Vobj],test$P_RF),1),2)
	#round(prop.table(table(x$Incumple,x$P_RF),1),2)
	
	library(mongolite)
	connection_string = 'mongodb+srv://UserAltasVistas:YNAltasVistas@cluster-altas-vistas.bwxiixl.mongodb.net/altasvisas?retryWrites=true&w=majority'
	trips_collection = mongo(collection="resultados", db="altasvisas", url=connection_string)
	
	criterio <- paste0('{"Fecha": "',FI,'"}')
	# criterio <- paste0('{"Fecha": "',fecha_actual,'"}')
	y <- data.table(trips_collection$find(query = criterio))
	
	`%notin%` <- Negate(`%in%`)
	
	x <- x[(Nombre_Completo %notin% y$Nombre_Completo) | Hora %notin% y$Hora,]
	
	if(nrow(x)>0){
	
		library(mongolite)
		connection_string = 'mongodb+srv://UserAltasVistas:YNAltasVistas@cluster-altas-vistas.bwxiixl.mongodb.net/altasvisas?retryWrites=true&w=majority'
		trips_collection = mongo(collection="resultados", db="altasvisas", url=connection_string)
		trips_collection$insert(x)
		
	}
	
	x
}

Calificar(FI=fecha_actual,FF=fecha_actual,Parametro=0.63)