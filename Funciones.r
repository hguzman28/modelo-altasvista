#############################################################
	  ### Consulta Cover Manager + Update Historico ####
#############################################################
# Fecha_Ini = "2024-03-15"
# Fecha_Fin = "2024-03-15"

Datos <- function(Fecha_Ini, Fecha_Fin){
	
	# Parameters
	email<-"reservasgrupoaltasvistas3@gmail.com"
	pass<-"123456"
	initial_date <- Fecha_Ini
	final_date <- Fecha_Fin

	# Create a session and simulate a login
	session <-session("https://www.covermanager.com/login?lang=es")
	login_form<-html_form(session)[[1]]
	filled_form<-html_form_set(login_form, email=email, password=pass)
	session_open<-session_submit(session,filled_form) # Submit the filled form to login

	# Navigate to a specified url 
	# url<-paste0("https://www.covermanager.com/Stats/reserv_stats_basic/all/",initial_date,"/",final_date,"/table/this/no/date")
	url<-paste0("https://www.covermanager.com/Stats/reserv_stats/all/",initial_date,"/",final_date,"/table/all/yes/no/null/no/date") #consulta con mas campos
	session_open<-session_jump_to(session_open,url)

	# Scrape html_table from the logged in session
	Tracking_reservas<- as.data.table(html_table(session_open)) 
	a <- grep("TRUE",(names(Tracking_reservas)%in% c("Nº.total.PAX","Zona.de.la.mesa","Código.reserva","Teléfono","Fecha.añadida","Hora.añadida","Hora.en.la.que.se.sentó")))
	names(Tracking_reservas)[a] <- c("N_total_PAX","Zona_de_la_mesa","Codigo_reserva","Telefono","Fecha_anadida","Hora_anadida","Hora_en_la_que_se_sento")
	
	filtro <- c("Fecha","N_total_PAX","Hora","Nombre","Apellidos","Servicio","PAX","Mesa","Zona_de_la_mesa",
			    "Telefono","Email","Origen","Fecha_anadida","Hora_anadida","Restaurante","Reconfirmado","Hora_en_la_que_se_sento","Estado")

	Tracking_reservas <- Tracking_reservas[,filtro,with=FALSE]
	Tracking_reservas[,Fecha:=as.Date(Fecha)]
	# hay registros que llegan  de COVER totalmente en NA
	Tracking_reservas <- Tracking_reservas[!is.na(Fecha),]
	Tracking_reservas[,Hora_en_la_que_se_sento:=as.Date(Hora_en_la_que_se_sento)]
	Tracking_reservas[,Hora_en_la_que_se_sento:=ifelse(is.na(Hora_en_la_que_se_sento), 0, Hora_en_la_que_se_sento)]
	
	## conectarse a mongo para los datos Historicos
	library(mongolite)
	connection_string = 'mongodb+srv://UserAltasVistas:YNAltasVistas@cluster-altas-vistas.bwxiixl.mongodb.net/'
	trips_collection = mongo(collection="Datos_Altas_Vistas", db="Historico", url=connection_string)
	criterio <- paste0('{"Fecha":"',Fecha_Fin,'"}')
	result <- data.table(trips_collection$find(query = criterio) )
	# result <- data.table(trips_collection$find(query = '{}') )
	
	if (nrow(result)>0){
		criterio <- paste0('{"Fecha":"',Fecha_Fin,'"}')
		trips_collection$remove(query = criterio)
		trips_collection$insert(Tracking_reservas)
	} else {
		trips_collection$insert(Tracking_reservas)
	}	

	
	# if((Fecha_Ini %in% as.character(a))==1){
		
	# 	result <- result[Fecha!=Fecha_Ini,]
		
	# 	criterio <- paste0('{"Fecha":"',Fecha_Ini,'"}')
	# 	trips_collection$remove(query = criterio)
	# 	trips_collection$insert(Tracking_reservas)
	
	# }else{
	# 	trips_collection$insert(Tracking_reservas)
		
	# 	# if(unique(result$Fecha)>230){
	# 	# 	a <- sort(unique(result$Fecha))[1]
	# 	# 	criterio <- paste0('{"Fecha":"',a,'"}')
	# 	# 	trips_collection$remove(query = criterio)
	# 	# }
	
	# }
	
	# if(length(a)==210){
		
		# a <- sort(unique(result$Fecha))[1]
		# criterio <- paste0('{"Fecha":"',a,'"}')
		# trips_collection$remove(query = criterio)
		
	# }
	#l = list(result,Tracking_reservas)
	
	#  el rbind sirve cuando se entrena el modelo y se necesita toda la info
	# Tracking_reservas <- rbind(result,Tracking_reservas,fill=TRUE)
	Tracking_reservas
}

#############################################################
	  ### Procesamiento Data para modelo ####
#############################################################

Prep_Data <- function(x,Entrena_Model=0){

	# Pfiltro <- grep("TRUE",names(x) %in% 
	# 	c("Email","Telefono","Hora_anadida","Hora_en_la_que_se_sento","Hora",
	# 		"Fecha","Nombre_Completo","Fecha_anadida","Restaurante","Nombre",
	# 		"Apellidos","Mesa","Zona_de_la_mesa","GFestivos","Origen",
	# 		"Reserva de grupo", "Restaurante2","Hora","PAX","Tiempo_Reserva",
	# 		"Mes_Ir","Dia_Mes_Ir","Dia_Semana_Ir","Mes_Res","Dia_Mes_Res",
	# 		"Dia_Semana_Res","Reconfirmado","Origen","Reservas_Antes",
	# 		"Reservas_Antes_Rest","Antiguedad","Antiguedad_Incumplimiento",
	# 		"N_total_PAX","Servicio","Estado"))
										
	# x <- x[,Pfiltro,with=FALSE]

	#x <- fread("E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Base.csv"),encoding="UTF-8")

	# temperatura <- fread("C:/Users/Johann/Downloads/Colombia.csv")
	
	#x <- Datos()

	x[,ZONA_MESA:=as.factor(x$Zona_de_la_mesa),]

	x[,Nombre_Completo:=gsub("([[:punct:]])","",trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(paste(Nombre,Apellidos), locale = "es"))))]
	x[,Restaurante2:=trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(Restaurante, locale = "es")))]

	x[,Fecha:=as.Date(Fecha,"%Y-%m-%d")]
	x[,Fecha_anadida:=as.Date(Fecha_anadida,"%Y-%m-%d")]

	filtro <- c("ASTORIA ROOFTOP | ALTAS VISTAS",
	        	"BOMBAY ROOFTOP | ALTAS VISTAS",
	        	"BOMBAY ROOFTOP 127 BOGOTA",
	        	"REYNA | ALTAS VISTAS",
	        	"SANTORINI ROOFTOP | ALTAS VISTAS", 
	        	"SEXY SEOUL KOREAN BBQ | ALTAS VISTAS",
	        	"SEXY TOKYO | ALTAS VISTAS")

	x <- x[Restaurante2 %in% filtro,]
	x <- x[Origen != "walk in",]

	setorderv(x,c("Nombre_Completo","Fecha","Restaurante2","Hora"))

	x[,A:=1]

	x[,Incumple:=ifelse(nchar(Hora_en_la_que_se_sento)>2,0,1)]
	x[,Incumple:=ifelse(is.na(Incumple)==1,1,Incumple),]
	x[,Tiempo_Reserva:=as.numeric(Fecha-Fecha_anadida)]
	x[,Mes_Ir:=month(Fecha)]
	x[,Dia_Mes_Ir:=day(Fecha),]
	x[,Dia_Semana_Ir:=wday(Fecha),]
	x[,Mes_Res:=month(Fecha_anadida)]
	x[,Dia_Mes_Res:=day(Fecha_anadida),]
	x[,Dia_Semana_Res:=wday(Fecha_anadida),]

	#x <- unique(x,by=c("N_total_PAX","Incumple","Nombre_Completo"))
	x <- unique(x)
	x[,tem:=cumsum(A),by=c("Nombre_Completo","N_total_PAX","Mes_Ir","Dia_Mes_Ir","Restaurante2")]
	x[,tem:=max(tem),by=c("Nombre_Completo","N_total_PAX","Mes_Ir","Dia_Mes_Ir","Restaurante2")]
	x[,tem:=ifelse(tem>1&Incumple==1,1,0),]

	#x <- x[tem==0,]

	x[,Reservas_Antes:=cumsum(A),by=c("Nombre_Completo")]
	x[,Reservas_Antes:=Reservas_Antes-1]

	x[,Reservas_Antes_Rest:=cumsum(A),by=c("Nombre_Completo","Restaurante2")]
	x[,Reservas_Antes_Rest:=Reservas_Antes_Rest-1]

	x[,A:=ifelse(Incumple==0,1,0),]
	x[,Antiguedad:=cumsum(A),by=c("Nombre_Completo")]
	x[Antiguedad != 0,Antiguedad:=Antiguedad-1]
	x[,Antiguedad_Incumplimiento:=cumsum(Incumple),by=c("Nombre_Completo")]
	x[Antiguedad_Incumplimiento != 0 & Incumple==1,Antiguedad_Incumplimiento:=Antiguedad_Incumplimiento-1]

	x[,Incumple:=as.factor(Incumple)]
	x[,Restaurante2:=as.factor(Restaurante2),]
	x[,Servicio:=as.factor(Servicio),]
	x[,Reconfirmado:=as.factor(Reconfirmado),]
	x[,Origen:=as.factor(Origen)]

	x[,Ano_Mes_Ir:=paste0(year(Fecha),"-",Mes_Ir)]
	x[,Mes_Dia_Ir:=paste0(Mes_Ir,"-",Dia_Mes_Ir)]
	
	# ESTO SOLO ES PARA PRODUCTIVO
	if(Entrena_Model==0){
		x <- x[Estado != "Cancelado por el cliente",]
	}
	
	# URL de la página web
	url <- "https://www.festivos.com.co/"

	# Realizar la solicitud a la página web
	pagina <- read_html(url)

	# Extraer la tabla de festivos
	tabla_festivos <- html_table(pagina)

	Festivos <- Fecha_Form(tabla_festivos[[1]]$Fecha)
	Festivos <- Festivos[!is.na(Festivos)]
		
	x[,GFestivos:=ifelse(Fecha %in% Festivos,1,0)]
	
	
	bigMap <- mapLevels(x=list(c("SI","NO"), x$Reconfirmado),codes=FALSE,combine=TRUE)
	mapLevels(x$Reconfirmado) <- bigMap
	
	res <- c("ASTORIA ROOFTOP | ALTAS VISTAS",
	        	"BOMBAY ROOFTOP | ALTAS VISTAS",
	        	"BOMBAY ROOFTOP 127 BOGOTA",
	        	"REYNA | ALTAS VISTAS",
	        	"SANTORINI ROOFTOP | ALTAS VISTAS", 
	        	"SEXY SEOUL KOREAN BBQ | ALTAS VISTAS",
	        	"SEXY TOKYO | ALTAS VISTAS")
	bigMap <- mapLevels(x=list((res), x$Restaurante2),codes=FALSE,combine=TRUE)
	mapLevels(x$Restaurante2) <- bigMap
	
	bigMap <- mapLevels(x=list(c("0","1"), x$Incumple),codes=FALSE,combine=TRUE)
	mapLevels(x$Incumple) <- bigMap
	
	bigMap <- mapLevels(x=list(c("appmovil","moduloweb","software","terceros","waitinglist"), x$Origen),codes=FALSE,combine=TRUE)
	mapLevels(x$Origen) <- bigMap

	# Vexp <- c("Nombre_Completo", "Fecha", 
	# 			"Hora", "PAX", "Mesa", "Zona_de_la_mesa", "Origen", 
	# 			"Fecha_anadida", "Hora_anadida", "Reconfirmado", 
	# 			"Restaurante2", "Tiempo_Reserva", "Mes_Ir", "Dia_Mes_Ir", 
	# 			"Dia_Semana_Ir", "Mes_Res", "Dia_Mes_Res", "Dia_Semana_Res", 
	# 			"Reservas_Antes", "Reservas_Antes_Rest", "Antiguedad", 
	# 			"Antiguedad_Incumplimiento", "GFestivos")

	Vexp <- c("Fecha", "N_total_PAX", "Hora", "Nombre", "Apellidos", "Servicio",
			"PAX", "Mesa", "Zona_de_la_mesa", "Telefono", "Email", "Origen", 
			"Fecha_anadida", "Hora_anadida", "Restaurante", "Reconfirmado", 
			"Estado", "Hora_en_la_que_se_sento", "ZONA_MESA", "Nombre_Completo", 
			"Restaurante2", "A", "Incumple", "Tiempo_Reserva", "Mes_Ir", "Dia_Mes_Ir",
			"Dia_Semana_Ir", "Mes_Res", "Dia_Mes_Res", "Dia_Semana_Res", "tem",
			"Reservas_Antes", "Reservas_Antes_Rest", "Antiguedad", 
			"Antiguedad_Incumplimiento", "Ano_Mes_Ir", "Mes_Dia_Ir", "GFestivos")

	x <- x[, Vexp, with = FALSE ]

	# NA's
	x[ , Hora_en_la_que_se_sento:= ifelse(is.na(Hora_en_la_que_se_sento)== TRUE, 0, Hora_en_la_que_se_sento)]
	x[ , Telefono:= ifelse(is.na(Telefono)== TRUE, 0, Telefono)]
	x[ , Telefono:= as.character(Telefono)]

	# x[is.na(Hora_en_la_que_se_sento)==1,Hora_en_la_que_se_sento:=1,]
	# x[,Hora_en_la_que_se_sento:=as.character(Hora_en_la_que_se_sento),]
	# x[,Fecha_anadida:=as.IDate(Fecha_anadida),]
	x	
}

#############################################################
	  ### consulta los dias festivos del año ####
#############################################################

Fecha_Form <- function(x){

	Sys.setlocale("LC_TIME", "Spanish")
	# Texto de la fecha
	fecha_texto <- x
	# Parsear el texto a una fecha
	fecha <- as.Date(fecha_texto, format = "%d de %B", tryFormats = c("%d de %B"))
	fecha   	
}

###################################################################
### descarga por hora la temperatura y el pronostico del tiempo ###
###################################################################

Temperatura <- function(Lis_Mes=c(2,3,4,5,6),Ano=2023){
	
	if (!require(lubridate)) {
	  install.packages("lubridate")
	  library(lubridate)
	}
	meses <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
	
	T2 <- data.table(Horas=0,Mes=0,Dia=0,Estados=0,Temp=0)
	
	for(i in Lis_Mes){
		
		T <- data.table(Horas=0,Mes=0,Dia=0,Estados=0,Temp=0)
		
		mes <- i  # Por ejemplo, febrero
		ano <- 2023  # Por ejemplo, 2023
		dias <- 1:days_in_month(as.Date(paste0(ano,"-",mes,"-1"),"%Y-%m-%d"))
		
		alea <- trunc(runif(length(dias), min = 10, max = 20))
		
		for(j in dias){
			
			print(paste("Dia",j,"del Mes",i))
			
			Sys.sleep(alea[dias])
			
			url <- paste0("https://www.tutiempo.net/registros/skbo/",j,"-",meses[i],"-",Ano,".html")

			pagina <- read_html(url)
			botones <- pagina %>% html_nodes("table")
			botones <- botones[2]
			Temporal  <- (botones %>% html_nodes("tr") %>% html_nodes("td") %>% html_text())
			Estados <- Temporal[seq(from = 2, to = length(Temporal), by = 6)]
			Horas <- Temporal[seq(from = 1, to = length(Temporal), by = 6)]
			Temp <- Temporal[seq(from = 3, to = length(Temporal), by = 6)]
			T1 <- data.frame(Horas,Mes=mes,Dia=j,Estados,Temp)
			
			l = list(T,T1)
			T <- rbindlist(l)
			
			
		}
		
		l = list(T2,T)
		T2 <- rbindlist(l)
	}
	T2
}

###################################################################
### VARIABLES DEL MODELO ###
###################################################################

Vexp <- c("Hora", "PAX", "Mesa", "Zona_de_la_mesa", "Origen", 
			"Fecha_anadida", "Hora_anadida", "Reconfirmado", 
			"Restaurante2", "Tiempo_Reserva", "Mes_Ir", "Dia_Mes_Ir", 
			"Dia_Semana_Ir", "Mes_Res", "Dia_Mes_Res", "Dia_Semana_Res", 
			"Reservas_Antes", "Reservas_Antes_Rest", "Antiguedad", 
			"Antiguedad_Incumplimiento", "GFestivos")

# save.image(file="/Users/admin/Documents/Projects/Altas Vistas/Desercion/GitHub/Funciones.RData") 