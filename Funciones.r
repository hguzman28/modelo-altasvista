
Prep_Data <- function(x){

	Pfiltro <- grep("TRUE",names(x) %in% c("Email","Teléfono","Hora.añadida","Hora.en.la.que.se.sentó","Hora","Fecha","Nombre_Completo","Fecha.añadida","Restaurante","Nombre",
										"Apellidos","Mesa","Zona.de.la.mesa","GFestivos","Origen","Reserva de grupo",
										"Restaurante2","Hora","PAX","Tiempo_Reserva","Mes_Ir","Dia_Mes_Ir","Dia_Semana_Ir","Mes_Res","Dia_Mes_Res",
										"Dia_Semana_Res","Reconfirmado","Origen","Reservas_Antes","Reservas_Antes_Rest","Antiguedad","Antiguedad_Incumplimiento",
										"Nº.total.PAX","Servicio"))
										
	x <- x[,Pfiltro,with=FALSE]

	#x <- fread("E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Base.csv"),encoding="UTF-8")

	# temperatura <- fread("C:/Users/Johann/Downloads/Colombia.csv")
	
	#x <- Datos()

	x[,ZONA_MESA:=as.factor(x$Zona.de.la.mesa),]


	x[,Nombre_Completo:=gsub("([[:punct:]])","",trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(paste(Nombre,Apellidos), locale = "es"))))]
	x[,Restaurante2:=trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(Restaurante, locale = "es")))]


	x[,Fecha:=as.Date(Fecha,"%Y-%m-%d")]
	x[,Fecha_anadida:=as.Date(Fecha.añadida,"%Y-%m-%d")]

	filtro <- c("BOMBAY ROOFTOP | ALTAS VISTAS","SEXY SEOUL KOREAN BBQ | ALTAS VISTAS","ASTORIA ROOFTOP | ALTAS VISTAS","SANTORINI ROOFTOP | ALTAS VISTAS","REYNA | ALTAS VISTAS")
	x <- x[Restaurante2 %in% filtro,]
	x <- x[Origen != "walk in",]


	setorderv(x,c("Nombre_Completo","Fecha","Restaurante2","Hora"))

	x[,A:=1]

	x[,Incumple:=ifelse(nchar(Hora.en.la.que.se.sentó)>5,0,1)]
	x[,Incumple:=ifelse(is.na(Incumple)==1,1,Incumple),]
	x[,Tiempo_Reserva:=as.numeric(Fecha-Fecha_anadida)]
	x[,Mes_Ir:=month(Fecha)]
	x[,Dia_Mes_Ir:=day(Fecha),]
	x[,Dia_Semana_Ir:=wday(Fecha),]
	x[,Mes_Res:=month(Fecha_anadida)]
	x[,Dia_Mes_Res:=day(Fecha_anadida),]
	x[,Dia_Semana_Res:=wday(Fecha_anadida),]



	x <- unique(x,by=c("Nº.total.PAX","Incumple","Nombre_Completo"))
	x[,tem:=cumsum(A),by=c("Nombre_Completo","Nº.total.PAX","Mes_Ir","Dia_Mes_Ir","Restaurante2")]
	x[,tem:=max(tem),by=c("Nombre_Completo","Nº.total.PAX","Mes_Ir","Dia_Mes_Ir","Restaurante2")]
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
	
	# URL de la página web
	url <- "https://www.festivos.com.co/"

	# Realizar la solicitud a la página web
	pagina <- read_html(url)

	# Extraer la tabla de festivos
	tabla_festivos <- html_table(pagina)

	Festivos <- Fecha_Form(tabla_festivos[[1]]$Fecha)
		
	x[,GFestivos:=ifelse(Fecha %in% Festivos,1,0)]
	
	levels(x$Reconfirmado) <- c("SI","NO")
	levels(x$Restaurante2) <- c("BOMBAY ROOFTOP | ALTAS VISTAS","SEXY SEOUL KOREAN BBQ | ALTAS VISTAS","ASTORIA ROOFTOP | ALTAS VISTAS","SANTORINI ROOFTOP | ALTAS VISTAS","REYNA | ALTAS VISTAS")
	levels(x$Incumple) <- c("0","1")
	levels(x$Origen) <- c("appmovil","moduloweb","software","terceros","waitinglist")
	x[,Hora.en.la.que.se.sentó:=ifelse(is.na(Hora.en.la.que.se.sentó)==1,1,Hora.en.la.que.se.sentó)]
	x[,Hora.en.la.que.se.sentó:=as.character(Hora.en.la.que.se.sentó),]
	x[,Fecha.añadida:=as.IDate(Fecha.añadida),]
	x
	
}

############################################
	  ### Consulta BAses de Datos ####
############################################
Datos <- function(Fecha_Ini="2023-06-01",Fecha_Fin="2023-06-30"){

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
	Tracking_reservas

}

###########################################
### consulta los dias festivos del año ####
###########################################

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






# Temperatura <- function(Lis_Mes=c(2,3,4,5,6)){

	# library(rvest)
	# library(xml2)
	# library(httr)
	
	# T <- data.table(Dia=0,Mes=0,Max=0,Min=0,Estados=0)
	
	# for(i in Lis_Mes){
		
		# meses <- c("January","February","March","April","May","June","July","August","September","October","November","December")
		
		# #mes <- month(Sys.Date())
		# mes <- i

		# # URL de la página web
		# url <- paste0("https://www.tiempo3.com/south-america/colombia/san-andres-y-providencia/bogota?page=month&month=",meses[i])

		# pagina <- read_html(url)

		# botones <- pagina %>% html_nodes("table")

		# botones <- botones[1]

		# temp <- botones %>% html_nodes("a") %>% html_nodes("div") %>% html_nodes("span") %>% html_nodes("span") %>% html_text()
		# temp <- temp[temp!="°"]

		# tempe <- data.table(Max=temp[1],Min=temp[2])

		# for(j in (seq(from = 4, to = length(temp), by = 2))){
			# a <- data.table(Max=temp[j-1],Min=temp[j])
			# tempe <- rbind(tempe,a)
		# }

		# Estados <- botones %>% html_nodes("img")
		# Estados <- html_attr(Estados, "alt")

		# tempe <- data.table(Dia=1:length(Estados),Mes=mes,tempe,Estados)
		
		# l = list(T,tempe)
		# T <- rbindlist(l)
		
	# }
	# T <- T[-1,]
	# T

# }
