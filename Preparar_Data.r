

Prep_Data <- function(x){
	library(data.table)

	#x <- fread("E:/OneDrive/YouNeed (J)/CLIENTES/Alta Vista/Base.csv",encoding = "UTF-8")

	# temperatura <- fread("C:/Users/Johann/Downloads/Colombia.csv")
	
	#x <- Datos()

	x[,ZONA_MESA:=as.factor(x$Zona.de.la.mesa),]


	x[,Nombre_Completo:=gsub("([[:punct:]])","",trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(paste(Nombre,Apellidos), locale = "es"))))]
	x[,Restaurante2:=trim(chartr('ÁÉÍÓÚ', 'AEIOU',str_to_upper(Restaurante, locale = "es")))]


	x[,Fecha:=as.Date(Fecha,"%d/%m/%Y")]
	x[,Fecha_anadida:=as.Date(Fecha.añadida,"%d/%m/%Y")]

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
	
}