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





#---------------------- Maracando los Festivos del  Año --------------------------.

	#----------------------------------------------------.
	#-- se extraen los datos de los dias festivos -------.
	
	
	

#----------------- Clima ---------------------------.

# Clima <- Temperatura(c(2,3,4,5,6,7,8),2023)
# Clima <- Clima[-(1:2),]
# Cl <- Clima
# Clima[,Hr:=ifelse(substr(Horas, start = 1, stop = 1)==0,substr(Horas, start = 2, stop = 3),substr(Horas, start = 1, stop = 2))]

# Clima <- Clima[substr(Horas, start = 4, stop = 5)=="00",]

# Clima[,Mes_Dia_Ir:=paste0(Mes,"-",Dia)]
# Clima <- Clima[,c("Mes_Dia_Ir","Hr","Temp","Estados")]


# x[,Hr:=substr(Hora, start = 1, stop = 2)]
# x <- merge(x,Clima,by=c("Mes_Dia_Ir","Hr"),all.x=TRUE)

# Eventos <- function(x){
	# a <- ifelse(sum(toupper(unlist(strsplit(x," "))) %in% "CUMPLEAÑOS")>0,1,0)
	# a <- ifelse(sum(toupper(unlist(strsplit(x," "))) %in% "GRUPO")>0,1,a)
	# a <- ifelse(sum(toupper(unlist(strsplit(x," "))) %in% "CELEBRACIÓN")>0,1,a)
	
	# a
# }

# x[,Even:=Eventos(Observaciones)]


######################################################################################################################

Vexp <- grep("TRUE",names(x) %in% c("Mesa","Zona.de.la.mesa","GFestivos","Origen","Reserva de grupo","Restaurante2","Hora","PAX","Tiempo_Reserva",
									"Mes_Ir","Dia_Mes_Ir","Dia_Semana_Ir","Mes_Res","Dia_Mes_Res","Dia_Semana_Res",
									"Reservas_Antes","Reservas_Antes_Rest","Antiguedad","Antiguedad_Incumplimiento"))
									
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
	Mod_RF <-randomForest(train[,Vexp],train[,Vobj], ntree=2000,doBest=TRUE,nodesize=933,classwt=c(1,0.99),mtry = 6)
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


as <- toupper(unlist(strsplit(x$Observaciones," ")))
as <- as[-c(grep("TRUE",as %in% c("EL","CON","QUE","ES","A","C:","R:","C:M","DE","Y","LA","UN","UNA","PARA","MÁS","ME")))]
sort(table(as))

aniversario
influenciadora
cumpleaños


# x[Nombre_Completo=="ZAYRA VALENZUELA",] 

# x[Nombre_Completo=="VALENTINA RODRIGUEZ",]

# a <- unique(a,by=c("N_PAX","Incumple"))

p <- predict(Mod_RF,x)
