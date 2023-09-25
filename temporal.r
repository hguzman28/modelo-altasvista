

xx <- Datos(Fecha_Ini="2023-09-14",Fecha_Fin="2023-09-14")
x <- xx

xx <- Datos(Fecha_Ini="2023-08-01",Fecha_Fin="2023-08-30")
xx <- xx[,1:48]
x = list(x,xx)
x <- rbindlist(x)