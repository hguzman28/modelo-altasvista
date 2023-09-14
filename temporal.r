

xx <- Datos(Fecha_Ini="2023-02-01",Fecha_Fin="2023-02-28")
x <- xx

xx <- Datos(Fecha_Ini="2023-08-01",Fecha_Fin="2023-08-30")
xx <- xx[,1:48]
x = list(x,xx)
x <- rbindlist(x)