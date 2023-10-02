library(rvest)
library(data.table)
library(lubridate)
library(randomForest)
library(mongolite)


Calificar <- function(FI = "2023-09-01", FF = "2023-09-01", Parametro = 0.63) {
  tryCatch({
    load("./Modelo_Final.RData")
    load("./Funciones.r")

    x <- Datos(Fecha_Ini = FI, Fecha_Fin = FF)

    x <- Prep_Data(x)

    x <- x[Fecha == FI, ]

    p <- predict(Mod_RF, x, type = "prob")[, 2]
    x <- cbind(x, P_RF2 = p)
    x[, P_RF := ifelse(P_RF2 > Parametro, 1, 0)]

    fwrite(x, "./Resultados.csv", bom = TRUE)
  }, error = function(e) {
    cat("Error al cargar los archivos:", e$message, "\n")
  })
}

Calificar(FI = "2023-09-01", FF = "2023-09-01", Parametro = 0.63)
