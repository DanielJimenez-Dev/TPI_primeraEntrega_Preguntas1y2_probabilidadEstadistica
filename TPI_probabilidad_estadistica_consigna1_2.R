##########################################################
# Trabajo Practico Integrador CONSIGNA 1 Y 2 
# Materia: Probabilidad y Estadistica
# Alumno: Daniel Alberto Jiemenez Valderrama
# Comisión: 12
#########################################################

#Consigna 1

# Cargamos el Excel

#Cargamos la libreria library(readxl)
library(readxl)

# Leemos el archivo excel 
datos <- read_excel("C:\\Users\\jimen\\OneDrive\\Documentos\\Pragramacion_R\\TPI_Probavilidad_Estadistica\\TPI_probabilidad_estadistica_1er_entrega\\TUPAD-2025-EST-TPI-planilla4.xlsx")

# Para tener un mejor orden, procedo a renombrar las variables con nombres mas simples, manteniendo el orden.

names(datos) <- c(
  "nro_encuenta","genero","fuma","nro_hnos","peso_kg","estatura_cm","edad",
  "gasto_aliment_semanal","cant_materias_aprob","satisfaccion","horas_estudio",
  "trabaja"
)

# Procedemos a seleccionar la variable a estudiar(horas de estudio)
# A.- Tiempo en horas semanales dedicadas al estudio. (Determinar la cantidad óptima de intervalos a utilizar)

# Guardamos la columna en un objeto'x' y removemos NA (si los hubiera)

x <- datos$horas_estudio
x <- x[!is.na(x)]

# Procedemos a calcular el tamaño muestral (cantidad de estudiantes con datos validos)
n <- length(x)

# Determinamos la cantidad optima de intervalos utilizando la regla de Sturges
# formula: k = 1 + 3.322 * log10(n)

k_sturges <- 1 + 3.322 * log10(n)

# usamos ceil para trabajar con un numero entero de intervalos
k <- ceiling(k_sturges)

#Redondeamos a 4 decimales segun lo requerido en el ejercicio
k_sturges <- round(k_sturges, 4)

# Calculamos rango y amplitud de clases
R <- max(x) - min(x)

# Amplitud teorica de cada clase A = R / k
A <- R / k

# Redondeamos valores de reporte a 4 decimales
R_rep <- round(R, 4)
A_rep <- round(A, 4)
min_x <- min(x)
max_x <- max(x)

# Construimos los puntos de breaks de las clases

breaks <- c(min_x, min_x + A * (1:(k -1)), max_x)

breaks_rep <- round(breaks, 4)

# Clasificamos los datos en los intervalos y armamos la taba de frecuencia
# usamos intervalos cerrados a la derecha: (a, b)
# utilizamos incluide.lowest=true para que el vaor minimo caiga en la primera clase

clases <- cut(
  x,
  breaks = breaks,
  include_lowest = TRUE,
  right = TRUE,
  dig.lab = 10
)

# Realizamos freccuencia absoluta por intervalo
f <- as.numeric(table(clases))

# frecuencia relativa proporcion y porcentaje
fr <- round(f / n, 4)
porc <- round(fr * 100, 4)

#Frecuencia acumulada y porcentaje
F_ac <- cumsum(f)
Fr_ac <- round(F_ac / n, 4)
porc_ac <- round(Fr_ac * 100, 4)

# Armamos la tabla final de frecuencias
tabla_horas <- data.frame(
  Intervalo = levels(clases),
  f = f,                      #Frecuencia absoluta
  fr = fr,                   #Frecuencia relativa
  porc = porc,               #Porcentaje
  F = F_ac,                  #frecuencia acumulada
  Fr = Fr_ac,                #frecuencia relativa acumulada
  porc_ac = porc_ac          # procentaje acumulado
)

# Mostramos lor resultados
cat("n =", n, "\n")
cat("Sturges (k real) =", k_sturges, "-> k (entero) =", k, "\n")
cat("Rango (R) =", R_rep, "\n")
cat("Amplitud (A) =", A_rep, "\n")
cat("Cortes (breaks) =", paste(breaks_rep, collapse = " | "), "\n\n")

#tabla de frecuencias de horas de estudio
print(tabla_horas, row.names = FALSE)

##########################################################################

# Nivel de satisfaccion de la carrera:
#Objetivo: tabla de frecuencias para la variable cualitativa ordinal “satisfacción”.
# En el cuestionario: 1=Muy satisfecho, 2=Satisfecho, 3=Insatisfecho, 4=Muy insatisfecho

sat_num <- datos$satisfaccion
sat_num <- sat_num[!is.na(sat_num)]     # quitamos NA por seguridad
n_sat <- length(sat_num)                # cantidad válida

# Convertimos a factor
sat <- factor(
  sat_num,
  levels = c(1, 2, 3, 4),
  labels = c("Muy satisfecho", "Satisfecho", "Insatisfecho", "Muy insatisfecho"),
  ordered = TRUE
)

#Construimos la tabla de frecuencia

# Frecuencia absoluta por categoría
f_sat <- as.numeric(table(sat))

# Frecuencia relativa y porcentaje (4 decimales)
fr_sat <- round(f_sat / n_sat, 4)
porc_sat <- round(fr_sat * 100, 4)

# Acumuladas (respetando el orden ordinal)
F_sat  <- cumsum(f_sat)
Fr_sat <- round(F_sat / n_sat, 4)
porc_ac_sat <- round(Fr_sat * 100, 4)

# Tabla final
tabla_sat <- data.frame(
  Categoria = levels(sat),
  f = f_sat,
  fr = fr_sat,
  porc = porc_sat,
  F = F_sat,
  Fr = Fr_sat,
  porc_ac = porc_ac_sat
)

# Mostrar tabla
print(tabla_sat, row.names = FALSE)
