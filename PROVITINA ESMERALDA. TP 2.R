# Trabajo Practico Nº 2
# Alumna: Esmeralda Provitina

# Carga de librerías ------------------------------------------------------
library(dplyr) ### Para manipular el DataFrame
library(ggplot2) ### Para visualizarlos


# Data Wrangling ----------------------------------------------------------
DF <- read.csv("7_smoking_prediction.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
###Ver nombres de variables
names(DF)
###Eliminación de casos incompletos
DF <- DF[complete.cases(DF),]
### Verificación de datos faltantes
anyNA(DF)
### Identificación de valores faltantes
is.na(DF)
### Verificación de valores numéricos inválidos
sapply (DF, is.infinite)
sapply(DF, is.nan)
###Eliminación de variable irrelevante
DF$ID <- NULL

# Análisis exploratorio ---------------------------------------------------
sapply(DF, class)
str(DF)
summary(DF)
class(DF)
numericas <- DF[sapply(DF,is.numeric)]
summary(numericas)
cualitativas <- DF[!sapply(DF,is.numeric)]
summary(cualitativas)
sapply(numericas, IQR)
sapply(numericas, range)
sapply(numericas, quantile)
cor(numericas)
sapply(numericas, mean)
sapply(numericas,median)


# Planteo de preguntas ----------------------------------------------------
# 1) ¿Cuál es el porcentaje de fumadores y no fumadores?
# 2) ¿La proporción de fumadores cambia según el género? ¿Cómo se distribuye?
# 3) ¿Cuál es la edad promedio de los fumadores en comparación de los que no
# fuman?
# 4) ¿Cuál es el nivel promedio de glucosa (azúcar en sangre en ayunas) en 
# fumadores y en los no fumadores?
# 5) ¿Existe relación entre la edad y el IMC aquí?
# 6) ¿Hay diferencia entre en la presión arterial entre fumadores y no fumadores?
# 7) ¿Existen diferencias en los niveles de grasas en sangre entre los que fuman
# y los que no?
# 8) ¿Los fumadores tienen los niveles de hemoglobina más altos?
# 9) ¿Los fumadores tienen peor salud bucal, teniendo en cuenta los indicadores
# de sarro y caries?
# 10) ¿Hay diferencias en la distribución del IMC entre fumadores y no fumadores?


# Respuestas a los interrogantes mediante tablas y gráficos ---------------
#Convertimos a factor para mejor manejo
DF$smoking_f <- factor(DF$smoking, levels = c(0, 1), labels = c("No fumador", "Fumador"))
DF$gender_f <- factor(DF$gender)

cat ("\n--- Tabla de contingencia cruzada ---\n")
tabla_fuma_gen <- table(DF$gender_f, DF$smoking_f)
print(tabla_fuma_gen)

cat ("\n--- Proporción de fumadores s/ género ---\n")
print(round(prop.table(tabla_fuma_gen, margin = 1) * 100, 2))

# Resumen usando dplyr y pipes
resumen_fum <- DF %>%
  group_by(gender_f, smoking_f) %>%
  summarise(cantidad = n (), .groups = 'drop') %>%
  group_by(gender_f) %>%
  mutate(porcentaje = cantidad / sum(cantidad) * 100)

cat("\n--- Resumen dplyr ---\n")
print(resumen_fum)

# Gráfico n°1

gr1 <- ggplot(resumen_fum, aes(x = gender_f, y = cantidad, fill = smoking_f)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_fill(vjust = 0.5), 
            color = "white") +
            labs(title = "1. Proporción de fumadores según género",
                 subtitle = "Distribución porcentual por género",
                 x = "Género", y = "Proporción", fill = "Estado") +
              theme_minimal()

# Gráfico n°2

gr2 <- ggplot(DF, aes(x = gender_f, fill = smoking_f)) +
  geom_bar(position = "dodge") +
  coord_flip() +
  labs(title = "2. Distribución agrupada de fumadores por género horizontal",
       x = "Género", y = "Cantidad absoluta", fill = "Estado") + 
  theme_bw()

# Gráfico n° 3

gr3 <- ggplot(DF, aes(x = smoking_f, fill = gender_f)) +
  geom_bar() +
  facet_wrap(~ gender_f) +
  labs (title = "3. Distribución separada por facetas",
        x= "Estado", y = "Cantidad absoluta") +
  theme_light() +
  theme(legend.position = "none")

# Gráfico n° 4

gr4 <- ggplot(DF, aes(x = gender_f, y = smoking_f)) +
  geom_count(color = "steelblue") +
  labs(title = "4. Frecuencia de combinaciones",
       x = "Género", y = "Estado", size = "N° de casos") +
  theme_classic()

# Gráfico n° 5

gr5 <- ggplot(resumen_fum, aes(x = smoking_f, y = porcentaje, fill = gender_f))+
                               geom_bar(stat = "identity", position = "dodge")+ 
                                 labs(title = "5. Comparativa de porcentajes",
                                      x ="Estado", y = "Porcentaje (%)", fill = "Género") +
                                 theme_minimal()

print(gr1)
print(gr2)
print(gr3)
print(gr4)
print(gr5)



                              
                               
                               
                               