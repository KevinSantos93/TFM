library(dplyr)
library(openxlsx)
library(tidyverse)
library(CTT)
library(ltm)
library(psych)
library(readxl)
library(RColorBrewer)

#prueba <- read.xlsx('7_2023.xlsx', sep.names = " ")

prueba <- read.xlsx('7.°.xlsx')
clavesOM <- read.xlsx('7.°.xlsx', sheet = 'key')
claves <- read_excel("7.°.xlsx", sheet = "key", col_types = c("text", "text", "text", "text"), guess_max = 1000) %>%
  mutate(Clave = ifelse(grepl("[^0-9.-]", Clave), Clave, as.numeric(Clave)))

#Eliminar caracteres con regex y gsub
prueba <- prueba %>% mutate_all(~ gsub("<[^>]*>", "", .))
prueba <- prueba %>% mutate_all(~ gsub("\\$", "", .))
prueba <- prueba %>% mutate_all(~ gsub("&deg;", "°", .))
prueba <- prueba %>% mutate_all(~ gsub("&divide;", "÷", .))
prueba <- prueba %>% mutate_all(~ gsub("&times;", "×", .))
prueba <- prueba %>% mutate_all(~ gsub("&nbsp;", "", .))
prueba <- prueba %>% mutate_all(~ gsub("&aacute;", "á", .))
prueba <- prueba %>% mutate_all(~ gsub("&eacute;", "é", .))
prueba <- prueba %>% mutate_all(~ gsub("&iacute;", "í", .))
prueba <- prueba %>% mutate_all(~ gsub("&oacute;", "ó", .))
prueba <- prueba %>% mutate_all(~ gsub("&uacute;", "ú", .))

#Elimina todasl la filas que contienen valores NA en las columnas 2 a la 17
mat <- prueba %>% 
  filter(!if_all(c(11:30), is.na))


##Elimina los espacios en blanco adicionales en las observaciones
mat <- mat %>%
  mutate_all(trimws)


#Estructura de los datos
str(mat)

##Creamos una copia de la base de datos
mat2 <- mat

###Obtenemos parámetros de los ítems
mat2 <- mat2 %>%
  mutate_at(vars(starts_with("MAT")), ~ifelse(is.na(.), "C99", .)) %>%
  mutate_at(vars(starts_with("MAT")), str_remove_all, pattern = "\\s+")

notas <- score(mat2[,11:30], clavesOM$Clave, output.scored = T)
mat2$nota <- notas$score
calif <- notas$scored

#ICC
# ICC for item 1
cttICC(notas$score, notas$scored[,1], colTheme="spartans", cex=1.5)
notas$scored
notas$score

###ICC
# Ejemplo para las curvas ICC
par(mfrow = c(2, 2))  # Divide la ventana gráfica en 2 filas y 3 columnas

# Graficar las curvas ICC
for (i in 1:20) {
  cttICC(notas$score, notas$scored[,i], colTheme="spartans", cex=1.5, plotTitle = paste('item', i))
}

# Restaurar la configuración original de la ventana gráfica
par(mfrow = c(1, 1))


###Análisis de distractores
distractorAnalysis(mat2[,11:30], clavesOM$Clave, pTable = T, csvReport = "Resultados.csv")

###Reporte de parámetros de los ítems
reporte = itemAnalysis(calif, NA.Delete = F, )
reporte$itemReport
reporte$alpha
reporte$scaleMean
reporte$scaleSD

write.csv(reporte$itemReport, "itemReport.csv")


###Análisis con ltm######



######Teoría de repuesta al item#####
##Análisis factorial
rs <- fa(calif)
rs$loadings
summary(rs)

irt.fa(calif)

# Ajustar modelos con diferentes números de factores
fit1 <- fa(r = calif, nfactors = 1, fm = "minres")
fit2 <- fa(r = calif, nfactors = 2, fm = "minres")
fit3 <- fa(r = calif, nfactors = 3, fm = "minres")

# Comparar los ajustes de los modelos
summary(fit1)
summary(fit2)
summary(fit3)

# Supongamos que habilidades_estimadas es un vector con los valores de habilidad
# Si 'calif' es una matriz, conviértela a un data frame
calif <- as.data.frame(calif)

# Ahora puedes agregar la columna 'habilidades_estimadas'
#calif <- calif %>%
#  mutate(habilidades_estimadas = habilidades_estimadas)


##Ajuste de modelo 2PL
itr <- ltm(calif ~ z1, IRT.param = TRUE)
#habilidad <- factor.scores.ltm(itr)
#habilidad$coef
#habilidades_estimadas <- habilidad$score.dat
#summary(itr)
#plot(itr, items = 0, type = "IIC")
information(itr, c(-3,3))

##irtoys
library(irtoys)
plot(trf(est(calif, model = "2PL", engine = "ltm")))

###mirt
library(mirt)
library(latticeExtra)
mirt.mtf = mirt(calif, 1, itemtype = "2PL")
plot(mirt.mtf, type = "infoSE")
estimated_skills <- fscores(mirt.mtf)
calif$hab <- estimated_skills


##Curvas característica de los ítems
plot(itr)

plot.ltm(itr, plot.type = 1:5)

# Ejemplo para las curvas ICC
par(mfrow = c(2, 2))  # Divide la ventana gráfica en 2 filas y 3 columnas

# Graficar las curvas ICC
for (i in 1:20) {
  plot(itr, type = "ICC", item = i, main = paste("Ítem", i),  col = "orange", pch = 16)
}

# Restaurar la configuración original de la ventana gráfica
par(mfrow = c(1, 1))

# Ejemplo para las curvas IIC
par(mfrow = c(2, 2))  # Divide la ventana gráfica en 2 filas y 3 columnas

# Graficar las curvas ICC
for (i in 1:20) {
  plot(itr, type = "IIC", item = i, main = paste("Ítem", i),  col = "orange", pch = 16)
}

# Restaurar la configuración original de la ventana gráfica
par(mfrow = c(1, 1))


plot.ltm(itr, type = "ICC")
plot.ltm(itr, type = "IIC")
plot.ltm(itr, type = "loadings")

hist(estimated_skills, main = "Distribución de la habilidad de los estudiantes", xlab = "Habilidad", ylab = "Estudiantes")
boxplot(estimated_skills)
plot(density(estimated_skills))


hist(estimated_skills, main = "Distribución de la habilidad de los estudiantes", xlab = "Habilidad", ylab = "Estudiantes",  breaks = 20, xlim = c(-3, 3))
abline(v = mean(estimated_skills), col = "red")

###Busca de grupos con características similares (escuelas)
##aka <- merge(mat2, escuelas[, c("CÓDIGO", "DEPARTAMENTO", "MUNICIPIO", "SECTOR", "ZONA")], by.x = "COD_INFRA", by.y = "CÓDIGO")
aka <- mat2[,2:9]
aka$hab <- estimated_skills
aka$score <- notas$score
str(aka)

# Convertir todas las columnas de tipo chr a factor
aka[] <- lapply(aka, function(x) if(is.character(x)) as.factor(x) else x)

# Instalar y cargar el paquete clustMixType
# install.packages("clustMixType")
library(clustMixType)

# Aplicar k-prototypes
# install.packages("clustMixType")
#library(clustMixType)
set.seed(123) # para reproducibilidad

##Determinar el número de clusters óptimos
Es <- numeric(10)
for(i in 1:10){
  kpres <- kproto(aka, k = i, nstart = 5)
  Es[i] <- kpres$tot.withinss
}
plot(1:10, Es, type = "b", ylab = "Objective Function", xlab = "# Clusters",
     main = "Scree Plot", col = "orange", pch = 19) # figure 2

kp_res <- kproto(aka, 3) # especificar el número de clusters
validation_kproto("silhouette", kp_res) ##Parámetros del ajuste
summary(kp_res)
str(kp_res)
kp_res$tot.withinss


cluster1 <- aka[kp_res$cluster == 1, ]
cluster2 <- aka[kp_res$cluster == 2, ]
cluster3 <- aka[kp_res$cluster == 3, ]

hist(cluster1$hab)
hist(cluster2$hab)
hist(cluster3$hab)

# Agregar la asignación de cluster al dataframe
aka$cluster <- factor(kp_res$cluster)


# Crear gráfico de barras para comparar los clusters
ggplot(aka, aes(x = cluster, y = hab, fill = cluster)) +
  geom_boxplot() +
  xlab('Cluster') +
  ylab("Habilidad") +
  ggtitle("Habilidad de los evaluados por cluster") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

cluster1 %>%
  group_by(`ZONA.(RURAL/URBANA)`) %>%
  summarise(total = n(),
            media = mean(hab))

cluster1 %>%
  group_by(`TIPO.(PUBLICO/PRIVADO)`) %>%
  summarise(total = n(),
            media = mean(hab))

cluster2 %>%
  group_by(`ZONA.(RURAL/URBANA)`) %>%
  summarise(total = n(),
            media = mean(hab))

cluster3 %>%
  group_by(`ZONA.(RURAL/URBANA)`) %>%
  summarise(total = n(),
            media = mean(hab))

aka %>%
  group_by(`ZONA.(RURAL/URBANA)`) %>%
  summarise(total = n(),
            media = mean(score))
