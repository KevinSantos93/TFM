library(dplyr)
library(openxlsx)
library(tidyverse)
library(CTT)
library(ltm)
library(psych)
library(readxl)
library(RColorBrewer)

prueba <- read.xlsx('7.°.xlsx')
prueba <- prueba[,1:30]
clavesOM <- read.xlsx('7.°.xlsx', sheet = 'key')
mat <- prueba %>% 
  filter(!if_all(c(11:30), is.na))

mat <- mat %>%
  mutate_at(vars(starts_with("MAT")), ~ifelse(is.na(.), "C99", .)) %>%
  mutate_at(vars(starts_with("MAT")), str_remove_all, pattern = "\\s+")

notas <- score(mat[,11:30], clavesOM$Clave, output.scored = T)
calif <- mat[,1:9]
calif <- cbind(calif, data.frame(notas$scored))
calif$nota <- (notas$score/20)*10

resultados <- calif %>%
  group_by(CÓDIGO.DE.INFRAESTRUCTURA) %>%
  summarise(MAT1 = round(mean(MAT1), 2),
            MAT2 = round(mean(MAT2), 2),
            MAT3 = round(mean(MAT3), 2),
            MAT4 = round(mean(MAT4), 2),
            MAT5 = round(mean(MAT5), 2),
            MAT6 = round(mean(MAT6), 2),
            MAT7 = round(mean(MAT7), 2),
            MAT8 = round(mean(MAT8), 2),
            MAT9 = round(mean(MAT9), 2),
            MAT10 = round(mean(MAT10), 2),
            MAT11 = round(mean(MAT11), 2),
            MAT12 = round(mean(MAT12), 2),
            MAT13 = round(mean(MAT13), 2),
            MAT14 = round(mean(MAT14), 2),
            MAT15 = round(mean(MAT15), 2),
            MAT16 = round(mean(MAT16), 2),
            MAT17 = round(mean(MAT17), 2),
            MAT18 = round(mean(MAT18), 2),
            MAT19 = round(mean(MAT19), 2),
            MAT20 = round(mean(MAT20), 2),
            nota = mean(nota))

write.xlsx(resultados, 'resultados.xlsx')
write.xlsx(calif[,3:30], 'estudiantes.xlsx')
