#Leemos el archivo en donde está el borrador de la nueva constitución
Constitucion <- readLines("C:\\Users\\ramc_\\Documents\\Proyectos R\\constitución\\Convención.txt", encoding="UTF-8")
#Leemos el archivo en donde está la actual constitución
Actual <- readLines("C:\\Users\\ramc_\\Documents\\Proyectos R\\constitución\\Actual.txt", encoding="UTF-8")

library(tidyverse)
library(tidytext)

#Se crean tablas con los parrafos por cada constitución
borrador <- tibble(parrafo = seq_along(Constitucion),
                  texto = Constitucion)

actual <- tibble(parrafo = seq_along(Actual),
                  texto = Actual)

#se le agrega la cantidad de palabras por oración por cada constitución
parrafos_borrador <- borrador %>%
  unnest_tokens(oracion,
                texto,
                token = "sentences") %>%
  mutate(NumPal = str_count(oracion,
                            pattern = "\\w+"))

parrafos_actual <- actual %>%
  unnest_tokens(oracion,
                texto,
                token = "sentences") %>%
  mutate(NumPal = str_count(oracion,
                            pattern = "\\w+"))

#tokenizamos las palabras de cada uno de los textos
palabras_borrador <- borrador %>%
  unnest_tokens(palabra, texto)

palabras_actual <- actual %>%
  unnest_tokens(palabra, texto)

rm(palabras_actual,palabras_borrador)

#------------------LIMPIEZA--------------------




# Eliminamos las palabras stopwords
palabras_actual <- palabras_actual %>%
  anti_join(get_stopwords("es")
            %>%rename(palabra = word))

palabras_borrador <- palabras_borrador %>%
  anti_join(get_stopwords("es")
            %>%rename(palabra = word))

#creo una tabla con el abcedario
borrar <- tibble(palabra = c("a","b","c","d","e","f","g","h",
                             "i","j","k","l","m","n","ñ","o","p","q",
                             "r","s","t","u","v","w","x","y","z"))

#Elimino el abecedario del corpus
palabras_actual <- palabras_actual %>%
  anti_join(borrar)

palabras_borrador <- palabras_borrador %>%
  anti_join(borrar)

#Creamos una tabla con los numeros del 1 al 100000 y los volvemos character
borrar <- tibble(palabra = c(as.character(seq(1,1000000))))

#eliminamos
palabras_actual <- palabras_actual %>%
  anti_join(borrar)

palabras_borrador <- palabras_borrador %>%
  anti_join(borrar)

#borro la variable solo por eficiencia
rm(borrar)


#---------------------------------------------------------


#calculamos la frecuencia de las palabras
A <- palabras %>%
  count(palabra,
        sort = TRUE)
B <- palabras_actual %>%
  count(palabra,
        sort = TRUE)

palabras_actual <- B
palabras <- A

rm(A,B)

# Contamos cuantas letras tiene cada palabra (token)
palabras_actual <- palabras_actual %>%
  mutate(NumLetras = nchar(palabra)) 

palabras_borrador <- palabras_borrador %>%
  mutate(NumLetras = nchar(palabra)) 

summary(palabras_borrador$NumLetras)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   7.000   9.000   9.046  11.000  23.000 

summary(palabras_actual$NumLetras)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   7.000   9.000   8.667  10.000  20.000 


#-----------------------------------
  
  

palabras_actual <- rename(palabras_actual, NumPal = n)
  
