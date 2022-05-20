
#cuantas filas de palabras tiene
length(Constitucion)
# 4287

#Imprimimos las primeras 5 filas de palabras
Constitucion[1:5]
Constitucion[c(1:5)]
# [1] "Este documento es un consolidado que reúne las normas aprobadas por el Pleno de "   
# [2] "la Convención Constitucional, ordenadas por comisión. La relación de números de "   
# [3] "los artículos obedece a lo dispuesto en los respectivos informes y no es el orden " 
# [4] "definitivo, ya que ese proceso deberá ser realizado por la Comisión de Armonización"
# [5] "(Actualizado el 14.05.22)." 

library(tidyverse)
library(tidytext)

#Creamos una tabla de 2 columnas. La primera contendrá el número de parrafos y la 
#segunda se guarda el texto que está escrito en el.
mensaje <- tibble(parrafo = seq_along(Constitucion),
                  texto = Constitucion)
#Presentamos
head(mensaje)

#Dividimos los parrafos en palabras (token) unificados
mensaje_palabras <- mensaje %>%
  unnest_tokens(palabra, texto)

head(mensaje_palabras)
 
# <int> <chr>      
#   1   este       
#   1   documento  
#   1   es         
#   1   un         
#   1   consolidado
#   1   que    

#Dividimos los parrafos en oraciones (token) unificados
#y contamos cuantas palabras hay por fila
mensaje_oracion <- mensaje %>%
  unnest_tokens(oracion,
                texto,
                token = "sentences") %>%
  mutate(NumPal = str_count(oracion,
                            pattern = "\\w+"))

head(mensaje_oracion)
                                              
# <parrafo>  <texto>                                                                      <NumPal>
#   1        este documento es un consolidado que reúne las normas aprobadas por el plen~     14
#   2        la convención constitucional, ordenadas por comisión.                             6
#   2        la relación de números de                                                         5
#   3        los artículos obedece a lo dispuesto en los respectivos informes y no es el~     15
#   4        definitivo, ya que ese proceso deberá ser realizado por la comisión de armo~     13
#   5        (actualizado el 14.05.22).                                                        5

sum(mensaje_oracion$NumPal)
# 49626 palabras totales

#palabras más frecuentes
frecuentes <- mensaje_palabras %>%
  count(palabra,
        sort = TRUE)
remove(frecuentes)

#    palabra      n
#     <chr>     <int>
# 1    de       3751
# 2    la       2806
# 3    y        2416
# 4    el       1387
# 5    las      1189
# 6    en       1152
# 7    los      1112
# 8    a        969
# 9    que      881
# 10   o        672

# Contamos cuantas letras tiene cada palabra (token)
mensaje_palabras <- mensaje_palabras %>%
  mutate(NumLetras = nchar(palabra))

# Longitudes de palabras
longitudes_palabras <- mensaje_palabras %>%
  count(NumLetras)

hist(mensaje_palabras$NumLetras)


#------------------LIMPIEZA--------------------




# Eliminamos las palabras stopwords
mensaje_palabras <- mensaje_palabras %>%
  anti_join(get_stopwords("es")
            %>%rename(palabra = word))

#creo una tabla con el abcedario
borrar <- tibble(palabra = c("a","b","c","d","e","f","g","h",
                             "i","j","k","l","m","n","ñ","o","p","q",
                             "r","s","t","u","v","w","x","y","z"))

#Elimino el abecedario del corpus
mensaje_palabras <- mensaje_palabras %>%
  anti_join(borrar)

#Creamos una tabla con los numeros del 1 al 100000 y los volvemos character
borrar <- tibble(palabra = c(as.character(seq(1,1000000))))

#eliminamos
mensaje_palabras <- mensaje_palabras %>%
  anti_join(borrar)

#borro la variable solo por eficiencia
rm(borrar)



# 4566 palabras de 1 letra
# 1 palabra de 23 letras "interdisciplinariamente"

# Mensaje frecuencias
frecuencias <- mensaje_palabras %>%
  count(palabra, sort = T) %>%
  mutate(F_relativa = n / sum(n)) 

#---------------------------------------

summary(mensaje_oracion$NumPal)
#  Min.  1st Qu.  Median  Mean  3rd Qu.  Max. 
#  0.00   6.00    11.00   9.61  14.00   29.00 

ggplot(mensaje_oracion,
       aes(1:nrow(mensaje_oracion),
           NumPal)) +
  geom_bar(stat = 'identity') +
  geom_hline(yintercept = mean(mensaje_oracion$NumPal),
             linetype = "solid",
             colour = "yellow",
             size = 0.7) +
  geom_hline(yintercept = max(mensaje_oracion$NumPal),
             linetype = "solid",
             colour = "red",
             size = 0.1) +
  geom_hline(yintercept = median(mensaje_oracion$NumPal),
             linetype = "solid",
             colour = "blue",
             size = 0.2) +
  labs(x = "Número de oración", y="Número de palabras") +
  ggtitle("Número de palabras por oración",
          subtitle = "borrador nueva constitución, Mayo 2022.")

#----------------------------------------------------------------

#Eliminamos stop_words

"Eliminamos las palabras stopwords"
palabras <- mensaje_palabras %>%
  anti_join(get_stopwords("es")
            %>%rename(palabra = word))

"creo una tabla con el abcedario"
munabc <- tibble(palabra = c("a","b","c","d","e","f","g","h",
                            "i","j","k","l","m","n","ñ","o","p","q",
                            "r","s","t","u","v","w","x","y","z"))

"Elimino el abecedario del corpus"
palabras <- palabras %>%
  anti_join(munabc)

" Creamos una tabla con los numeros del 1 al 1000 y los volvemos character"
munabc <- tibble(palabra = c(as.character(seq(1,1000))))

"eliminamos"
palabras <- palabras %>%
  anti_join(munabc)

summary(palabras$NumLetras)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   7.000   8.000   8.396  10.000  23.000 


view(palabras %>%
  count(palabra,
        sort = TRUE))

remove(frecuentes)


view(
  palabras %>%
  count(palabra, sort = T) %>%
  mutate(F_relativa = n / sum(n)) 
)


mensaje_palabras %>%
  count(palabra, sort = T) %>%
  top_n(40,n) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_bw() +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 3.2
  )+
  theme(aspect.ratio = 1)+
  theme(legend.position = "none") +
  labs (title= "Palabras más utilizadas en el borrador de la nueva constitución.", 
        subtitle="borrador nueva contitución, Mayo 2022.", 
        x="Frecuencia de palabras", 
        y="Cantidad de Palabras", 
        caption= "Texto extraido de https://www.chileconvencion.cl \n Análisis realizado por Roberto Muñoz Campos",
  )+
  coord_flip()

palabras %>%
  count(NumLetras, sort = T) %>%
  mutate(NumLetras = reorder(NumLetras, n)) %>%
  ggplot(aes(x = NumLetras, y = n, fill = NumLetras)) +
  geom_bar(stat="identity") +
  theme_bw() +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 3
  )+
  theme(aspect.ratio = 1)+
  theme(legend.position = "none") +
  labs (title= "Largo de las palabras utilizadas en el borrador de la nueva constitución.", 
        subtitle="borrador nueva contitución de Chile, Mayo 2022.", 
        x="Número de letras por palabra", 
        y="Cantidad de Palabras", 
        caption= "Texto extraido de https://www.chileconvencion.cl \n Análisis realizado por Roberto Muñoz Campos",
  )+
  coord_flip()

------------------------------------------------------------------------------------
  
  

#cuantas filas de palabras tiene
length(actual)
# 3609

actual_mensaje <- tibble(parrafo = seq_along(actual),
                  texto = actual)
actual_palabras <- actual_mensaje %>%
  unnest_tokens(palabra, texto)

"Eliminamos las palabras stopwords"
actual_palabras <- actual_palabras %>%
  anti_join(get_stopwords("es")
            %>%rename(palabra = word))

"creo una tabla con el abcedario"
munabc <- tibble(palabra = c("a","b","c","d","e","f","g","h",
                             "i","j","k","l","m","n","ñ","o","p","q",
                             "r","s","t","u","v","w","x","y","z"))

"Elimino el abecedario del corpus"
actual_palabras <- actual_palabras %>%
  anti_join(munabc)

" Creamos una tabla con los numeros del 1 al 1000 y los volvemos character"
munabc <- tibble(palabra = c(as.character(seq(1,1000))))

"eliminamos"
actual_palabras <- actual_palabras %>%
  anti_join(munabc)


actual_palabras <- actual_palabras %>%
  mutate(NumLetras = nchar(palabra))

actual_frecuencias <- actual_palabras %>%
  count(palabra, sort = T) %>%
  mutate(F_relativa = n / sum(n)) 



actual_palabras %>%
  count(palabra, sort = T) %>%
  filter(n > 75) %>%
  mutate(palabra = reorder(palabra, n)) %>%
  ggplot(aes(x = palabra, y = n, fill = palabra)) +
  geom_bar(stat="identity") +
  theme_bw() +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 3
  )+
  theme(aspect.ratio = 1)+
  theme(legend.position = "none") +
  labs (title= "Palabras más utilizadas en la actual constitución de Chile.", 
        subtitle="creada en octubre de 1980 y actualizada en diciembre de 2019.", 
        x="Frecuencia de palabras", 
        y="Cantidad de Palabras", 
        caption= "Texto extraido de https://www.camara.cl \n Análisis realizado por Roberto Muñoz Campos",
  )+
  coord_flip()





actual_palabras <- actual_palabras %>% mutate(Constitucion = 0)
palabras <- palabras %>% mutate(Constitucion = 1)

as.data.frame(actual_palabras)
as.data.frame(palabras)

merge(actual_palabras, palabras)
constituciones <- bind_rows(palabras, actual_palabras)


install.packages(gridExtra)
install.packages(scales)

library(gridExtra)
library(scales)

p1 <- ggplot(constituciones, aes(1, 0)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = palabra), check_overlap = TRUE, vjust = 1.5, size=3) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red", size = 1) +
  theme_bw() +
  labs (title= "Similaridad entre palabras publicadas escritas por los candidatos", 
        subtitle="al hacer una publicaci???n en Twitter", 
        y="Gabriel Boric Font", 
        x="Jos??? Antonio Kast", 
        caption= "Datos extraidos desde las timeline de cada candidato, hasta 18/12/2021.\n Autor: Roberto Mu???oz Campos"
  )

p1

grid.arrange(p1, nrow = 1)




A <- mensaje_palabras
A %>% mutate(lemma_dict = lematiza(palabra))
+ s


