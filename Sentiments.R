library(tidyverse)
library(tidytext)

sentimientos <- read_tsv("https://raw.githubusercontent.com/7PartidasDigital/AnaText/master/datos/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")

# ----------------- BING ------------------

palabras %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

recuenta_palabras_bing <- palabras %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE)

# 1 negativo     2264
# 2 positivo     1768

recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(25) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 3
  )+
  theme_bw() +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs (title= "Top 25 palabras según su polaridad encontradas en el borrador de la nueva constitución.", 
        subtitle="análisis de sentimientos bajo léxico BING \nborrador nueva contitución de Chile, Mayo 2022.", 
        x="", 
        y="Cantidad de Palabras en el borrador", 
        caption= "Texto extraido de https://www.chileconvencion.cl \n Análisis realizado por Roberto Muñoz Campos | Ingeniero informático empresarial")+
  coord_flip()


# ----------------- AFINN ------------------



palabras %>%
  right_join(get_sentiments("AFINN")) %>%
  filter(!is.na(valor)) %>%
  count(valor, sort = TRUE)

recuenta_palabras_afinn <- palabras %>%
  inner_join(get_sentiments("AFINN")) %>%
  count(palabra, valor, sort = TRUE)


# valor     n
#  1     1182
#  2     654
# -2     605
# -1     282
# -3     171
#  3     117
# -4     18
#  4     10
# -5     6
#  5     6

recuenta_palabras_afinn %>%
  group_by(valor) %>%
  top_n(10) %>%
  ggplot(aes(reorder(palabra, n), n, fill = valor)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 3
  )+
  theme_bw() +
  facet_wrap(~valor, scales = "free_y") +
  labs (title= "Top 10 palabras según su polaridad encontradas en el borrador de la nueva constitución.", 
        subtitle="análisis de sentimientos bajo léxico AFINN\nsentimientos polarizados bajo numeración desde -5 al 5 representando muy negativo a muy positivo respectivamente \nborrador nueva contitución de Chile, Mayo 2022.", 
        x="Palabra relacionada a una polaridad", 
        y="Cantidad de Palabras en el borrador", 
        caption= "Texto extraido de https://www.chileconvencion.cl \n Análisis realizado por Roberto Muñoz Campos | Ingeniero informático empresarial")+
  coord_flip()


# ----------------- NRC ------------------

palabras %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

recuenta_palabras_nrc <- palabras %>%
  inner_join(get_sentiments("nrc")) %>%
  count(palabra, sentimiento, sort = TRUE)

recuenta_palabras_nrc %>%
  group_by(sentimiento) %>% 
  filter(sentimiento != "alegría | asombro | confianza | positivo | premonición")%>%
  top_n(10) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(
    aes(label = n, y = n + 0.1),
    position = position_dodge(1), hjust = -0.05 , vjust = 0.4 , size= 2.5
  )+
  theme_bw() +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs (title= "Top 10 palabras según su sentimiento encontradas en el borrador de la nueva constitución.", 
        subtitle="análisis de sentimientos bajo léxico NRC \nborrador nueva contitución de Chile, Mayo 2022.", 
        x="Palabras por sentimiento", 
        y="Cantidad de Palabras en el borrador", 
        caption= "Texto extraido de https://www.chileconvencion.cl \n Análisis realizado por Roberto Muñoz Campos | Ingeniero informático empresarial")+
  coord_flip()


# positivo     6238
# confianza    4988
# negativo     4054
# premonición  2288
# miedo        2226
# ira          1745
# tristeza     1700
# alegría      1667
# disgusto     1455
# asombro      952