library(tm)
library(tidyverse)
library(pdftools)
library(remotes)
library(extrafont)
install_version("Rttf2pt1", version = "1.3.8")


theme_propio2 <- function(){
  theme(
    plot.title        = element_text(size = 20,
                                     color = "black",
                                     hjust = 0, 
                                     vjust = 0.1,
                                     face = "bold"), 
    plot.subtitle     = element_text(color = "black",
                                     size = 11),
    
    plot.caption      = element_text(face = "bold", color = "black"),
    
    axis.title.x      = element_text(size = 10, color = "black", face = "bold"),
    axis.text.x       = element_text(color = "black", size = 8, face = "bold"),
    axis.title.y      = element_text(size = 10, color = "black", face = "bold" ),
    axis.text.y       = element_text(color = "black", size = 8, face = "bold"),
    
    axis.ticks        = element_line(color = "white"),
    
    panel.border      = element_rect(color = "#ffffff", fill = NA, size = 1,linetype = 1),
    
    panel.background  = element_rect(fill = "#595757"),
    
    plot.background   = element_rect(fill = "#dbdbdb"),
    
    panel.grid        = element_line(color = "#211e1e", linetype = 1, size = 0.1),
    
    legend.background = element_rect(color = "black", fill = "#D5D8DC", linetype = 0.5),
    legend.title      = element_text(face = "bold", color = "black", size = 11),
    legend.text       = element_text(face = "bold", color = "black", size = 9),
    legend.position   = "right"
  )
}


d_90_af =  pdf_text("como-mentir-con-estadisticas.pdf")                 

view(d_90_af)

d_90_af <- d_90_af[c(14,15)]

d_90_af = paste(d_90_af, collapse = " ")

d_90_af =  str_remove_all(d_90_af, "CÓMO MENTIR CON ESTADÍSTICAS")
d_90_af =  str_remove_all(d_90_af, "DARRELL HUFF")
d_90_af =  str_remove_all(d_90_af, "CÓMO MENTIR")


# Buscando los números de página 

str_count(d_90_af, "\n[:blank:]+[:digit:]+\n")

d_90_af = str_replace_all(d_90_af, "[:blank:]{2,}", " ")


# Símbolos de puntuación 

str_count(d_90_af, "[:punct:]")

d_90_af = str_remove_all(d_90_af, "[:punct:]")


# Buscando los números 

str_count(d_90_af, "[:digit:]")

d_90_af = str_remove_all(d_90_af, "[:digit:]")


# Guardando como un archivo de texto plano 

write_lines(d_90_af, "analizar")



data = scan("analizar",
                 encoding = "UTF-8", what = "char",
                 sep = "\n")


data = tibble(data) |> 
  unnest_tokens(Token, data) |>
  mutate(Token= removeNumbers(Token))

# LEYENDO STOPWORDS -------------------------------------------------------

#stopwords 1
stopwords_es_1 = read_excel("CustomStopWords.xlsx")

names(stopwords_es_1) <- c("Token","Fuente")


#stopwords 2

stopwords_es_2 = tibble(Token= c(""), Fuente="Mis StopWords")  #agregar palabras para no ser analizadas 


#Uniendo ambos stopwords

stopwords_es   <- rbind(stopwords_es_1, stopwords_es_2)
stopwords_es   <- stopwords_es[!duplicated(stopwords_es$Token),]

data = data |> anti_join(stopwords_es_1)     

dataf = data |> count(Token, sort = TRUE)   # contar y ordenar 

#Gràfica
dataf |> top_n(12) |> ggplot(aes(x=reorder(Token,n) , y = n , col=Token)) + geom_lollipop()+
  geom_text(aes(label=n , hjust=-0.25)) + coord_flip() + theme_propio2()+theme(legend.position = "none")

