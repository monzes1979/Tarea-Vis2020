
#Tarea 1
##Código Juan Martín Montes

###Practicar la realización de mapas y gráficos interactivos y no interactivos, aplicando estos conocimientos prácticos en datos de su interés.

####Elabore un documento en HTML (en RMarkdown) que contenga al menos 4 de los siguientes 5 elementos:

#Una gráfica en ggplot. 
#Un mapa estático en ggplot.
#Una gráfica en plotly (u otra librería de visualización interactiva)
#Un mapa en leaflet (u otra librería de visualización interactiva)
#Una tabla

library(tidyverse)
library(sf)
library(leaflet)
library(plotly)


pobrezaExtrema <- read_csv("01_datos/pobrezaExtrema.csv", 
                           col_types = cols(Valor = col_number()))

mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE) %>%
  filter (NOM_ENT == "Coahuila de Zaragoza" | NOM_ENT == "Colima" | NOM_ENT == "Ciudad de México" | NOM_ENT == "Nuevo León" | NOM_ENT == "Querétaro" | NOM_ENT == "Sonora")

edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE)

pobrezaExtrema$Entidad %>% 
  unique()

pobreza <- pobrezaExtrema %>% 
  filter (Entidad == "Coahuila" | Entidad == "Colima" | Entidad == "Distrito Federal" | Entidad == "Nuevo León" | Entidad == "Querétaro" | Entidad == "Sonora") %>% 
  filter(Valor < 0.8 & Year == "2010")

view(mpios)
view(pobreza)

mpios$NOM_ENT %>% unique()
glimpse(pobrezaExtrema)

view(pobrezaExtrema)
summary(pobrezaExtrema$Valor)

sort(pobrezaExtrema$Valor, decreasing = T) %>% 
  tail(n=20)

pobreza <- pobrezaExtrema %>% 
  filter (Valor < 0.8 & Year == "2010")

view(pobreza)

#####UNA GRÁFICA EN GGPLOT 

pobreza %>% 
  ggplot(aes(x = Municipio,
             y = Valor, fill = Entidad, text = paste0("<b>Entidad: </b>", Entidad, "<br>", "<b>Valor: </b>", Valor, "<br>")))+
  geom_col()+
  coord_flip() +
  labs (title = "Los municipios con menor porcentaje de pobreza extrema de 2010")+
  scale_fill_manual(values= c("orange", 
                              "brown", 
                              "steelblue", 
                              "purple", 
                              "red",
                              "seagreen"))+
  theme(legend.position = "none")


####HACER UNA GRÁFICA INTERACTIVA
ggplotly(tooltip = "text") %>% 
  config(displayModeBar = F)


####UN MAPA ESTÁTICO EN GGPLOT

nuevoobjeto <- pobrezaExtrema %>% 
  filter(Valor < 0.8 & Year == "2010")

mapa <- merge(x = mpios, 
              y = nuevoobjeto,
              all.y = TRUE, 
              by.x = "CVE_MUN",
              by.y = "CVE_MUN")

view(mapa)
head(pobrezaEx)
view(nuevoobjeto)

nuevoobjeto$CVE_MUN %>% 
  unique()

mpios$CVE_MUN %>% 
  unique()

class(mapa)

mapa %>% 
  ggplot(aes(fill = Valor)) +
  geom_sf(data = edo, fill = NA, color = "purple")+
  geom_sf(data = mpios, fill = "white", color = "black")+
  geom_sf()


####UN MAPA INTERACTIVO EN GGPLOT

leaflet(mapa) %>%
  addTiles() %>%
  addPolygons(data = edo, fill = NA, color = "purple") %>% 
  addPolygons(data = mpios, fill = "white", color = "black") %>% 
  addPolygons(opacity = 0.3, popup = paste0("<b>Municipio: </b>", mapa$Municipio, "<br>", "Valor:", mapa$Valor))

head(pobrezaExtrema)
head(pobreza)
head(mapa)
