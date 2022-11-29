## Que tanto llovió en el 2021?, de donde tomamos los datos
# con datos de: https://smn.conagua.gob.mx/es/climatologia/temperaturas-y-lluvias/resumenes-mensuales-de-temperaturas-y-lluvias

#Necesitamos el paquete devtools, para poder instalar otro paquete
### Cuando quise instalar el paquete devtools me pidio instalar el paquete de usethis
install.packages("usethis")
install.packages("devtools")

#### Instalar paquetes ####
install.packages("devtools") #pqquete que te permite instalar otro paquete
install.packages("dplyr")
install.packages("vroom") ##paquete que lee csv
install.packages("tidyr")
install.packages("ggplot2")
install.packages("lubridate")

#### Cargamos paquetes ####
library("devtools")
library("dplyr")
library("vroom")
library("tidyr")
library("ggplot2")
library("lubridate")

#### Cargamos datos ####
lluvias <- vroom ( file = "lluvia.csv", show_col_types = F) #uso vroom porque me deja leer mas fácil los files csv

# transformamos a formato largo de solo una columna
largo <- pivot_longer( data = lluvias,                   # datos
                       cols = -Entidad,                  # transformar todo menos la columna de Entidad
                       names_to = "mes",                 # la nueva columna por mes
                       values_to = "precipitacion_mm" )  # cuanto llovio

largo ## vemos el formato

# Ponemos el mes en formato date
confecha <- largo %>%
  mutate( mes = as.numeric( mes ),              # convertimos el mes a formato numero, y luego a fecha
          fecha = month( x = mes,               # en una nueva columna ponemos el nombre del mes
                         label = TRUE ) ) %>%   # la funcion month convierte un numero a mes
  filter( precipitacion_mm != 0 )               # quitamos los 0 que significa que no llovio con filter

#### Crear grafico en forma de gotas ####
ggotas <- ggplot( data = confecha,   # datos de donde haremos el grafico
                  mapping = aes( x = fecha,               # En x van los meses
                                 y = Entidad  ) ) +       # En y van los estados
  geom_point( mapping = aes( size = precipitacion_mm ),   # graficamos las precipitaciones
              fill = "skyblue",
              shape = 21)

# Vemos el grafico
ggotas

# Ordenamos para una mejor visualizacion

# Calculamos el promedio anual por estado y agruopamos por estado
promedio <- confecha %>%
  group_by( Entidad ) %>%                                # agrupamos por estado
  summarize( promedio = mean( precipitacion_mm ) ) %>%   # calculamos el promedio por grupo (por estado)
  arrange( -promedio  )                                  # Ordenamos por la columna promedio en orden descendiente (-promedio)

# Sacamos el orden
ordenados <- promedio %>%
  pull( Entidad )          # Sacamos el vector de los nombres ordenados

#hacemos el primer grafico
ggotas2 <- ggotas +
  scale_y_discrete( limits = ordenados ) +                 # le pasamos el vector con los nombres de los estados en el orden que queremos
  labs( title = "Promedio mensual de lluvia en Mexico",   # ponemos titulo
        x = "2021",                                       # cambiamos nombre del eje x
        y = "",                                           # Borramos el nombre del eje y
        size = "Precipitacion (mm)" )                     # cambiamos el titulo de la leyenda

# View
ggotas2

####y si lo visualizamos por estado en un mapa ####

##instalamos mxmaps desde un github, usamos la funcion install_github
install_github("diegovalle/mxmaps")

#cargamos la libreria
library("mxmaps")

#otra formade leer una tabla
lluviamap <- read.table(file = "lluvia.csv",
                             header = T, sep = ",")

#agrupamos de nuevo por estado
total_lluvia <- confecha %>%
  group_by( Entidad ) %>%
  summarize( value = mean( precipitacion_mm ) )  #importante nombrar con value la variable para que sea reconocido

#Utilizamos un diccionario del INEGI para ajustar los datos
# al formato del paquete mxmaps
diccionario.df <- read.table(file = "diccionario_mapa.tsv",
                             header = T, sep = "\t")
#arregle manualmente el diccionario
diccionario.df <- read.table(file = "diccionario_mapa2.tsv",
                             header = T, sep = "\t")


lluviaplotable <- left_join( x = diccionario.df,
                          y = total_lluvia,
                            by = "Entidad" ) %>%
  mutate( region = as.character(region)) ##Convertimos la columna de region a caracter


library("ggplot2")
library("mxmaps")
#Ver en totales
mapa.p <- mxstate_choropleth(df = lluviaplotable,
                             num_colors = 1,   #Este numero decide cuantas "canastas" o bins se haran para repartir el rango de valores
                             title = "Promedio lluvia anual 2021, por estado",
                             legend = "precipitaciones mm") +
  theme( plot.title = element_text( hjust = 0.5 )) #centramos el titulo con hjust =

mapa.p


mapa2.p <- mxstate_choropleth(df = lluviaplotable,
                             num_colors = 9,   #Este numero decide cuantas "canastas" o bins se haran para repartir el rango de valores
                             title = "Promedio lluvia anual 2021, por estado",
                             legend = "precipitaciones mm") +
  theme( plot.title = element_text( hjust = 0.5 )) #centramos el titulo con hjust =

mapa2.p



#podemos hacer un zoom

mapa3.p <- mxstate_choropleth(df = lluviaplotable,
                              num_colors = 9,
                              zoom = subset(lluviaplotable %in%
                                              c("Puebla",
                                                "Michoacán",
                                                "Ciudad de México"))$region,
                              #Este numero decide cuantas "canastas" o bins se haran para repartir el rango de valores
                              title = "Promedio lluvia anual 2021, por estado",
                              legend = "precipitaciones mm") +
  theme( plot.title = element_text( hjust = 0.5 )) #centramos el titulo con hjust =

mapa2.p


#
ggsave( filename = "lluvia.jpg",    # El nombre del archivo resultante
        plot = mapa2.p,             # guardamos la ultima version del plot
        width = 5,                 # ancho de 5 pulgadas
        height = 7,                # alto de 7 pulgadas
        dpi = 300 )                # resolucion de 300 puntos por pulgada

# FIN DEL EJERCICIO
# BIOFREELANCER 2021
