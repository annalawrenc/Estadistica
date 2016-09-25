############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendra el precio (entre otras variables interesantes) de unos 54.000 diamantes.

# Objetivo : Realizar distintos tipos de analisis estadistico de sus variables para intentar
# averiguar algun tipo de comportamiento oculto aparentemente en los datos. 

# Pasos: 
# 0. Muestra representativa
# 1. Tipos de variables
# 2. Medidas de posicion central
# 3. Medidas de dispersion
# 4. Distribucion y relacion entre ellas
# 5. Analisis de regresion




#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# 0. Muestra representativa
# Selecciona una muestra representativa para "cut"

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Para un análisis de prueba de concepto se utilizaria una muestra representativa. 
# Para obtener una muestra representativa necesito una extraccion que tenga el mismo porcentaje de observaciones de un determinado tipo que se pueden observar en la población.
# La poblacion es en este caso el dataset de diamantes

library(ggplot2)
dt<-as.data.frame(diamonds) # cargo los datos como date frame
head(dt) #veo las primeras filas


# Primero voy a comprobar que porcentaje de cada tipo de Cut existe en población.


Fair<-dt [dt$cut=='Fair',]
Good<-dt [dt$cut=='Good',]
VeryGood<-dt [dt$cut=='Very Good',]
Premium<-dt [dt$cut=='Premium',]
Ideal<-dt [dt$cut=='Ideal',]

Porcentajes <-c(nrow(fair)/nrow(dt),nrow(Good)/nrow(dt),nrow(VeryGood)/nrow(dt),nrow(Premium)/nrow(dt),nrow(Ideal)/nrow(dt))
Porcentajes

# Tambien puedo representar el reparto como porcentajes en un grafico. Para ello usare la función pie_chart



# ****************************funcion para representar % del total en un grafico circular*******************************************************

# *******fuente; http://stackoverflow.com/questions/26392818/r-pie-chart-with-percentage-as-labels-using-ggplot2********************************


pie_chart <- function(df, main, labels = NULL, condition = NULL) {
  
  # convert the data into percentages. group by conditional variable if needed
  df <- group_by_(df, .dots = c(condition, main)) %>%
    summarize(counts = n()) %>%
    mutate(perc = counts / sum(counts)) %>%
    arrange(desc(perc)) %>%
    mutate(label_pos = cumsum(perc) - perc / 2,
           perc_text = paste0(round(perc * 100), "%"))
  
  # reorder the category factor levels to order the legend
  df[[main]] <- factor(df[[main]], levels = unique(df[[main]]))
  
  # if labels haven't been specified, use what's already there
  if (is.null(labels)) labels <- as.character(df[[main]])
  
  p <- ggplot(data = df, aes_string(x = factor(1), y = "perc", fill = main)) +
    
    # make stacked bar chart with black border
    geom_bar(stat = "identity", color = "black", width = 1) +
    
    # add the percents to the interior of the chart
    geom_text(aes(x = 1.25, y = label_pos, label = perc_text), size = 4) +
    
    # add the category labels to the chart
    # increase x / play with label strings if labels aren't pretty
    geom_text(aes(x = 1.82, y = label_pos, label = labels), size = 4) +
    
    # convert to polar coordinates
    coord_polar(theta = "y") +
    
    # formatting
    scale_y_continuous(breaks = NULL) +
    scale_fill_discrete(name = "", labels = unique(labels)) +
    theme(text = element_text(size = 22),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  # facet wrap if that's happening
  if (!is.null(condition)) p <- p + facet_wrap(condition)
  
  return(p)
}

#**********************************************************************************************************************************************

# para utilizar la funcion pie_chart necesito las librerias ddplot2 y dplyr

library(ggplot2)
library(dplyr)

pie_chart(dt, main = "cut", labels = NULL) +
  labs(title = "porcentaje cut poblacion")

# también puedo ver porcentaje de otras variables como color y clarity.

pie_chart(muestra2, main = "color", labels = NULL) +
  labs(title = "porcentaje color poblacion")

pie_chart(dt, main = "clarity", labels = NULL) +
  labs(title = "porcentaje clarity poblacion")


# A continuación obtendre una muestra representativa con el metodo de estratificacion proporcional, manteniendo en la muestra las proporciones de reparto de la variable cut
# Voy a usar la funcion stratified



#************************************************Funcion para eestratificacion proporcional***********************************************************
#**********************************************Fuente: https://gist.github.com/mrdwab/6424112****************************************************
  
  stratified <- function(df, group, size, select = NULL, replace = FALSE, bothSets = FALSE) {
    if (is.null(select)) {
      df <- df
    } else {
      if (is.null(names(select))) stop("'select' must be a named list")
      if (!all(names(select) %in% names(df)))
        stop("Please verify your 'select' argument")
      temp <- sapply(names(select),
                     function(x) df[[x]] %in% select[[x]])
      df <- df[rowSums(temp) == length(select), ]
    }
    df.interaction <- interaction(df[group], drop = TRUE)
    df.table <- table(df.interaction)
    df.split <- split(df, df.interaction)
    if (length(size) > 1) {
      if (length(size) != length(df.split))
        stop("Number of groups is ", length(df.split),
             " but number of sizes supplied is ", length(size))
      if (is.null(names(size))) {
        n <- setNames(size, names(df.split))
        message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
                paste(n, collapse = ", "), "),\n.Names = c(",
                paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
      } else {
        ifelse(all(names(size) %in% names(df.split)),
               n <- size[names(df.split)],
               stop("Named vector supplied with names ",
                    paste(names(size), collapse = ", "),
                    "\n but the names for the group levels are ",
                    paste(names(df.split), collapse = ", ")))
      }
    } else if (size < 1) {
      n <- round(df.table * size, digits = 0)
    } else if (size >= 1) {
      if (all(df.table >= size) || isTRUE(replace)) {
        n <- setNames(rep(size, length.out = length(df.split)),
                      names(df.split))
      } else {
        message(
          "Some groups\n---",
          paste(names(df.table[df.table < size]), collapse = ", "),
          "---\ncontain fewer observations",
          " than desired number of samples.\n",
          "All observations have been returned from those groups.")
        n <- c(sapply(df.table[df.table >= size], function(x) x = size),
               df.table[df.table < size])
      }
    }
    temp <- lapply(
      names(df.split),
      function(x) df.split[[x]][sample(df.table[x],
                                       n[x], replace = replace), ])
    set1 <- do.call("rbind", temp)
    
    if (isTRUE(bothSets)) {
      set2 <- df[!rownames(df) %in% rownames(set1), ]
      list(SET1 = set1, SET2 = set2)
    } else {
      set1
    }
  }

#***************************************************************************************************************************************


# Voy a obtener una muestra representativa para cut con el tamaño de 10% de la poblacion.  
  
muestra <- stratified(dt, "cut", .1) # muestreo via estratificacion para cut
nrow(muestra)/nrow(dt) #compruebo que el tamaño de la muestra es 10% de la poblacion

dim(muestra)/dim(dt) # otra forma de comprobar que el tamaño de la muestra es 10% de la poblacion

# compruebo que el reparto de cut en la muestra es igual que el reparto de cut en la poblacion.
pie_chart(muestra, main = "cut", labels = NULL) +
  labs(title = "porcentaje cut muestra")

# Para ver si la muestra es representativa compruebo adicionalmente que las medias y los cuartiles de las variables continuas de la poblacion y de la muestra son parecidos.
summary(dt[,c(1,5:10)])
summary(muestra[,c(1,5:10)])

# Obtengo una nueva muestra, esta vez teniendo tambien en cuenta el reparto de la variable color.

muestra2 <- stratified(dt, c("cut","color"), .1)

# Observo que los repartos de ambas variables no varian en comparacion con la poblacion. 
pie_chart(muestra2, main = "cut", labels = NULL) +
  labs(title = "porcentaje cut muestra2")

pie_chart(muestra2, main = "color", labels = NULL) +
  labs(title = "porcentaje color muestra2")

# Observo que al añadir la variable color como criterio de estratificación las medias y los cuartiles ajustan mejor.
summary(dt[,c(1,5:10)])
summary(muestra2[,c(1,5:10)])

# Obtengo otra muestra añadiendo un tercer criterio de estratificación proporcional: clarity

muestra3 <- stratified(dt, c("cut","color", "clarity"), .1)

# Observo que los repartos de las tres variables no varian en comparacion con la poblacion. 
pie_chart(muestra3, main = "cut", labels = NULL) +
  labs(title = "porcentaje cut muestra3")

pie_chart(muestra3, main = "color", labels = NULL) +
  labs(title = "porcentaje color muestra3")

pie_chart(muestra3, main = "clarity", labels = NULL) +
  labs(title = "porcentaje clarity muestra3")


summary(dt[,c(1,5:10)])
summary(muestra3[,c(1,5:10)]) # La aportacion de la inclusion de la tercera variable al ajuste de las medias y cuartiles es menos destacable


# Dado que en un análisis de big data no se suele usar las tecnicas de estratificacion, seguire trabajando con los detos de la poblacion.

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# 1. Análisis de las variables

# Análisis descriptivo de las variables: Tipo de variable, distribución y representación
# Detección de casos atípicos y su tratamiento

#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 

# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relación al punto más ancho 


str(dt) 
# veo las características del data frame en cuanto a:
# - numero de observaciones: el data set contiene 53940 operaiones
# - numero y tipo de variables: hay 10 variables, cut, color y clarity son variables categoricas con varios niveles ordenados, el resto de las variables son numericas
#   * ejemplos en caso de variables numericas
#   * niveles y orden en caso de los factores

'data.frame:	53940 obs. of  10 variables:
$ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
$ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
$ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
$ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
$ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
$ table  : num  55 61 65 58 58 57 57 55 61 61 ...
$ price  : int  326 326 327 334 335 336 336 337 337 338 ...
$ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
$ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
$ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...'

# en un resumen vemos las características principales de las variables

summary(dt) # análisis preeliminar
# analizando las medias y medianas de las variables numericas veo que en las variables depth, table, x,y,z son parecidas.
# Las medias y medianas de esas variables se encuentran en parecida distancia del primer y tercer cuartil, lo qual indica cierta simetria
# No observo valores nulos ni NA's, sin embargo existen valores invormados con 0 en las variables x,y y z. Dado que la variable significa tamaño, el cero no tiene sentido y estos datos deben ser incorrectos.
nrow(dt[dt$x==0 | dt$y==0 | dt$z==0,] ) # encuentro 20 observaciones con valores x,y o z igual a cero
dt[dt$x==0 | dt$y==0 | dt$z==0,] # veo las observaciones

dt2 <- dt[dt$x!=0 & dt$y!=0 & dt$z!=0,] # elimino los valores 0 incorrectos y creo un data set nuevo limpio

nrow (dt)-nrow(dt2) # compruebo que la diferencia son los 20 registros analizados

# a partir de ahora voy a trabajar con el set de datos nuevo 

# Adicionalmente puedo ver que los valores minimos y maximos se alejan bastante del primer y tercer cuartil, lo cual indica colas largas y existencia de outliers.

# Las variables carat y price tienen media y mediana distinta. EN la variable price la diferencia es mayor.
# La media mas grande que mediana y desplazada hacía el tercer cuartil en ambos casos indica que la mayoría de observaciones estan por debajo de la media y existe cola derecha.

# Para las variables categoricas puedo ver el numero de observaciones por categoria.
# Observo creciente numero de observaciones en cuanto mejor es el corte (cut). 
# El numero de observaciones es elevado tambien para color y claridad superior.

# ------variable carat-----------
    
#Variable carat
hist(dt2$carat) # el histograma muestra claramente skew - la cola derecha es larga. 
plot(density(dt2$carat)) # Observo que los valores no se centran alrededor de la media y sin embargo se centran alrededor de varios valores, lo cual indicaría que a la distribución influye una segmentación según algun criterio adicional.

qqnorm(dt2$carat)
qqline(dt2$carat, col=2)
# Observamos cierto parecido a distribucion normal unicamente en el primer cuantil, aunque con desplazamiento a la isquierda y cola derecha.
# En los demás cuantiles el grafico de la muestra se encuentra integramente por encima de la recta de la distibución normal hipotética. 

boxplot(dt2$carat) #observamos gran cantidad de outliers, una cumulación de valores en la parte baja de la distribucion y cola derecha.
                   # veo que los outliers son los diamantes con peso mayor que 2 quilates
nrow(dt2[dt2$carat>2,] ) /nrow(dt2) # compruebo si los outliers son muchos: son 3,4% del total.
summary(dt2[dt2$carat>2,]) # Compruebo las características de los outliers. 
# Son diamantes grandes y con bastante buen corte, pero color y claridad regular
# Dado que los diamantes representan solo 3,4% del total y que no observo características interesantes para analizarlos a parte, los voy a eliminar
dt3<- dt2[dt2$carat<=2,] # en mi nuevo data set solo me quedo con los diamantes de 2 kilates como máximo
boxplot(dt3$carat) # compruebo que se han eliminado los outliers
plot(density(dt3$carat)) # ahora la distribción tiene cola derecha mucho más corta

qqnorm(dt3$carat)
qqline(dt3$carat, col=2) # la distribución sigue sin ser normal pero se asemeja más

# a partir de ahora trabajaré con el data set con diamantas hasta 2 kilates


# ------variable cut-----------


table(dt3$cut) # veo que el número las observaciones crece en cuanto aumenta la calidad del corte
barplot(table(dt3$cut)) # el reparto y la tendencia se ve mejor de forma grafica: cuanto mejor el corte mas observaciones 
pie(table(dt3$cut)) # en el grafico circular se ve que ca. 2/3 de los diamantes tienen corte "premium" o "ideal"

# ------variable color-----------

table(dt3$color) # los diamantes  de colores intermedios son los más numerosos en el dataset
pie(table(dt3$color)) # los diamantes tipo F,G,H (valores intermedios) suman mas de la mitaad de las onservaciones, tambien hay una gran cantidad de diamantes de tipo E
barplot(table(dt3$color)) # el reparto presenta una ligera inclinación hacía los diamantes de mejor calidad

# ------variable clarity-----------

table(dt3$clarity)

barplot(table(dt3$clarity)) # el gráfico muestra una concentración de observacioens alrederor de claridad intermeda o mala
pie(table(dt3$clarity)) # los diamandes con claridad intermedia/baja (SI y VS) suman mas de 3/4 del total. Hay muy pocos diamantes con claridad superior.


# ------variable depth-----------

hist(dt3$depth) #los valores se concentran alrededor a la media
plot(density(dt3$depth)) #el grafico de la densidad indica una distribucion desplazada ligeramente a la derecha y leptocurtica.

qqnorm(dt3$depth)
qqline(dt3$depth, col=2) # en comparación con la normal hipotetica observamos bastante parecido en los cuartiles intermedios. Vemos que existen valores extremos.

boxplot(dt3$depth) # se confirma existencia de muchos valores exremos

nrow(dt3[dt3$depth<60 | dt3$depth>65,])/nrow(dt3) # los outliers de la variable depth suponen bastantes observaciones (10,7%)

boxplot(dt3[dt3$depth<60 | dt3$depth>65,]$depth) # reviso la distribucion de los outliers. Dentro del grupo se ven valores mas extremos: mayores de 75 y menores de 45
# en lugar de eliminar una cantidad considerable de datos analizare los valores mas extremos dentro de este grupo
nrow(dt3[dt3$depth<55 | dt3$depth>72,]) # los valores extremos son solo 28
boxplot(dt3[dt3$depth>=55 & dt3$depth<=72,]$depth)# compruebo que quitando los 28 valores extremos eliminaria una parte de la cola izquierda
#analizo la cola derecha que queda
summary(dt3[dt3$depth>=65 & dt3$depth<72,])  # observo que los outliers menos extremos de la cola derecha tienen en comun algunas caracteristicas: por ej. variable cut
# concluyo que pueden formar parte de un grupo especifico. No los eliminare.

# Procedo a eliminar solamente los 28 valores extemos
dt4<-dt3[dt3$depth>=55 & dt3$depth<=72,]

# a partir de ahora voy a trabajar con el nuevo data set sin valores extremos de depth

# ------variable table-----------

hist(dt4$table) # valores concentrados cerca de la media en una distribución desplazada a la izquierda
plot(density(dt4$table)) # acumulación de las observaciones cerca de varios valores concretos
boxplot(dt4$table) # existen varios valores extremos

summary(dt4[dt4$table<=52 |dt4$table>=65 ,]) # reviso las caracteristicas de los outliers, no observo ningun patron interesante
nrow(dt4[dt4$table<=52 |dt4$table>=65 ,])/nrow(dt4) # los outliers suponen 0.7% del total. Los voy a eliminar.
dt5<- dt4[dt4$table>52 & dt4$table<65 ,]

boxplot(dt5$table) # veo el reparto del nuevo data set

plot(density(dt5$table)) # disminuyen las colas de distribucion
qqnorm(dt5$table)
qqline(dt5$table, col=2)# las concentraciones cerca de los valores determinados provoca desviaciones de la distribucion normal

# ------variable price-----------

hist(dt5$price) # la mayoría de los diamantes en el dataset tienen precio bajo. A la medida que crece el precio, disminuye el numero de operaciones, aunque se observa una ligera subida alrededor de los 5000.
plot(density(dt5$price)) # observamos concentracion cerca de los valores bajos y una larga cola a la derecha. Se confirma el ascenso de las observaciones alrededor de los 5000.

qqnorm(dt5$price)
qqline(dt5$price, col=2) # no se observa pacecido a la distribucion normal hay muchos outliers

boxplot(dt5$price) # observamos muchos valores extremos en el intervalo superior: mientras que muchos diamantes tienen precio bajo hay tambien una gran cantidad de diamantes muy caros

nrow(dt5[dt5$price>1100,])/nrow(dt5) #los diamantes caros son 66% del total
#comparo las caracteristicas de ambos grupos
summary(dt5[dt5$price>1100,])
summary(dt5[dt5$price<=1100,])
# llego a la conclusion que los diamantes mas caros tienen precio mayor poque son mas grandes

# voy a comprobar la hipótesis que los diamantes caros (outliers) son en media mas grandes que los diamantes baratos
precioGood<-dt$carat[dt5$price>1100]
precioVeryGood<-dt$carat[dt5$price<=1100]

t.test(dt5$carat[dt5$price>1100], dt$carat[dt5$price<=1100])

# Two Sample t-test

#data:  dt5$carat[dt5$price > 1100] and dt$carat[dt5$price <= 1100]
#t = 53.39, df = 22559, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.2498720 0.2689178
#sample estimates:
#  mean of x mean of y 
#0.9349369 0.6755420 

# p-value es mas bajo que el limite de rechazo bajo nivel de confianza de 95%, por tanto rechazo la hipotesis Ho de igualdad de las medias
# concluyo que hay una diferencia significativa en el peso entre los diamantes caros y diamantes baratos.

# voy a analizar los diamantes caros y los diamantes baratos por separado
DiamantesCaros<-dt5[dt5$price>1100,]
DiamantesBaratos<-dt5[dt5$price<=1100,]


boxplot(DiamantesBaratos$price)
boxplot(DiamantesCaros$price)
boxplot(DiamantesCaros$carat)

plot(density(DiamantesCaros$price))
plot(density(DiamantesBaratos$price))

summary(DiamantesCaros)
nrow(DiamantesCaros[DiamantesCaros$price>13000,])/nrow(DiamantesCaros)

# ya que dentro de los diamantes caros existen outliers vuelvo a comprobar significativamente mas grandes que los demas diamantes de la muestra 
t.test(DiamantesCaros$carat[DiamantesCaros$price>12500], DiamantesCaros$carat[DiamantesCaros$price<=12500])
# hay diferencia significativa en el tamaño de los diamantes muy caros

#concluyo que la variable precio tiene mayor valor cuando el diamante es mas grande
# para analizar otros factores a parte de tamaño que influyen al recio introduzco una variable nueva precio/quilate


library(dplyr) # primero cargamos paquete dplyr
dt6 <- mutate(dt5, precio_quilate =price/carat)
summary(dt6)


head(dt6) 

# ------variable precio_quilate-----------

plot(density(dt6$price)) 
plot(density(dt6$precio_quilate))  # lavariable precio_quilate presenta menos cola derecha que la variable precio
boxplot(dt6$precio_quilate) # te todos modos observo que la variable precio_quilate tambien tiene bastantes outliers


plot(dt6$carat,dt6$precio_quilate)   # parece que precio por kilate es mayor si hay mas quilates                               


# ------variable x,y,z-----------


hist(dt$x)
hist(dt$y)
hist(dt$z)

boxplot(dt6$x) # no se observan valores extremos de la variable x

boxplot(dt6$y)
dt6[dt6$y>30,] # hay dos valores extremos: diamantes de mas de 30 mm.Debe de ser incorrecto. Los voy a eliminar.
dt7<- dt6[dt6$y<30,]
summary(dt7)
boxplot(dt7$z)
dt7[dt7$z>6,] # hay un valor extremo: diamante de mas de 30 mm. Debe de ser incorrecto. Lo voy a eliminar.
dt8<- dt7[dt7$z<30,]
summary(dt8) # vemos que ya no están los valores excesivos de diamantes

# las variables x,y,z significan tamaño en las tres dimensiones. Para analizarlas conjuntamente introduzco una nueva variable size = x*y*z.
library(dplyr) # primero cargamos paquete dplyr

dt9 <- mutate(dt7, size =x*y*z)
summary(dt9) # observamos que precio por quilate aumenta a la hora que asciende el tamaño (size)

#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 

Inferencia
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hipótesis

#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 


t.test(dt9$carat) # realizo un t test y determino que la media de los quilates es significativamente distinta de cero
# uno de los resultados es el intervalo de confianza:

#One Sample t-test

#data:  dt9$carat
#t = 422.65, df = 51638, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  0.7442987 0.7512341
#sample estimates:
#  mean of x 
#0.7477664 

# Bajo el nivel de confianza de 95% la media de la variable carat se encuentra entre 0.7442987 y 0.7512341

t.test(dt9$depth) # realizo un t test y determino que la media de la variable depth es significativamente distinta de cero
# uno de los resultados es el intervalo de confianza:

#One Sample t-test

#data:  dt9$depth
#t = 10204, df = 51638, p-value < 2.2e-16
#alternative hypothesis: true mean is not equal to 0
#95 percent confidence interval:
#  61.74954 61.77327
#sample estimates:
#  mean of x 
#61.7614 

# Bajo el nivel de confianza de 95% la media de la variable depth se encuentra entre 61.74954 y 61.77327.


# Hipotesis:
# Los diamantes pequeños (peso en quilates por debajo de la media) tienen "depth" distinto que los diamantes grandes (peso en quilates medio o mayor)

# divido los diamantes en dos grupos con el fin de hacer el test
DiamantesPequenos<- dt9[dt9$carat<0.7477664 ,]
summary(DiamantesPequenos)
DiamantesGrandes<- dt9[dt9$carat>=0.7477664 ,]
summary(DiamantesGrandes)


#Compruebo la hipotesis Ho que en ambos grupos depth medio es igual
t.test(DiamantesPequenos$depth, DiamantesGrandes$depth)

#Welch Two Sample t-test

#data:  DiamantesPequenos$depth and DiamantesGrandes$depth
#t = -10.418, df = 40443, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1568222 -0.1071572
#sample estimates:
#  mean of x mean of y 
#61.70572  61.83771 

# P value es muy pequeno, por lo cual esta en region de rechazo bajo nivel de confianza 95%.
# Rechazo la hipotesis Ho y concluyo que bajo nivel de confianza 95% depth medio en ambos grupos es significativamente diferente


#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 

Relaciones entre las variables
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlación)
#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 

plot(dt9[,c(1,5:11)]) # veo que existe una relación entre precio y kilates, peso y precio también parece dependen de tamaño
                      # precio por quilate y peso en quilates tienen relacion positiva, mientras que precio en quilates y depth, parece negativa

cor(dt9[,c(1,5:11)]) # se confirman las conslusiones que se veian en los graficos: observamos fuerte correlacion entre el precio y el peso en quilates y tambien entre precio y tamaño. 
                     # el precio por quilate tiene correlacion positiva con el peso: en cuanto mas grande el diamante mas vale el quilate
                     # como era de esperar, cuanto mas grande el diamante mayor es su peso
                     # hay una correlacion negativa entre depth y precio (o precio por quilate)

# voy a calcular precio medio por quilate desglosando por colores
library(dplyr)
by_color <- group_by(dt9, color)
pr_med_quil_by_color <- summarise(by_color, mean(precio_quilate))
pr_med_quil_by_color # precio parece mayor para valores intermedios y menor en colores peor clasificados, pero no se ven diferencias llamativas entre los distintos colores

sd_med_quil_by_color <- summarise(by_color, sd(precio_quilate))
sd_med_quil_by_color # desviación tipica también es mayor para colores intermedios, pero sin grandes diferencias entre grupos, excepto J, donde desviacion es menor

by_clarity <- group_by(dt9, clarity)
pr_med_quil_by_clarity <- summarise(by_clarity, mean(precio_quilate))
pr_med_quil_by_clarity  # precio medio or quilate es mejor cuanto mejor claridad. Los diamantes con peor claridad muestran precio medio mas bajo. 

sd_med_quil_by_clarity <- summarise(by_clarity, sd(precio_quilate))
sd_med_quil_by_clarity  # desviación tipica de precio por quilate tambien crece en cuanto mejora la claridad


boxplot(dt9$precio_quilate~dt9$color) # observamos que cuanto mejor el color, mas outliers en precio por quilate - mas precios muy altos
boxplot(dt9$precio_quilate~dt9$clarity) # el patron en cuanto a claridad es parecido: cuanto mejor la claridad, mas outliers en precio por quilate - mas observaciones con precio muy alto



library(ggplot2)
q<- ggplot(dt9, aes( x=carat, y= price)) 
q + geom_point(aes(color=color)) # observamos que el precio de diamante depende del peso con una pendiente distinta en funcion del color

                  
# voy a investigar el ascenso de la densidad de precio alrededor del valor de 5000 observado anteriormente 
a <- ggplot(dt9, aes(price))
a + geom_density(kernel = "gaussian")
                    
                    
a <- ggplot(dt9, aes(price))
a + geom_density(aes(color=color)) # vemos que los precios se concentran alrededor de valores bajos y la concentracion es mayor para colores clasificados como mejor
                                   # también vemos que el ascenso en el valor cercano a 5000 se corresponde con lo diamantes de color H-J.
                    
a <- ggplot(dt9, aes(precio_quilate)) 
a + geom_density(aes(color=color)) # el precio por quilate tiene patron menos definido en cuanto a color 


a <- ggplot(dt9, aes(precio_quilate))
a + geom_density(aes(color=clarity)) # el precio por quilate tampoco tiene patron muy definido en cuanto a clarity
                              
                    
g <- ggplot(dt9, aes(clarity, carat)) 
g + geom_boxplot(aes(color=color)) # observamos que los diamantes de mejor claridad son en medio mas pequeños, aunque con mas dispersion y outliers en cuanto a peso
                                   # dentro de la misma categoria de claridad, los diamantes de peor color suelen ser mas grandes
                                   # conclusion: en media, cuanto mas grande el diamante peor color y claridad, aunque hay bastantes exepciones entre los diamantes grandes              
                    


m <- ggplot(dt9, aes(color, clarity))                   
m + geom_raster(aes(fill = precio_quilate), hjust=0.5,  vjust=0.5, interpolate=FALSE) 
# obsevamos que el precio por quilate asciende en cuanto mejora el color y claridad
# este patron se observa para los diamantes catagorizados muy bien o muy mal y destaca mejos para los colores y claridad intermedios
                 
 
h <- ggplot(dt9, aes(color, clarity)) 
h + geom_jitter(aes(color=cut)) 
#observamos que hay muchos diamantes de corte y color bastente buenos
#los menos frecuentes son diamantes de fair cut, estos diamantes son de claridad muy buena
# predomina claridad intermedia y parece 


m <- ggplot(dt9, aes(color, clarity))
m + geom_tile(aes(fill = cut)) 
# los diamantes de claridad y color superior suelen tenet cut ideal, very good o premium
# para claridades peor clasificadas tambien empeora el cut
# hay muchos diamantes de color y clarity intermedio y pocos de claridad muy buena y muy mala, en cuanto a color la concentracion es menor

                    
f <- ggplot(dt9, aes(carat, size))
f + geom_point(aes(color=clarity)) # la relacion entre el peso en quilates y el tamao parece lineal
 
                    
f <- ggplot(dt9, aes(carat, precio_quilate))
f + geom_point(aes(color=clarity)) 
# se puede observar una relacion lineal creciente entre el peso en quilates y el precio por quilate
# la pendiente de la recta parece estar en relacion con clarity y es mayor en cuanto la claridad mejora 
                    
 
f <- ggplot(dt9, aes(carat, precio_quilate))
f + geom_point(aes(color=color))
# adicionalmente, la pendiente de la recta que relaciona el precio por quilate con el peso es mayor en cuanto mejora el color del diamante


f <- ggplot(dt9, aes(carat, precio_quilate))
f + geom_point(aes(color=cut))
# en caso de variable cut no se observa dependencia de la pendiente de la categoria cut distintas de fair
# precio por quilate de los diamantes con fair cut es mas bajo y la pendiente de la recta en mas pequeña tambien
                    


f <- ggplot(dt9, aes(table, depth))
f + geom_rug(aes(color=cut)) 
# El corte se clasifica como mejor en cuanto la profundidad se acerca a 62 aprox. Es decir: los diamantes cuya anchura supone 2/3 de la altura se consideran cortados mejor.
# El corte es mejor cuanto mas pequeña la base de diamante (mas bajo el valor de la variable table)


f <- ggplot(dt9, aes(table, depth))
f + geom_point() 
f + geom_point(aes(color=cut)) 
# Vemos que los diamantes con mejor corte (mas proporcionales) son los de base pequeña y profundidad cercana a 62.
# En cuanto aumenta la base en relacion a la anchura y cuanto mas afilado (mayor altura en comparacion con anchura) el peor se clasifica el corte.
# Podemos observar un grupo grande de diamantes con corte clasificado como inferior (fair): con depth mayor de 65% (mas afilados en parte superior) 
# Tambien hay un grupo de diamantes muy planos (table > 60) con fair cut
   
                    
f <- ggplot(dt9, aes(carat, depth))
f + geom_point() 
f + geom_point(aes(color=cut))# los diamantes afilados (depth grande) existen de varios tamanos, la forma afilada no es proporcional al tamaño, sin embargo se ven concentraciones de diamantes afilados alrededor de ciertos tamaños determinados.                   



                   
f <- ggplot(dt9, aes(price, depth))
f + geom_point() 
f + geom_point(aes(color=cut))# los diamantes afilados son mas baratos                   
                    

boxplot(dt9$carat~dt9$clarity)


.anovacut<-aov(dt9$price~dt9$cut)
summary(.anovacut)

.anovadepth<-aov(dt9$precio_quilate~dt9$depth)
summary(.anovadepth)

.anovadepth<-aov(dt9$price~dt9$depth)
summary(.anovadepth)

.anovacolor<-aov(dt9$precio_quilate~dt9$color)
summary(.anovacolor)

.anovacolor<-aov(dt9$price~dt9$color)
summary(.anovacolor)

.anovaclarity<-aov(dt9$price~dt9$clarity)
summary(.anovaclarity)

.anovadepth<-aov(dt9$depth~dt9$cut)
summary(.anovadepth)

.anovatable<-aov(dt9$table~dt9$cut)
summary(.anovatable)

.anovatable<-aov(dt9$precio_quilate~dt9$table)
summary(.anovatable)
 #----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 

Análisis de regresión
# Formular un modelo de regresión y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformación a la regresión y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresión

#----------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------- 


library(ggplot2)
dt<-as.data.frame(diamonds)

head(dt)
attach(dt)


