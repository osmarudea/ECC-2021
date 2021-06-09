################################################################################
############################## Encuesta ########################################
############ Índice de Participación Ciudadana de Medellín #####################
############################ Participantes #####################################
################################################################################

################################################################################
# 1. Tamaño muestral
# 2. Distribución de la muestra

# 3. Muestreo
################################################################################

# Población por Comuna
archivo <- "/mnt/windows/Users/osmar_000/Documents/IPCM 2019/Proyecciones_De_Poblaci_n_Medell_n_2016_2020.csv"
pob <- read.table(archivo,header=T,sep=',',encoding='UTF-8')

names(pob)[3] <- 'Comuna'

pobC <- aggregate(pob[,c(5:19)],by=list(COMUNA=pob$Comuna),sum)
pobC <- pobC[,c('COMUNA','Total.2020')]

names(pobC)[2] <- 'pob_2020'



################################################################################

# 1. Tamaño muestral

# Para determinar el tamaño muestral para la estimación de una 
# proporción se puede tomar como referente la distribución de probabilidad 
# binomial. A partir de esta distribución se puede definir el tamaño muestral
# para una población de tamaño conocido como sigue:

sampSizeFin <- function(N,Z,p,E){
  n <- (N*(Z^2)*p*(1-p))/((E^2)*(N-1)+(Z^2)*p*(1-p))
  return(n)
}

errImpl <- function(N,Z,p,n){
  (((N/n)-1)*(Z^2)*p*(1-p)/(N-1))^(1/2)
}

## Donde:
# N es el tamaño de la población.
# Z es el valor crítico extraído de una distribución gaussiana 
#  según el nivel de confianza deseado
# p es la proporción que se desea estimar
# E es el margen de error
# n es el tamaño de la muestra
# Error implicado en población finita según tamaño muestral
#  elegido

# Cargue proyecciones poblacionales para obtener población total
# de Medellín
archivo <- "/mnt/windows/Users/osmar_000/Documents/Censo/Proyecciones-poblacion-Municipal_2018-2035.csv"
pobMed <- read.table(archivo,header=T,sep=';',encoding='UTF-8')

pobMed <- subset(pobMed, AÑO==2022 & DPMP==5001)

## Obtenga tamaño muestral
# Criterios: Confianza=95%, Márgen de Error=2%, p=.5
sampSizeFin(pobMed$Total[3],Z=qnorm(0.975),p=0.5,E=0.02)

# Note que cuando la proporción p se establece en 0.5 la varianza de la 
# distribución binomial es máxima. Por tanto, asumir p=0.5 es un criterio
# conservador.

# El tamaño muestral requerido bajo los anterioes supuestos 
# es de aproximadamente 2400 encuestas.

# Para dar cabida a corregimientos, se añaden 100 encuestas:
n <- 2500

#  Con base en ECV Medellín, recuerde que se asume que 
# la población participante es el 5% del total

cat('\nPoblación Total de Medellín:', pobMed$Total[3], '\n')
cat('Márgen de error Implicado por la Muestra propuesta',
    round(errImpl(pobMed$Total[3],Z=qnorm(0.975),p=0.5,n=n)*100,3),'%'
) 

# El margen de error implicado por esta muestra es de 1.96% aproximadamente.
# Dadas la hipótesis en que se funda (particularmente que p=0.5) el anterior
# estimativo puede ser tomado como una cota superior del margen de error.

# Además, el margen de error es específico a cada pregunta de la encuesta. 
# Dado el proceder general que se sigue, lo anterior es una justificación
# adicional para interpretar el anterior estimativo como una cota superior
# del margen de error.

################################################################################

# 2. Distribución de la muestra

# Operativamente es conveniente distribuir la muestra entre las comunas
# y corregimientos de Medellín de manera proporcional.

n <- 2500
cuota <- n*shC
names(cuota) <- pobC$COMUNA
cuota

# Márgen de error por Comuna/Corregimiento
errImpl(pobC$pob_2020,Z=qnorm(0.975),p=0.5,n=cuota)



################################################################################

require(rgdal)
library(tmap)

#ruta <- "D:/Documentos/Datos/ICFES/shp mde"
#shp <- readOGR(dsn=ruta, layer='BarrioVereda_2014',
#               encoding='UTF-8')

#
ruta <- "/mnt/windows/Users/osmar_000/Documents/Indice Violencia Mujer"
shp <- readOGR(dsn=ruta, layer='Limite_Barrio_Vereda_Catastral',
               encoding='UTF-8')

plot(shp)
str(shp@data)
shp$COMUNA <- as.character(shp$COMUNA)

plot(shp)

################################################################################
ruta <- "/mnt/windows/Users/osmar_000/Documents/Indice Violencia Mujer"

# Shape nomenclatura urbana
lotes <- readOGR(dsn=ruta, layer='Nomenclatura_Domiciliaria',
                 encoding='UTF-8')
# Shape manzanas catastrales
manzanas <- readOGR(dsn=ruta, layer='Manzana_Catastral',
                    encoding='UTF-8')

# Comuna está en los primeros dos dígitos de CBML
lotes$Comuna <- substr(lotes$CBML,1,2)
unique(lotes$Comuna)

# Comuna está en los primeros dígitos de COBAMA
manzanas$Comuna <- substr(manzanas$COBAMA, 1,2)
unique(manzanas$Comuna)

# Identificador de Manzana está entre los dígitos 5 a 7 de CBML
lotes$Manzana <- substr(lotes$CBML,1,7)
length(unique(lotes$Manzana))

# Grafique las direcciones de una manzana cualquiera
plot(lotes[!is.na(lotes$Manzana) & lotes$Manzana=='0815011',]) 

# Calcule cantidad de lotes/direcciones por manzana
WManzana <- tapply(lotes$Manzana,lotes$Manzana,length)
length(WManzana)

# Obtenta coordandas de direcciones urbanas
coords <- coordinates(lotes)

# Obtenga centroide de manzana con base en direcciones catastrales
meanCoord <- aggregate(coords,by=list(lotes$Manzana),mean)
dim(meanCoord)
names(meanCoord) <- c('Manzana','long','lat')


# Conforme un data.frame con identificador de manzana 
#  con centroide de manzanas construido a partir de
#  shape de direcciones, y
#  con número de direcciones por manzana,  

WManzana <- data.frame(Manzana=row.names(WManzana),
                       numDir=WManzana)
WManzana <- merge(WManzana,meanCoord,by='Manzana')
dim(WManzana)

coordinates(WManzana) <- ~long+lat

plot(shp,lty=2)
plot(manzanas)
plot(WManzana,add=TRUE,col='red',pch='.')


# -- Muestreo de Manzanas
# Se va a hacer un muestreo de manzanas por comuna, donde la probabilidad
#  de elegir cada manzana es proporcional a su tamaño,
#  el cual está dado por la cantidad de direcciones

# Calcule el número de direcciones por comuna
WManzana$Comuna <- substr(WManzana$Manzana,1,2)
Ndir <- tapply(WManzana$numDir,WManzana$Comuna,sum)
Ndir2 <- tapply(lotes$Comuna,lotes$Comuna,length)
Ndir;Ndir

shC <- pobC$pob_2020/sum(pobC$pob_2020)
shC <- data.frame(nomComuna=pobC$COMUNA, shC=shC)

codComuna <- read.table("/home/osmar/Documents/ECC 2021/codComuna.csv", 
                        header=T, sep=';')
shC <- merge(shC, codComuna, by='nomComuna', all=T)
shD <- data.frame(codComuna=as.numeric(names(Ndir2)),shD=Ndir2/sum(Ndir2))

shD <- merge(shC,shD,by='codComuna',all=T)
names(shD)[3:4] <- c('participacion.poblacion','particiacion.direcciones')

write.table(shD,'/home/osmar/Documents/ECC 2021/sh_dir.csv',sep=';',row.names=F)


# Se establecen las probabilidades de elección de cada manzana
com <- unique(WManzana$Comuna)

WManzana$SelProb <- NA
for(i in seq_along(com)){
  WManzana@data[WManzana$Comuna==com[i],'SelProb'] <- 
    WManzana@data[WManzana$Comuna==com[i],'numDir']/Ndir[i]
}

# Verifique que las probabilidades de elección de cada 
#  manzana suman 1 en cada comuna
tapply(WManzana$SelProb,WManzana$Comuna,sum)

# Verifique el número de manzanas por comuna según shape de
#  manzanas catastrales y según shape de direcciones.
Nmanz <- tapply(WManzana$Manzana,WManzana$Comuna,
                function(x) length(unique(x)))
Nmanz2 <- tapply(manzanas$COBAMA,manzanas$Comuna,
                 function(x) length(unique(x)))
Nmanz3 <- tapply(lotes$Manzana,lotes$Comuna,
                 function(x) length(unique(x)))

Nmanz;Nmanz2;Nmanz3
# En shape de manzanas catastrales aparentemente Altavista
#  tiene 162 manzanas, mientras en shape de lotes se estiman
#  117 manzanas. ¿Tiene que ver esto con problema de 
#  georreferenciación del shape de Manzanas para el 
#  corregimiento de Altavista?

# - Por comuna, se va a seleccionar aleatoriamente 1/4 de las
# manzanas.

# Es menester establecer el valor semilla para obtener la misma
#  muestra cada vez que se corra este código.
set.seed(2102018)
sel <- list()
for(i in seq_along(com)){
  sel[[i]] <- sample(WManzana@data[WManzana$Comuna==com[i],'Manzana'],
                     round(Nmanz[i]/4),
                     prob=WManzana@data[WManzana$Comuna==com[i],'SelProb'])
}

proj4string(WManzana) <- proj4string(manzanas)

manzanas$sampstatus <- 'No Seleccionada'
manzanas[manzanas$COBAMA %in% unlist(sel),'sampstatus'] <- 
  'Seleccionada'

################################################################################
# Repita cada código de manzana según el número de direcciones que posee

manzanas <- rep(shp$MANZ_CCNCT,shp$NUMERO_HOG)

set.seed(123)
seleccion <- sample(manzanas,270)

seleccion <- data.frame(table(seleccion))

mean(seleccion$Freq)
quantile(seleccion$Freq)

table(seleccion$Freq)

names(seleccion) <- c('MANZ_CCNCT','n')

# Mejoras futuras:
# Pegar manzanas con pocas direcciones a manzanas vecinas
#  Transforme a proyeción local
#  Checar manzanas con menos de 30 direcciones.
#  Crear matriz de vecindades con un radio pequeño (50m2?)
#  Para manzanas con menos de 30 direcciones, 
#    checar número de direcciones de manzanas vecinas.
#  Agregar manzana con vecinos más pequeños hasta lograr
#   una manzana de 30 direcciones.

# Una cuestión a resolver es, si una vez seleccionadas
#  las manzanas, escojo el mismo número de direcciones
#  por manzana, o escojo un número de direcciones 
#  proporcional al tamaño de la manzana.

#
tmap_mode('view')
# Centroides manzanas seleccionadas
tm_shape(manzanas) + tm_borders() + 
  tm_shape(WManzana[WManzana$Manzana %in% unlist(sel),]) +
  tm_dots(col='red',size=0.1) +
  tm_shape(WManzana[!(WManzana$Manzana %in% unlist(sel)),]) +
  tm_dots(col='blue',size=0.1) +
  tm_shape(bord) + tm_borders(col='lightgreen')

tm_shape(manzanas[manzanas$Comuna=='02',]) + tm_borders() + 
  tm_shape(WManzana[WManzana$Manzana %in% unlist(sel),]) +
  tm_dots(col='red',size=0.1) +
  tm_shape(WManzana[!(WManzana$Manzana %in% unlist(sel)),]) +
  tm_dots(col='blue',size=0.1) +
  tm_shape(bord) + tm_borders(col='lightgreen')

with(subset(WManzana, Comuna=='02'),
     table(WManzana$Manzana %in% unlist(sel))/nrow(WManzana))

# En rojo manzanas seleccionadas
tm_shape(manzanas) + tm_fill(col='blue') +
  tm_shape(manzanas[manzanas$COBAMA %in% unlist(sel),]) +
  tm_fill(col='red') + 
  tm_shape(WManzana) + tm_bubbles(size='numDir',
                                  col='white',
                                  alpha=0.5
  ) +
  tm_shape(bord) + tm_borders(col='lightgreen')


#row.names(manzanas) <- as.character(manzanas$COBAMA)

# En rojo manzanas seleccionadas Comuna 8 y 10
tm_shape(manzanas[!(manzanas$Comuna %in% c('02','70')),],name='Manzanas') + 
  tm_fill(col='sampstatus', palette=c('blue','red'),
          style='cat', title='',popup.vars = 'COBAMA') +
  tm_shape(WManzana[WManzana$Comuna!='02',],name='Tamaño') + 
  tm_bubbles(size='numDir', col='white', alpha=0.5) +
  tm_shape(bord,name='Barrios') + tm_borders(col='lightgreen') +
  tm_layout(legend.outside=TRUE,main.title='Villa Hermosa y Centro')


# Comuna 2
tm_shape(manzanas[manzanas$Comuna=='02',],name='Manzanas') + 
  tm_fill(col='sampstatus', palette=c('blue','red'),
          style='cat', title='') +
  tm_shape(WManzana[WManzana$Comuna=='02',],name='Tamaño') + 
  tm_bubbles(size='numDir', col='white', alpha=0.5) +
  tm_shape(bord,name='Barrios') + tm_borders(col='lightgreen') +
  tm_layout(legend.outside=TRUE,main.title='Santa Cruz')

#Comuna 70
tm_shape(manzanas[manzanas$Comuna=='70',],name='Manzanas') + 
  tm_fill(col='sampstatus', palette=c('blue','red'),
          style='cat', title='') +
  tm_shape(WManzana[WManzana$Comuna=='02',],name='Tamaño') + 
  tm_bubbles(size='numDir', col='white', alpha=0.5) +
  tm_shape(bord,name='Barrios') + tm_borders(col='lightgreen') +
  tm_layout(legend.outside=TRUE,main.title='Santa Cruz')

table(manzanas$sampstatus)/nrow(manzanas)

with(manzanas@data,table(sampstatus)/length(sampstatus))
with(subset(manzanas@data,Comuna=='02'),
     table(sampstatus)/length(sampstatus))


################################################################################


# -- Muestreo de Direcciones

direcciones <- lotes
direcciones$direcciones <- paste(direcciones$VIA,
                                 direcciones$PLACA,sep=' #')

table(duplicated(direcciones$direcciones))

direcciones <- direcciones[!duplicated(direcciones$direcciones),]

direcciones <- subset(direcciones, Manzana %in% unlist(sel))

com <- c('02','08','10','70')
n <- c(1115,500,500,500)

set.seed(2102018)

seldir <- list()
for(i in seq_along(com)){
  seldir[[i]] <- sample(direcciones@data[direcciones$Comuna==com[i],'direcciones'],
                        n[i]  )
}

sapply(seldir,length)

direcciones <- subset(direcciones,direcciones %in% unlist(seldir))
dim(direcciones)
row.names(direcciones) <- direcciones$direcciones

manzanas <- merge(manzanas,
                  bord@data[,c("CODIGO","NOMBRE_BAR")],
                  by.x='BARRIOVERE',
                  by.y='CODIGO')
tmap_mode('view')

TF1 <- tm_basemap("OpenStreetMap") +
  tm_shape(
    manzanas[manzanas$Comuna=='02' &
               manzanas$sampstatus=='Seleccionada',],
    name='Manzanas') + 
  tm_fill('red4',alpha=0.5) +
  tm_shape(direcciones[direcciones$Comuna=='02',],name='Direcciones') + 
  tm_dots('red',popup.vars='direcciones') +
  tm_layout(legend.outside=TRUE,main.title='Santa Cruz')


tmap_save(TF1, 
          filename = "muestra santa cruz.html")



TF2 <- tm_basemap("OpenStreetMap") +
  tm_shape(
    manzanas[!(manzanas$Comuna %in% c('02','70')) &
               manzanas$sampstatus=='Seleccionada',],
    name='Manzanas') + 
  tm_fill('red4',alpha=0.5) +
  tm_shape(direcciones[!(direcciones$Comuna %in% c('02','70')),],name='Direcciones') + 
  tm_dots('red',popup.vars='direcciones') +
  tm_layout(legend.outside=TRUE,main.title='Villa Hermosa y Centro')

tmap_save(TF2,
          filename = "muestra villa hermos y centro.html")


TF3 <- tm_basemap("OpenStreetMap") +
  tm_shape(
    manzanas[manzanas$Comuna=='70' &
               manzanas$sampstatus=='Seleccionada',],
    name='Manzanas') + 
  tm_fill('red4',alpha=0.5) +
  tm_shape(direcciones[direcciones$Comuna=='70',],name='Direcciones') + 
  tm_dots('red',popup.vars='direcciones') +
  tm_layout(legend.outside=TRUE,main.title='Santa Cruz')


tmap_save(TF3, 
          filename = "muestra altavista.html")

borde
shp

x <- over(direcciones,shp[,c('COMUNA','NOMBRE_BAR')])
direcciones$Barrio <- x$NOMBRE_BAR
direcciones$Comuna2 <- x$COMUNA

write.table(direcciones@data,
            'Indice Violencia Mujer/muestra violencia mujer.csv',
            sep=';',row.names=FALSE)

writeOGR(direcciones,
         dsn='Indice Violencia Mujer',
         layer='muestra violencia mujer',
         driver="ESRI Shapefile",
         overwrite_layer=TRUE)



################################################################################

orSample <- read.table(
  'Indice Violencia Mujer/muestra violencia mujer.csv',
  sep=';',header=TRUE,comment.char='',colClasses = 'character')
all.equal(sort(orSample$direcciones),
          sort(direcciones$direcciones))

bord@data[bord$COMUNA=='08','NOMBRE_BAR']

vetados <- c('Las Estancias','Villa Liliam',
             'Villa Turbay','La Sierra','Llanaditas')
vetados <- subset(bord, NOMBRE_BAR %in% vetados)

tm_basemap("OpenStreetMap") +
  tm_shape(vetados) + tm_fill('black',alpha=0.5) +
  tm_borders('blue') +
  tm_shape(
    manzanas[manzanas$Comuna!='02' &
               manzanas$sampstatus=='Seleccionada',],
    name='Manzanas') + 
  tm_fill('red4',alpha=0.5) 

# Direcciones para reemplazar las de barrios vetados
direccionesR <- lotes
direccionesR$direcciones <- paste(direccionesR$VIA,
                                  direccionesR$PLACA,sep=' #')

table(duplicated(direccionesR$direcciones))

vetados <- c('Las Estancias','Villa Liliam',
             'Villa Turbay','La Sierra','Llanaditas')
bordR <- subset(bord, !(NOMBRE_BAR %in% vetados))

direccionesR <- direccionesR[!duplicated(direccionesR$direcciones),]

direccionesR <- subset(direccionesR, Manzana %in% unlist(sel))

direccionesR <- direccionesR[bordR,]

com <- '08'
n <- 300

set.seed(2102019)

seldir <- list()
for(i in seq_along(com)){
  seldir[[i]] <- sample(direccionesR@data[direccionesR$Comuna==com[i],'direcciones'],
                        n[i]  )
}

sapply(seldir,length)

direccionesR <- subset(direccionesR,direcciones %in% unlist(seldir))
dim(direccionesR)
row.names(direccionesR) <- direccionesR$direcciones

dupl <- direccionesR$direcciones %in% orSample$direcciones
table(dupl)

direccionesR <- direccionesR[!(dupl),]
dim(direccionesR)

direccionesR <- direccionesR[1:150,]

write.table(direccionesR@data,
            'Indice Violencia Mujer/muestra violencia mujer reemplazo comuna 8.csv',
            sep=';',row.names=FALSE)

TF3 <- tm_basemap("OpenStreetMap") +
  tm_shape(
    manzanas[manzanas$Comuna!='02' &
               manzanas$sampstatus=='Seleccionada',],
    name='Manzanas') + 
  tm_fill('red4',alpha=0.5) +
  tm_shape(direcciones[direcciones$Comuna!='02',],name='Direcciones') + 
  tm_dots('red',popup.vars='direcciones') +
  tm_shape(direccionesR,name='Direcciones Reemplazo') + 
  tm_dots('blue',popup.vars='direcciones') +
  tm_layout(legend.outside=TRUE,main.title='Villa Hermosa y Centro')
TF3

tmap_save(TF3,
          filename = "muestra villa hermosa reemplazo.html")
