getwd()
nuevo_dir<- "c:/Pr5_21feb"
setwd(nuevo_dir)
getwd()

#Ejercicio 1
set.seed(20)
registros <- 200
archaeological_data <- data.frame(
  yacimiento=sample(c("Site1","Site2","Site3","Site4","Site5","Site6","Site7","Site8","Site9","Site10"),registros,replace=T), tipo_artef= sample(c("Pottery","Tools","Jewellery","Weapons"), registros, replace=T),
  num_artef=sample(1:1000,registros,replace=T), contexto=sample(c("Habitacional","Funerario","Otros"),registros,replace=T),
  latitud=runif(200,min=0,max=90),
  longitud=runif(200,min=-180,max=180))
View(archaeological_data)


#Ejercicio 2
cuartiles <- quantile(archaeological_data$num_artef,probs=c(0.25,0.5,0.75))
print(cuartiles)
media_num_artef <- mean(archaeological_data$num_artef)
print(media_num_artef)

#Ejercicio 3
Histograma_num_artef <- ggplot(archaeological_data,aes(x=num_artef))+geom_histogram(binwidth = 50,fill="blue",color="black",alpha=0.7)+
  labs(title="Histograma de Artefactos",x="Número de artefactos",y="Frecuencia")
print(Histograma_num_artef)

#En cuanto a la asimetría, se puede comparar la media y la mediana. Si la media es mayor que la mediana, se sugiere que la distribución de 
#se sugiere que la distribución es sesgada a la derecha (positiva) y si la media es menor que la mediana 
#la distribución se considera sesgada hacia la izquierda (negativa).

media_num_artef
mediana_num_artef <- median(archaeological_data$num_artef)
print(mediana_num_artef)
#En este caso la media es mayor que la mediana por lo que la distribución es sesgada a la derecha y es positiva

#Ejercicio 4
gráfico_big_num_art <- ggplot(archaeological_data,aes(y=num_artef))+geom_boxplot(fill="lightblue",color="black")+
  labs(title="Boxplot del Número de artefactos",y="Número de artefactos")
print(gráfico_big_num_art)

#El boxplot mostrará la mediana (línea central dentro de la caja), los cuartiles (límites de la caja), los bigotes que indican la dispersión y la presencia de posibles valores atípicos (puntos fuera de los bigotes).

#La línea central de la caja representa la mediana.
#La caja en sí abarca el rango intercuartílico (IQR), que es la distancia entre el primer y tercer cuartil (Q1 y Q3).
#Los bigotes se extienden hasta los valores más extremos dentro de 1.5 veces el IQR. Puntos fuera de estos bigotes se consideran valores atípicos.
#Observando el boxplot, puedes obtener información sobre la dispersión de los datos, la presencia de valores atípicos y la posición relativa de los cuartiles.

#Ejercicio 5
#Calcular el numero medio de artefactos por yacimiento
df_promedio_yac <- archaeological_data %>% group_by(yacimiento) %>% summarize(media_num_artef=mean(num_artef))
print(df_promedio_yac)

#Crear gráfico de barras con este numero medio de artefactos por yacimiento
ggplot(df_promedio_yac,aes(x=factor(yacimiento),y=media_num_artef))+
  geom_bar(stat="identity",fill="blue",color="black",alpha=0.7)+
  labs(title="Número medio de artefactos por yacimiento",x="Yacimiento",y="Número medio de artefactos por yacimiento")+
  theme(axis.text.x= element_text(angle=45,hjust = 1))

#Ejercicio 6
mapa_calor <- ggplot(archaeological_data,aes(x=longitud,y=latitud))+geom_tile(aes(fill=num_artef),width=10,height=4)+
  scale_fill_gradient(low="white",high="blue")+labs(title="Mapa de calor Artefactos",x="Longitud",y="Latitud")+
  theme_minimal()
print(mapa_calor)

#Ejercicio 7
#Calcular el numero total de artefactos por yacimiento

tot_arte_yac <- sum(archaeological_data$num_artef)
print(tot_arte_yac)

#Ejercicio 8
mediana_artef_yac <- median(archaeological_data$num_artef)
print(mediana_artef_yac)

#Ejercicio 9
desviacion_estandar_artef <- sd(archaeological_data$num_artef)
print(desviacion_estandar_artef)

#Ejercicio 10
yacimiento_max_artefactos <- archaeological_data$yacimiento[which.max(archaeological_data$num_artef)]
max_artefactos <- max(archaeological_data$num_artef)
print(yacimiento_max_artefactos)
print(max_artefactos)

#Ejercicio 11
tabla_resumen <- archaeological_data %>% group_by(yacimiento) %>% 
  summarize( media_artef=mean(num_artef),
             mediana_artefactos=median(num_artef),
             desviacion_estandar_artefactos=sd(num_artef))
print(tabla_resumen)
View(tabla_resumen)

#Ejercicio 12
ggplot(archaeological_data,aes(y=num_artef))+geom_boxplot(fill="lightblue",color="black")+
  labs(title= "Diagrama de Cajas del Número de Artefactos", y="Número de Artefactos")+
  theme_minimal()
