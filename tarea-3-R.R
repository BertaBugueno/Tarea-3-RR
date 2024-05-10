
#TAREA N° 3

#Anexar problema del trabajo 1


#CARGAR PAQUETES##

pacman::p_load(tidyverse, #Conjunto de paquetes, sobre todo dplyr y ggplot2
               car, #Para recodificar
               haven,
               summarytools, #Para descriptivos
               sjmisc,
               psych,
               sjlabelled,
               sjPlot,
               corrplot,
               psych)
               



##CARGAR BASE DE DATOS#
#Se toma como base la encuesta de Bienestar Social (EBS)
#del Ministerio de Desarrollo Social, se considera una
#base de datos que contiene las variables idóneas para
#este estudio, adempas de ser de muy fácil acceso

Base_de_datos_EBS_2021_STATA <- read_dta("Imput/Base de datos EBS 2021 STATA.dta")


##selección de variables ##
#Se trabajará con variables nivel educacional, satisfacción de ingresos, satisfacción de vivienda
#educación ingresos. La encuesta EBS, contiene estas variables que se corresponden o contienen
#lo que esta investigación se plantea dilucidar#
#En esta etapa se desarrollará nivel educacional y satisfacción con los ingresos, ya que como
#hipótesis se plantea que ambas variables tienen correlación inversa, es decir a menor nivel educacional
#mayor insatisfacción con los ingresos y a medida que aumenta el nivel educacional hay una mayor satisfacción
#con los ingresos, 


 
view(dfSummary(Base_de_datos_EBS_2021_STATA, headings=FALSE, graph.col = FALSE))

proc_data <- Base_de_datos_EBS_2021_STATA %>% select(zona, sexo, l6, #nivel educacional
                                                     a3_3, #satisfacción ingresos
                                                     a3_7, # satisfacción vivienda
                                                     d1_1) # educación-ingresos


# Comprobar
names(proc_data)


sjlabelled::get_label(proc_data)
#Cambiar nombres y etiquetas
# Se siguen los pasos para el cambio de etiquetas, las que quedarán finalmente como; nivel educacional
#satisfacción con los ingresos, satisfacción con la vivienda, efecto de la educación sobre los ingresos#

proc_data = proc_data %>% rename("nivel_educ"=l6, "satis_ingreso"=a3_3, "satis_vivienda"=a3_7, "educ_ingresos"=d1_1)

proc_data$nivel_educ = set_label(x = proc_data$nivel_educ,label = "nivel educacional")
proc_data$satis_ingreso = set_label(x = proc_data$satis_ingreso,label = "satisfacción con los ingresos")
proc_data$satis_vivienda = set_label(x = proc_data$satis_vivienda,label = "satisfacción con la vivienda")
proc_data$educ_ingresos = set_label(x = proc_data$educ_ingresos,label = "efecto de la educación sobre los ingresos")

#### TABLA DE CONTINGENCIA DE VARIABLES###
#Según cuadro con las variables satisfacción con los ingresos variable nivel educacional
#De 1871 encuestados la moda la tiene “insatisfecho” con 652 preferencias y dentro de esta misma la educación media científico humanista aporta 124 preferencias.
#De esas mismas preferencias el menor porcentaje lo tiene la opción “totalmente satisfecho” con 132 preferencias, dentro de ellas se puede mencionar que no obtuvieron ninguna preferencia las personas que nunca asistieron y educación especial (diferencial), con 2 votos primaria, cojn 1 humanidades (sistema antiguo) dentro de este rango la mayor votación la tiene con 23 preferencias la aporta nivel técnico superior completo.
#Por lo tanto, se puede evidenciar que la variable satisfacción con los ingresos es proporcional a nivel educacional, mientras menos educación hay menor satisfacción con los ingresos y mientras mayor nivel educacional existe mayor satisfacción con los ingresos.



###HACER UN GRÁFICO####

sjt.xtab(proc_data$nivel_educ, proc_data$satis_ingreso)

M <- cor(proc_data, use = "complete.obs")
corrplot.mixed(M)
#Grafico figura 4, se aprecia las 6 variables, en esta etapa del trabajo estamos trabajando con dos 
#que son: nivel educacional y satisfacción con los ingresos, y se puede apreciar que ambas son bajas
#mientras la satisfacción con los ingresos llega en su valor más alto a un 0,28 el nivel educacional 
#marca un -0,18 interesante se infiere que estas variables tambien tienen una variación con respecto
#a la zona.


#Otro gráfico#
ggplot(proc_data, aes(x = nivel_educ))
ggplot()
ggplot(proc_data, aes(x = nivel_educ))ggplot(proc_data, aes(x = nivel_educ))
ggplot(proc_data, aes(x = nivel_educ))
proc_data %>% ggplot(aes(x = nivel_educ)) + 
  geom_bar()
#Gráfico con colores#
proc_data %>% ggplot(aes(x = nivel_educ)) + 
  geom_bar(fill = "coral")
#En este gráfico de nivel educacional se puede constatar los sesgos educacionales
#en Chile, siendo el sesgo más importante o más extenso las personas sin educación
# o poca escolaridad, la curva es de correlación positiva y la moda la marca las personas que #tienen 13 años de escolaridad 


#Se puede concluir, entonces,  que con las evidencias de las encuestas, graficadas y documentadas
#que la pobreza y el desarrollo humano, tienen incidencias en muchas variables, en Chile el Ministerio
# de Desarrollo Social lo mide en 5 variables, sin embargo, para este trabajo se eligieron 6 variables 
# ya que no se está midiendo solo la pobreza, sino que la pobreza extrema y el desarrollo humano
#siendo, hasta esta etapa de la investigación muy relevantes las variables trabajadas hasta ahora
# nivel educacional y satisfacción con los ingresos.ambas variables tienen una correlación inversa



