setwd("/")
setwd("Users/user/Documents/Encuesta")

survey<-read.csv("encuesta_jornada.csv",sep=",", header=T,na.strings = c("","NA")) 
#NA treatment
summary(survey)
na.summary<- c()
survey()

for (myname in names(survey)){
 

  s<-as.data.frame( prop.table(table(is.na(survey[,myname]))))#verifica que valores son nulos

  operacion <-s %>% filter(Var1==TRUE) %>% select(Freq)
  df_temp<-data.frame(column.name=c(myname), na.percentage=ifelse(length(operacion$Freq)==0,0,operacion$Freq[1]))
  na.summary<-rbind(na.summary,df_temp)
}


na.summary %>% arrange(-na.percentage) %>% filter(na.percentage >0)
survey$operaciones_matematicas

survey$veces._cambio_carrera

#survey$veces._cambio_carrera reemplazar por ceros, por que son estudiantes que no han hecho un cambio de carrera

survey[is.na(survey$veces._cambio_carrera),"veces._cambio_carrera "] <- 0 #nofunca

survey$veces._cambio_carrera[is.na(survey$veces._cambio_carrera)]<-0
survey$factores_no_cambio

names(survey)
summary(survey)
types(survey)
x<-survey %>% filter(is.na(lugar_test_vocacional))

x %>% filter(x$previo_test_vocacional=='Si')
#ya validado para estas columnas que sean estudiantes que no han hecho un test
#reemplazar los NA de momento test vocacional por "No aplica" por que son preguntas con dependecia de respuestas
survey$momento_test_vocacional<- as.character(survey$momento_test_vocacional)

survey$momento_test_vocacional[is.na(survey$momento_test_vocacional)] <- "No aplica"

survey$momento_test_vocacional<- as.factor(survey$momento_test_vocacional)
#reemplazar los NA de resultados test vocacional por "No aplica" por que son preguntas con dependecia de respuestas
survey$resultados_congruentes_test<- as.character(survey$resultados_congruentes_test)

survey$resultados_congruentes_test[is.na(survey$resultados_congruentes_test)] <- "No aplica"

survey$resultados_congruentes_test<- as.factor(survey$resultados_congruentes_test)

# reemplazar por no contesto al campo del lugar de test por "no contesto al unico registro que no contesto esa pregunta pese a que si hizo el test previo"

x<-survey %>% filter(is.na(lugar_test_vocacional), survey$previo_test_vocacional=='Si')
survey$lugar_test_vocacional<-as.character(survey$lugar_test_vocacional)
survey$lugar_test_vocacional[survey$Timestamp==x$Timestamp]<-"No contesto"
survey$lugar_test_vocacional<-as.factor(survey$lugar_test_vocacional)

survey %>% filter(lugar_test_vocacional=='No contesto')
summary(survey)

#reemplazar por "no aplica" el campo de lugar de test por que son preguntas con dependencia de respuestas
survey$lugar_test_vocacional<- as.character(survey$lugar_test_vocacional)

survey$lugar_test_vocacional[is.na(survey$lugar_test_vocacional)] <- "No aplica"

survey$lugar_test_vocacional<- as.factor(survey$lugar_test_vocacional)

#cambiar factor no cambio por no aplica por que son personas que no han hecho un cambio pero que si han pesado cambiarse
survey$factores_no_cambio

#cambiar los NA de pensado en cambio de carrerra por no aplica por que son personas que si han hecho un cambio de Carrera
survey$pensado_cambio_carrera<- as.character(survey$pensado_cambio_carrera)
survey$pensado_cambio_carrera[is.na(survey$pensado_cambio_carrera)] <- "No aplica"
survey$pensado_cambio_carrera<-as.factor((survey$pensado_cambio_carrera))
summary(survey)

#cambiar a no contesto a personas que si han pensado en hacer ambio de carrera y no contesto los factores
survey %>% filter(is.na(survey$factores_no_cambio) & (survey$pensado_cambio_carrera=='Sí' | survey$pensado_cambio_carrera=='Tal vez'))

