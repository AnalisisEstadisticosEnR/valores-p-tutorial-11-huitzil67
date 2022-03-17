# Entendiendo los valores-p
# 10/03/2022
# Calcular un valor p de una distribución normal
library(tidyverse) 
# Vamos primero a ver los resultados reales (inventados)
#del 10 de abril para entender los resultados de la 
#consulta 
Morelos <- c(rep("sigue", 2473707), rep("revoca", 2461779)) 
Guanajuato <- c(rep("sigue", 153778), rep("revoca", 189951)) 
proportions(table(Morelos)) 
proportions(table(Guanajuato))
# Un muestreo de 1,000 votantes
set.seed(2020) 
survey_a1 <- sample(Guanajuato,1000,replace=F) 
table(survey_a1) %>% as.data.frame %>% 
  ggplot(aes(x=survey_a1, y = Freq, label = Freq)) + 
  geom_col(fill=c("blue", "red")) + 
  geom_label(label = paste(proportions(table(survey_a1))*100,"%")) + 
  theme_minimal() + ggtitle("Resultados de encuesta en Guanajuato (N = 1,000)")
prop.test(table(survey_a1))
# Repetición de la encuesta
survey_a2 <- sample(Guanajuato,1000,replace=F) 
table(survey_a2) %>% as.data.frame %>% 
  ggplot(aes(x=survey_a2, y = Freq, label = Freq)) + 
  geom_col(fill=c("blue", "red")) + 
  geom_label(label = paste(proportions(table(survey_a2))*100,"%")) + 
  theme_minimal() + 
  ggtitle("Resultados de la segunda encuesta en Guanajuato (N = 1,000)")
draw_sample <- function(x,n=1000){ 
  s <- sample(x, n, F) 
  proportions(table(s)) 
  } 
set.seed(2020) 
Guanajuato_sim <- replicate(1000,draw_sample(Guanajuato))
Guanajuato_sim[1:2,1:20]
dat <- data.frame(t(Guanajuato_sim)) 
qplot() + theme_minimal() + 
  geom_histogram(aes(x=dat$sigue),fill="red", alpha=.5) + 
  geom_histogram(aes(x=dat$revoca), fill="blue", alpha=.5) + 
  geom_vline(xintercept = mean(dat$sigue), color="red") + 
  geom_vline(xintercept = mean(dat$revoca), color="blue") + 
  ggtitle("1,000 encuestas en Guanajuato de 1,000 votantes cada una ", 
          subtitle="Histogramas de valores simulados para revocación (azul) y 
          seguir en mandato (rojo)") 
c(mean(dat$sigue), mean(dat$revoca))
c(mean(dat$sigue), mean(dat$revoca))
voto <- .564 
N <- 1000 
mean <- voto*N 
sd <- sqrt(voto*(1-voto)*N) 
x <- (mean - 5*sd):(mean + 5*sd) 
norm1 <- dnorm(x,mean,sd) 
p <- pnorm(x,mean,sd) 
qplot() + theme_minimal() + 
  geom_line(aes(x=x,y=norm1), color="blue") + 
  geom_area(aes(x=x,y=norm1),fill="blue",alpha=.1) + 
  geom_vline(xintercept = x[which.min(abs(p-0.025))], color="blue") + 
  geom_vline(xintercept = x[which.min(abs(p-0.975))], color="blue") + 
  ggtitle("Distribucion normal con N = 1,000", 
          subtitle= paste0("Lineas verticales = 95% intervalo alrededor de la 
                           media: ", 
                           round(x[which.min(abs(p-0.025))]/N*100,1), "% a ", 
                           round(x[which.min(abs(p-0.975))]/N*100,1),"%"))
# El efecto de tamaño de muestra
N <- 10000 
mean <- voto*N 
sd <- sqrt(voto*(1-voto)*N) 
x <- x*10 
norm1 <- dnorm(x,mean,sd) 
p <- pnorm(x,mean,sd) 
qplot() + theme_minimal() + 
  geom_line(aes(x=x,y=norm1), color="blue") + 
  geom_area(aes(x=x,y=norm1),fill="blue",alpha=.1) + 
  geom_vline(xintercept = x[which.min(abs(p-0.025))], color="blue") + 
  geom_vline(xintercept = x[which.min(abs(p-0.975))], color="blue") + 
  ggtitle("Distribucion normal con N = 1,000", 
          subtitle= paste0("Lineas verticales = 95% intervalo alrededor de la 
                           media: ", 
                           round(x[which.min(abs(p-0.025))]/N*100,1), "% a ", 
                           round(x[which.min(abs(p-0.975))]/N*100,1),"%"))
# ¿Qué es el error estándar?
dat <- data.frame(t(Guanajuato_sim)) 
sd(dat$revoca)
#Error estándar 
sqrt(0.564*0.436/1000)
#Intervalo de confianza 
se = sqrt(0.564*0.436/1000) 
c(.546-2*se, .564+2*se)
# Y a todo esto, ¿cómo lo hago en R?
  binom.test(564,1000, p=0.5, alternative="greater")
# Una encuesta en Morelos 
set.seed(2020) 
survey_g1 <- sample(Morelos,1000,replace=F) 
table(survey_g1) %>% as.data.frame %>% 
  ggplot(aes(x=survey_g1, y = Freq, label = Freq)) + 
  geom_col(fill=c("red", "blue")) + 
  geom_label(label = paste(proportions(table(survey_g1))*100,"%")) + 
  theme_minimal() + 
  ggtitle("Resultados de encuesta en Morelos (N = 1,000)") 
prop.test(table(survey_g1))
# ¿Cómo evitamos un error de tipo II (dado por tamaño de muestra)?
# ¿Qué tan grande debe de ser mi muestra?
# Aumentamos el tamaño de muestra y le preguntamos a 
# 100,000 personas en vez de 1,000 
set.seed(2020) 
survey_g2 <- sample(Morelos,100000,replace=F) 
table(survey_g2) %>% as.data.frame %>% 
  ggplot(aes(x=survey_g2, y = Freq, label = Freq)) + 
  geom_col(fill=c("red", "blue")) + 
  geom_label(label = paste(proportions(table(survey_g2))*100,"%")) + 
  theme_minimal() + 
  ggtitle("Resultados de la segunda encuesta en Morelos (N = 100,000)") 
prop.test(table(survey_g2))
# (¡tan grandes son nuestros fondos que nos permite 
#preguntarle a más personas que la población completa 
# de Morelos!): 
survey_g4 <- sample(Morelos,3000000,replace=F) 
table(survey_g4) %>% as.data.frame %>% 
  ggplot(aes(x=survey_g4, y = Freq, label = Freq)) + 
  geom_col(fill=c("red", "blue")) + 
  geom_label(label = paste(round(proportions(table(survey_g4))*100,4),"%")) + 
  theme_minimal() + 
  ggtitle("Resultados de la tercera encuesta en Morelos (N = 3,000,000)") 
prop.test(table(survey_g4))
