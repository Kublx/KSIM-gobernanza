#########################
### F U N C I O N E S ###
#########################

### ECUACIÓN DIFERENCIAL KSIM ###

#  dxit = xit * ln(xit) (suma_alfa + suma_beta) * dt

# DONDE:
# xi = cantidad de una variable
# xi0 = cantidad inicial
# xit = valor de la variable en el tiempo anterior t
# suma_alfa = suma de productos entre los valores xi en la matriz alfa de la variable xj
# suma_beta = suma de productos entre los valores xi en la matriz beta de la variable xj
# dt = intervalo de tiempo

func_dxit <- function(xit, ksim_evaluar, alfaiter, betaiter, dksim_evaluar, dt) {
  
  xit_log <- log(xit)
  prod_xit <- (-xit * xit_log)
  suma_alfa <- crossprod(alfaiter, ksim_evaluar)
  suma_beta <- crossprod(betaiter, dksim_evaluar)
  suma_alfabeta <- (suma_alfa + suma_beta)
  dxit <- (prod_xit * suma_alfabeta) * dt
  
  return(dxit)
}


##################################################
### C O N D I C I O N E S    I N I C I A L E S ###
##################################################

vars <- c(rnat, rart, prod, vc, av, ing, gan, tc, ea, er) # Variables
t <- 1:25 # Vector de tiempo
ini <- c(0.5,0.05,0.25,0.5,0.3,0.05,0.5,0.5,0.3,0.25,0.2,0.1) # Valores iniciales
zeros <- c(0,0,0,0,0,0,0,0,0,0,0,0)
dt <- 0.1 # Intervalo de tiempo

##############################################
### M A R I C E S   A L F A    Y   B E T A ###
##############################################

# matriz de interacción alfa
input_alfa <- read.csv("~/Academico/1. Maestria Ciencias de la Sostenibilidad/0_Proyecto_de_Investigacion/4_Modelo/KSIM_R/input_alfa.csv")
input_alfa <- as.matrix(input_alfa) #Matriz de intensidad de relacion entre variables
# matriz de interacción beta
input_beta <- read.csv("~/Academico/1. Maestria Ciencias de la Sostenibilidad/0_Proyecto_de_Investigacion/4_Modelo/KSIM_R/input_beta.csv")
input_beta <- as.matrix(input_beta) #Matriz de intensidad de relacion entre variables

##########################################################################
### M A R T I Z   D E   C O M P O R T A M I E N T O   K S I M  (VACÍA) ###
##########################################################################

ksim_evaluar <- matrix(c(
  ini,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  
), nrow = 26, ncol = 12, byrow = TRUE)

colnames(ksim_evaluar) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", 
                           "regeneracion artificial", "esfuerzo de reforestacion",
                           "regeneracion natural", "productividad")

rownames(ksim_evaluar) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                           "11", "12", "13", "14", "15", "16", "17", "18", "19",
                           "20", "21", "22", "23", "24", "25")

##############################################################
### M A T R I Z   D I F E R E N C I A L   K S I M  (VACÍA) ###
##############################################################

dksim_evaluar <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  
), nrow = 26, ncol = 12, byrow = TRUE)

colnames(dksim_evaluar) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", "regeneracion artificial", 
                           "esfuerzo de reforestacion", "regeneracion natural", "productividad")



rownames(dksim_evaluar) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "13", "14", "15", "16", "17", "18", "19",
                    "20", "21", "22", "23", "24", "25")


###############
### L O O P ###
###############


for (i in 1:25){ ###i son filas
 
  j<-1

     for (j in 1:12){
      xit <- ksim_evaluar[i,j] # Estado de la variable a evaluar en iteracion i
  
      alfa <- as.vector(input_alfa[j,]) #Estos tienen que cambiar por fila
      beta <- as.vector(input_beta[j,]) #Estos tienen que cambiar pr fila
      

       alfaiter<- alfa
       betaiter <- beta
 
 
       if (i==1) { # The condition must return TRUE or FALSE
        ksimini <-  as.vector(ksim_evaluar[i,])
        dksimini<- rep(0,12)
        dksim_evaluar[i+1,j] <- func_dxit(xit, ksimini, alfaiter, betaiter,dksimini, dt)
        ksim_evaluar[i+1,j]  <-  ksim_evaluar[i,j]+dksim_evaluar[i+1,j]
        } else {
        dksim_evaluar[i+1,j] <- func_dxit(xit, ksim_evaluar[i,], alfaiter, betaiter, dksim_evaluar[i,], dt)
        ksim_evaluar[i+1,j]  <-  ksim_evaluar[i,j]+dksim_evaluar[i+1,j]
        }
     }
}



