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

ksim <- matrix(c(
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

colnames(ksim) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", 
                           "regeneracion artificial", "esfuerzo de reforestacion",
                           "regeneracion natural", "productividad")

rownames(ksim) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                           "11", "12", "13", "14", "15", "16", "17", "18", "19",
                           "20", "21", "22", "23", "24", "25")

##############################################################
### M A T R I Z   D I F E R E N C I A L   K S I M  (VACÍA) ###
##############################################################

dksim <- matrix(c(
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

colnames(dksim) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", "regeneracion artificial", 
                           "esfuerzo de reforestacion", "regeneracion natural", "productividad")



rownames(dksim) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "13", "14", "15", "16", "17", "18", "19",
                    "20", "21", "22", "23", "24", "25")

#########################################################
### E C U A C I Ó N   D I F E R E N C I A L   K S I M ###
#########################################################

#  dxit = xit * ln(xit) (suma_alfa + suma_beta) * dt

# DONDE:
# xi = cantidad de una variable
# xi0 = cantidad inicial
# xit = valor de la variable en el tiempo anterior t
# suma_alfa = suma de productos entre los valores xi en la matriz alfa de la variable xj
# suma_beta = suma de productos entre los valores xi en la matriz beta de la variable xj
# dt = intervalo de tiempo

xit <- ksim[1,1]
xit_log <- log(xit)
prod_xit <- (-xit * xit_log)
suma_alfa <- crossprod(input_alfa[1,], ksim[1,])
suma_beta <- crossprod(input_beta[1,], dksim[1,])
dxit <- (prod_xit * ( suma_alfa + suma_beta ))*dt

# func_dxit <- function(xit, xit_log, prod_xit, suma_alfa, suma_beta)
# {dxit <- (prod_xit * ( suma_alfa + suma_beta ))*dt
#  return(dxit)
#  }

###############
### L O O P ###
###############

for (i in dksim[2,]){

    
  }
