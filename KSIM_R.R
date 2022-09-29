##################################################
######## P R E P A R A C I O N    D E    #########
### C O N D I C I O N E S    I N I C I A L E S ###
##################################################

vars <- c(rnat, rart, prod, vc, av, ing, gan, tc, ea, er)
t <- 1:25
ini <- c(0.5,0.05,0.25,0.5,0.3,0.05,0.5,0.5,0.3,0.25,0.2)
zeros <- c(0,0,0,0,0,0,0,0,0,0,0,0)
dt <- 0.1

input_alfa <- read.csv("~/Academico/1. Maestria Ciencias de la Sostenibilidad/4_ Modelo/4_ Modelo/KSIM_R/input_alfa.csv")
input_alfa <- as.matrix(input_alfa)
input_beta <- read.csv("~/Academico/1. Maestria Ciencias de la Sostenibilidad/4_ Modelo/4_ Modelo/KSIM_R/input_beta.csv")
input_beta <- as.matrix(input_beta)

alfa <- matrix(c(
  ini, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  
), nrow = 26, ncol = 11, byrow = TRUE)

colnames(alfa) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", 
                           "regeneracion artificial", "esfuerzo de reforestacion",
                           "regeneracion natural")

rownames(alfa) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                           "11", "12", "13", "14", "15", "16", "17", "18", "19",
                           "20", "21", "22", "23", "24", "25")

alfa

beta <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  
), nrow = 26, ncol = 11, byrow = TRUE)

colnames(beta) <- c("unidades de recursos", "volumen de corta", 
                           "acuerdos de venta", "ingresos", "ganancias", 
                           "sanciones", "trabajo comunitario", 
                           "eficiencia de aprovechamiento", "regeneracion artificial", 
                           "esfuerzo de reforestacion", "regeneracion natural")



rownames(beta) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                    "11", "12", "13", "14", "15", "16", "17", "18", "19",
                    "20", "21", "22", "23", "24", "25")

beta

########################################################
### S U M A S    D E    P R O D U C T O S    K S I M ###
########################################################

### B E T A ###

### FILA 2, TIEMPO 1 ###
beta[2,1] <- ((-alfa[1,1]) * log(alfa[1,1])) * crossprod(input_alfa[1,], alfa[1,]) + (crossprod(input_beta[1,], beta[1,]))
beta[2,2] <- ((-alfa[1,2]) * log(alfa[1,2])) * crossprod(input_alfa[2,], alfa[1,]) + (crossprod(input_beta[2,], beta[1,]))
beta[2,3] <- ((-alfa[1,3]) * log(alfa[1,3])) * crossprod(input_alfa[3,], alfa[1,]) + (crossprod(input_beta[3,], beta[1,]))
beta[2,4] <- ((-alfa[1,4]) * log(alfa[1,4])) * crossprod(input_alfa[4,], alfa[1,]) + (crossprod(input_beta[4,], beta[1,]))
beta[2,5] <- ((-alfa[1,5]) * log(alfa[1,5])) * crossprod(input_alfa[5,], alfa[1,]) + (crossprod(input_beta[5,], beta[1,]))
beta[2,6] <- ((-alfa[1,6]) * log(alfa[1,6])) * crossprod(input_alfa[6,], alfa[1,]) + (crossprod(input_beta[6,], beta[1,]))
beta[2,7] <- ((-alfa[1,7]) * log(alfa[1,7])) * crossprod(input_alfa[7,], alfa[1,]) + (crossprod(input_beta[7,], beta[1,]))
beta[2,8] <- ((-alfa[1,8]) * log(alfa[1,8])) * crossprod(input_alfa[8,], alfa[1,]) + (crossprod(input_beta[8,], beta[1,]))
beta[2,9] <- ((-alfa[1,9]) * log(alfa[1,9])) * crossprod(input_alfa[9,], alfa[1,]) + (crossprod(input_beta[9,], beta[1,]))
beta[2,10] <- ((-alfa[1,10]) * log(alfa[1,10])) * crossprod(input_alfa[10,], alfa[1,]) + (crossprod(input_beta[10,], beta[1,]))
beta[2,11] <- ((-alfa[1,11]) * log(alfa[1,11])) * crossprod(input_alfa[11,], alfa[1,]) + (crossprod(input_beta[11,], beta[1,]))
### FILA 3, TIEMPO 2 ###
beta[3,1] <- ((-alfa[2,1]) * log(alfa[2,1])) * crossprod(input_alfa[1,], alfa[2,]) + (crossprod(input_beta[1,], beta[2,]))
beta[3,2] <- ((-alfa[2,2]) * log(alfa[2,2])) * crossprod(input_alfa[2,], alfa[2,]) + (crossprod(input_beta[2,], beta[2,]))
beta[3,3] <- ((-alfa[2,3]) * log(alfa[2,3])) * crossprod(input_alfa[3,], alfa[2,]) + (crossprod(input_beta[3,], beta[2,]))
beta[3,4] <- ((-alfa[2,4]) * log(alfa[2,4])) * crossprod(input_alfa[4,], alfa[2,]) + (crossprod(input_beta[4,], beta[2,]))
beta[3,5] <- ((-alfa[2,5]) * log(alfa[2,5])) * crossprod(input_alfa[5,], alfa[2,]) + (crossprod(input_beta[5,], beta[2,]))
beta[3,6] <- ((-alfa[2,6]) * log(alfa[2,6])) * crossprod(input_alfa[6,], alfa[2,]) + (crossprod(input_beta[6,], beta[2,]))
beta[3,7] <- ((-alfa[2,7]) * log(alfa[2,7])) * crossprod(input_alfa[7,], alfa[2,]) + (crossprod(input_beta[7,], beta[2,]))
beta[3,8] <- ((-alfa[2,8]) * log(alfa[2,8])) * crossprod(input_alfa[8,], alfa[2,]) + (crossprod(input_beta[8,], beta[2,]))
beta[3,9] <- ((-alfa[2,9]) * log(alfa[2,9])) * crossprod(input_alfa[9,], alfa[2,]) + (crossprod(input_beta[9,], beta[2,]))
beta[3,10] <- ((-alfa[2,10]) * log(alfa[2,10])) * crossprod(input_alfa[10,], alfa[2,]) + (crossprod(input_beta[10,], beta[2,]))
beta[3,11] <- ((-alfa[2,11]) * log(alfa[2,11])) * crossprod(input_alfa[11,], alfa[2,]) + (crossprod(input_beta[11,], beta[2,]))
### FILA 4, TIEMPO 3 ###
beta[4,1] <- ((-alfa[3,1]) * log(alfa[3,1])) * crossprod(input_alfa[1,], alfa[3,]) + (crossprod(input_beta[1,], beta[3,]))
beta[4,2] <- ((-alfa[3,2]) * log(alfa[3,2])) * crossprod(input_alfa[2,], alfa[3,]) + (crossprod(input_beta[2,], beta[3,]))
beta[4,3] <- ((-alfa[3,3]) * log(alfa[3,3])) * crossprod(input_alfa[3,], alfa[3,]) + (crossprod(input_beta[3,], beta[3,]))
beta[4,4] <- ((-alfa[3,4]) * log(alfa[3,4])) * crossprod(input_alfa[4,], alfa[3,]) + (crossprod(input_beta[4,], beta[3,]))
beta[4,5] <- ((-alfa[3,5]) * log(alfa[3,5])) * crossprod(input_alfa[5,], alfa[3,]) + (crossprod(input_beta[5,], beta[3,]))
beta[4,6] <- ((-alfa[3,6]) * log(alfa[3,6])) * crossprod(input_alfa[6,], alfa[3,]) + (crossprod(input_beta[6,], beta[3,]))
beta[4,7] <- ((-alfa[3,7]) * log(alfa[3,7])) * crossprod(input_alfa[7,], alfa[3,]) + (crossprod(input_beta[7,], beta[3,]))
beta[4,8] <- ((-alfa[3,8]) * log(alfa[3,8])) * crossprod(input_alfa[8,], alfa[3,]) + (crossprod(input_beta[8,], beta[3,]))
beta[4,9] <- ((-alfa[3,9]) * log(alfa[3,9])) * crossprod(input_alfa[9,], alfa[3,]) + (crossprod(input_beta[9,], beta[3,]))
beta[4,10] <- ((-alfa[3,10]) * log(alfa[3,10])) * crossprod(input_alfa[10,], alfa[3,]) + (crossprod(input_beta[10,], beta[3,]))
beta[4,11] <- ((-alfa[3,11]) * log(alfa[3,11])) * crossprod(input_alfa[11,], alfa[3,]) + (crossprod(input_beta[11,], beta[3,]))
### FILA 5, TIEMPO 4 ###
beta[5,1] <- ((-alfa[4,1]) * log(alfa[4,1])) * crossprod(input_alfa[1,], alfa[4,]) + (crossprod(input_beta[1,], beta[4,]))
beta[5,2] <- ((-alfa[4,2]) * log(alfa[4,2])) * crossprod(input_alfa[2,], alfa[4,]) + (crossprod(input_beta[2,], beta[4,]))
beta[5,3] <- ((-alfa[4,3]) * log(alfa[4,3])) * crossprod(input_alfa[3,], alfa[4,]) + (crossprod(input_beta[3,], beta[4,]))
beta[5,4] <- ((-alfa[4,4]) * log(alfa[4,4])) * crossprod(input_alfa[4,], alfa[4,]) + (crossprod(input_beta[4,], beta[4,]))
beta[5,5] <- ((-alfa[4,5]) * log(alfa[4,5])) * crossprod(input_alfa[5,], alfa[4,]) + (crossprod(input_beta[5,], beta[4,]))
beta[5,6] <- ((-alfa[4,6]) * log(alfa[4,6])) * crossprod(input_alfa[6,], alfa[4,]) + (crossprod(input_beta[6,], beta[4,]))
beta[5,7] <- ((-alfa[4,7]) * log(alfa[4,7])) * crossprod(input_alfa[7,], alfa[4,]) + (crossprod(input_beta[7,], beta[4,]))
beta[5,8] <- ((-alfa[4,8]) * log(alfa[4,8])) * crossprod(input_alfa[8,], alfa[4,]) + (crossprod(input_beta[8,], beta[4,]))
beta[5,9] <- ((-alfa[4,9]) * log(alfa[4,9])) * crossprod(input_alfa[9,], alfa[4,]) + (crossprod(input_beta[9,], beta[4,]))
beta[5,10] <- ((-alfa[4,10]) * log(alfa[4,10])) * crossprod(input_alfa[10,], alfa[4,]) + (crossprod(input_beta[10,], beta[4,]))
beta[5,11] <- ((-alfa[4,11]) * log(alfa[4,11])) * crossprod(input_alfa[11,], alfa[4,]) + (crossprod(input_beta[11,], beta[4,]))
### FILA 6, TIEMPO 5 ###
beta[6,1] <- ((-alfa[5,1]) * log(alfa[5,1])) * crossprod(input_alfa[1,], alfa[5,]) + (crossprod(input_beta[1,], beta[5,]))
beta[6,2] <- ((-alfa[5,2]) * log(alfa[5,2])) * crossprod(input_alfa[2,], alfa[5,]) + (crossprod(input_beta[2,], beta[5,]))
beta[6,3] <- ((-alfa[5,3]) * log(alfa[5,3])) * crossprod(input_alfa[3,], alfa[5,]) + (crossprod(input_beta[3,], beta[5,]))
beta[6,4] <- ((-alfa[5,4]) * log(alfa[5,4])) * crossprod(input_alfa[4,], alfa[5,]) + (crossprod(input_beta[4,], beta[5,]))
beta[6,5] <- ((-alfa[5,5]) * log(alfa[5,5])) * crossprod(input_alfa[5,], alfa[5,]) + (crossprod(input_beta[5,], beta[5,]))
beta[6,6] <- ((-alfa[5,6]) * log(alfa[5,6])) * crossprod(input_alfa[6,], alfa[5,]) + (crossprod(input_beta[6,], beta[5,]))
beta[6,7] <- ((-alfa[5,7]) * log(alfa[5,7])) * crossprod(input_alfa[7,], alfa[5,]) + (crossprod(input_beta[7,], beta[5,]))
beta[6,8] <- ((-alfa[5,8]) * log(alfa[5,8])) * crossprod(input_alfa[8,], alfa[5,]) + (crossprod(input_beta[8,], beta[5,]))
beta[6,9] <- ((-alfa[5,9]) * log(alfa[5,9])) * crossprod(input_alfa[9,], alfa[5,]) + (crossprod(input_beta[9,], beta[5,]))
beta[6,10] <- ((-alfa[5,10]) * log(alfa[5,10])) * crossprod(input_alfa[10,], alfa[5,]) + (crossprod(input_beta[10,], beta[5,]))
beta[6,11] <- ((-alfa[5,11]) * log(alfa[5,11])) * crossprod(input_alfa[11,], alfa[5,]) + (crossprod(input_beta[11,], beta[5,]))
### FILA 7, TIEMPO 6 ###
beta[7,1] <- ((-alfa[6,1]) * log(alfa[6,1])) * crossprod(input_alfa[1,], alfa[6,]) + (crossprod(input_beta[1,], beta[6,]))
beta[7,2] <- ((-alfa[6,2]) * log(alfa[6,2])) * crossprod(input_alfa[2,], alfa[6,]) + (crossprod(input_beta[2,], beta[6,]))
beta[7,3] <- ((-alfa[6,3]) * log(alfa[6,3])) * crossprod(input_alfa[3,], alfa[6,]) + (crossprod(input_beta[3,], beta[6,]))
beta[7,4] <- ((-alfa[6,4]) * log(alfa[6,4])) * crossprod(input_alfa[4,], alfa[6,]) + (crossprod(input_beta[4,], beta[6,]))
beta[7,5] <- ((-alfa[6,5]) * log(alfa[6,5])) * crossprod(input_alfa[5,], alfa[6,]) + (crossprod(input_beta[5,], beta[6,]))
beta[7,6] <- ((-alfa[6,6]) * log(alfa[6,6])) * crossprod(input_alfa[6,], alfa[6,]) + (crossprod(input_beta[6,], beta[6,]))
beta[7,7] <- ((-alfa[6,7]) * log(alfa[6,7])) * crossprod(input_alfa[7,], alfa[6,]) + (crossprod(input_beta[7,], beta[6,]))
beta[7,8] <- ((-alfa[6,8]) * log(alfa[6,8])) * crossprod(input_alfa[8,], alfa[6,]) + (crossprod(input_beta[8,], beta[6,]))
beta[7,9] <- ((-alfa[6,9]) * log(alfa[6,9])) * crossprod(input_alfa[9,], alfa[6,]) + (crossprod(input_beta[9,], beta[6,]))
beta[7,10] <- ((-alfa[6,10]) * log(alfa[6,10])) * crossprod(input_alfa[10,], alfa[6,]) + (crossprod(input_beta[10,], beta[6,]))
beta[7,11] <- ((-alfa[6,11]) * log(alfa[6,11])) * crossprod(input_alfa[11,], alfa[6,]) + (crossprod(input_beta[11,], beta[6,]))