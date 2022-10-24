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

func_dxit <- function(xit, ksim, alfa, beta, dkism_evaluar, dt) {
  
  xit_log <- log(xit)
  prod_xit <- (-xit * xit_log)
  suma_alfa <- crossprod(alfa, ksim_evaluar)
  suma_beta <- crossprod(beta, dkism_evaluar)
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


#############
### APPLY ###
#############




###############
### L O O P ###
###############

# YOSUNE

Y=26
for (i in 1:Y){
estado_variables_dinamica <- as.vector(ksim[i,])  
xit <- estado_variables_dinamica


if (i==1) { # The condition must return TRUE or FALSE
  dkism_evaluar=rep(0,12)# Run some code
} else {
  dkism_evaluar=as.vector(dksim[i-1,]) # Run other code
}

if ((i==1)) print("Code") else print("More code")
 
alfa <- as.vector(input_alfa[i,])
beta <- as.vector(input_beta[i,])
dksim[i,] <- func_dxit(xit, ksim, alfa, beta, dkism_evaluar,dt)

}

return (dksim)

##i =renglón

#################
### N U E V O ###
#################

for (i in 1:26){
  
  estado_variables_dinamica <- as.vector(ksim[i,]) # Estado de la variable dinámica
  xit <- estado_variables_dinamica
  
  alfa <- as.vector(input_alfa[1,]) #Estos tienen que cambiar
  beta <- as.vector(input_beta[1,]) #Estos tienen que cambiar
  ksim_evaluar <- as.vector(ksim[1,])
  dksim_evaluar <- as.vector(dksim[1,])
  
  dksim[i,] <- func_dxit(xit, ksim, alfa, beta, dkism_evaluar,dt)
  
  return (dksim)
  
}

#########################################
### F U N C I O N   D E   C A M B I O ###
#########################################

### 1 con if ###

for (i in 1:26){
  
  estado_variables_dinamica <- as.vector(ksim[i,]) # Estado de la variable dinámica
  xit <- estado_variables_dinamica
  
if(i==1){
  alfa <- as.vector(input_alfa[1,])
  if(i==2){
    alfa <- as.vector(input_alfa[2,])
    if(i==3){
      alfa <- as.vector(input_alfa[3,])
      if(i==4){
        alfa <- as.vector(input_alfa[4,])
        if(i==5){
          alfa <- as.vector(input_alfa[5,])
            if(i==6){
              alfa <- as.vector(input_alfa[6,])
              if(i==7){
                alfa <- as.vector(input_alfa[7,])
                if(i==8){
                  alfa <- as.vector(input_alfa[8,])
                  if(i==9){
                    alfa <- as.vector(input_alfa[9,])
                    if(i==10){
                      alfa <- as.vector(input_alfa[10,])
                      if(i==11){
                        alfa <- as.vector(input_alfa[11,])
                        if(i==12){
                          alfa <- as.vector(input_alfa[12,])
                        }
                      }
                    }
                  }
                }
              }
          }
        }
      }
    }
  }
}

  if(i==1){
    beta <- as.vector(input_beta[1,])
    if(i==2){
      beta <- as.vector(input_beta[2,])
      if(i==3){
        beta <- as.vector(input_beta[3,])
        if(i==4){
          beta <- as.vector(input_beta[4,])
          if(i==5){
            beta <- as.vector(input_beta[5,])
            if(i==6){
              beta <- as.vector(input_beta[6,])
              if(i==7){
                beta <- as.vector(input_beta[7,])
                if(i==8){
                  beta <- as.vector(input_beta[8,])
                  if(i==9){
                    beta <- as.vector(input_beta[9,])
                    if(i==10){
                      beta <- as.vector(input_beta[10,])
                      if(i==11){
                        beta <- as.vector(input_beta[11,])
                        if(i==12){
                          beta <- as.vector(input_beta[12,])
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }  
  
  
  ksim_evaluar <- as.vector(ksim[1,])
  dksim_evaluar <- as.vector(dksim[1,])
  
  dksim[i,] <- func_dxit(xit, ksim, alfa, beta, dkism_evaluar,dt)
  
  return (dksim)
  
}
  
### con nested loop ###
  
  

for (j in 1:12){
  alfa <- as.vector(input_alfa[j,])
  print(alfa)
}





beta <- as.vector(input_beta[1,])
