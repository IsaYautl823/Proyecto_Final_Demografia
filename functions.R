#******************************#
#******************************#
#
#                          Trabajo Final del Curso de Demografía 
#                                        2026_1
#                             Facultad de Ciencias UNAM
#
#                 Tablas de mortalidad San Luis Potosí 2010,2019 y 2021
#
#                                      Funciones
#
#
#         Creado por:               Isa Yautl Cortes Lima.
#                                   Natalia Cruz Noriega
#         Fecha de creación:        04/11/2025
#         Actualizado por:          Isa Yautl Cortes.
#         Fecha de actualización:   25/11/2025
#         Contacto:                 isacorlim@ciencias.unam.mx
#
#******************************#
#******************************#


# 1. Tablas de vida ----

lt_abr <- function(x, mx, sex="f", IMR=NA){
  
  m <- length(x)
  n <- c(diff(x), NA)  
  
  ax <- n/2    
  
  # Pag. 4 notas de clase - cuadro
  
  ## Coale y Demeny edades 0 a 1
  
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[1] <- 0.330 }else{
      ax[1] <- 0.045+2.684*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[1] <- 0.350 }else{
      ax[1] <- 0.053+2.800*mx[1]
    }  
  }
  
  ## Coale y Demeny edades 1 a 4
  if(sex=="m"){
    if(mx[1]>=0.107){ ax[2] <- 1.352 }else{
      ax[2] <- 1.651-2.816*mx[1]
    } 
  } else if(sex=="f"){
    if(mx[1]>=0.107){ ax[2] <- 1.361 }else{
      ax[2] <- 1.522-1.518*mx[1]
    }  
  }
  
  # Probabilidad de muerte
  qx <- (n*mx)/(1+(n-ax)*mx)
  qx[m] <- 1
  
  # Proba de sobrevivir
  px <- 1-qx
  
  # l_x
  lx <- 100000 * cumprod(c(1,px[-m]))
  
  # Defunciones
  dx <- c(-diff(lx), lx[m])
  
  # Años persona vividos
  Lx <- n* c(lx[-1], 0) + ax*dx
  Lx[m] <- lx[m]/mx[m]
  
  # Años persona vividos acumulados
  
  Tx <- rev(cumsum(rev(Lx)))
  
  # Esperanza de vida
  ex <- Tx/lx
  
  return(data.table(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex))
  
  
}

# Uso la función lt_abr
# lt_abr(x, mx)



# 2. Crecimiento exponencial ----

expo <- function(N_0, N_T, t_0, t_T, t){
  
  dt <- decimal_date(as.Date(t_T)) - decimal_date(as.Date(t_0))
  r <- log(N_T/N_0)/dt
  
  h <- t - decimal_date(as.Date(t_0))
  N_h <- N_0 * exp(r*h)  
  
  return(N_h)
  
}

#3 Descomposición por edad de la diferencia de la e_0 entre períodos----

desc <- function(lx1, Lx1, lx2, Lx2, age) {
  Tx1 <- rev(cumsum(rev(Lx1)))
  Tx2 <- rev(cumsum(rev(Lx2)))
  ex1 <- Tx1/lx1
  ex2 <- Tx2/lx2
  
  # Verificar que los vectores tengan la misma longitud
  if(length(lx1) != length(lx2) | length(Lx1) != length(Lx2)) {
    stop("Los vectores de entrada deben tener la misma longitud")
  }
  
  n <- length(lx1)
  dif <- numeric(n)
  
  # contribuciones por edad
  for (i in 1:(n-1) ) {
    
    # primer sumando
    term1 <- (lx1[i]/lx1[1])*((Lx2[i]/lx2[i]) - (Lx1[i]/lx1[i]))
    # Segundo sumando
    term2 <- (Tx2[i+1]/lx1[1])*((lx1[i]/lx2[i])-(lx1[i+1]/lx2[i+1]))
    dif[i] <- term1 + term2
  }
  
  # ultimo grupo de edad 
  
  dif[n] <- (lx1[n]/lx1[1]) * ((Tx2[n]/lx2[n]) - (Tx1[n]/lx1[n]))
  
  return(data.table(age,lx1,Lx1,Tx1,ex1,lx2,Lx2,Tx2,ex2,dif))
}


