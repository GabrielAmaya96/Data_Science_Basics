
# Cargar archivos

library(openxlsx)
Pruebas_Estado <- read.xlsx('C:/Users/gabrielamabec/Personal_Projects/Data_Science_Basics/Pruebas Estado/BD/BD Pruebas de Estado.xlsx',sheet = 'Prueba_S11_vs_SP')


# Realizar pruebas de normalidad

    # Histograma Puntaje inglés prueba saber 11
      S11_Summary_Ing = summary(Pruebas_Estado$PUNT_INGLES_S11_2015)
      S11_Summary_Ing
      
      hist(x = Pruebas_Estado$PUNT_INGLES_S11_2015,
           main = 'Histograma puntaje de inglés prueba saber 11' ,
           xlab = 'Puntaje obtenido',
           ylab = 'Frecuencia',
           col = 'aquamarine')
      
      # Prueba de normalidad 
      library("nortest")
        Alpha <- 0.05
      
        
        # Gráfico de normalidad
          qqnorm(Pruebas_Estado$PUNT_INGLES_S11_2015, xlab = "", ylab = "",
                 main = "Puntaje Inglés Saber 11 - 2015", col = "aquamarine")
          qqline(Pruebas_Estado$PUNT_INGLES_S11_2015)
        
        # Anderson Darling y Lilliefors
            # Ho: Normalidad en la población.
            # Ha: No existe normalidad en la población.
              
              Ander_S11 <- ad.test(Pruebas_Estado$PUNT_INGLES_S11_2015)
              Lillie_S11 <- lillie.test(Pruebas_Estado$PUNT_INGLES_S11_2015)
    
          # Validación por p-value vs alpha
              
            Ander_S11$p.value < Alpha
            Lillie_S11$p.value < Alpha
        # Con un nivel de significancia del 5% se puede afirmar que los resultados de la prueba no siguen una distribución normal.
      
    # Histograma Puntaje inglés prueba saber pro 2019
        SP_Summary_Ing = summary(Pruebas_Estado$PUNT_INGLES_SP_2019)
        SP_Summary_Ing
        hist(x = Pruebas_Estado$PUNT_INGLES_SP_2019,
             main = 'Histograma puntaje de inglés prueba saber pro' ,
             xlab = 'Puntaje obtenido',
             ylab = 'Frecuencia',
             col = 'indianred')
        
      # Pruebas de normalidad
        
        # Gráfico de normalidad
          qqnorm(Pruebas_Estado$PUNT_INGLES_SP_2019, xlab = "", ylab = "",
                 main = "Puntaje Inglés Saber Pro - 2019", col = "indianred")
          qqline(Pruebas_Estado$PUNT_INGLES_SP_2019)
        
        
        
        # Anderson Darling y Lilliefors
          # Ho: Normalidad en la población.
          # Ha: No existe normalidad en la población.
            Ander_SP <- ad.test(Pruebas_Estado$PUNT_INGLES_SP_2019)
            Lillie_SP <- lillie.test(Pruebas_Estado$PUNT_INGLES_SP_2019)
        
        # Validación por p-value vs alpha
            Ander_SP$p.value < Alpha
            Lillie_SP$p.value < Alpha
        # Con un nivel de significancia del 5% se puede afirmar que los resultados de la prueba no siguen una distribución normal.  
            
  
  # Debido a que son pruebas emparejadas no es posible realizar una prueba F para la varianza, ya que parte del supuesto de ser muestras independientes.
          
            
    # Previo a realizar la prueba se normalizaran las dos pruebas, dado que no siguen una distribución normal.
          
            Normalizar <- function(x){
              return((x-min(x))/(max(x)-min(x)))
            }
            
            Pruebas_Estado <- cbind(Pruebas_Estado, Norm_Saber_11_Ing = Normalizar(Pruebas_Estado$PUNT_INGLES_S11_2015), Norm_Saber_Pro_Ing = Normalizar(Pruebas_Estado$PUNT_INGLES_SP_2019))
            
            
            # Hipotesis
            # Ho: Ud = 0
            # Ha: Ud > 0
            
            # Criterio de rechazo
                  # Alpha <- 0.05
            
            # Prueba t emparejados
            T_Paired_Test <- t.test(
              x = Pruebas_Estado$Norm_Saber_Pro_Ing,
              y = Pruebas_Estado$Norm_Saber_11_Ing,
              alternative = "greater",
              var.equal = TRUE,
              paired = TRUE
            )
            T_Paired_Test$p.value
            T_Paired_Test$p.value < Alpha
            
            T_Paired_Test
      # Basado en la prueba de hipotesis se determina que si existe diferencia positiva entre el examen de inglés del saber pro y el saber 11.
            
            
### Comparativo de varianzas entre pruebas saber 11 y saber pro 2019.
            Saber_11_2019 <- read.xlsx('C:/Users/gabrielamabec/Personal_Projects/Data_Science_Basics/Pruebas Estado/BD/BD Pruebas de Estado.xlsx',sheet = 'SB11_2019')
            Saber_Pro_2019 <- read.xlsx('C:/Users/gabrielamabec/Personal_Projects/Data_Science_Basics/Pruebas Estado/BD/BD Pruebas de Estado.xlsx',sheet = 'SaberPro_Genéricas_2019')

        # Histograma Puntaje inglés prueba saber 11 2019
            S11_2019_Summary_Ing = summary(Saber_11_2019$PUNT_INGLES)
            S11_2019_Summary_Ing
            
            hist(x = Saber_11_2019$PUNT_INGLES,
                 main = 'Histograma puntaje de inglés prueba saber 11 del 2019' ,
                 xlab = 'Puntaje obtenido',
                 ylab = 'Frecuencia',
                 col = 'royalblue1')
            
            # Prueba de normalidad 

            # Gráfico de normalidad
            qqnorm(Saber_11_2019$PUNT_INGLES, xlab = "", ylab = "",
                   main = "Puntaje Inglés Saber 11 - 2019", col = "royalblue1")
            qqline(Saber_11_2019$PUNT_INGLES)
            
            # Anderson Darling y Lilliefors
            # Ho: Normalidad en la población.
            # Ha: No existe normalidad en la población.
            
            Ander_S11_2019 <- ad.test(Saber_11_2019$PUNT_INGLES)
            Lillie_S11_2019 <- lillie.test(Saber_11_2019$PUNT_INGLES)
            
            # Validación por p-value vs alpha
            
            Ander_S11_2019$p.value < Alpha
            Lillie_S11_2019$p.value < Alpha
                        
            
        # Histograma Puntaje inglés prueba saber pro 2019
            SP_2019_Summary_Ing = summary(Saber_Pro_2019$MOD_INGLES_PUNT)
            SP_2019_Summary_Ing
            
            hist(x = Saber_Pro_2019$MOD_INGLES_PUNT,
                 main = 'Histograma puntaje de inglés prueba saber 11 del 2019' ,
                 xlab = 'Puntaje obtenido',
                 ylab = 'Frecuencia',
                 col = 'royalblue4')
            
            # Prueba de normalidad 
            
            # Gráfico de normalidad
            qqnorm(Saber_Pro_2019$MOD_INGLES_PUNT, xlab = "", ylab = "",
                   main = "Puntaje Inglés Saber Pro - 2019", col = "royalblue4")
            qqline(Saber_Pro_2019$MOD_INGLES_PUNT)
            
            # Anderson Darling y Lilliefors
            # Ho: Normalidad en la población.
            # Ha: No existe normalidad en la población.
            
            Ander_SP_2019 <- ad.test(Saber_Pro_2019$MOD_INGLES_PUNT)
            Lillie_SP_2019 <- lillie.test(Saber_Pro_2019$MOD_INGLES_PUNT)
            
            # Validación por p-value vs alpha
            
            Ander_SP_2019$p.value < Alpha
            Lillie_SP_2019$p.value < Alpha
            
            
          # Normalización
            Saber_11_2019 <- cbind(Saber_11_2019, Norm_Saber_11_2019 = Normalizar(Saber_11_2019$PUNT_INGLES))
            Saber_Pro_2019 <- cbind(Saber_Pro_2019, Norm_Saber_Pro_2019 = Normalizar(Saber_Pro_2019$MOD_INGLES_PUNT))
            
          # Debido a que la prueba saber 11 tiene más resultados que la prueba saber pro se realiza un aleatorio para seleccionar la muestra.
              # Para ello se seleccionará la columna datos, estos fueron elegidos bajo una distribución normal.

            Saber_11_2019_Modified <- Saber_11_2019[Saber_11_2019$Selección == 1, ]
            
            # Prueba F con las variables normalizadas
            var.test(
              x= Saber_Pro_2019$Norm_Saber_Pro_2019,
              y= Saber_11_2019_Modified$Norm_Saber_11_2019,
              conf.level = 1- Alpha
            )
            