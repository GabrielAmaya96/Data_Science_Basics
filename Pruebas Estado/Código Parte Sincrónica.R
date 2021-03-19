# Cargar la librería para leer archivos excel.
library(readxl)

# Lectura del archivo de excel
Egyptian_skulls_2 <- read_excel("Personal/M. Analítica/Métodos Estadísticos/Egyptian_skulls 2.xlsx")
View(Egyptian_skulls_2)



# 1. Validación de si existe diferencia significativa entre la variación de la altura nasal para el periodo predinástico antiguo y el periodo romano.

    
    # Generar los subsets de datos filtrando por los periodos a evaluar.
    Early_predinastic <- Egyptian_skulls_2[Egyptian_skulls_2$Period == 'Early_predynastic', ]
    Roman <- Egyptian_skulls_2[Egyptian_skulls_2$Period == 'Roman_period', ]
    
    
    # Nivel de significancia para la prueba.
    Alpha <- 0.1
    
    
    # Validación de supuestos normalidad.
        # Normalidad para el periodo Predinástico Antiguo
          hist(x = Early_predinastic$Nasal_height, main = 'Histograma de Early_predinastic for Nasal Height')
          qqnorm(Early_predinastic$Nasal_height, xlab = "", ylab = "",
                 main = "Early_predinastic", col = "firebrick")
          qqline(Early_predinastic$Nasal_height)
          
          
          # Recordar que en las pruebas H0 siempre es normal
          library('nortest')
          Lillie_Predinastic <- lillie.test(Early_predinastic$Nasal_height)
          Shapiro_Predinastic <- shapiro.test(Early_predinastic$Nasal_height)
    
          
          # Si p-value es menor que alfa se rechaza H0
          Shapiro_Predinastic$p.value < Alpha
          Lillie_Predinastic$p.value < Alpha
          
          # Observar los resultados completos de la prueba.
          Shapiro_Predinastic
          Lillie_Predinastic
    
          
          
        # Normalidad en el periodo Romano
          hist(x = Roman$Nasal_height, main = 'Histograma de Roman for Nasal Height')
          qqnorm(Roman$Nasal_height, xlab = "", ylab = "",
                 main = "Roman", col = "royalblue4")
          qqline(Roman$Nasal_height)
          
          # Recordar que en las pruebas H0 siempre es normal
          Lillie_Romano <- lillie.test(Roman$Nasal_height)
          Shapiro_Romano <- shapiro.test(Roman$Nasal_height)
          
          
          # Si p-value es menor que alfa se rechaza H0
          Shapiro_Romano$p.value < Alpha
          Lillie_Romano$p.value < Alpha         
    
          # Observar los resultados completos de la prueba.
          Shapiro_Romano
          Lillie_Romano    
    
    
    # Dado que se quiere validar un cociente de varianzas se usa la Prueba F
          F_Exp <- var.test( x =Early_predinastic$Nasal_height, 
                            y = Roman$Nasal_height,
                            alternative = 'two.sided',
                            conf.level = 1- Alpha)
          
          # Obtener todos los resultados de la prueba F.
          F_Exp
          
          # Si p-value es menor que alfa se rechaza H0
          F_Exp$p.value < Alpha
          
   
  
# 2. La longitud basialveolar del periodo predinástico tardio es mayor que la del periodo Ptolemaico
     
          
      #Generar los subsets de datos filtrando por los periodos a evaluar.

      Late_predynastic <- Egyptian_skulls_2[Egyptian_skulls_2$Period == 'Late_predynastic', ]
      Ptolemaic <- Egyptian_skulls_2[Egyptian_skulls_2$Period == 'Ptolemaic_period', ]
      
      
      # Nivel de significancia para la prueba.
      Alpha <- 0.07
      
      
      # Validación de supuestos normalidad.
        # Normalidad en periodo predinástico tardío
        hist(x = Late_predynastic$Basialveolar_length, main = 'Histograma de Late_predynastic for Basialveolar length')
        qqnorm(Late_predynastic$Basialveolar_length, xlab = "", ylab = "",
               main = "Late_predynastic Basialveolar_length", col = "firebrick")
        qqline(Late_predynastic$Basialveolar_length)
        
        # Recordar que en las pruebas H0 siempre es normal
        Lillie_Late <- lillie.test(Late_predynastic$Basialveolar_length)
        Shapiro_Late <- shapiro.test(Late_predynastic$Basialveolar_length)
        
        
        # Si p-value es menor que alfa se rechaza H0
        Shapiro_Late$p.value < Alpha
        Lillie_Late$p.value < Alpha
        
        # Observar los resultados completos de la prueba.
        Shapiro_Late
        Lillie_Late  
        
        
        # Normalidad Ptolemaico
        hist(x = Ptolemaic$Basialveolar_length, main = 'Histograma de Ptolemaic for Basialveolar length')
        qqnorm(Ptolemaic$Basialveolar_length, xlab = "", ylab = "",
               main = "Ptolemaic Basialveolar_length", col = "royalblue4")
        qqline(Ptolemaic$Basialveolar_length)
        
        # Recordar que en las pruebas H0 siempre es normal
        Lillie_Ptolemaic <- lillie.test(Ptolemaic$Basialveolar_length)
        Shapiro_Ptolemaic <- shapiro.test(Ptolemaic$Basialveolar_length)
        
        # Si p-value es menor que alfa se rechaza H0
        Shapiro_Ptolemaic$p.value < Alpha
        Lillie_Ptolemaic$p.value < Alpha
        
        
        # Observar los resultados completos de la prueba.
        Lillie_Ptolemaic
        Shapiro_Ptolemaic
      
        # Validación de las varianzas
          F_Late_Ptolemaic <- var.test( x =Late_predynastic$Basialveolar_length, 
                             y = Ptolemaic$Basialveolar_length,
                             alternative = 'two.sided',
                             conf.level = 1- Alpha)
          # Observar los resultados completos de la prueba.
          F_Late_Ptolemaic
          
          # Si p-value es menor que alfa se rechaza H0
          F_Late_Ptolemaic$p.value < Alpha      
      
    # Realizar la prueba de diferencia de medias con varianzas iguales
      T_Late_Ptolemaic <- t.test(
        x = Late_predynastic$Basialveolar_length,
        y = Ptolemaic$Basialveolar_length,
        alternative = "greater",
        var.equal = TRUE,
        conf.level = 1- Alpha,
        paired = FALSE,
      )
      
      # Si p-value es menor que alfa se rechaza H0
      T_Late_Ptolemaic$p.value < Alpha
      
      # Observar los resultados completos de la prueba.
      T_Late_Ptolemaic
      
      
      
# Construir un intervalo de confianza para la dinastía 12 y 13 utilizando un nivel de confianza del 85%

  
  # Nivel de significancia y confianza para la prueba.
  conf_level = 0.85
  Alpha <- 1 - conf_level
  
  
  #Generar los subsets de datos filtrando por los periodos a evaluar.
  Dinastia_12_13 <- Egyptian_skulls_2[Egyptian_skulls_2$Period == '12th_and_13th_Dynasty', ]    
      
  # Validación de normalidad.
    # Nomralidad Dinastría 12 y 13
    hist(x = Dinastia_12_13$Basibregmatic_height, main = 'Histograma de Dinastia 12 y 13 for Basibregmatic height')
    qqnorm(Dinastia_12_13$Basibregmatic_height, xlab = "", ylab = "",
           main = "Dinastia 12 y 13  Basibregmatic height", col = "firebrick")
    qqline(Dinastia_12_13$Basibregmatic_height)
    
    # Recordar que en las pruebas H0 siempre es normal
    Lillie_Dinastia <- lillie.test(Dinastia_12_13$Basibregmatic_height)
    Shapiro_Dinastia <- shapiro.test(Dinastia_12_13$Basibregmatic_height)
    
    #Si p-value es menor que alfa se rechaza H0
    Shapiro_Dinastia$p.value < Alpha
    Lillie_Dinastia$p.value < Alpha   
    
    # Observar los resultados completos de la prueba.
    Shapiro_Dinastia
    Lillie_Dinastia
  
  # Crear el intervalo de confianza para una sola varianza
  
    # Librería para construir el IC de una sola varianza
    library(EnvStats)
    
    # Prueba para generar el intervalo de confianza.
      Var_Test <- varTest(x = Dinastia_12_13$Basibregmatic_height, conf.level = conf_level)
      
    # Intervalo de confianza de la varianza.
      Var_Test$conf.int
      
    # Observar los resultados completos de la prueba.
      Var_Test   

# Ignorando todos los periodos se desea validar si la amplitud máxima es menor a 110 mm.
    
    # Nivel de significancia y confianza para la prueba.      
    Alpha <- 0.12

    
  
    # Validación de normalidad.
      # Normalidad de la amplitud máxima.
      hist(x = Egyptian_skulls_2$Maximum_breadth, main = 'Histograma de Egyptian_skulls_2 for Maximum breadth')
      qqnorm(Egyptian_skulls_2$Maximum_breadth, xlab = "", ylab = "",
             main = "Egyptian_skulls_2  Maximum breadth", col = "firebrick")
      qqline(Egyptian_skulls_2$Maximum_breadth)
      
      # Recordar que H0 siempre es normal
      Lillie_EMB <- lillie.test(Egyptian_skulls_2$Maximum_breadth)
      Shapiro_EMB <- shapiro.test(Egyptian_skulls_2$Maximum_breadth)
      
      # Si p-value es menor que alfa se rechaza H0
      Shapiro_EMB$p.value < Alpha
      Lillie_EMB$p.value < Alpha   
      
      # Observar los resultados completos de la prueba.
      Shapiro_EMB
      Lillie_EMB 
    
      
  # Realizar la prueba para una sola media dónde la alternativa es menor a 110 mm.
      Mu = 110
      Test <- t.test(Egyptian_skulls_2$Maximum_breadth,
                     mu = Mu, 
                     alternative = "less",
                     conf.level = 1 - Alpha)
      Test
      Test$p.value < Alpha 
    
  