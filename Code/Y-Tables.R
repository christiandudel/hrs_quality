### Packages ###################################################################

  library(writexl)  
  library(tidyverse)


### Descriptive ################################################################

  load(file="Results/descriptive_big.rda")
  
  write_xlsx(descriptive,path="Results/descriptive.xlsx")
  
  
### Main regression results ####################################################  
  
  # Load
  load("Results/main.rda")

  # Object for results
  results <- expand.grid(gender=c("Total","Male","Female"),
                         race=c("Total","White","Black","Hispanic"),
                         outcome=c("Physical","Stress","Poverty"))
  results <- results |> filter( !(gender=="Total" & race!="Total"))
  
  # Variables to keep from summary object
  vars <- c("AME","SE","z","p","lower","upper")
  
  # Paste together all estimates 
  estimates <- rbind(
    ame_physical[ame_physical$factor=="statebothworking/unhealthy",vars],  
    ame_physical_m[ame_physical_m$factor=="statebothworking/unhealthy",vars],
    ame_physical_f[ame_physical_f$factor=="statebothworking/unhealthy",vars],
    ame_physical_m_w[ame_physical_m_w$factor=="statebothworking/unhealthy",vars],
    ame_physical_f_w[ame_physical_f_w$factor=="statebothworking/unhealthy",vars],
    ame_physical_m_b[ame_physical_m_b$factor=="statebothworking/unhealthy",vars],
    ame_physical_f_b[ame_physical_f_b$factor=="statebothworking/unhealthy",vars],
    ame_physical_m_h[ame_physical_m_h$factor=="statebothworking/unhealthy",vars],
    ame_physical_f_h[ame_physical_f_h$factor=="statebothworking/unhealthy",vars],
    ame_stress[ame_stress$factor=="statebothworking/unhealthy",vars],  
    ame_stress_m[ame_stress_m$factor=="statebothworking/unhealthy",vars],
    ame_stress_f[ame_stress_f$factor=="statebothworking/unhealthy",vars],
    ame_stress_m_w[ame_stress_m_w$factor=="statebothworking/unhealthy",vars],
    ame_stress_f_w[ame_stress_f_w$factor=="statebothworking/unhealthy",vars],
    ame_stress_m_b[ame_stress_m_b$factor=="statebothworking/unhealthy",vars],
    ame_stress_f_b[ame_stress_f_b$factor=="statebothworking/unhealthy",vars],
    ame_stress_m_h[ame_stress_m_h$factor=="statebothworking/unhealthy",vars],
    ame_stress_f_h[ame_stress_f_h$factor=="statebothworking/unhealthy",vars],
    ame_poverty[ame_poverty$factor=="statebothworking/unhealthy",vars],  
    ame_poverty_m[ame_poverty_m$factor=="statebothworking/unhealthy",vars],
    ame_poverty_f[ame_poverty_f$factor=="statebothworking/unhealthy",vars],
    ame_poverty_m_w[ame_poverty_m_w$factor=="statebothworking/unhealthy",vars],
    ame_poverty_f_w[ame_poverty_f_w$factor=="statebothworking/unhealthy",vars],
    ame_poverty_m_b[ame_poverty_m_b$factor=="statebothworking/unhealthy",vars],
    ame_poverty_f_b[ame_poverty_f_b$factor=="statebothworking/unhealthy",vars],
    ame_poverty_m_h[ame_poverty_m_h$factor=="statebothworking/unhealthy",vars],
    ame_poverty_f_h[ame_poverty_f_h$factor=="statebothworking/unhealthy",vars])
  
  # Combine 
  results <- cbind(results,estimates)
  
  # Save
  write_xlsx(results,path="Results/mainresults.xlsx")
  
  