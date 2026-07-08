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
  write_xlsx(results,path="Output/mainresults.xlsx")
  
  
### Robustness check poverty ################################################### 
  
  # Load
  load("Results/poverty.rda")
  
  # Object for results
  results <- expand.grid(gender=c("Total"),
                         race=c("Total"),
                         outcome=c("Poverty (main)","75%","125%","150%","200%"))

  # Variables to keep from summary object
  vars <- c("AME","SE","z","p","lower","upper")
  
  estimates <- rbind(
    ame_p100[ame_p100$factor=="statebothworking/unhealthy",vars],  
    ame_p75[ame_p75$factor=="statebothworking/unhealthy",vars],
    ame_p125[ame_p125$factor=="statebothworking/unhealthy",vars],
    ame_p150[ame_p150$factor=="statebothworking/unhealthy",vars],
    ame_p200[ame_p200$factor=="statebothworking/unhealthy",vars])
  
  # Combine 
  results <- cbind(results,estimates)
  
  # Save
  write_xlsx(results,path="Output/povertyresults.xlsx")
  
  
### Other robustness checks ####################################################
  
  # Load
  load("Results/no_covid.rda")
  load("Results/healthdef.rda")
  load("Results/pooled.rda")
  
  # Object for results
  results <- expand.grid(genderrace=c("Total"),
                         variant=c("Main","No COVID","Depression","Cogn. impairment","Self-rated health","Pooled"),
                         outcome=c("Physical","Stress","Poverty"))
  
  # Combine estimates
  estimates <- rbind(
    ame_physical[ame_physical$factor=="statebothworking/unhealthy",vars],  
    ame_covid_physical[ame_covid_physical$factor=="statebothworking/unhealthy",vars],
    ame_depyr_physical[ame_depyr_physical$factor=="workdepyrworking/unhealthy",vars],
    ame_cogd_physical[ame_cogd_physical$factor=="workcogdworking/unhealthy",vars],
    ame_shlt_physical[ame_shlt_physical$factor=="workshltworking/unhealthy",vars],
    ame_pool_physical[ame_pool_physical$factor=="statebothworking/unhealthy",vars],
    ame_stress[ame_stress$factor=="statebothworking/unhealthy",vars],  
    ame_covid_stress[ame_covid_stress$factor=="statebothworking/unhealthy",vars],
    ame_depyr_stress[ame_depyr_stress$factor=="workdepyrworking/unhealthy",vars],
    ame_cogd_stress[ame_cogd_stress$factor=="workcogdworking/unhealthy",vars],
    ame_shlt_stress[ame_shlt_stress$factor=="workshltworking/unhealthy",vars],
    ame_pool_stress[ame_pool_stress$factor=="statebothworking/unhealthy",vars],
    ame_poverty[ame_poverty$factor=="statebothworking/unhealthy",vars],  
    ame_covid_poverty[ame_covid_poverty$factor=="statebothworking/unhealthy",vars],
    ame_depyr_poverty[ame_depyr_poverty$factor=="workdepyrworking/unhealthy",vars],
    ame_cogd_poverty[ame_cogd_poverty$factor=="workcogdworking/unhealthy",vars],
    ame_shlt_poverty[ame_shlt_poverty$factor=="workshltworking/unhealthy",vars],
    ame_pool_poverty[ame_pool_poverty$factor=="statebothworking/unhealthy",vars])
  
  # Combine 
  results <- cbind(results,estimates)
  
  # Save
  write_xlsx(results,path="Output/robustness.xlsx")
  