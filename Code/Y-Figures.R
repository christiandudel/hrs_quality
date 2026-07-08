### Packages ###################################################################

  library(tidyverse)
  library(writexl)  
  library(readxl)


### Load main results ##########################################################

  load("Results/main.rda")


### AMEs: Gender ###############################################################

  AMEmen <- c(ame_physical_m[ame_physical_m$factor=="statebothworking/unhealthy","AME"],
              ame_stress_m[ame_stress_m$factor=="statebothworking/unhealthy","AME"],
              ame_poverty_m[ame_poverty_m$factor=="statebothworking/unhealthy","AME"])
  
  AMEmenlow <- c(ame_physical_m[ame_physical_m$factor=="statebothworking/unhealthy","lower"],
                 ame_stress_m[ame_stress_m$factor=="statebothworking/unhealthy","lower"],
                 ame_poverty_m[ame_poverty_m$factor=="statebothworking/unhealthy","lower"])
  
  AMEmenup <- c(ame_physical_m[ame_physical_m$factor=="statebothworking/unhealthy","upper"],
                 ame_stress_m[ame_stress_m$factor=="statebothworking/unhealthy","upper"],
                 ame_poverty_m[ame_poverty_m$factor=="statebothworking/unhealthy","upper"])
  
  
  AMEwomen <- c(ame_physical_f[ame_physical_f$factor=="statebothworking/unhealthy","AME"],
              ame_stress_f[ame_stress_f$factor=="statebothworking/unhealthy","AME"],
              ame_poverty_f[ame_poverty_f$factor=="statebothworking/unhealthy","AME"])
  
  AMEwomenlow <- c(ame_physical_f[ame_physical_f$factor=="statebothworking/unhealthy","lower"],
                 ame_stress_f[ame_stress_f$factor=="statebothworking/unhealthy","lower"],
                 ame_poverty_f[ame_poverty_f$factor=="statebothworking/unhealthy","lower"])
  
  AMEwomenup <- c(ame_physical_f[ame_physical_f$factor=="statebothworking/unhealthy","upper"],
                ame_stress_f[ame_stress_f$factor=="statebothworking/unhealthy","upper"],
                ame_poverty_f[ame_poverty_f$factor=="statebothworking/unhealthy","upper"])
  
  labels <- c("Physical","Stress","Poverty")
  fake <- rep(100,3)
  names(fake) <- labels
  pdf(file="Output/mainresults.pdf",width=7,height=7)
  dotchart(x=rev(fake),xlab="Average marginal effect of unhealthy work",xlim=c(-0.05,0.1),pch=15,ylim=c(0.5,3.5))
  abline(v=0,col="grey70")
  abline(h=1.5)
  abline(h=2.5)
  segments(x0=rev(AMEmenlow),x1=rev(AMEmenup),y0=(1:3)-0.2,y1=(1:3)-0.2)
  segments(x0=rev(AMEwomenlow),x1=rev(AMEwomenup),y0=(1:3)+0.2,y1=(1:3)+0.2)
  points(y=(1:3)-0.2,x=rev(AMEmen),pch=15,col="#1b9e77",cex=1.25)
  points(y=(1:3)+0.2,x=rev(AMEwomen),pch=17,col="#d95f02",cex=1.25)
  legend(x=0.06,y=1,pch=c(17,15),legend=c("Women","Men"),col=c("#d95f02","#1b9e77"),cex=1.25)
  dev.off()
  
### AMEs: gender & race ########################################################
  
  AMEmen_w <- c(ame_physical_m_w[ame_physical_m_w$factor=="statebothworking/unhealthy","AME"],
                ame_stress_m_w[ame_stress_m_w$factor=="statebothworking/unhealthy","AME"],
                ame_poverty_m_w[ame_poverty_m_w$factor=="statebothworking/unhealthy","AME"])
  
  AMEmenlow_w <- c(ame_physical_m_w[ame_physical_m_w$factor=="statebothworking/unhealthy","lower"],
                   ame_stress_m_w[ame_stress_m_w$factor=="statebothworking/unhealthy","lower"],
                   ame_poverty_m_w[ame_poverty_m_w$factor=="statebothworking/unhealthy","lower"])
  
  AMEmenup_w <- c(ame_physical_m_w[ame_physical_m_w$factor=="statebothworking/unhealthy","upper"],
                  ame_stress_m_w[ame_stress_m_w$factor=="statebothworking/unhealthy","upper"],
                  ame_poverty_m_w[ame_poverty_m_w$factor=="statebothworking/unhealthy","upper"])
  
  
  AMEwomen_w <- c(ame_physical_f_w[ame_physical_f_w$factor=="statebothworking/unhealthy","AME"],
                  ame_stress_f_w[ame_stress_f_w$factor=="statebothworking/unhealthy","AME"],
                  ame_poverty_f_w[ame_poverty_f_w$factor=="statebothworking/unhealthy","AME"])
  
  AMEwomenlow_w <- c(ame_physical_f_w[ame_physical_f_w$factor=="statebothworking/unhealthy","lower"],
                     ame_stress_f_w[ame_stress_f_w$factor=="statebothworking/unhealthy","lower"],
                     ame_poverty_f_w[ame_poverty_f_w$factor=="statebothworking/unhealthy","lower"])
  
  AMEwomenup_w <- c(ame_physical_f_w[ame_physical_f_w$factor=="statebothworking/unhealthy","upper"],
                    ame_stress_f_w[ame_stress_f_w$factor=="statebothworking/unhealthy","upper"],
                    ame_poverty_f_w[ame_poverty_f_w$factor=="statebothworking/unhealthy","upper"])
  
  AMEmen_b <- c(ame_physical_m_b[ame_physical_m_b$factor=="statebothworking/unhealthy","AME"],
                ame_stress_m_b[ame_stress_m_b$factor=="statebothworking/unhealthy","AME"],
                ame_poverty_m_b[ame_poverty_m_b$factor=="statebothworking/unhealthy","AME"])
  
  AMEmenlow_b <- c(ame_physical_m_b[ame_physical_m_b$factor=="statebothworking/unhealthy","lower"],
                   ame_stress_m_b[ame_stress_m_b$factor=="statebothworking/unhealthy","lower"],
                   ame_poverty_m_b[ame_poverty_m_b$factor=="statebothworking/unhealthy","lower"])
  
  AMEmenup_b <- c(ame_physical_m_b[ame_physical_m_b$factor=="statebothworking/unhealthy","upper"],
                  ame_stress_m_b[ame_stress_m_b$factor=="statebothworking/unhealthy","upper"],
                  ame_poverty_m_b[ame_poverty_m_b$factor=="statebothworking/unhealthy","upper"])
  
  
  AMEwomen_b <- c(ame_physical_f_b[ame_physical_f_b$factor=="statebothworking/unhealthy","AME"],
                  ame_stress_f_b[ame_stress_f_b$factor=="statebothworking/unhealthy","AME"],
                  ame_poverty_f_b[ame_poverty_f_b$factor=="statebothworking/unhealthy","AME"])
  
  AMEwomenlow_b <- c(ame_physical_f_b[ame_physical_f_b$factor=="statebothworking/unhealthy","lower"],
                     ame_stress_f_b[ame_stress_f_b$factor=="statebothworking/unhealthy","lower"],
                     ame_poverty_f_b[ame_poverty_f_b$factor=="statebothworking/unhealthy","lower"])
  
  AMEwomenup_b <- c(ame_physical_f_b[ame_physical_f_b$factor=="statebothworking/unhealthy","upper"],
                    ame_stress_f_b[ame_stress_f_b$factor=="statebothworking/unhealthy","upper"],
                    ame_poverty_f_b[ame_poverty_f_b$factor=="statebothworking/unhealthy","upper"])
  
  AMEmen_h <- c(ame_physical_m_h[ame_physical_m_h$factor=="statebothworking/unhealthy","AME"],
                ame_stress_m_h[ame_stress_m_h$factor=="statebothworking/unhealthy","AME"],
                ame_poverty_m_h[ame_poverty_m_h$factor=="statebothworking/unhealthy","AME"])
  
  AMEmenlow_h <- c(ame_physical_m_h[ame_physical_m_h$factor=="statebothworking/unhealthy","lower"],
                   ame_stress_m_h[ame_stress_m_h$factor=="statebothworking/unhealthy","lower"],
                   ame_poverty_m_h[ame_poverty_m_h$factor=="statebothworking/unhealthy","lower"])
  
  AMEmenup_h <- c(ame_physical_m_h[ame_physical_m_h$factor=="statebothworking/unhealthy","upper"],
                  ame_stress_m_h[ame_stress_m_h$factor=="statebothworking/unhealthy","upper"],
                  ame_poverty_m_h[ame_poverty_m_h$factor=="statebothworking/unhealthy","upper"])
  
  
  AMEwomen_h <- c(ame_physical_f_h[ame_physical_f_h$factor=="statebothworking/unhealthy","AME"],
                  ame_stress_f_h[ame_stress_f_h$factor=="statebothworking/unhealthy","AME"],
                  ame_poverty_f_h[ame_poverty_f_h$factor=="statebothworking/unhealthy","AME"])
  
  AMEwomenlow_h <- c(ame_physical_f_h[ame_physical_f_h$factor=="statebothworking/unhealthy","lower"],
                     ame_stress_f_h[ame_stress_f_h$factor=="statebothworking/unhealthy","lower"],
                     ame_poverty_f_h[ame_poverty_f_h$factor=="statebothworking/unhealthy","lower"])
  
  AMEwomenup_h <- c(ame_physical_f_h[ame_physical_f_h$factor=="statebothworking/unhealthy","upper"],
                    ame_stress_f_h[ame_stress_f_h$factor=="statebothworking/unhealthy","upper"],
                    ame_poverty_f_h[ame_poverty_f_h$factor=="statebothworking/unhealthy","upper"])
  
  pdf(file="Output/mainresults_white.pdf",width=7,height=7)
  labels <- c("Physical","Stress","Poverty")
  fake <- rep(100,3)
  names(fake) <- labels
  dotchart(x=rev(fake),xlab="Average marginal effect of unhealthy work",xlim=c(-0.075,0.15),pch=15,ylim=c(0.5,3.5),main="White")
  abline(v=0,col="grey70")
  abline(h=1.5)
  abline(h=2.5)
  segments(x0=rev(AMEmenlow_w),x1=rev(AMEmenup_w),y0=(1:3)-0.2,y1=(1:3)-0.2)
  segments(x0=rev(AMEwomenlow_w),x1=rev(AMEwomenup_w),y0=(1:3)+0.2,y1=(1:3)+0.2)
  points(y=(1:3)-0.2,x=rev(AMEmen_w),pch=15,col="#1b9e77",cex=1.25)
  points(y=(1:3)+0.2,x=rev(AMEwomen_w),pch=17,col="#d95f02",cex=1.25)
  legend(x=0.1,y=2,pch=c(17,15),legend=c("Women","Men"),col=c("#d95f02","#1b9e77"),cex=1.25)
  dev.off()
  
  pdf(file="Output/mainresults_black.pdf",width=7,height=7)
  dotchart(x=rev(fake),xlab="Average marginal effect of unhealthy work",xlim=c(-0.075,0.15),pch=15,ylim=c(0.5,3.5),main="Black")
  abline(v=0,col="grey70")
  abline(h=1.5)
  abline(h=2.5)
  segments(x0=rev(AMEmenlow_b),x1=rev(AMEmenup_b),y0=(1:3)-0.2,y1=(1:3)-0.2)
  segments(x0=rev(AMEwomenlow_b),x1=rev(AMEwomenup_b),y0=(1:3)+0.2,y1=(1:3)+0.2)
  points(y=(1:3)-0.2,x=rev(AMEmen_b),pch=15,col="#1b9e77",cex=1.25)
  points(y=(1:3)+0.2,x=rev(AMEwomen_b),pch=17,col="#d95f02",cex=1.25)
  legend(x=0.1,y=2,pch=c(17,15),legend=c("Women","Men"),col=c("#d95f02","#1b9e77"),cex=1.25)
  dev.off()
  
  pdf(file="Output/mainresults_hispanic.pdf",width=7,height=7)
  dotchart(x=rev(fake),xlab="Average marginal effect of unhealthy work",xlim=c(-0.075,0.15),pch=15,ylim=c(0.5,3.5),main="Hispanic")
  abline(v=0,col="grey70")
  abline(h=1.5)
  abline(h=2.5)
  segments(x0=rev(AMEmenlow_h),x1=rev(AMEmenup_h),y0=(1:3)-0.2,y1=(1:3)-0.2)
  segments(x0=rev(AMEwomenlow_h),x1=rev(AMEwomenup_h),y0=(1:3)+0.2,y1=(1:3)+0.2)
  points(y=(1:3)-0.2,x=rev(AMEmen_h),pch=15,col="#1b9e77",cex=1.25)
  points(y=(1:3)+0.2,x=rev(AMEwomen_h),pch=17,col="#d95f02",cex=1.25)
  legend(x=0.1,y=2,pch=c(17,15),legend=c("Women","Men"),col=c("#d95f02","#1b9e77"),cex=1.25)
  dev.off()

  
### Robustness checks ##########################################################
  
  load("Results/no_covid.rda")
  load("Results/healthdef.rda")
  load("Results/pooled.rda")
  
  AME <- c(ame_physical[ame_physical$factor=="statebothworking/unhealthy","AME"],
              ame_stress[ame_stress$factor=="statebothworking/unhealthy","AME"],
              ame_poverty[ame_poverty$factor=="statebothworking/unhealthy","AME"])
  
  AMElow <- c(ame_physical[ame_physical$factor=="statebothworking/unhealthy","lower"],
                 ame_stress[ame_stress$factor=="statebothworking/unhealthy","lower"],
                 ame_poverty[ame_poverty$factor=="statebothworking/unhealthy","lower"])
  
  AMEup <- c(ame_physical[ame_physical$factor=="statebothworking/unhealthy","upper"],
                ame_stress[ame_stress$factor=="statebothworking/unhealthy","upper"],
                ame_poverty[ame_poverty$factor=="statebothworking/unhealthy","upper"])
  
  AME_nocov <- c(ame_covid_physical[ame_covid_physical$factor=="statebothworking/unhealthy","AME"],
           ame_covid_stress[ame_covid_stress$factor=="statebothworking/unhealthy","AME"],
           ame_covid_poverty[ame_covid_poverty$factor=="statebothworking/unhealthy","AME"])
  
  AMElow_nocov <- c(ame_covid_physical[ame_covid_physical$factor=="statebothworking/unhealthy","lower"],
              ame_covid_stress[ame_covid_stress$factor=="statebothworking/unhealthy","lower"],
              ame_covid_poverty[ame_covid_poverty$factor=="statebothworking/unhealthy","lower"])
  
  AMEup_nocov <- c(ame_covid_physical[ame_covid_physical$factor=="statebothworking/unhealthy","upper"],
             ame_covid_stress[ame_covid_stress$factor=="statebothworking/unhealthy","upper"],
             ame_covid_poverty[ame_covid_poverty$factor=="statebothworking/unhealthy","upper"])
  
  AME_depyr <- c(ame_depyr_physical[ame_depyr_physical$factor=="workdepyrworking/unhealthy","AME"],
           ame_depyr_stress[ame_depyr_stress$factor=="workdepyrworking/unhealthy","AME"],
           ame_depyr_poverty[ame_depyr_poverty$factor=="workdepyrworking/unhealthy","AME"])
  
  AMElow_depyr <- c(ame_depyr_physical[ame_depyr_physical$factor=="workdepyrworking/unhealthy","lower"],
              ame_depyr_stress[ame_depyr_stress$factor=="workdepyrworking/unhealthy","lower"],
              ame_depyr_poverty[ame_depyr_poverty$factor=="workdepyrworking/unhealthy","lower"])
  
  AMEup_depyr <- c(ame_depyr_physical[ame_depyr_physical$factor=="workdepyrworking/unhealthy","upper"],
             ame_depyr_stress[ame_depyr_stress$factor=="workdepyrworking/unhealthy","upper"],
             ame_depyr_poverty[ame_depyr_poverty$factor=="workdepyrworking/unhealthy","upper"])
  
  
  
  AME_cogd <- c(ame_cogd_physical[ame_cogd_physical$factor=="workcogdworking/unhealthy","AME"],
                 ame_cogd_stress[ame_cogd_stress$factor=="workcogdworking/unhealthy","AME"],
                 ame_cogd_poverty[ame_cogd_poverty$factor=="workcogdworking/unhealthy","AME"])
  
  AMElow_cogd <- c(ame_cogd_physical[ame_cogd_physical$factor=="workcogdworking/unhealthy","lower"],
                    ame_cogd_stress[ame_cogd_stress$factor=="workcogdworking/unhealthy","lower"],
                    ame_cogd_poverty[ame_cogd_poverty$factor=="workcogdworking/unhealthy","lower"])
  
  AMEup_cogd <- c(ame_cogd_physical[ame_cogd_physical$factor=="workcogdworking/unhealthy","upper"],
                   ame_cogd_stress[ame_cogd_stress$factor=="workcogdworking/unhealthy","upper"],
                   ame_cogd_poverty[ame_cogd_poverty$factor=="workcogdworking/unhealthy","upper"])
  
  AME_shlt <- c(ame_shlt_physical[ame_shlt_physical$factor=="workshltworking/unhealthy","AME"],
                ame_shlt_stress[ame_shlt_stress$factor=="workshltworking/unhealthy","AME"],
                ame_shlt_poverty[ame_shlt_poverty$factor=="workshltworking/unhealthy","AME"])
  
  AMElow_shlt <- c(ame_shlt_physical[ame_shlt_physical$factor=="workshltworking/unhealthy","lower"],
                   ame_shlt_stress[ame_shlt_stress$factor=="workshltworking/unhealthy","lower"],
                   ame_shlt_poverty[ame_shlt_poverty$factor=="workshltworking/unhealthy","lower"])
  
  AMEup_shlt <- c(ame_shlt_physical[ame_shlt_physical$factor=="workshltworking/unhealthy","upper"],
                  ame_shlt_stress[ame_shlt_stress$factor=="workshltworking/unhealthy","upper"],
                  ame_shlt_poverty[ame_shlt_poverty$factor=="workshltworking/unhealthy","upper"])
  
  AME_pool <- c(ame_pool_physical[ame_pool_physical$factor=="statebothworking/unhealthy","AME"],
                ame_pool_stress[ame_pool_stress$factor=="statebothworking/unhealthy","AME"],
                ame_pool_poverty[ame_pool_poverty$factor=="statebothworking/unhealthy","AME"])
  
  AMElow_pool <- c(ame_pool_physical[ame_pool_physical$factor=="statebothworking/unhealthy","lower"],
                   ame_pool_stress[ame_pool_stress$factor=="statebothworking/unhealthy","lower"],
                   ame_pool_poverty[ame_pool_poverty$factor=="statebothworking/unhealthy","lower"])
  
  AMEup_pool <- c(ame_pool_physical[ame_pool_physical$factor=="statebothworking/unhealthy","upper"],
                  ame_pool_stress[ame_pool_stress$factor=="statebothworking/unhealthy","upper"],
                  ame_pool_poverty[ame_pool_poverty$factor=="statebothworking/unhealthy","upper"])
  
  
  color <- c("#7fc97f","#beaed4","#fdc086","#ffff99","#386cb0")
  color <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#ffd92f")
  plotchar <- c(15:18,4,2)
  
  pdf(file="Output/robustness.pdf",width=7,height=7)
  labels <- c("Physical","Stress","Poverty")
  fake <- rep(100,3)
  names(fake) <- labels
  dotchart(x=rev(fake),xlab="Average marginal effect of unhealthy work",xlim=c(-0.075,0.1),pch=15,ylim=c(1,3))
  abline(v=0,col="grey70")
  abline(h=2.5)
  abline(h=1.5)
  
  segments(x0=rev(AMElow),x1=rev(AMEup),y0=(1:3)+0.25,y1=(1:3)+0.25)
  points(y=(1:3)+0.25,x=rev(AME),pch=plotchar[1],col=color[1],cex=1.25)
  
  segments(x0=rev(AMElow_nocov),x1=rev(AMEup_nocov),y0=(1:3)+0.15,y1=(1:3)+0.15)
  points(y=(1:3)+0.15,x=rev(AME_nocov),pch=plotchar[2],col=color[2],cex=1.25)
  
  segments(x0=rev(AMElow_depyr),x1=rev(AMEup_depyr),y0=(1:3)+0.05,y1=(1:3)+0.05)
  points(y=(1:3)+0.05,x=rev(AME_depyr),pch=plotchar[3],col=color[3],cex=1.25)
  
  segments(x0=rev(AMElow_cogd),x1=rev(AMEup_cogd),y0=(1:3)-0.05,y1=(1:3)-0.05)
  points(y=(1:3)-0.05,x=rev(AME_cogd),pch=plotchar[3],col=color[4],cex=1.25)
  
  segments(x0=rev(AMElow_shlt),x1=rev(AMEup_shlt),y0=(1:3)-0.15,y1=(1:3)-0.15)
  points(y=(1:3)-0.15,x=rev(AME_shlt),pch=plotchar[3],col=color[5],cex=1.25)
  
  segments(x0=rev(AMElow_pool),x1=rev(AMEup_pool),y0=(1:3)-0.25,y1=(1:3)-0.25)
  points(y=(1:3)-0.25,x=rev(AME_pool),pch=plotchar[4],col=color[6],cex=1.25)
  
  legend(x=0.04,y=1.4,pch=plotchar[c(1,2,3,3,3,4)],legend=c("Main analysis","No COVID years","Depression","Cogn. impairment","Self-rated health","Pooled"),col=color,cex=1)
  dev.off()
  