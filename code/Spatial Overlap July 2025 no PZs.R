
          library(sf)
          library(terra)
          library(viridis)
          library(CCAMLRGIS)

          coast=load_Coastline() 
          
          #LOAD CONSUMPTION RASTERS
          cpf<-terra::rast( ".../cpf_summerkgkrillperday_2024espg6932.tif")
          pel<-terra::rast( ".../pelagics_summerkgkrillperday_2024espg6932.tif")
           
          ##### now winter
          wintcpf1<-terra::rast( ".../cpf winter 2024epsg6932_cropped.tif")
          #use the old one for aug sept 
          wintpel1<-terra::rast(".../pelagics winter 2024epsg6932_cropped.tif")

          
          julwint10<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_julywhales10.tif")
          
          apwint25<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_aprilwhales25.tif")
          maywint25<-terra::rast(".../pelagics winter 2024epsg6932_cropped_maywhales25.tif")
          junwint25<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_junewhales25.tif")
          julwint25<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_julywhales25.tif")
          
          
          apwint50<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_aprilwhales50.tif")
          maywint50<-terra::rast(".../pelagics winter 2024epsg6932_cropped_maywhales50.tif")
          junwint50<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_junewhales50.tif")
          julwint50<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_julywhales50.tif")
          
          apwint100<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_aprilwhales100.tif")
          maywint100<-terra::rast(".../pelagics winter 2024epsg6932_cropped_maywhales100.tif")
          junwint100<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_junewhales100.tif")
          julwint100<-terra::rast( ".../pelagics winter 2024epsg6932_cropped_julywhales100.tif")
         
          #read in the krill rasters
          krillmask1<-terra::rast(".../krill 2024epsg6932 tonneskm2.tif")
          wintkrill<-terra::rast( ".../krill wint 2024epsg6932 tonneskm2.tif")

          
          #juvenile krill - these are proportions
          early<-terra::rast( ".../juv krill early 2024epsg6932 tonneskm2.tif")
          late<-terra::rast( ".../juv krill late 2024epsg6932 tonneskm2.tif")

          #desirability
          summean<-terra::rast(".../summer mean prop2013_2018epsg.tif")
          wintmean<-terra::rast(".../winter mean prop2013_2018epsg.tif")
          
          #this now needs to be monthly 
          jan18<-terra::rast(".../jan mean prop2013_2018.tif")
          feb18<-terra::rast(".../feb mean prop2013_2018.tif")
          mar18<-terra::rast(".../mar mean prop2013_2018.tif")
          apr18<-terra::rast(".../apr mean prop2013_2018.tif")
          may18<-terra::rast(".../may mean prop2013_2018.tif")
          jun18<-terra::rast(".../jun mean prop2013_2018.tif")
          jul18<-terra::rast(".../jul mean prop2013_2018.tif")
          dec18<-terra::rast(".../dec mean prop2013_2018.tif")
          
          #no fishing aug-nov so we dont have rasters for this.... 
          #in the months that dont have fishing then set the proportion equal across areas
          
          
          ##################################################################
        
          ####  divide the krill layer by 6 as its for the whole summer, whereas we are now running the SOA on a monthly timestep. 
          #THIS is to make sure the results are consistent with previous iterations when the SOA was run in just two seasons, not monthly. 
          #predation pressure and therefore risk changes considerably if the entire summer biomass is taken each month
          
          krillmask1<-krillmask1/6
          wintkrill<-wintkrill/6
          
          
          
          
          #change nam depending on which year fishing data is used for desirabiliity
          scenario<-"split"
          
          nam<-paste("2013_2018_s",scenario,"winterwhales_noPZs",sep="")
          
          
          if(scenario==25){
            apwint<-apwint25
            maywint<-maywint25
            junwint<-junwint25
            julwint<-julwint25}
          if(scenario==50){
            apwint<-apwint50
            maywint<-maywint50
            junwint<-junwint50
            julwint<-julwint50}
          if(scenario==100){
            apwint<-apwint100
            maywint<-maywint100
            junwint<-junwint100
            julwint<-julwint100}
          
          if(scenario=="split"){
            apwint<-apwint100
            maywint<-maywint50
            junwint<-junwint25
            julwint<-julwint10}
          name<-paste0(nam,"July2025")
          
          #read in managament units
          shapefile<-st_read( ".../Proposed_Krill_MUs_new2024_scenario2.shp")
          
          months<-c(1:12)
          i<-4
          
          #use the same shapefile for each month
          for(i in months){
            month<-i
            print(i)
            shape=shapefile[shapefile$id%in%c("DP2","PB2")==F,]
            plot(shape)[1]
            #RECALCULATE AREA as this was not on all original shapefiles
            shape$areakm2<-st_area(shape)/1000000
            
            #now we need to extract the values of consumption within each ssmu boundary
            if(month==1|month==2|month==3|month==10|month==11|month==12){  #summer
              shape$cpf<-extract(cpf,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(pel,shape,fun=sum,na.rm=T)[,2]#pelkgday
            }
            
            if(month==4){  #winter
              shape$cpf<-extract(wintcpf1,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(apwint,shape,fun=sum,na.rm=T)[,2]#pelkgday

            }
            if(month==5){  #winter
              shape$cpf<-extract(wintcpf1,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(maywint,shape,fun=sum,na.rm=T)[,2]#pelkgday

            }
            if(month==6){  #winter
              shape$cpf<-extract(wintcpf1,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(junwint,shape,fun=sum,na.rm=T)[,2]#pelkgday

            }
            if(month==7){  #winter
              shape$cpf<-extract(wintcpf1,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(julwint,shape,fun=sum,na.rm=T)[,2]#pelkgday

            }
            if(month==8|month==9){  #winter
              shape$cpf<-extract(wintcpf1,shape,fun=sum,na.rm=T)[,2]#cpfkgday
              shape$pel<-extract(wintpel1,shape,fun=sum,na.rm=T)[,2]#pelkgday
            }
            
            
            # multiply by number of days each month then divide to go from kg to tonnes
            if (month==1|month==3|month==5|month==7|month==8|month==10|month==12){
              shape$den_cpf<-shape$cpf/shape$areakm2*31/1000
              shape$den_pel<-shape$pel/shape$areakm2*31/1000
            }
            
            if (month==2){
              shape$den_cpf<-shape$cpf/shape$areakm2*28/1000
              shape$den_pel<-shape$pel/shape$areakm2*28/1000
            }
            
            if (month==4|month==6|month==9|month==11){
              shape$den_cpf<-shape$cpf/shape$areakm2*30/1000
              shape$den_pel<-shape$pel/shape$areakm2*30/1000
            }
            
            #the krill is already a density tonnes per km2 so take the average not the sum
            if(month==1|month==2|month==3|month==10|month==11|month==12){  #summer
              shape$krill<-extract(krillmask1,shape,fun=mean,na.rm=T)[,2] #
            }
            
            if(month==4|month==5|month==6|month==7|month==8|month==9){  #winter
              shape$krill<-extract(wintkrill,shape,fun=mean,na.rm=T)[,2] #
            }
            #then calculate predation pressure  
            shape$pp_cpf<-shape$den_cpf/shape$krill
            shape$pp_pel<-shape$den_pel/shape$krill
            
            
            #calculate the scaling factor for the predation pressure
            ###########################################
            # these values came from Constable et al. 2016
            h<-3
            v<-3
            x50<-0.5
            x0<-0
            x1<-4
            y0<-0
            y1<-1
            #so first calculate l.x0 
            p1<- -h*(x0-x50)
            p2<-1+exp(1)^p1
            p3<-p2^(1/v)
            l.x0<-1/p3
            l.x0
            
            #now l.x1
            p1<- -h*(x1-x50)
            p2<-1+exp(1)^p1
            p3<-p2^(1/v)
            l.x1<-1/p3
            l.x1
            
            #shape cpf
            p1_cpf<- -h*(as.numeric(shape$pp_cpf)-x50)
            p2_cpf<- 1+exp(1)^p1_cpf
            p3_cpf<-p2_cpf^(1/v)
            l.x_cpf<-1/p3_cpf
            shape$sclppcpf_s<-y0+((l.x_cpf-l.x0)/(l.x1-l.x0))
            
            # now pelagics
            p1_pel<- -h*(as.numeric(shape$pp_pel)-x50)
            p2_pel<- 1+exp(1)^p1_pel
            p3_pel<-p2_pel^(1/v)
            l.x_pel<-1/p3_pel
            shape$sclpppel_s<-y0+((l.x_pel-l.x0)/(l.x1-l.x0))
            
            #now extract values for juvenile krill
            if(month==1|month==2|month==3|month==10|month==11|month==12){  #summer
              shape$juvkrl<-extract(late,shape,fun=mean,na.rm=T)[,2]
            }
            
            if(month==4|month==5|month==6|month==7|month==8|month==9){  #winter
              shape$juvkrl<-extract(early,shape,fun=mean,na.rm=T)[,2]
            }
            
            #calculate risk
            shape$risk<- 1-((1-shape$juvkrl)*(1-shape$sclppcpf_s)*(1-shape$sclpppel_s))
            
            
            #use monthly desirability
            if(month==1){  
              shape$fishery<-extract(jan18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==2){  
              shape$fishery<-extract(feb18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==3){  
              shape$fishery<-extract(mar18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==4){  
              shape$fishery<-extract(apr18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==5){  
              shape$fishery<-extract(may18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==6){  
              shape$fishery<-extract(jun18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==7){  
              shape$fishery<-extract(jul18,shape,fun=sum,na.rm=T)[,2]
            }
            if(month==8){  
              shape$fishery<-c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
              
            }
            if(month==9){  
              shape$fishery<-c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
              
            }
            if(month==10){  
              shape$fishery<-c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
              
            }
            if(month==11){  
              shape$fishery<-c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
            }
            if(month==12){  
              shape$fishery<-extract(dec18,shape,fun=sum,na.rm=T)[,2]
            }
            
            
            #now scale the fishery using parameters from Constable et al 2016
            h<-60
            v<-1
            x50<-0.08
            x0<-0
            x1<-1
            y0<-0
            y1<-1
            
            #so first calculate l.x0 
            p1<- -h*(x0-x50)
            p2<-1+exp(1)^p1
            p3<-p2^(1/v)
            l.x0<-1/p3
            l.x0
            
            #calculate x1
            p1<- -h*(x1-x50)
            p2<-1+exp(1)^p1
            p3<-p2^(1/v)
            l.x1<-1/p3
            l.x1
            
            p1<- -h*(shape$fishery-x50)
            p2<-1+exp(1)^p1
            p3<-p2^(1/v)
            l.x<-1/p3
            f<-y0+((l.x-l.x0)/(l.x1-l.x0))
            shape$sclfsh<-f
            assign(paste0("month",i),shape)
            
          } 
          
          
          #do the actual Risk Assessment
          #first do baseline
          ca<-1#this is desirability
          zsum<-1/12 ## z is the proportion of catch taken each time period. i did different scenarios, detailed in the paper, but this one has equal values each month
         
          #first calculate bottom line
          #calculate (1-rap)*cap*Zap*Kap*Aap for each of shape and winter
          m1<-sum((1-month1$risk)*ca*zsum*month1$krill*month1$areakm2)
          m2<-sum((1-month2$risk)*ca*zsum*month2$krill*month2$areakm2)
          m3<-sum((1-month3$risk)*ca*zsum*month3$krill*month3$areakm2)
          m4<-sum((1-month4$risk)*ca*zsum*month4$krill*month4$areakm2)
          m5<-sum((1-month5$risk)*ca*zsum*month5$krill*month5$areakm2)
          m6<-sum((1-month6$risk)*ca*zsum*month6$krill*month6$areakm2)
          m7<-sum((1-month7$risk)*ca*zsum*month7$krill*month7$areakm2)
          m8<-sum((1-month8$risk)*ca*zsum*month8$krill*month8$areakm2)
          m9<-sum((1-month9$risk)*ca*zsum*month9$krill*month9$areakm2)
          m10<-sum((1-month10$risk)*ca*zsum*month10$krill*month10$areakm2)
          m11<-sum((1-month11$risk)*ca*zsum*month11$krill*month11$areakm2)
          m12<-sum((1-month12$risk)*ca*zsum*month12$krill*month12$areakm2)
          
          
          bot<-m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
          #now calculate alpha by month
          month1$alph<- ((1-month1$risk)*ca*zsum*month1$krill*month1$areakm2)/bot
          month2$alph<- ((1-month2$risk)*ca*zsum*month2$krill*month2$areakm2)/bot
          month3$alph<- ((1-month3$risk)*ca*zsum*month3$krill*month3$areakm2)/bot
          month4$alph<- ((1-month4$risk)*ca*zsum*month4$krill*month4$areakm2)/bot
          month5$alph<- ((1-month5$risk)*ca*zsum*month5$krill*month5$areakm2)/bot
          month6$alph<- ((1-month6$risk)*ca*zsum*month6$krill*month6$areakm2)/bot
          month7$alph<- ((1-month7$risk)*ca*zsum*month7$krill*month7$areakm2)/bot
          month8$alph<- ((1-month8$risk)*ca*zsum*month8$krill*month8$areakm2)/bot
          month9$alph<- ((1-month9$risk)*ca*zsum*month9$krill*month9$areakm2)/bot
          month10$alph<- ((1-month10$risk)*ca*zsum*month10$krill*month10$areakm2)/bot
          month11$alph<- ((1-month11$risk)*ca*zsum*month11$krill*month11$areakm2)/bot
          month12$alph<- ((1-month12$risk)*ca*zsum*month12$krill*month12$areakm2)/bot
          
          
          
          sum(month1$alph)+sum(month2$alph)+sum(month3$alph)+sum(month4$alph)+sum(month5$alph)+sum(month6$alph)+sum(month7$alph)+
            sum(month8$alph)+sum(month9$alph)+sum(month10$alph)+sum(month11$alph)+sum(month12$alph)
          # alpha should sum to 1 across all months
          
          #now calculate regional risk per month
          month1$regrsk<- sum(month1$risk*month1$alph)
          month2$regrsk<- sum(month2$risk*month2$alph)
          month3$regrsk<- sum(month3$risk*month3$alph)
          month4$regrsk<- sum(month4$risk*month4$alph)
          month5$regrsk<- sum(month5$risk*month5$alph)
          month6$regrsk<- sum(month6$risk*month6$alph)
          month7$regrsk<- sum(month7$risk*month7$alph)
          month8$regrsk<- sum(month8$risk*month8$alph)
          month9$regrsk<- sum(month9$risk*month9$alph)
          month10$regrsk<- sum(month10$risk*month10$alph)
          month11$regrsk<- sum(month11$risk*month11$alph)
          month12$regrsk<- sum(month12$risk*month12$alph)
          
          
          baseline_regrsk<-sum(month1$regrsk[1],month2$regrsk[1],month3$regrsk[1],month4$regrsk[1],month5$regrsk[1],month6$regrsk[1],
                               month7$regrsk[1],month8$regrsk[1],month9$regrsk[1],month10$regrsk[1],month11$regrsk[1],month12$regrsk[1])
          baseline_regrsk
          
          #now include fisheries desirability
          # ca (desirability) is now the scaled fishing value which changes monthly
          
          #first calculate bottom line
          m1<-sum((1-month1$risk)*month1$sclfsh*zsum*month1$krill*month1$areakm2)
          m2<-sum((1-month2$risk)*month2$sclfsh*zsum*month2$krill*month2$areakm2)
          m3<-sum((1-month3$risk)*month3$sclfsh*zsum*month3$krill*month3$areakm2)
          m4<-sum((1-month4$risk)*month4$sclfsh*zsum*month4$krill*month4$areakm2)
          m5<-sum((1-month5$risk)*month5$sclfsh*zsum*month5$krill*month5$areakm2)
          m6<-sum((1-month6$risk)*month6$sclfsh*zsum*month6$krill*month6$areakm2)
          m7<-sum((1-month7$risk)*month7$sclfsh*zsum*month7$krill*month7$areakm2)
          m8<-sum((1-month8$risk)*month8$sclfsh*zsum*month8$krill*month8$areakm2)
          m9<-sum((1-month9$risk)*month9$sclfsh*zsum*month9$krill*month9$areakm2)
          m10<-sum((1-month10$risk)*month10$sclfsh*zsum*month10$krill*month10$areakm2)
          m11<-sum((1-month11$risk)*month11$sclfsh*zsum*month11$krill*month11$areakm2)
          m12<-sum((1-month12$risk)*month12$sclfsh*zsum*month12$krill*month12$areakm2)
          
          
          bot<-m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
          
          
          #now calculate alpha by month 
          month1$alph_des<- ((1-month1$risk)*month1$sclfsh*zsum*month1$krill*month1$areakm2)/bot
          month2$alph_des<- ((1-month2$risk)*month2$sclfsh*zsum*month2$krill*month2$areakm2)/bot
          month3$alph_des<- ((1-month3$risk)*month3$sclfsh*zsum*month3$krill*month3$areakm2)/bot
          month4$alph_des<- ((1-month4$risk)*month4$sclfsh*zsum*month4$krill*month4$areakm2)/bot
          month5$alph_des<- ((1-month5$risk)*month5$sclfsh*zsum*month5$krill*month5$areakm2)/bot
          month6$alph_des<- ((1-month6$risk)*month6$sclfsh*zsum*month6$krill*month6$areakm2)/bot
          month7$alph_des<- ((1-month7$risk)*month7$sclfsh*zsum*month7$krill*month7$areakm2)/bot
          month8$alph_des<- ((1-month8$risk)*month8$sclfsh*zsum*month8$krill*month8$areakm2)/bot
          month9$alph_des<- ((1-month9$risk)*month9$sclfsh*zsum*month9$krill*month9$areakm2)/bot
          month10$alph_des<- ((1-month10$risk)*month10$sclfsh*zsum*month10$krill*month10$areakm2)/bot
          month11$alph_des<- ((1-month11$risk)*month11$sclfsh*zsum*month11$krill*month11$areakm2)/bot
          month12$alph_des<- ((1-month12$risk)*month12$sclfsh*zsum*month12$krill*month12$areakm2)/bot
          
          sum(month1$alph_des)+sum(month2$alph_des)+sum(month3$alph_des)+sum(month4$alph_des)+sum(month5$alph_des)+sum(month6$alph_des)+sum(month7$alph_des)+
            sum(month8$alph_des)+sum(month9$alph_des)+sum(month10$alph_des)+sum(month11$alph_des)+sum(month12$alph_des)
          # alpha should sum to 1 across all months
          
          
          month1$regrsk_des<- sum(month1$risk*month1$alph_des)
          month2$regrsk_des<- sum(month2$risk*month2$alph_des)
          month3$regrsk_des<- sum(month3$risk*month3$alph_des)
          month4$regrsk_des<- sum(month4$risk*month4$alph_des)
          month5$regrsk_des<- sum(month5$risk*month5$alph_des)
          month6$regrsk_des<- sum(month6$risk*month6$alph_des)
          month7$regrsk_des<- sum(month7$risk*month7$alph_des)
          month8$regrsk_des<- sum(month8$risk*month8$alph_des)
          month9$regrsk_des<- sum(month9$risk*month9$alph_des)
          month10$regrsk_des<- sum(month10$risk*month10$alph_des)
          month11$regrsk_des<- sum(month11$risk*month11$alph_des)
          month12$regrsk_des<- sum(month12$risk*month12$alph_des)
          
          
          desir_regrsk<-sum(month1$regrsk_des[1],month2$regrsk_des[1],month3$regrsk_des[1],month4$regrsk_des[1],month5$regrsk_des[1],month6$regrsk_des[1],
                            month7$regrsk_des[1],month8$regrsk_des[1],month9$regrsk_des[1],month10$regrsk_des[1],month11$regrsk_des[1],month12$regrsk_des[1])
          desir_regrsk
          
          
          month1$alph
          id<-month1$id
          id#
          #baseline alphas
          jan<-round(month1$alph,3)
          feb<-round(month2$alph,3)
          march<-round(month3$alph,3)
          april<-round(month4$alph,3)
          may<-round(month5$alph,3)
          june<-round(month6$alph,3)
          july<-round(month7$alph,3)
          aug<-round(month8$alph,3)
          sept<-round(month9$alph,3)
          oct<-round(month10$alph,3)
          nov<-round(month11$alph,3)
          dec<-round(month12$alph,3)
          
          #desirability alphas
          jan_des<-round(month1$alph_des,3)
          feb_des<-round(month2$alph_des,3)
          march_des<-round(month3$alph_des,3)
          april_des<-round(month4$alph_des,3)
          may_des<-round(month5$alph_des,3)
          june_des<-round(month6$alph_des,3)
          july_des<-round(month7$alph_des,3)
          aug_des<-round(month8$alph_des,3)
          sept_des<-round(month9$alph_des,3)
          oct_des<-round(month10$alph_des,3)
          nov_des<-round(month11$alph_des,3)
          dec_des<-round(month12$alph_des,3)
          
          all<-data.frame(id,jan,feb,march,april,may,june,july,aug,sept,oct,nov,dec,
                          jan_des,feb_des,march_des,april_des,may_des,june_des,july_des,aug_des,sept_des,oct_des,nov_des,dec_des)
          all
          write.csv(all, paste0(".../MONTHLY ALPHAS",name,"_july25.csv"))
          
          
          summm<-jan+feb+march+oct+nov+dec
          summm
          wintt<-april+may+june+july+aug+sept
          wintt
          id
          season<-data.frame(id,summm,wintt)
          season
          
          #now desirability
          
          summm_des<-jan_des+feb_des+march_des+oct_des+nov_des+dec_des
          summm_des
          wintt_des<-april_des+may_des+june_des+july_des+aug_des+sept_des
          wintt_des
          id
          
          season<-data.frame(id,summm,wintt,summm_des,wintt_des)
          season
          write.csv(season, paste0(".../seasonal MONTHLY ALPHAS",name,".csv"))
          
          
          
          # i need to rasterise before plotting 
          #make a raster covering the study area for plotting
          
          r<-cpf
          values(r)<-1
          r<-aggregate(r,10)
          r
          
          #so i need a shapefile with the sum of alphas
          MU=st_read(".../EMM_24_Candidate_Krill_MUs_V2.shp",quiet = T)
          plot(MU)
          MU=MU[MU$id%in%c("DP2","PB2")==F,]
          trial<-MU
          MU$id
          trial$sumofalphaSUMMER<-summm
          trial$sumofalphaWINTER<-wintt
          trial$sumofalphaSUMMER_DES<-summm_des
          trial$sumofalphaWINTER_DES<-wintt_des
          
  
          
          sum1 <- rasterize(trial, r, 'sumofalphaSUMMER',fun="min")
          wint1 <- rasterize(trial, r, 'sumofalphaWINTER',fun="min")
          sum2 <- rasterize(trial, r, 'sumofalphaSUMMER_DES',fun="min")
          wint2 <- rasterize(trial, r, 'sumofalphaWINTER_DES',fun="min")
          
          
          
          bb=st_bbox(shape) #Get bounding box (x/y limits) + buffer
          bx=st_as_sfc(bb) #Build spatial box to plot
          
          #Use spatial box to crop coastline and ASDs
          coast=suppressWarnings(st_intersection(coast,bx))
          
          #now plot the seasonal ones 
          
          png(paste0(".../",name,"monthly added to make season.png"),units="cm", width=16, height=21, res=100)
          par(mfrow=c(2,2))
          par(mar=c(2,2,3,2))
          
          #LOOK AT MAX VALUE ON SEASON DF
         
          mm<-max(values(wint1),na.rm=T)+0.01
          breaks=seq(0,mm,0.01)
          num<-length(breaks)+1
          #cols<-inlmisc::GetTolColors(num, scheme="smooth rainbow")
          cols<-viridis(num)
          
          plot(sum1,legend=F, col=cols, breaks=seq(0,mm,0.01),main="Summer",xlim=c(-3000000,-2200000),ylim=c(900000,2200000) ) 
          plot(sum1,legend.only=T,legend.width = 0.5,lty=0.5,smallplot=c(0.84,0.86, 0.1,0.85), col=cols,breaks=seq(0,mm,0.01) ,
               axis.args=list(at=seq(0,mm,0.05),labels=seq(0,mm,0.05),cex.axis=0.85))
          plot(MU, add=T,col="NA")
          plot(coast,add=T,col="grey")
          
          plot(wint1,legend=F, col=cols, breaks=seq(0,mm,0.01),main="Winter",xlim=c(-3000000,-2200000),ylim=c(900000,2200000) ) 
          plot(wint1,legend.only=T,legend.width = 0.5,lty=0.5,smallplot=c(0.84,0.86, 0.1,0.85), col=cols,breaks=seq(0,mm,0.01) ,
               axis.args=list(at=seq(0,mm,0.05),labels=seq(0,mm,0.05),cex.axis=0.85))
          plot(MU, add=T,col="NA")
          plot(coast,add=T,col="grey")
          
          mm<-max(values(wint2),na.rm=T)+0.01
          breaks=seq(0,mm,0.01)
          num<-length(breaks)+1
          cols<-viridis(num)
          
          plot(sum2,legend=F, col=cols, breaks=seq(0,mm,0.01),main="Summer",xlim=c(-3000000,-2200000),ylim=c(900000,2200000) ) 
          plot(sum2,legend.only=T,legend.width = 0.5,lty=0.5,smallplot=c(0.84,0.86, 0.1,0.85), col=cols,breaks=seq(0,mm,0.01) ,
               axis.args=list(at=seq(0,mm,0.05),labels=seq(0,mm,0.05),cex.axis=0.85))
          plot(MU, add=T,col="NA")
          plot(coast,add=T,col="grey")
          
          plot(wint2,legend=F, col=cols, breaks=seq(0,mm,0.01),main="Winter",xlim=c(-3000000,-2200000),ylim=c(900000,2200000) ) 
          plot(wint2,legend.only=T,legend.width = 0.5,lty=0.5,smallplot=c(0.84,0.86, 0.1,0.85), col=cols,breaks=seq(0,mm,0.01) ,
               axis.args=list(at=seq(0,mm,0.05),labels=seq(0,mm,0.05),cex.axis=0.85))
          plot(MU, add=T,col="NA")
          plot(coast,add=T,col="grey")
          mtext(paste0("Baseline ",round(baseline_regrsk,2)),side =3,line=-2, outer=T)
          mtext(paste0("Desirability ",round(desir_regrsk,2)),side =3,line=-28, outer=T)
          
          dev.off()
          
          