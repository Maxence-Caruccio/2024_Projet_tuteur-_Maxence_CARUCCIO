# UN Comtrade data import

# Install pacman package if not installed
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
}

# Install and load used packages
pacman::p_load(reticulate, rstudioapi, dplyr)

# Indicate anaconda environment
use_condaenv("C:/Users/asus/Anaconda3/python.exe")# More info on how to find anaconda environment path here:
# https://docs.anaconda.com/free/working-with-conda/configurations/python-path/

# Set up working directory
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)

# Run python script for data download
for (file in list.files(path = dir, pattern = ".py")){
  source_python(file)
}

# Get API key
apikey <- readLines(list.files(path = dir, pattern = ".txt"))

# Set up parameters
years <- 2000:2022
cmd_code <- paste(sort(c("4403")),
                  sep = "",
                  collapse = ",")
flow_code <- paste(sort(c("M", "X")),
                   sep = "",
                   collapse = ",")

# Store data into a data.frame
annual_trade_list <- list()
for (i in seq_along(years)) {
  annual_trade_list[[i]] <- get_UN_Comtrade_data(apikey,
                                                 year = as.character(years[i]),
                                                 cmd = cmd_code,
                                                 flow = flow_code)
}
trade_data <- do.call(rbind, annual_trade_list)

# Check if data have been correctly downloaded
print(paste0("Years covered: ",
             paste(unique(trade_data$period), sep = "", collapse = ", ")))
print(paste0("Commodities considered: ",
             paste(unique(trade_data$cmdCode), sep = "", collapse = ", ")))
print(paste0("Flows considered: ",
             paste(unique(trade_data$flowDesc), sep = "", collapse = ", ")))

# Export data as a csv file
write.csv(trade_data,
          "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/raw_data/uncomtrade_data.csv",
          row.names = FALSE)

# To load back the csv file
data <- read.csv(list.files(path = "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/raw_data",
                            pattern = ".csv",
                            full.names = TRUE))
all(dim(data) == dim(trade_data)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

UE27=c("Austria","Croatia","Czechia","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Luxembourg","Malta","Netherlands","Portugal","Romania","Spain","Sweden","Slovenia","Poland","Lithuania","Germany","Denmark","Cyprus","Bulgaria","Belgium","Slovakia")
PAYSILL=c("Dem. Rep. of the Congo","Congo","Brazil","Colombia","Ecuador","Peru","Mexico","Nicaragua","Honduras","Russian Federation","Tajikistan","Bosnia Herzegovina","Serbia and Montenegro (...2005)","North Macedonia","Albania","Estonia","Bulgaria","Slovakia","Latvia","Cameroon","Gabon","Ghana","Nigeria","Equatorial Guinea","Benin","Mozambique","China","Indonesia","Cambodia","Lao People's Dem. Rep.","Malaysia","Papua New Guinea","Philippines","Thailand","Viet Nam","Myanmar","Rep. of Korea")


data[(data$flowCode == "X") &
       (data$reporterDesc %in% PAYSILL) &
       (data$partnerDesc %in% UE27),]

library('dplyr')



export = data[(data$flowCode == "X"),]
import = data[(data$flowCode == "M"),]
mirror_flow = full_join(export[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")], 
                        import[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")], 
                        by=c("cmdCode", "period", "reporterDesc"="partnerDesc", "partnerDesc"="reporterDesc"))

mirror_flow = merge(x= export[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                    y= import[c("cmdCode","period","reporterDesc","partnerDesc","primaryValue")],
                    by.x=c("cmdCode", "period", "reporterDesc", "partnerDesc"),
                    by.y=c("cmdCode", "period", "partnerDesc", "reporterDesc"),
                    all.x = TRUE, all.y = TRUE)


###########coef bas ###############
Donnees <-mirror_flow[(mirror_flow$reporterDesc %in% PAYSILL) &
                        (mirror_flow$partnerDesc %in% UE27),]

Donnee <- Donnees %>%
  mutate(coefficient_bas = ifelse(reporterDesc == "Brazil", 0.20,
                                  ifelse(reporterDesc == "Benin", 0.80,
                                         ifelse(reporterDesc == "Bosnia Herzegovina", 0.80,
                                                ifelse(reporterDesc == "Bulgaria", 0.40,
                                                       ifelse(reporterDesc == "Cameroon", 0.50,
                                                              ifelse(reporterDesc == "China", 0.32,
                                                                     ifelse(reporterDesc == "Colombia", 0.42,
                                                                            ifelse(reporterDesc == "Ecuador", 0.70,
                                                                                   ifelse(reporterDesc == "Estonia", 0.50,
                                                                                          ifelse(reporterDesc == "Gabon", 0.70,
                                                                                                 ifelse(reporterDesc == "Ghana", 0.60,
                                                                                                        ifelse(reporterDesc == "Honduras", 0.75,
                                                                                                               ifelse(reporterDesc == "Indonesia", 0.60,
                                                                                                                      ifelse(reporterDesc == "Latvia", 0.20,
                                                                                                                             ifelse(reporterDesc == "Mozambique", 0.50,
                                                                                                                                    ifelse(reporterDesc == "Lao People's Dem. Rep.", 0.45,
                                                                                                                                           ifelse(reporterDesc == "Malaysia", 0.35,
                                                                                                                                                  ifelse(reporterDesc == "Mexico", 0.70,
                                                                                                                                                         ifelse(reporterDesc == "Myanmar", 0.50,
                                                                                                                                                                ifelse(reporterDesc == "Nicaragua", 0.50,
                                                                                                                                                                       ifelse(reporterDesc == "Nigeria", 0.90,
                                                                                                                                                                              ifelse(reporterDesc == "North Macedonia", 0.25,
                                                                                                                                                                                     ifelse(reporterDesc == "Papua New Guinea", 0.70,
                                                                                                                                                                                            ifelse(reporterDesc == "Peru", 0.80,
                                                                                                                                                                                                   ifelse(reporterDesc == "Philippines", 0.46,
                                                                                                                                                                                                          ifelse(reporterDesc == "Rep. of Korea", 0.30,
                                                                                                                                                                                                                 ifelse(reporterDesc == "Russian Federation", 0.15,
                                                                                                                                                                                                                        ifelse(reporterDesc == "Albania", 0.81,
                                                                                                                                                                                                                               ifelse(reporterDesc == "Slovakia", 0.10,
                                                                                                                                                                                                                                      ifelse(reporterDesc == "Dem. Rep. of the Congo", 0.90,
                                                                                                                                                                                                                                             ifelse(reporterDesc == "Congo", 0.70,
                                                                                                                                                                                                                                                    ifelse(reporterDesc == "Equatorial Guinea", 0.50,
                                                                                                                                                                                                                                                           ifelse(reporterDesc == "Thailand", 0.40,
                                                                                                                                                                                                                                                                  ifelse(reporterDesc == "Viet Nam", 0.20,
                                                                                                                                                                                                                                                                         ifelse(reporterDesc == "Cambodia", 0.90,
                                                                                                                                                                                                                                                                                ifelse(reporterDesc == "Tajikistan", 0.20,
                                                                                                                                                                                                                                                                                       ifelse(reporterDesc == "Serbia and Montenegro (...2005)", 0.50,NA_real_))))))))))))))))))))))))))))))))))))))
#####vérif####
length(unique(Donnees$reporterDesc))
length(PAYSILL)         
which(is.na(Donnee$coefficient))

table(Donnee$reporterDesc)
table(PAYSILL)

############### coef haut ############

Donnee <- Donnee %>%
  mutate(coefficient_haut = ifelse(reporterDesc == "Brazil", 0.50,
                                   ifelse(reporterDesc == "Benin", 0.90,
                                          ifelse(reporterDesc == "Bosnia Herzegovina", 0.80,
                                                 ifelse(reporterDesc == "Bulgaria", 0.40,
                                                        ifelse(reporterDesc == "Cameroon", 0.65,
                                                               ifelse(reporterDesc == "China", 0.32,
                                                                      ifelse(reporterDesc == "Colombia", 0.42,
                                                                             ifelse(reporterDesc == "Ecuador", 0.70,
                                                                                    ifelse(reporterDesc == "Estonia", 0.50,
                                                                                           ifelse(reporterDesc == "Gabon", 0.70,
                                                                                                  ifelse(reporterDesc == "Ghana", 0.70,
                                                                                                         ifelse(reporterDesc == "Honduras", 0.85,
                                                                                                                ifelse(reporterDesc == "Indonesia", 0.80,
                                                                                                                       ifelse(reporterDesc == "Latvia", 0.20,
                                                                                                                              ifelse(reporterDesc == "Mozambique", 0.50,
                                                                                                                                     ifelse(reporterDesc == "Lao People's Dem. Rep.", 0.80,
                                                                                                                                            ifelse(reporterDesc == "Malaysia", 0.35,
                                                                                                                                                   ifelse(reporterDesc == "Mexico", 0.70,
                                                                                                                                                          ifelse(reporterDesc == "Myanmar", 0.50,
                                                                                                                                                                 ifelse(reporterDesc == "Nicaragua", 0.50,
                                                                                                                                                                        ifelse(reporterDesc == "Nigeria", 0.90,
                                                                                                                                                                               ifelse(reporterDesc == "North Macedonia", 0.30,
                                                                                                                                                                                      ifelse(reporterDesc == "Papua New Guinea", 0.70,
                                                                                                                                                                                             ifelse(reporterDesc == "Peru", 0.90,
                                                                                                                                                                                                    ifelse(reporterDesc == "Philippines", 0.46,
                                                                                                                                                                                                           ifelse(reporterDesc == "Rep. of Korea", 0.30,
                                                                                                                                                                                                                  ifelse(reporterDesc == "Russian Federation", 0.50,
                                                                                                                                                                                                                         ifelse(reporterDesc == "Albania", 0.81,
                                                                                                                                                                                                                                ifelse(reporterDesc == "Slovakia", 0.10,
                                                                                                                                                                                                                                       ifelse(reporterDesc == "Dem. Rep. of the Congo", 0.90,
                                                                                                                                                                                                                                              ifelse(reporterDesc == "Congo", 0.70,
                                                                                                                                                                                                                                                     ifelse(reporterDesc == "Equatorial Guinea", 0.50,
                                                                                                                                                                                                                                                            ifelse(reporterDesc == "Thailand", 0.40,
                                                                                                                                                                                                                                                                   ifelse(reporterDesc == "Viet Nam", 0.40,
                                                                                                                                                                                                                                                                          ifelse(reporterDesc == "Cambodia", 0.90,
                                                                                                                                                                                                                                                                                 ifelse(reporterDesc == "Tajikistan", 0.30,
                                                                                                                                                                                                                                                                                        ifelse(reporterDesc == "Serbia and Montenegro (...2005)", 0.50,NA_real_))))))))))))))))))))))))))))))))))))))
### création colonne des valeurs #########
Donnee$valeur <- ifelse(is.na(Donnee$primaryValue.y), Donnee$primaryValue.x, Donnee$primaryValue.y)
Donnee$Vraie_ValeurB <- Donnee$valeur*Donnee$coefficient_bas
Donnee$Vraie_ValeurH <- Donnee$valeur*Donnee$coefficient_haut

############# Calculs valeur total

Valeur_ILL_ImportUE_B <- sum(Donnee$Vraie_ValeurB)  #### 8,1 Milliards $
Valeur_ILL_ImportUE_H <- sum(Donnee$Vraie_ValeurH) #### 12,3 Milliards $

###### Calculs % par rapport aux imports totaux de l'UE
Imports_UE <-mirror_flow[(mirror_flow$partnerDesc %in% UE27),]
Imports_UE$valeur <- ifelse(is.na(Imports_UE$primaryValue.y), Imports_UE$primaryValue.x, Imports_UE$primaryValue.y)
Valeur_Total_ImportUE <-sum(Imports_UE$valeur)


Rapport_Bas <-Valeur_ILL_ImportUE_B / Valeur_Total_ImportUE  #### 4,25 %
Rapport_Haut <- Valeur_ILL_ImportUE_H / Valeur_Total_ImportUE  #### 6,48 %

######## Participation des pays de l'ue 

grouped_dataB <- Donnee %>%
  group_by(partnerDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))


pie(grouped_dataB$total_primaryValue,label=grouped_dataB$partnerDesc)

grouped_dataH <- Donnee %>%
  group_by(partnerDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

grouped_dataB$Pourcentage <- grouped_dataB$total_primaryValue / Valeur_ILL_ImportUE_B
grouped_dataH$Pourcentage <- grouped_dataH$total_primaryValue / Valeur_ILL_ImportUE_H

#### participation des pays à coeff illégal

grouped_dataBill <- Donnee %>%
  group_by(reporterDesc,period) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))

grouped_dataHill <- Donnee %>%
  group_by(reporterDesc,period) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

grouped_dataBill$Pourcentage <- grouped_dataBill$total_primaryValue / Valeur_ILL_ImportUE_B
grouped_dataHill$Pourcentage <- grouped_dataHill$total_primaryValue / Valeur_ILL_ImportUE_H

grouped_dataHillpourcentage <- grouped_dataHill %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(total_primaryValue, na.rm = TRUE))

grouped_dataBillpourcentage <- grouped_dataBill %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(total_primaryValue, na.rm = TRUE))

grouped_dataBillpourcentage$Pourcentage <- grouped_dataBillpourcentage$total_primaryValue / Valeur_ILL_ImportUE_B
grouped_dataHillpourcentage$Pourcentage <- grouped_dataHillpourcentage$total_primaryValue / Valeur_ILL_ImportUE_H



ggplot(Donnee, aes(x=period, y=Vraie_ValeurB)) +
  geom_line()

ggplot(Donnee, aes(x=period, y=Vraie_ValeurH)) +
  geom_line()
###inutile

###################### influence du prix du bois russe en 2007-2008

##création data 2000-2008 et 2008-2020

an2000_2008 <- c("2000","2001","2002","2003","2004","2005","2006","2007","2008")
an2009_2022 <- c("2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")

av2008 <-Donnee[(Donnee$reporterDesc %in% PAYSILL) &
                  (Donnee$partnerDesc %in% UE27) & (Donnee$period %in% an2000_2008),]

ap2008 <-Donnee[(Donnee$reporterDesc %in% PAYSILL) &
                  (Donnee$partnerDesc %in% UE27) & (Donnee$period %in% an2009_2022),]

grouped_dataBill_av2008 <- av2008 %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))

grouped_dataBill_ap2008 <- ap2008 %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))

grouped_dataHill_av2008 <- av2008 %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

grouped_dataHill_ap2008 <- ap2008 %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

totalBAv2008 <- sum(grouped_dataBill_av2008$total_primaryValue) # 4,6 Milliards
totalBAp2008 <- sum(grouped_dataBill_ap2008$total_primaryValue) # 3,50 Milliards

totalHAv2008 <- sum(grouped_dataHill_av2008$total_primaryValue) # 7,32 Milliards
totalHAp2008 <- sum(grouped_dataHill_ap2008$total_primaryValue) # 5,01 Milliards
totalHAp2008+totalHAv2008 ### VERIF ,total correspond

pourcentageB_av2008 <- (totalBAv2008/Valeur_ILL_ImportUE_B)*100 #56,8%
pourcentageB_ap2008 <- (totalBAp2008/Valeur_ILL_ImportUE_B)*100 #43,2%
pourcentageH_av2008 <- (totalHAv2008/Valeur_ILL_ImportUE_H)*100 #59,4%
pourcentageH_ap2008 <- (totalHAp2008/Valeur_ILL_ImportUE_H)*100 #40,6%

####plot de l'évolution dans le temps des exports
ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Évolution des valeurs de différents pays dans le temps",
       x = "Période",
       y = "Valeur principale",
       color = "Pays") +
  theme_minimal()

####selection des 5 premiers exportateurs avant et apres 2008: russie, estonie,gabon,lettonie,RDC,Congo
country_colors <- c("Estonia" = "red", "Russian Federation" = "blue", "Latvia" ="green", "Gabon" = "yellow", "Dem. Rep. of the Congo" ="purple", "Congo" = "cyan")

ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors, "gray")) +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color = "Pays") 

###### tendance bois ill 10 dernières années #######
##### regrouper total bois ill pour chaque année ###

data_annéesB <- Donnee %>%
  group_by(period) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))

sum(data_annéesB$total_primaryValue)

data_annéesH <- Donnee %>%
  group_by(period) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

 sum(data_annéesH$total_primaryValue)###c'est bien équivalent à la valeur total calculée avant

 
 data_années <- Donnee %>%
   group_by(period) %>%
   summarize((total_primaryValueH = sum(Vraie_ValeurH, na.rm = TRUE)),total_primaryValueB = sum(Vraie_ValeurB, na.rm = TRUE))

 data_années$moyenne<- (data_années$total_primaryValueB+data_années$`(total_primaryValueH = sum(Vraie_ValeurH, na.rm = TRUE))`)/2
 
  plot(data_annéesB)
 
            

ggplot(data_années, aes(x = period, y = moyenne,)) +
   geom_line(size = 1) +
   geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.7)+
   geom_ribbon(aes(ymin = data_années$total_primaryValueB, ymax = data_années$`(total_primaryValueH = sum(Vraie_ValeurH, na.rm = TRUE))`), alpha = 0.2) +
   scale_color_manual(values = c(country_colors, "gray")) +
   scale_fill_manual(values = c(country_colors, "gray")) +
   labs(title = "Évolution des valeurs d'import illégal de 2000 à 2022",
        x = "Période",
        y = "Valeur en Dollars USD",
        color = "Pays",
        fill = "Pays") +
  theme_minimal()+
  annotate("text", x = 2005, y = 2.2e+08, label = paste("p-value:1.751e-05"), 
           hjust = 1.1, vjust = 2, size = 4, color = "red")
 model <- lm(data_années$moyenne~data_années$period)
summary(model)
 ###rajout d'un graph avec le pourcentage d'ill par année en fonction de l'import total
  data_annéesTotal <- Imports_UE %>%
    group_by(period) %>%
    summarize(total_primaryValue = sum(valeur, na.rm = TRUE))
  sum(data_annéesTotal$total_primaryValue)
  
 data_années$pourcentageB <- (data_années$total_primaryValueB/data_annéesTotal$total_primaryValue)*100
data_années$pourcetageH <-(data_années$`(total_primaryValueH = sum(Vraie_ValeurH, na.rm = TRUE))`/data_annéesTotal$total_primaryValue)*100

data_années$moyennepourcentage<- (data_années$pourcentageB+data_années$pourcetageH)/2


ggplot(data_années, aes(x = period, y = moyennepourcentage,)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.7)+
  geom_ribbon(aes(ymin = data_années$pourcentageB, ymax = data_années$pourcetageH), alpha = 0.2) +
  scale_color_manual(values = c(country_colors, "gray")) +
  scale_fill_manual(values = c(country_colors, "gray")) +
  labs(title = "Évolution du pourcentage d'import illégal sur import total de bois rond en Europe",
       x = "Période",
       y = "Pourcentage %",
       color = "Pays",
       fill = "Pays") +
  theme_minimal()+
  annotate("text", x = 2005, y = 3, label = paste("p-value:8.558e-12"), 
           hjust = 1.1, vjust = 2, size = 4, color = "red")

model1 <- lm(data_années$moyennepourcentage~data_années$period)
summary(model1)

###### principaux exportateurs illégaux

country_colors <- c("Estonia" = "red", "Russian Federation" = "blue", "Latvia" ="green", "Gabon" = "yellow", "Dem. Rep. of the Congo" ="purple", "Congo" = "cyan")
country_colors1 <- c("Estonia" = "red")
country_colors2 <- c("Russian Federation" = "red")
country_colors3 <- c("Latvia" ="red")
country_colors4 <- c("Gabon" = "red")
country_colors5 <- c("Dem. Rep. of the Congo" ="red")
country_colors6 <- c( "Congo" = "red")


g1<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors1, "gray")) +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color="Pays")


g2<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors2, "gray"),label = "Russie") +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color = "Pays")

g3<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors3, "gray")) +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color = "Pays")
g4<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors4, "gray")) +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color = "Pays")

g5<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors5, "gray"),label = "DRC") +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Valeur principale",
       color = "Pays")

g6<-ggplot(grouped_dataBill, aes(x = period, y = total_primaryValue, color = reporterDesc, group= reporterDesc)) +
  geom_line(size = 1) +
  geom_point(size = 1)+
  scale_color_manual(values = c(country_colors6, "gray")) +
  labs(title = "Évolution des valeurs pour tous les pays",
       x = "Période",
       y = "Pays",
       color="Pays")
library(cowplot)
plot_grid(g1,g2,g3,g4,g5,g6)

par(mfrow = c(1,1))
######### carte choroplèthe ############

library(sf)
shapefile_path <- "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/Europe_merged.shp"

# Lire le shapefile
shape_data <- st_read(shapefile_path)

# Afficher les premières lignes des données
print(head(shape_data))

# Afficher un résumé des données
summary(shape_data)

# Visualiser le shapefile avec ggplot2
ggplot(data = shape_data) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Carte du Shapefile")

shape_data$COUNTRY

Europe <- shape_data %>%
  left_join(grouped_dataH, by = c("COUNTRY" = "partnerDesc"))

datatable(europe)
europe <- europe %>%
  mutate(category = case_when(
    Pourcentage < 0.02 ~ "a:moins de 2%",
    Pourcentage >= 0.02 & Pourcentage < 0.1 ~ "b:entre 2 et 10 %",
    Pourcentage >= 0.1 & Pourcentage < 0.2 ~ "c:entre 10 et 20 %",
    Pourcentage >= 0.2 ~ "d:plus de 20%",
    TRUE ~ NA_character_
  ))
ggplot(data = europe) +
  geom_sf(aes(fill = category)) +
  scale_fill_manual(values = c("a:moins de 2%" = "yellow", "b:entre 2 et 10 %" = "orange", "c:entre 10 et 20 %" = "red","d:plus de 20%" = "darkred"), na.value = "gray") +
  theme_minimal() +
  labs(title = "Implication des pays de l'Europe dans l'import de bois rond illégal",
       fill = "Catégorie",
       label = "Part des importations européennes de bois rond illégaux")

###### Valeurs avec et sans la russie #####
###avant et apres 2008 ####
BillAv2008_NoRussie <- grouped_dataBill_av2008[-32,]
BillAp2008_NoRussie <- grouped_dataBill_ap2008[-32,]

HillAv2008_NoRussie <- grouped_dataHill_av2008[-32,]
HillAp2008_NoRussie <- grouped_dataHill_ap2008[-32,]

totalB_NoRussie <- sum(BillAv2008_NoRussie$total_primaryValue)+sum(BillAp2008_NoRussie$total_primaryValue) ##6,39 Milliards
totalH_NoRussie <- sum(HillAv2008_NoRussie$total_primaryValue)+sum(HillAp2008_NoRussie$total_primaryValue) ##6,64 Milliards

##### par annee ###

Bill_NoRussie <- grouped_dataBill[-c(615:637),]
Hill_NoRussie <- grouped_dataHill[-c(615:637),]

No_Russie <- Bill_NoRussie %>%
  left_join(Hill_NoRussie, by = c("period" = "period","reporterDesc"="reporterDesc"))
No_Russie$valeurB <- No_Russie$total_primaryValue.x 
No_Russie$PourcentageB <- No_Russie$Pourcentage.x
No_Russie$valeurH <- No_Russie$total_primaryValue.y 
No_Russie$PourcentageH <- No_Russie$Pourcentage.y
No_Russie <-No_Russie[,-c(3,4,5)]

period_NoRussie <- No_Russie %>%
  group_by(period) %>%
  summarize(valeurB = sum(valeurB, na.rm = TRUE),
            ValeurH = sum(valeurH, na.rm= TRUE))
period_NoRussie$moyenne <- (period_NoRussie$valeurB+period_NoRussie$ValeurH)/2

sum(period_NoRussie$valeurB)
sum(No_Russie$valeurB)

ggplot(period_NoRussie, aes(x = period, y = moyenne,)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.7)+
  geom_ribbon(aes(ymin = period_NoRussie$valeurB, ymax = period_NoRussie$ValeurH), alpha = 0.2) +
  scale_color_manual(values = c(country_colors, "gray")) +
  scale_fill_manual(values = c(country_colors, "gray")) +
  labs(title = "Évolution des valeurs d'import illégal de bois rond en Europe sans la Russie",
       x = "Période",
       y = "valeur en dollars USD",
       color = "Pays",
       fill = "Pays") +
  theme_minimal()+
annotate("text", x = 2020, y =5e+08, label = paste("p-value = 9,1e-06"), 
                          hjust = 1.1, vjust = 2, size = 4, color = "red")


model2 <- lm(period_NoRussie$moyenne~period_NoRussie$period)
summary(model2)

## russie av 2008

russieH <- subset(grouped_dataHill, reporterDesc == "Russian Federation")
russieB <- subset(grouped_dataBill, reporterDesc == "Russian Federation")

RussieB_av2008 <- russieB[russieB$period >= 2000 & russieB$period <= 2008, ]
RussieB_ap2008 <- russieB[russieB$period >= 2009 & russieB$period <= 2022, ]

sum(RussieB_av2008$total_primaryValue) ## 1.110.987.081
sum(RussieB_ap2008$total_primaryValue)## 599.371.009

RussieH_av2008 <- russieH[russieH$period >= 2000 & russieH$period <= 2008, ]
RussieH_ap2008 <- russieH[russieH$period >= 2009 & russieH$period <= 2022, ]

sum(RussieH_av2008$total_primaryValue) ## 3.703.290.269
sum(RussieH_ap2008$total_primaryValue)## 1.997.903.362

sum(RussieB_av2008$Pourcentage) ## 13,7%
sum(RussieB_ap2008$Pourcentage) ## 7,3%

sum(RussieH_av2008$Pourcentage) ## 30%
sum(RussieH_ap2008$Pourcentage) ## 16,2%



sum(is.na(Imports_UE$primaryValue.x))
sum(is.na(Imports_UE$primaryValue.y))
sum(Imports_UE$primaryValue.x , na.rm=TRUE)
sum(Imports_UE$primaryValue.y , na.rm=TRUE) 
(sum(is.na(Imports_UE$primaryValue.x)))/21710  ##33,8%
sum(is.na(Imports_UE$primaryValue.y))/21710 ### 16%
### graph de l'évolution des valeurs globales, pas seulement illégales.

Donnees$valeurs <- ifelse(is.na(Donnees$primaryValue.y), Donnees$primaryValue.x, Donnees$primaryValue.y)
grouped_data_total <- Donnees %>%
  group_by(partnerDesc) %>%
  summarize(total_primaryValue = sum(valeurs, na.rm = TRUE))

data_années_total <- Donnees %>%
  group_by(period) %>%
  summarize(total_primaryValue = sum(valeurs, na.rm = TRUE))


ggplot(data_années_total, aes(x = period, y =total_primaryValue, color="red")) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  labs(title = "Évolution des valeurs d'import total de 2000 à 2022",
       x = "Période",
       y = "Valeur en Dollars USD") +
  theme_minimal()

