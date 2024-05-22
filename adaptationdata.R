# UN Comtrade data import

# Install pacman package if not installed
if (!("pacman" %in% rownames(installed.packages()))) {
  install.packages("pacman")
}

# Install and load used packages
pacman::p_load(reticulate, rstudioapi)

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
years <- 2000:2020
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
          row.names = FALSE,sep=",")

# To load back the csv file
data <- read.csv(list.files(path = "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/raw_data",sep = "",
                            pattern = ".csv",
                            full.names = TRUE))
all(dim(data) == dim(trade_data)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)


####changement pour adaptation aux données####



#Data <- trade_data [,-c(1,2,3,5,6,7,12,13,15,16,18,19,20,23:41,45,46,47)]
##on garde les colonnes qui nous intéressent
Data <- trade_data

export=subset(Data,flowCode=="X")
import=subset(Data,flowCode=="M")

table(export$partnerDesc)
table(import$partnerDesc)
table(Data$partnerDesc)

####definition des blocs pays#######

UE27=c("Austria","Croatia","Czechia","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Luxembourg","Malta","Netherlands","Portugal","Romania","Spain","Sweden","Slovenia","Poland","Lithuania","Germany","Denmark","Cyprus","Bulgaria","Belgium","Slovakia")
PAYSILL=c("Bolivia","Brazil","Colombia","Ecuador","Peru","Mexico","Nicaragua","Honduras","Russian Federation","Tajikistan","Bosnia Herzegovina","Serbia and Montenegro","North Macedonia","Albiania","Estonia","Bulgaria","Slovakia","Latvia","Cameroon","Gabon","Ghana","Nigeria","Equatorial Guinea","Benin","Mozambique","China","Indonesia","Cambodia","Lao People's Dem. Rep.","Malaysia","Papua New Guinea","Philippines","Thailand","Viet Nam","Myanmar","Rep. of Korea")
table(PAYSILL) ###il y a bien les 36
table(UE27)# il y en a bien 27

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

toMatch = c('\\d','XX','_X')
hop <-Data[!(grepl(paste(toMatch,collapse="|"), Data$reporterISO)) &
             !(grepl(paste(toMatch,collapse="|"), Data$partnerISO)),]
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

Valeur_ILL_ImportUE_B <- sum(Donnee$Vraie_ValeurB)  ####7,6 Milliards $
Valeur_ILL_ImportUE_H <- sum(Donnee$Vraie_ValeurH) #### 11,6 Milliards $

###### Calculs % par rapport aux imports totaux de l'UE
Imports_UE <-mirror_flow[(mirror_flow$partnerDesc %in% UE27),]
Imports_UE$valeur <- ifelse(is.na(Imports_UE$primaryValue.y), Imports_UE$primaryValue.x, Imports_UE$primaryValue.y)
Valeur_Total_ImportUE <-sum(Imports_UE$valeur)

Rapport_Bas <-Valeur_ILL_ImportUE_B / Valeur_Total_ImportUE  #### 4,49 %
Rapport_Haut <- Valeur_ILL_ImportUE_H / Valeur_Total_ImportUE  #### 6,89 %

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
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurB, na.rm = TRUE))

grouped_dataHill <- Donnee %>%
  group_by(reporterDesc) %>%
  summarize(total_primaryValue = sum(Vraie_ValeurH, na.rm = TRUE))

grouped_dataBill$Pourcentage <- grouped_dataBill$total_primaryValue / Valeur_ILL_ImportUE_B
grouped_dataHill$Pourcentage <- grouped_dataHill$total_primaryValue / Valeur_ILL_ImportUE_H

ggplot(Donnee, aes(x=period, y=Vraie_ValeurB)) +
  geom_line()

ggplot(Donnee, aes(x=period, y=Vraie_ValeurH)) +
  geom_line()

### influence du prix du bois russe en 2007-2008

##création data 2000-2008 et 2008-2020


