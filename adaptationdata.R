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

##########################
#########test avec subset############
impUE <- subset(import, reporterDesc == UE27)
table(impUE$reporterDesc)

expUE <- subset(impUE,partnerDesc==PAYSILL)
table(expUE$partnerDesc)


###########################
#Données Miroirs
##########################
expILL=subset(export,reporterDesc==c("Bolivia","Brazil","Colombia","Ecuador","Peru","Mexico","Nicaragua","Honduras","Russian Federation","Tajikistan","Bosnia Herzegovina","Serbia and Montenegro","North Macedonia","Albiania","Estonia","Bulgaria","Slovakia","Latvia","Cameroon","Gabon","Ghana","Nigeria","Equatorial Guinea","Benin","Mozambique","China","Indonesia","Cambodia","Lao People's Dem. Rep.","Malaysia","Papua New Guinea","Philippines","Thailand","Viet Nam","Myanmar","Rep. of Korea"))
table(expILL$reporterDesc)

impILL=subset(expILL,partnerDesc==c("Austria","Croatia","Czechia","Estonia","Finland","France","Greece","Hungary","Ireland","Italy","Latvia","Luxembourg","Malta","Netherlands","Portugal","Romania","Spain","Sweden","Slovenia","Poland","Lithuania","Germany","Denmark","Cyprus","Bulgaria","Belgium","Slovakia"))
table(impILL$partnerDesc)
#~~~~~~~~non~~~~~~
###########################

##########test avec les crochets ##################
impue <- import[import$reporterDesc == UE27,]
exp <- import[import$partnerDesc == PAYSILL, ]
v<- impue[impue$partnerDesc== PAYSILL,]
#~~~~~~~~non~~~~~~~~

############test avec dplyr###################
library(dplyr)

impue2 <- filter(import, reporterDesc == UE27, partnerDesc == PAYSILL)
impue2b <- filter(import, reporterDesc == UE27)
impue2t <- filter(import, partnerDesc == PAYSILL)
v1 <- filter(import, impue2b & impue2t)
v1 <- filter(import, reporterDesc == UE27 & partnerDesc == PAYSILL)



impue2b <- filter(import, reporterDesc == UE27)
impue2t <- filter(import, partnerDesc == PAYSILL)
v2 <- inner_join(impue2b, impue2t)

xylophone <- Data %>%
  filter(reporterDesc == UE27, partnerDesc == PAYSILL)

#######~~~~~~~non~~~~~~~~~##

# Export data as a csv file
write.csv(impUE,
          "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/raw_data/uncomtrade_datatest.csv",
          row.names = FALSE,sep=",")

# To load back the csv file
data <- read.csv(list.files(path = "C:/Users/asus/Documents/GCRE/S8/stage/un_comtrade_data/raw_data",sep = "",
                            pattern = ".csv",
                            full.names = TRUE))
all(dim(data) == dim(impUE)) # if TRUE: same dimensions
# The two dataframes are not exactly equals: some minor differences remain
# To check differences, use: all.equal(data, trade_data)

#########le csv crée ne me donne pas toutes les données qu'il devrait y avoir.
