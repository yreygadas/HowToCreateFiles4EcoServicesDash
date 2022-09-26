# THIS SCRIPT calculates scenarios of ET based on a given percentage of degraded and deforested area"
########################################################################################################
############################# User-defined parameters ##################################################
# Set study area prefix
prefix<-"IndTerr"
# Set up the working directory where all the .dbf files with the variable values are located
dir.var <- "D:/Yunuen/6PostDoc/Dashboards/ET/DBFiles"
# Set up the working directory where all the .dbf files with the cell counts are located
dir.cellcounts<- "D:/Yunuen/6PostDoc/Dashboards/DBFCellCounts"
#Set the name of the output directory where the results will be stored
dir.out <- "D:/Yunuen/6PostDoc/Dashboards/Output"
#Number of polygons in the shapefile
polNum<- 554
#######################################################################################################
############Creates a table with variable values and cell counts by forest condition ##################
#Require the packages
require(rkt)
require(foreign)
require(ggplot2)
require(peRspective)
require(tidyverse)

#Climate variable
cv<- "ET"

#Create the output directory
dir.create(dir.out)

#Set the cell counts working directory
setwd(dir.cellcounts)

#Create a list of the cell counts .dbf file
list.cellcountNames <- list.files(pattern=prefix)
print(list.cellcountNames)

#Create a LOOP that goes through each cell counts .dbf file
for (i in 1:length(list.cellcountNames)) {
  
  #Set the name of the .csv files
  dbf.name <- list.cellcountNames[i]
  
  #Set time id based on the .csv file name  
  timeId_0 <- sapply(strsplit(dbf.name, split='_', fixed=TRUE), function(x) (x[2]))
  timeId_1 <- sapply(strsplit(timeId_0, split='.', fixed=TRUE), function(x) (x[1]))
  timeId<- paste0("CellCounts_",timeId_1)
  print(timeId)
  
  #Bring the dataset into R
  df0 <- read.dbf(dbf.name, as.is = FALSE) 
  
  #Extract the CV values by forest condition
  df0<-df0 [, c(1,2)]
  #Change the column names
  colnames(df0) <- c("ID", paste0("Count_",timeId_1))
  
  
  #Retain this dataframe that will be used later on
  assign(timeId, df0)
  
}

#Set the cell counts working directory
setwd(dir.var)

#Create a list of the variable values .dbf files
list.varNames <- list.files(pattern=prefix)
print(list.varNames)


#Create a LOOP that goes through each variable values .dbf file
for (i in 1:length(list.varNames)) {
  
  #Set the name of the .csv files
  dbf.name <- list.varNames[i]
  
  #Set time id based on the .csv file name  
  timeId_0 <- sapply(strsplit(dbf.name, split='_', fixed=TRUE), function(x) (x[2]))
  timeId_1 <- sapply(strsplit(timeId_0, split='.', fixed=TRUE), function(x) (x[1]))
  timeId<- paste0("Var_",timeId_1)
  print(timeId)
  
  #Bring the dataset into R
  df0 <- read.dbf(dbf.name, as.is = FALSE) 
  
  #Extract the CV values by forest condition
  df0<-df0 [, c(1,4)]
  #Change the column names
  colnames(df0) <- c("ID",paste0(cv,"_",timeId_1))
  

  #Retain this dataframe that will be used later on
  assign(timeId, df0)

}

# Puts all dataframes together
list.dfs= list(CellCounts_0,CellCounts_1,CellCounts_2,CellCounts_3,
                         Var_0,Var_1,Var_2,Var_3)
df.all<-as.data.frame(list.dfs%>%reduce(full_join,by="ID", all=TRUE))

#Make sure df.all is sorted by id
df.all <- df.all[with(df.all, order(ID)),]

########################################################################################################
################### Calculates all statistics needed to perform weighted averages #####################
#################### based on variable values and cell counts by forest condition ######################

# Calculate total cells
df.all$TotalCount<- rowSums(df.all[,c("Count_0","Count_1","Count_2","Count_3")],na.rm=TRUE )

# Calculate area by forest condition based on cell counts 
df.all$AreaPr_0<- (df.all$Count_0*100)/df.all$TotalCount
df.all$AreaPr_1<- (df.all$Count_1*100)/df.all$TotalCount
df.all$AreaPr_2<- (df.all$Count_2*100)/df.all$TotalCount
df.all$AreaPr_3<- (df.all$Count_3*100)/df.all$TotalCount

#Convert NA percentages of area to Zero values
df.all$AreaPr_0[is.na(df.all$AreaPr_0)] <- 0
df.all$AreaPr_1[is.na(df.all$AreaPr_1)] <- 0
df.all$AreaPr_2[is.na(df.all$AreaPr_2)] <- 0
df.all$AreaPr_3[is.na(df.all$AreaPr_3)] <- 0

#Calculate total area
df.all$AreaPr_Total<-rowSums(df.all[,c("AreaPr_0","AreaPr_1","AreaPr_2","AreaPr_3")])

#Calculate the 2020 true weighted average
df.all$Zero<-df.all$ET_0*df.all$AreaPr_0
df.all$One<-df.all$ET_1*df.all$AreaPr_1
df.all$Two<-df.all$ET_2*df.all$AreaPr_2
df.all$Three<-df.all$ET_3*df.all$AreaPr_3
df.all$true_wAv2020<- rowSums(df.all[,c("Zero","One","Two","Three")],na.rm = TRUE)/100

#Calculate the 2020 baseline weighted average, it should be based on the proportions of intact forest, def & deg
# in 2020 in the entire SWA (excluding wat 8,18,19). Those proportions were calculated in advance as:
# Intact forest 1, Degradation = 0.911998565876277, Deforestation = 0.770534293 with respect to intact forest
# New values= Intact forest 1, Degradation = 0.90917778, Deforestation = 0.718137913 with respect to intact forest
df.all$Two<-(df.all$ET_1*0.90917778)*df.all$AreaPr_2
df.all$Three<-(df.all$ET_1*0.718137913)*df.all$AreaPr_3
df.all$bline_wAv2020<- rowSums(df.all[,c("Zero","One","Two","Three")],na.rm = TRUE)/100

# Deletes the temporary columns created to calculate the weighted averages
df.all<-df.all[,c(-16,-17,-18,-19)]

#See the difference between the true and the aline averages
df.all$difference<-100-((df.all$bline_wAv2020*100)/df.all$true_wAv2020)

########################################################################################################
################### Calculates weighted averages based on  deg-def scenarios ###########################
################### and 2020 values ####################################################################

## Create a table with all possible combinations of def and deg area to calculate scenarios ##
# Sequence of possible deforestation
pr_def<- rep(0:100,each=101)
#Sequence of possible degradation 
pr_deg<- rep(0:100,times=101)

# Starts the final dataset with both sequences 
df.final<- data.frame(pr_def,pr_deg)

# Sum of deg and def
df.final$pr_degdef<-rowSums(df.final[,c("pr_def","pr_deg")])

# Creates a list with the number of polygons contained in the shapefile
polList<- as.list(1:polNum)
#print(polList)

# Iterates through each polygon
for (poly in 1:length(polList)) {
  print(paste0(polList[[poly]],"---",df.all[poly,1]))
  
  #Calculates the estimated weighted average  
  #not-forest and intact forest ET/LST values and areas are based on 2020
  #First, calculate the new intact forest area, non-forest area is always the same, intact forest changes based on the
  #deg and def area to be modeled
  df.final$est_Area1<-100-(df.all[poly,11]+df.final$pr_def+df.final$pr_deg) ###****###
  #To double check the calculation of area (total estimated area)
  df.final$est_totalArea<-rowSums(df.final[,c("pr_def","pr_deg","est_Area1")])+df.all[poly,11] ###****###
  
  #Estimated wAv
  df.final$Zero<-df.all[poly,6]*df.all[poly,11] ###****###
  df.final$One<-df.all[poly,7]*df.final$est_Area1###****###
  df.final$Two<-(df.all[poly,7]*0.90917778)*df.final$pr_deg###****###
  df.final$Three<-(df.all[poly,7]*0.7181379137)*df.final$pr_def###****###
  df.final$Est_wAv<- rowSums(df.final[,c("Zero","One","Two","Three")],na.rm = TRUE)/100
  
  # Calculate the difference in % 
  df.final$Est_difference_pr<- (100-(df.final$Est_wAv*100)/df.all[poly,17])*-1 ###****###
  # Difference in mm
  df.final$Est_difference_mm<-(df.all[poly,17]-df.final$Est_wAv)*-1 ###****###
  
  ########################################################################################################
  ##### Creates the final column with the final response #################################################
  
  #If % of deg+def > 100-non-forest area, then "Combination of deforested & degraded area exceeds the limits"
  
  #IN text all
  df.final$Answer_tx<-ifelse(df.final$pr_degdef>(100-df.all[poly,11]),"Los valores superan el % de bosque intacto en 2020", ###****###
                             ifelse(df.final$Est_difference_pr>0, paste0("+",round(df.final$Est_difference_pr,digits=2),"%, ","+",round(df.final$Est_difference_mm,digits=2),"mm/año"),
                                    ifelse(df.final$Est_difference_pr<0, paste0(round(df.final$Est_difference_pr,digits=2),"%, ",round(df.final$Est_difference_mm,digits=2),"mm/año"),
                                           ifelse(NA))))
  #Symbols
  df.final$Answer_sym<-ifelse(df.final$pr_degdef>(100-df.all[poly,11])," ",###****###
                              ifelse(df.final$Est_difference_pr>0, "+",
                                     ifelse(NA)))
  #In number %
  df.final$Answer_pr<-ifelse(df.final$pr_degdef>(100-df.all[poly,11]),-999,###****###
                             ifelse(df.final$Est_difference_pr!=-999999, round(df.final$Est_difference_pr,digits=2)))#)))"Difference of ET/LST with respect to 2020"
  #In number mm
  df.final$Answer_mm<-ifelse(df.final$pr_degdef>(100-df.all[poly,11]),-999,###****###
                             ifelse(df.final$Est_difference_pr!=-999999, round(df.final$Est_difference_mm,digits=2)))#)))"Difference 
  
  #Assigns the polygon ID
  df.final$Polygon<-df.all[poly,1]
  
  #################################################################
  #Calculate statistics
  df.final$ToMakeCal<-ifelse(df.final$pr_degdef>(100-df.all[poly,11]),NA,
                             #ifelse(df.final$pr_def<=df.all[18,13]&df.final$pr_deg<=df.all[18,12], "Area already deforested and degraded",
                             #ifelse(df.final$pr_def<=df.all[18,13], "Area already deforested",
                             #ifelse(df.final$pr_deg<=df.all[18,12], "Area already degraded", 
                             #ifelse(df.final$Def_Df_pr<=-100| df.final$Deg_Df_pr<=-100|df.final$Est_df_pr<=-100, "Tipping point",
                             ifelse(df.final$Est_difference_pr!=-9999999, df.final$Est_difference_pr))#)))"Difference of ET/LST with respect to 2020"
  
  
  # Creates a dataframe with the statistics
  MaxNeg<-round(min(df.final$ToMakeCal, na.rm = TRUE),digits=2)
  MaxPos<-round(max(df.final$ToMakeCal, na.rm = TRUE),digits=2)
  Average<- round(mean(df.final$ToMakeCal, na.rm = TRUE),digits=2)
  
  df.stats<-data.frame(MaxNeg,MaxPos,Average)
  
  #Assigns the polygon ID to the stats dataframe
  df.stats$Polygon<-df.all[poly,1]
  
  #Reorganize columns
  df.stats<-df.stats[,c(4,1,2,3)]
  
  ##Retain the de.final dataframe that will be used later on
  assign(paste0("PolyStats_",poly), df.stats)

  
  ###################################################################
  
  #Removes unnecessary columns and reorganize them
  df.final<-df.final[,c(17,1,2,14,15,16,13)]
  
  # Delete NAs
  df.final[is.na(df.final)] <- ""
  
  #Retain the df.final dataframe that will be used later on
  assign(paste0("PolyScenarios_",poly), df.final)
  
  #Leave df.final clean and prepared for the next iteraction  
  df.final<-df.final[,c(2,3)]
  df.final$pr_degdef<-rowSums(df.final[,c("pr_def","pr_deg")])

}

  
# Put all PolyScenarios dataframes together 
all.PolyScenarios <- do.call(rbind, mget(ls(pattern = "PolyScenarios_")))

# Put all PolyStats dataframes together 
all.PolyStats <- do.call(rbind, mget(ls(pattern = "PolyStats_")))


# Delete NAs and unnecessary columns of df.all before exporting
df.all[is.na(df.all)] <- ""
df.all<-df.all[,c(1,11:14,17)]
# Rename columns
colnames(df.all)<-c('ID','Area2020_0','Area2020_1','Area2020_2','Area2020_3','ET2020_mm/yr')

############################ Get rid of -999 ##################################

all.PolyScenarios.short<-filter(all.PolyScenarios, Answer_pr!= -999)
#################### Add text names to the final tables ####################### 


###############################################################################################################
###################################### Exports the three final tables #########################################

#Set the output  directory
setwd(dir.out)

#Export the df.all dataframe
write.csv(df.all, paste0(prefix,"_FCareas_ET&LSTvalues.csv"), row.names = TRUE)

#Export the all.PolyScenarios dataframe
#write.csv(all.PolyScenarios, paste0(prefix,"_",cv,"_ScenariosDegDef_long.csv"), row.names = TRUE)
#Export the all.PolyScenarios dataframe, the version without -999
write.csv(all.PolyScenarios.short, paste0(prefix,"_",cv,"_ScenariosDegDef.csv"), row.names = TRUE)

#Export the all.PolyScenarios dataframe
#write.csv(all.PolyStats, paste0(prefix,"_",cv,"_StatisticsOfScenarios.csv"), row.names = TRUE)
