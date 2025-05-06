#TEMPERATURE DATA 

#NUMBER OF COLONIES 

# INSTALL PACKAGES AND RUN LIBRARIES
install.packages("tidyverse")
install.packages("janitor")
install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(readxl)

# CLEAN UP DATASET FIRST 
library(readxl)
temperature <- read_excel("DATA/TEMPERATURESIZEDATA.xlsx")
View(temperature)
print(temperature)

# filter for each strain 
strain2299 <- filter(temperature, Strain == "2299")
strain2299
summary(strain2299)

strain3201 <- filter(temperature, Strain == "3201")
strain3201
summary(strain3201)

strain3188 <- filter(temperature, Strain == "3188")
strain3188
summary(strain3188)

# ANOVA- to determine if there are significant differences between the mean size of colonies between temperatures
# Creating histograms to determine 
ggplot(strain2299, aes(x = `25ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 25ºC for Strain 2299")
ggplot(strain2299, aes(x = `30ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 30ºC for Strain 2299")
ggplot(strain2299, aes(x = `37ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 37ºC for Strain 2299")

ggplot(strain3201, aes(x = `25ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 25ºC for Strain 3201")
ggplot(strain3201, aes(x = `30ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 30ºC for Strain 3201")
ggplot(strain3201, aes(x = `37ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 37ºC for Strain 3201")

ggplot(strain3188, aes(x = `25ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 25ºC for Strain 3188")
ggplot(strain3188, aes(x = `30ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 30ºC for Strain 3188")
ggplot(strain3188, aes(x = `37ºC`)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~Strain) +
  labs(title = "Distribution of Colony Sizes at 37ºC for Strain 3188")

#Based on the histograms, the data does not seem to be normally distributed, meaning that an assumption of the ANOVA statistical
# test has been violated and cannot be used. Instead a Kruskal-Wallis Test can be used- a non-paramteric equivalent to ANOVA that
# does not require normality. 

# Reshape the data for strain2299
library(tidyr)
strain2299_long <- strain2299 %>%
  pivot_longer(
    cols = c(`25ºC`, `30ºC`, `37ºC`),  # Specify the columns you want to reshape
    names_to = "Temperature",  # The new column name for temperature
    values_to = "ColonySize"   # The new column name for colony sizes
  )
# Check the reshaped data
print(strain2299_long)

kruskal_test_result2299 <- kruskal.test(ColonySize ~ Temperature, data = strain2299_long)
print(kruskal_test_result2299)
# there is a significant difference in size between temperatures, but I don't know which conditions differ, so I will have 
# do a post-hoc test to determine which are different. This post hoc test will be a Pairwise Wilcox- why? 
wilcoxresult2299 <- pairwise.wilcox.test(strain2299_long$ColonySize, 
                                        strain2299_long$Temperature, 
                                        p.adjust.method = "bonferroni")
print(wilcoxresult2299)
# result suggests that 2299 shows significant differences in colony size at each temperature- each temperature is 
# significantly different from each other. 

# strain3201
library(tidyr)
strain3201_long <- strain3201 %>%
  pivot_longer(
    cols = c(`25ºC`, `30ºC`, `37ºC`), 
    names_to = "Temperature",  
    values_to = "ColonySize"   
  )
print(strain3201_long)
kruskal_test_result3201 <- kruskal.test(ColonySize ~ Temperature, data = strain3201_long)
print(kruskal_test_result3201)
# also significant difference, wilcox to check which are different 
wilcoxresult3201 <- pairwise.wilcox.test(strain3201_long$ColonySize, 
                                         strain3201_long$Temperature, 
                                         p.adjust.method = "bonferroni")
print(wilcoxresult3201)
# all conditions are significantly different from each other :) 

# strain 3188 
library(tidyr)
strain3188_long <- strain3188 %>%
  pivot_longer(
    cols = c(`25ºC`, `30ºC`, `37ºC`), 
    names_to = "Temperature",  
    values_to = "ColonySize"   
  )
print(strain3188_long)
kruskal_test_result3188 <- kruskal.test(ColonySize ~ Temperature, data = strain3188_long)
print(kruskal_test_result3188)
# also significant difference, wilcox to check which are different 
wilcoxresult3188 <- pairwise.wilcox.test(strain3188_long$ColonySize, 
                                         strain3188_long$Temperature, 
                                         p.adjust.method = "bonferroni")
print(wilcoxresult3188)
# significant difference between 25ºC and 30ºC, and 25ºC and 37ºC, but not 30ºC and 37ºC. 

boxplot2299 <- ggplot(strain2299_long, aes(x = Temperature, y = ColonySize))+
  geom_boxplot()+
  theme_classic()+
  labs(title = "Boxplot to show Colony Size vs Temperature in Wildtype Strain", x = "Temperature", y = "Colony Size")+
  annotate(geom = "text", x = "30ºC", y = 775, label = "*", fontface = "bold", size = 5)+
  annotate(geom = "text", x = "37ºC", y = 775, label = "***", fontface = "bold", size = 5)+
  annotate(geom = "text", x = "37ºC", y = 750, label = "***", fontface = "bold", colour = "grey", size = 5)
boxplot2299

boxplot3201 <- ggplot(strain3201_long, aes(x = Temperature, y = ColonySize))+
  geom_boxplot()+
  theme_classic()+
  labs(title = "Boxplot to show Colony Size vs Temperature in V412L Strain", x = "Temperature", y = "Colony Size")+
  annotate(geom = "text", x = "30ºC", y = 775, label = "***", fontface = "bold", size = 5)+
  annotate(geom = "text", x = "37ºC", y = 775, label = "***", fontface = "bold", size = 5)+
  annotate(geom = "text", x = "37ºC", y = 750, label = "***", fontface = "bold", colour = "grey", size = 5)
boxplot3201

boxplot3188 <- ggplot(strain3188_long, aes(x = Temperature, y = ColonySize))+
  geom_boxplot()+
  theme_classic()+
  labs(title = "Boxplot to show Colony Size vs Temperature in L425V Strain", x = "Temperature", y = "Colony Size")+
  annotate(geom = "text", x = "30ºC", y = 675, label = "***", fontface = "bold", size = 5)+
  annotate(geom = "text", x = "37ºC", y = 675, label = "***", fontface = "bold", size = 5)
boxplot3188

# DETERMINING IF THERE IS A SIG DIFF IN MUT RATE BETWEEN TEMPS (INTRA)
tempfa<- read_excel("DATA/TEMPERATUREMUTATIONSDATA.xlsx")
print(tempfa)
view(tempfa)

# 2299 
strain2299_25 <- filter(tempfa, Strain == "229925")
print(strain2299_25)
str(strain2299_25)
mut2299_25 <- strain2299_25$Mutant_count
mut2299_25
mutrate2299_25 <- newton.LD(mut2299_25)
mutrate2299_25
CI2299_25 <- confint.LD(mut2299_25)
CI2299_25

strain2299_30 <- filter(tempfa, Strain == "229930")
print(strain2299_30)
str(strain2299_30)
mut2299_30 <- strain2299_30$Mutant_count
mut2299_30
mutrate2299_30 <- newton.LD(mut2299_30)
mutrate2299_30
CI2299_30 <- confint.LD(mut2299_30)
CI2299_30

strain2299_37 <- filter(tempfa, Strain == "229937")
print(strain2299_37)
str(strain2299_37)
mut2299_37 <- strain2299_37$Mutant_count
mut2299_37
mutrate2299_37 <- newton.LD(mut2299_37)
mutrate2299_37
CI2299_37 <- confint.LD(mut2299_37)
CI2299_37

tempmut_list <- list( 
  "25" = mut2299_25,
  "30" = mut2299_30,
  "37" = mut2299_37
) # creates a dataset for all mutant counts and their strains

# Create an empty results data frame
tempresults <- data.frame(
  strain1 = character(),
  strain2 = character(),
  tempLRT_statistic = numeric(),
  tempp_value = numeric(),
  stringsAsFactors = FALSE
) # creates a dataset for a table that the results of the lrt and p-values can be displayed in 

# Loop through all unique strain pairs
strain_names <- names(tempmut_list)
for (i in 1:(length(strain_names) - 1)) {
  for (j in (i + 1):length(strain_names)) {
    s1 <- strain_names[i]
    s2 <- strain_names[j]
    # creates a loop so that all of the different strains will be compared against each other, but only once 
    
    comp <- compare.LD(tempmut_list[[s1]], tempmut_list[[s2]]) # the lrt test 
    tempresults <- rbind(tempresults, data.frame( # adds the results from the lrt tests to the empty table dataframe
      Strain1 = s1,
      Strain2 = s2,
      tempLRT_statistic = comp[1],
      tempp_value = comp[2]
    ))
  }
}
library(dplyr) # uses the dplyr package
temp2299results <- arrange(tempresults, tempp_value) # inserts the p values into the table in numerical order- most significant to least signficiant) 
print(temp2299results) 


#3201
strain3201_25 <- filter(tempfa, Strain == "320125")
print(strain3201_25)
str(strain3201_25)
mut3201_25 <- strain3201_25$Mutant_count
mut3201_25
mutrate3201_25 <- newton.LD(mut3201_25)
mutrate3201_25
CI3201_25 <- confint.LD(mut3201_25)
CI3201_25

strain3201_30 <- filter(tempfa, Strain == "320130")
print(strain3201_30)
str(strain3201_30)
mut3201_30 <- strain3201_30$Mutant_count
mut3201_30
mutrate3201_30 <- newton.LD(mut3201_30)
mutrate3201_30
CI3201_30 <- confint.LD(mut3201_30)
CI3201_30

strain3201_37 <- filter(tempfa, Strain == "320137")
print(strain3201_37)
str(strain3201_37)
mut3201_37 <- strain3201_37$Mutant_count
mut3201_37
mutrate3201_37 <- newton.LD(mut3201_37)
mutrate3201_37
CI3201_37 <- confint.LD(mut3201_37)
CI3201_37

tempmut_list <- list( 
  "25" = mut3201_25,
  "30" = mut3201_30,
  "37" = mut3201_37
) # creates a dataset for all mutant counts and their strains

# Create an empty results data frame
tempresults <- data.frame(
  strain1 = character(),
  strain2 = character(),
  tempLRT_statistic = numeric(),
  tempp_value = numeric(),
  stringsAsFactors = FALSE
) # creates a dataset for a table that the results of the lrt and p-values can be displayed in 

# Loop through all unique strain pairs
strain_names <- names(tempmut_list)
for (i in 1:(length(strain_names) - 1)) {
  for (j in (i + 1):length(strain_names)) {
    s1 <- strain_names[i]
    s2 <- strain_names[j]
    # creates a loop so that all of the different strains will be compared against each other, but only once 
    
    comp <- compare.LD(tempmut_list[[s1]], tempmut_list[[s2]]) # the lrt test 
    tempresults <- rbind(tempresults, data.frame( # adds the results from the lrt tests to the empty table dataframe
      Strain1 = s1,
      Strain2 = s2,
      tempLRT_statistic = comp[1],
      tempp_value = comp[2]
    ))
  }
}
library(dplyr) # uses the dplyr package
temp3201results <- arrange(tempresults, tempp_value) # inserts the p values into the table in numerical order- most significant to least signficiant) 
print(temp3201results) 

#3188
strain3188_25 <- filter(tempfa, Strain == "318825")
print(strain3188_25)
str(strain3188_25)
mut3188_25 <- strain3188_25$Mutant_count
mut3188_25
mutrate3188_25 <- newton.LD(mut3188_25)
mutrate3188_25
CI3188_25 <- confint.LD(mut3188_25)
CI3188_25

strain3188_30 <- filter(tempfa, Strain == "318830")
print(strain3188_30)
str(strain3188_30)
mut3188_30 <- strain3188_30$Mutant_count
mut3188_30
mutrate3188_30 <- newton.LD(mut3188_30)
mutrate3188_30
CI3188_30 <- confint.LD(mut3188_30)
CI3188_30

strain3188_37 <- filter(tempfa, Strain == "318837")
print(strain3188_37)
str(strain3188_37)
mut3188_37 <- strain3188_37$Mutant_count
mut3188_37
mutrate3188_37 <- newton.LD(mut3188_37)
mutrate3188_37
CI3188_37 <- confint.LD(mut3188_37)
CI3188_37

tempmut_list <- list( 
  "25" = mut3188_25,
  "30" = mut3188_30,
  "37" = mut3188_37
) # creates a dataset for all mutant counts and their strains

# Create an empty results data frame
tempresults <- data.frame(
  strain1 = character(),
  strain2 = character(),
  tempLRT_statistic = numeric(),
  tempp_value = numeric(),
  stringsAsFactors = FALSE
) # creates a dataset for a table that the results of the lrt and p-values can be displayed in 

# Loop through all unique strain pairs
strain_names <- names(tempmut_list)
for (i in 1:(length(strain_names) - 1)) {
  for (j in (i + 1):length(strain_names)) {
    s1 <- strain_names[i]
    s2 <- strain_names[j]
    # creates a loop so that all of the different strains will be compared against each other, but only once 
    
    comp <- compare.LD(tempmut_list[[s1]], tempmut_list[[s2]]) # the lrt test 
    tempresults <- rbind(tempresults, data.frame( # adds the results from the lrt tests to the empty table dataframe
      Strain1 = s1,
      Strain2 = s2,
      tempLRT_statistic = comp[1],
      tempp_value = comp[2]
    ))
  }
}
library(dplyr) # uses the dplyr package
temp3188results <- arrange(tempresults, tempp_value) # inserts the p values into the table in numerical order- most significant to least signficiant) 
print(temp3188results) 


tempstrains <- c("2299 25ºC", "2299 30ºC", "2299 37ºC", "3201 25ºC", "3201 30ºC", "3201 37ºC", "3188 25ºC", "3188 30ºC", "3188 37ºC")
tempmutation_rates <- c(mutrate2299_25, mutrate2299_30, mutrate2299_37, mutrate3201_25, mutrate3201_30, mutrate3201_37, mutrate3188_25, mutrate3188_30, mutrate3188_37)
tempCI <- c(CI2299_25, CI2299_30, CI2299_37, CI3201_25, CI3201_30, CI3201_37, CI3188_25, CI3188_30, CI3188_37)
tempCI

tempCI_lower <- c(0.4562806348558188229703, 3.1668485481461541297676, 8.7329037272316512030557, 3.4663256971248919136031, 10.0794557044245891574974, 7.8642960957306895508623, 14.7819728237710101836910, 15.2992521781866841479314, 27.6323096688915121887931)
tempCI_upper <- c(1.2563171084971320734525, 5.8078460861483431898478, 14.6100669707970975963462, 6.3367089878951379233740, 16.0674842933815291701194, 11.7424540511428894262735, 23.4209192554162868304957, 23.9983811058684786132744, 37.9465213526321321069190)

tempratedata <- data.frame(tempstrains, tempmutation_rates, tempCI_lower, tempCI_upper)
tempratedata

temprateplot <- ggplot(tempratedata, aes(x = tempstrains, y = tempmutation_rates)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = tempCI_lower, ymax = tempCI_upper), width = 0.2) +
  labs(
    title = "Comparison of Mutation Rates at 25ºC, 30ºC and 37ºC",
    y = "Mutation Rate (mutations per cell division)",
    x = "Strains"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific)+
  facet_wrap(~ tempstrains)
temprateplot

strain2299plotdata <- filter(tempfa, Strain == c("229925", "229930", "229937"))
strain2299plotdata
tempmutrat2299 <- c(mutrate2299_25, mutrate2299_30, mutrate2299_37)
tempCI2299_lower <- c(0.4562806348558188229703, 3.1668485481461541297676, 8.7329037272316512030557)
tempCI2299_upper <- c(1.2563171084971320734525, 5.8078460861483431898478, 14.6100669707970975963462)
temperature2299 <- c("25ºC", "30ºC", "37ºC")
temp2299plotdata <- data.frame(tempmutrat2299, tempCI2299_lower, tempCI2299_upper, temperature2299)
temp2299plotdata
  
temprateplot2299 <- ggplot(temp2299plotdata, aes(x= temperature2299, y = tempmutrat2299))+
  geom_point(size = 4, colour = "black")+
  geom_errorbar(aes(ymin = tempCI2299_lower, ymax = tempCI2299_upper), width = 0.2) +
  labs(
    title = "Wildtype Mutation Rates at 25ºC, 30ºC and 37ºC",
    y = "Mutation Rate (mutations per cell division)",
    x = "Temperature") +
  theme_classic()+
  annotate(geom = "text", x = "30ºC", y = 15.3, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "37ºC", y = 15.3, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "37ºC", y = 15, label = "***", fontface = "bold", size = 4, color = "grey")
temprateplot2299

tempmutrat3201 <- c(mutrate3201_25, mutrate3201_30, mutrate3201_37)
tempCI3201_lower <- c(3.4663256971248919136031, 10.0794557044245891574974, 7.8642960957306895508623)
tempCI3201_upper <- c(6.3367089878951379233740, 16.0674842933815291701194, 11.7424540511428894262735)
temperature3201 <- c("25ºC", "30ºC", "37ºC")
temp3201plotdata <- data.frame(tempmutrat3201, tempCI3201_lower, tempCI3201_upper, temperature3201)
temp3201plotdata

temprateplot3201 <- ggplot(temp3201plotdata, aes(x= temperature3201, y = tempmutrat3201))+
  geom_point(size = 4, colour = "black")+
  geom_errorbar(aes(ymin = tempCI3201_lower, ymax = tempCI3201_upper), width = 0.2) +
  labs(
    title = "V412L Mutation Rates at 25ºC, 30ºC and 37ºC",
    y = "Mutation Rate (mutations per cell division)",
    x = "Temperature") +
  theme_classic()+
  annotate(geom = "text", x = "30ºC", y = 16.5, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "37ºC", y = 16.5, label = "***", fontface = "bold", size = 4)
temprateplot3201

tempmutrat3188 <- c(mutrate3188_25, mutrate3188_30, mutrate3188_37)
tempCI3188_lower <- c(14.7819728237710101836910, 15.2992521781866841479314, 27.6323096688915121887931)
tempCI3188_upper <- c(23.4209192554162868304957, 23.9983811058684786132744, 37.9465213526321321069190)
temperature3188 <- c("25ºC", "30ºC", "37ºC")
temp3188plotdata <- data.frame(tempmutrat3188, tempCI3188_lower, tempCI3188_upper, temperature3188)
temp3188plotdata

temprateplot3188 <- ggplot(temp3188plotdata, aes(x= temperature3188, y = tempmutrat3188))+
  geom_point(size = 4, colour = "black")+
  geom_errorbar(aes(ymin = tempCI3188_lower, ymax = tempCI3188_upper), width = 0.2) +
  labs(
    title = "L425V Mutation Rates at 25ºC, 30ºC and 37ºC",
    y = "Mutation Rate (mutations per cell division)",
    x = "Temperature") +
  theme_classic()+
  annotate(geom = "text", x = "37ºC", y = 39, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "37ºC", y = 38.5, label = "***", fontface = "bold", size = 4, color = "grey")
temprateplot3188 


# DOUBLE MUTANT ANALYSIS 
# NEWTON.LD COMMAND LOOKS AT THE NUMBER OF MUTATIONS PER CULTURE, NOT THE MUTATION RATE!!!!!
# Determining if mutation rates between double mutants and controls are significant, using rSalvador package 
# install.packages("rSalvador") rSalvador is no longer available on CRAN, so will have to go through github 
install.packages("devtools")
devtools::install_github("eeeeeric/rSalvador", subdir = "rsalvador")
install.packages("hypergeo")
install.packages("gdata")
library(hypergeo)
library(gdata)
library("rsalvador")

doublemutants <- read_excel("DATA/DOUBLEMUTANTDATA.xlsx")
print(doublemutants)
view(doublemutants)

# filter for only strain2299
DMstrain2299 <- filter(doublemutants, Strain == "2299")
print(DMstrain2299)
DMstrain2299$Strain <- as.numeric(as.character(DMstrain2299$Strain))
str(DMstrain2299)
# now to filter variable, mutant count
mut2299 <- DMstrain2299$Mutant_Count
cells2299 <- DMstrain2299$YES_Count
CI2299 <- confint.LD(mut2299)
#estimating mutation rate 
mutrate2299 <- newton.LD(mut2299)
mutrate2299
# so this is saying that 2299 has an expected number of 2.084179 mutations per culture.

#filter for strain3201
DMstrain3201 <- filter(doublemutants, Strain == "3201")
print(DMstrain3201)
DMstrain3201$Strain <- as.numeric(as.character(DMstrain3201$Strain))
str(DMstrain3201)
# now to filter variable, mutant count
mut3201 <- DMstrain3201$Mutant_Count
cells3201 <- DMstrain3201$YES_Count
CI3201 <- confint.LD(mut3201)
#estimating mutation rate 
mutrate3201 <- newton.LD(mut3201)
mutrate3201
# now saying that 3201 has 10.88462 mutations per culture

#3188
DMstrain3188 <- filter(doublemutants, Strain == "3188")
print(DMstrain3188)
DMstrain3188$Strain <- as.numeric(as.character(DMstrain3188$Strain))
str(DMstrain3188)
# now to filter variable, mutant count
mut3188 <- DMstrain3188$Mutant_Count
cells3188 <- DMstrain3188$YES_Count
CI3188 <- confint.LD(mut3188)
#estimating mutation rate 
mutrate3188 <- newton.LD(mut3188)
mutrate3188
#12.501 mutations per culture

#3507 
DMstrain3507 <- filter(doublemutants, Strain == "3507")
print(DMstrain3507)
DMstrain3507$Strain <- as.numeric(as.character(DMstrain3507$Strain))
str(DMstrain3507)
# now to filter variable, mutant count
mut3507 <- DMstrain3507$Mutant_Count
cells3507 <- DMstrain3507$YES_Count
CI3507 <- confint.LD(mut3507)
#estimating mutation rate 
mutrate3507 <- newton.LD(mut3507)
mutrate3507
#0.02875048 mutations per culture

#3507 x 3188 
DMstrainX <- filter(doublemutants, Strain == "X")
print(DMstrainX)
DMstrainX$Strain <- as.numeric(as.character(DMstrainX$Strain))
str(DMstrainX)
# now to filter variable, mutant count
mutX <- DMstrainX$Mutant_Count
cellsX <- DMstrainX$YES_Count
CIX <- confint.LD(mutX)
#estimating mutation rate 
mutrateX <- newton.LD(mutX)
mutrateX
#0.05957417 mutations per culture

CI2299 <- confint.LD(mut2299)
plot.likelihood.LD(mut2299) # will be good to include in the github appendix 


#compare.LD(CI2299, CIX, init.m1 = mutrate2299, init.m2 = mutrateX)


strains <- c("wt", "V412L (h+//h-)", "L425V", "V412L (h+//h+)", "V412L (h+//h+) x L425V")
mutation_rates <- c(mutrate2299, mutrate3201, mutrate3188, mutrate3507, mutrateX)
CI <- c(CI2299, CI3201, CI3188, CI3507, CIX)
CI

CI_lower <- c(1.381725082, 8.740018747, 9.998648617, 0.001640550, 0.009907756)
CI_upper <- c(2.957398740, 13.201186474, 15.184396860, 0.126567182, 0.183872061)

ratedata <- data.frame(strains, mutation_rates, CI_lower, CI_upper)
ratedata

DMrateplot <- ggplot(ratedata, aes(x = strains, y = mutation_rates)) +
  geom_point(size = 4, color = "blue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  labs(
    title = "Double Mutant Mutation Rates with 95% Confidence Intervals",
    y = "Mutation Rate (mutations per cell division)",
    x = "Strains"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific)
DMrateplot


# comparing m for 2299 and 3201 
compare.LD(mut2299, mut3201, init.m1 = mutrate2299, init.m2 = mutrate3201)

#comparing m for 2299 and 3188
compare.LD(mut2299, mut3188, init.m1 = mutrate2299, init.m2 = mutrate3188)
options(digits = 22)  # increases precision of p value 
compare.LD(mut2299, mut3188)


# Pairwise comparisons of all strains so that they don't have to be manually imputted
mut_list <- list( 
  "2299" = mut2299,
  "3201" = mut3201,
  "3188" = mut3188,
  "3507" = mut3507,
  "X" = mutX
) # creates a dataset for all mutant counts and their strains

# Create an empty results data frame
results <- data.frame(
  strain1 = character(),
  strain2 = character(),
  LRT_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
) # creates a dataset for a table that the results of the lrt and p-values can be displayed in 

# Loop through all unique strain pairs
strain_names <- names(mut_list)
for (i in 1:(length(strain_names) - 1)) {
  for (j in (i + 1):length(strain_names)) {
    s1 <- strain_names[i]
    s2 <- strain_names[j]
    # creates a loop so that all of the different strains will be compared against each other, but only once 
    
    comp <- compare.LD(mut_list[[s1]], mut_list[[s2]]) # the lrt test 
    results <- rbind(results, data.frame( # adds the results from the lrt tests to the empty table dataframe
      Strain1 = s1,
      Strain2 = s2,
      LRT_statistic = comp[1],
      p_value = comp[2]
    ))
  }
}
library(dplyr) # uses the dplyr package
DMresults <- arrange(results, p_value) # inserts the p values into the table in numerical order- most significant to least signficiant) 
print(DMresults) # print the table so that results can be displayed

ratedata$strains <- factor(ratedata$strains, levels = c("wt", "V412L (h+//h-)", "L425V", "V412L (h+//h+)", "V412L (h+//h+) x L425V"))
ratedata$strains <- factor(ratedata$strains, levels = c(
  "wt", 
  "V412L (h+//h-)", 
  "L425V", 
  "V412L (h+//h+)", 
  "V412L (h+//h+) x L425V"
))


DMrateplot <- ggplot(ratedata, aes(x = strains, y = mutation_rates)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  labs(
    title = "Double Mutant Mutation Rates with 95% Confidence Intervals",
    y = "Mutation Rate (mutations per cell division)",
    x = "Strains"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::scientific) +
  annotate(geom = "text", x = "L425V", y = 1.7e+01, label = "***", fontface = "bold") +
  annotate(geom = "text", x = "V412L (h+//h-)", y = 1.7e+01, label = "***", fontface = "bold")+
  annotate(geom = "text", x = "V412L (h+//h+)", y = 1.7e+01, label = "***", fontface = "bold")+
  annotate(geom = "text", x = "V412L (h+//h+) x L425V", y = 1.7e+01, label = "***", fontface = "bold")+
  annotate(geom = "text", x = "V412L (h+//h+)", y = 1.6e+01, label = "***", fontface = "bold", color = "grey")+
  annotate(geom = "text", x = "V412L (h+//h+) x L425V", y = 1.6e+01, label = "***", fontface = "bold", color = "grey")+
  theme_classic()
DMrateplot
# black asterisk show significance against wt, and grey show significance against 3201


## CROSSES 
crosses <- read_excel("DATA/CROSSESDATA.xlsx")
print(crosses)
view(crosses)

# 2299C
Cstrain2299 <- filter(crosses, Strain == "2299")
print(Cstrain2299)
Cstrain2299$Strain <- as.numeric(as.character(Cstrain2299$Strain))
str(Cstrain2299)
# now to filter variable, mutant count
Cmut2299 <- Cstrain2299$Mutant_Count
Cmut2299
Ccells2299 <- Cstrain2299$YES_Count
CCI2299 <- confint.LD(Cmut2299)
CCI2299
#estimating mutation rate 
Cmutrate2299 <- newton.LD(Cmut2299)
Cmutrate2299

cap_value <- 1000
Cmut2299_capped <- pmin(Cmut2299, cap_value)
#estimating mutation rate 
Cmutrate2299 <- newton.LD(Cmut2299_capped, init.m = 0.1)
Cmutrate2299
CCI2299 <- confint.LD(Cmut2299_capped, init.m = 0.1)
CCI2299

#3201 
Cstrain3201 <- filter(crosses, Strain == "3201")
print(Cstrain3201)
Cstrain3201$Strain <- as.numeric(as.character(Cstrain3201$Strain))
str(Cstrain3201)
# now to filter variable, mutant count
Cmut3201 <- Cstrain3201$Mutant_Count
Cmut3201
Ccells3201 <- Cstrain3201$YES_Count
CCI3201 <- confint.LD(Cmut3201)
CCI3201
#estimating mutation rate 
Cmutrate3201 <- newton.LD(Cmut3201)
Cmutrate3201

cap_value <- 1000
Cmut3201_capped <- pmin(Cmut3201, cap_value)
#estimating mutation rate 
Cmutrate3201 <- newton.LD(Cmut3201_capped, init.m = 0.1)
Cmutrate3201
CCI3201 <- confint.LD(Cmut3201_capped, init.m = 0.1)
CCI3201

#3169 
Cstrain3169 <- filter(crosses, Strain == "3169")
print(Cstrain3169)
Cstrain3169$Strain <- as.numeric(as.character(Cstrain3169$Strain))
str(Cstrain3169)
# now to filter variable, mutant count
Cmut3169 <- Cstrain3169$Mutant_Count
Cmut3169
Ccells3169 <- Cstrain3169$YES_Count
CCI3169 <- confint.LD(Cmut3169)
CCI3169
#estimating mutation rate 
Cmutrate3169 <- newton.LD(Cmut3169)
Cmutrate3169

cap_value <- 1000
Cmut3169_capped <- pmin(Cmut3169, cap_value)
#estimating mutation rate 
Cmutrate3169 <- newton.LD(Cmut3169_capped, init.m = 0.1)
Cmutrate3169
CCI3169 <- confint.LD(Cmut3169_capped, init.m = 0.1)
CCI3169

#V3169 
CstrainV3169 <- filter(crosses, Strain == "22993169")
print(CstrainV3169)
CstrainV3169$Strain <- as.numeric(as.character(CstrainV3169$Strain))
str(CstrainV3169)
# now to filter variable, mutant count
CmutV3169 <- CstrainV3169$Mutant_Count
CmutV3169
CcellsV3169 <- CstrainV3169$YES_Count
CCIV3169 <- confint.LD(CmutV3169, init.m = 0.1)
CCIV3169
cap_value <- 1000
CmutV3169_capped <- pmin(CmutV3169, cap_value)
#estimating mutation rate 
CmutrateV3169 <- newton.LD(CmutV3169_capped, init.m = 0.1)
CmutrateV3169
CCIV3169 <- confint.LD(CmutV3169_capped, init.m = 0.1)
CCIV3169

#3285
Cstrain3285 <- filter(crosses, Strain == "3285")
print(Cstrain3285)
Cstrain3285$Strain <- as.numeric(as.character(Cstrain3285$Strain))
str(Cstrain3285)
# now to filter variable, mutant count
Cmut3285 <- Cstrain3285$Mutant_Count
Cmut3285
Ccells3285 <- Cstrain3285$YES_Count
CCI3285 <- confint.LD(Cmut3285, init.m = 1)
CCI3285
cap_value <- 1000
# Cap the values
Cmut3285_capped <- pmin(Cmut3285, cap_value)
# Sanity check to make sure that the cap value is less than 10000, and no new saturation values appear
summary(Cmut3285_capped)
#estimating mutation rate 
Cmutrate3285 <- newton.LD(Cmut3285_capped, init.m = 0.1)
Cmutrate3285

CCCI3285 <- confint.LD(Cmut3285_capped, init.m = 0.1)
CCCI3285

#V3285 
CstrainV3285 <- filter(crosses, Strain == "22993285")
print(CstrainV3285)
CstrainV3285$Strain <- as.numeric(as.character(CstrainV3285$Strain))
str(CstrainV3285)
# now to filter variable, mutant count
CmutV3285 <- CstrainV3285$Mutant_Count
CmutV3285
CcellsV3285 <- CstrainV3285$YES_Count
cap_value <- 1000
# Cap the values
CmutV3285_capped <- pmin(CmutV3285, cap_value)
# Sanity check to make sure that the cap value is less than 10000, and no new saturation values appear
summary(CmutV3285_capped)
#estimating mutation rate 
CmutrateV3285 <- newton.LD(CmutV3285_capped, init.m = 0.1)
CmutrateV3285
CCCIV3285 <- confint.LD(CmutV3285_capped, init.m = 0.1)
CCCIV3285

#3061 
Cstrain3061 <- filter(crosses, Strain == "3061")
print(Cstrain3061)
Cstrain3061$Strain <- as.numeric(as.character(Cstrain3061$Strain))
str(Cstrain3061)
# now to filter variable, mutant count
Cmut3061 <- Cstrain3061$Mutant_Count
Cmut3061
Ccells3061 <- Cstrain3061$YES_Count
cap_value <- 1000
# Cap the values
Cmut3061_capped <- pmin(Cmut3061, cap_value)
# Sanity check to make sure that the cap value is less than 10000, and no new saturation values appear
summary(Cmut3061_capped)
#estimating mutation rate 
Cmutrate3061 <- newton.LD(Cmut3061_capped, init.m = 0.1)
Cmutrate3061
CCCI3061 <- confint.LD(Cmut3061_capped, init.m = 0.1)
CCCI3061
# IT'S LIKELY FOR ALL OF THESE STRAINS THERE IS AN UNDERESTIMATE FOR THE MUTATION RATE AS THE DATA WAS TOO SATURATED AS A RESULT OF 
# OF MANY STRAINS HAVING TOO MANY COLONIES TO COUNT. IMPROVEMENT WOULD BE TO DO FURTHER DILUTIONS TO GET A MORE ACCURATE ESTIMATE 
# OF THE MUTATION RATE THAT DID NOT REQUIRE CAPPING, SO THAT THE FULL DISTRIBUTION OF COLONIES AND MUTATIONS CAN BE ANALYSED 

# V3061 
CstrainV3061 <- filter(crosses, Strain == "22993061")
print(CstrainV3061)
CstrainV3061$Strain <- as.numeric(as.character(CstrainV3061$Strain))
str(CstrainV3061)
# now to filter variable, mutant count
CmutV3061 <- CstrainV3061$Mutant_Count
CmutV3061
CcellsV3061 <- CstrainV3061$YES_Count
cap_value <- 1000
# Cap the values
CmutV3061_capped <- pmin(CmutV3061, cap_value)
# Sanity check to make sure that the cap value is less than 10000, and no new saturation values appear
summary(CmutV3061_capped)
#estimating mutation rate 
CmutrateV3061 <- newton.LD(CmutV3061_capped, init.m = 0.1)
CmutrateV3061
CCCIV3061 <- confint.LD(CmutV3061_capped, init.m = 0.1)
CCCIV3061

# DO THE COMPARISON AND MAKE A PLOT TO SHOW DATA THEN DATA ANALYSIS IS DONE 
# Pairwise comparisons of all strains so that they don't have to be manually imputted
str(mut_list[[s1]])
str(mut_list[[s2]])

str(Cmut_list[[1]])

Cmut_list <- list( 
  "2299" = Cmut2299_capped,
  "3201" = Cmut3201_capped,
  "3285" = Cmut3285_capped,
  "V3285" = CmutV3285_capped
) # creates a dataset for all mutant counts and their strains

# Create an empty results data frame
Cresults <- data.frame(
  Cstrain1 = character(),
  Cstrain2 = character(),
  CLRT_statistic = numeric(),
  Cp_value = numeric(),
  stringsAsFactors = FALSE
) # creates a dataset for a table that the results of the lrt and p-values can be displayed in 

# Loop through all unique strain pairs
Cstrain_names <- names(Cmut_list)
for (i in 1:(length(Cstrain_names) - 1)) {
  for (j in (i + 1):length(Cstrain_names)) {
    s1 <- Cstrain_names[i]
    s2 <- Cstrain_names[j]
    # creates a loop so that all of the different strains will be compared against each other, but only once 
    
    comp <- compare.LD(Cmut_list[[s1]], Cmut_list[[s2]]) # the lrt test 
    Cresults <- rbind(Cresults, data.frame( # adds the results from the lrt tests to the empty table dataframe
      Strain1 = s1,
      Strain2 = s2,
      CLRT_statistic = comp[1],
      Cp_value = comp[2]
    ))
  }
}
library(dplyr) # uses the dplyr package
Cresults <- arrange(Cresults, Cp_value) # inserts the p values into the table in numerical order- most significant to least signficiant) 
print(Cresults)


# alter data names
Cstrains <- c("wt", "V412L (h+//h-)", "Δmsh2", "V412L x Δmsh2")
Cmutation_rates <- c(Cmutrate2299, Cmutrate3201, Cmutrate3285, CmutrateV3285)
CCI <- c(CCI2299, CCI3201, CCI3169, CCIV3169, CCCI3285, CCCIV3285, CCCI3061, CCCIV3061)
CCI

CCI2299
CCI3201
CCCI3285
CCCIV3285

CCI_lower <- c(4.542663043191482508121, 4.800130440730624670209, 166.5831301982861702982, 166.5831301982861702982)
CCI_upper <- c(8.278463735147029822770, 8.662684303170765431901, 201.2553698408895854755, 201.2553698408895854755)
Cratedata <- data.frame(Cstrains, Cmutation_rates, CCI_lower, CCI_upper)
Cratedata

Crateplot <- ggplot(Cratedata, aes(x = fct_reorder(Cstrains, Cmutation_rates), y = Cmutation_rates)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = CCI_lower, ymax = CCI_upper), width = 0.2) +
  labs(
    title = " V412L-Δmsh2 Mutation Rates with 95% Confidence Intervals",
    y = "Mutation Rate (mutations per cell division)",
    x = "Strains") +
  theme_classic()+
  annotate(geom = "text", x = "Δmsh2", y = 220, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "V412L x Δmsh2", y = 220, label = "***", fontface = "bold", size = 4)+
  annotate(geom = "text", x = "Δmsh2", y = 215, label = "***", fontface = "bold", size = 4, color = "grey")+
  annotate(geom = "text", x = "V412L x Δmsh2", y = 215, label = "***", fontface = "bold", size = 4, color = "grey")
Crateplot



