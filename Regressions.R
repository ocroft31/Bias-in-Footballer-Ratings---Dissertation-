library(tidyverse)
library(haven)
library(estimatr)
library(plm)
library(readxl)
library(summarytools)
library(ggplot2)
library(sandwich)
library(lmtest)
library(miceadds)
library(rifreg)
library(gridExtra)

path <- 'C:/Users/Owen/Desktop/University/Research/Dissertation/Data Analysis'
setwd(path)

rm(list = ls())

bias_data <- read_dta("Racial_Bias_EER_dataset.dta") %>%
  mutate(
    ROLE = as_factor(ROLE),
    id_name = as_factor(id_name),
    SeasonStart = as_factor(SeasonStart),
    citizen = as_factor(citizen),
    TEAM = as_factor(TEAM)
  )


colnames(bias_data)

dependent_vars <- c("rating_AVG", "rating_G", "rating_C", "rating_T", "lnwage")
vars_of_interest <- c("rating_AVG", "rating_G", "rating_C", "rating_T", "lnwage", "Dblack")
independent_vars <- c("Dblack", "age", "age2", "Goals", "Assists", "Yel", "Red", "SpG",
                      "PS", "Aerialswon", "Tackles", "Interceptions", "Fouls", "Offsidesdefensive", "Clear", 
                      "Drbdefensive", "Blocks", "KeyP", "Drboffensive", "Fouled", "Off", "UnsTch", "AvgP", 
                      "Crosses", "LongB", "ThrB", "OffTarget", "OnPost", "OnTarget", "Blocked", "Dispossessed", 
                      "InAccCr", "InAccCrn", "TEAM", "ROLE", "SeasonStart")

##### Main Results #####
# Summary Statistics


# Mean Ratings by Groups
  t_test_1 <- t.test(bias_data[bias_data$Dblack == 1, "rating_AVG"], bias_data[bias_data$Dblack == 0, "rating_AVG"])
  print(t_test_1)
  t_test_2 <- t.test(bias_data[bias_data$Dblack == 1, "rating_C"], bias_data[bias_data$Dblack == 0, "rating_C"])
  print(t_test_2)
  t_test_3 <- t.test(bias_data[bias_data$Dblack == 1, "rating_G"], bias_data[bias_data$Dblack == 0, "rating_G"])
  print(t_test_3)
  t_test_4 <- t.test(bias_data[bias_data$Dblack == 1, "rating_T"], bias_data[bias_data$Dblack == 0, "rating_T"])
  print(t_test_4)
  t_test_5 <- t.test(bias_data[bias_data$Dblack == 1, "lnwage"], bias_data[bias_data$Dblack == 0, "lnwage"])
  print(t_test_5)
# OLS Estimates
  tb3_form1 <- as.formula(paste('rating_AVG', '~', paste(independent_vars, collapse = '+')))
  tb3_mod1 <- lm.cluster(tb3_form1, cluster = 'id_name', data = bias_data)
  
  summary(tb3_mod1)
  
  tb3_form2 <- as.formula(paste('rating_G', '~', paste(independent_vars, collapse = '+')))
  tb3_mod2 <- lm.cluster(tb3_form2, cluster = 'id_name', data = bias_data)
  
  summary(tb3_mod2)
  
  tb3_form3 <- as.formula(paste('rating_C', '~', paste(independent_vars, collapse = '+')))
  tb3_mod3 <- lm.cluster(tb3_form3, cluster = 'id_name', data = bias_data)
  
  summary(tb3_mod3)
  
  tb3_form4 <- as.formula(paste('rating_T', '~', paste(independent_vars, collapse = '+')))
  tb3_mod4 <- lm.cluster(tb3_form4, cluster = 'id_name', data = bias_data)
  
  summary(tb3_mod4)

  tb3_form5 <- as.formula(paste('lnwage', '~', paste(independent_vars, collapse = '+')))
  tb3_mod5 <- lm.cluster(tb3_form5, cluster = 'id_name', data = bias_data)
  
  summary(tb3_mod5)
  

##### Sensitivity Analysis Sub samples ##### 
  
bias_data$DItalian <- ifelse(bias_data$citizen == 'italian', 1, 0) # Creating dummy variable

  #Subsample - Non-Black - Parameter - Italian - Newspaper - All
  NonBlack_Italian_All <- bias_data %>% 
    filter(Dblack == 0) %>%
    lm.cluster(rating_AVG ~ DItalian + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonBlack_Italian_All)
  
  filtered_NonBlack_Italian_All <- bias_data %>%
    filter(Dblack == 0)

  print(nrow(filtered_NonBlack_Italian_All))
  
  #Subsample - Non-Black - Parameter - Italian - Newspaper - Gazzetta
  NonBlack_Italian_Gazzetta <- bias_data %>% 
    filter(Dblack == 0) %>%
    lm.cluster(rating_G ~ DItalian + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonBlack_Italian_Gazzetta)

  #Subsample - Non-Black - Parameter - Italian - Newspaper - Corriere
  NonBlack_Italian_Corriere <- bias_data %>% 
    filter(Dblack == 0) %>%
    lm.cluster(rating_C ~ DItalian + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonBlack_Italian_Corriere)

  #Subsample - Non-Black - Parameter - Italian - Newspaper - Tutto
  NonBlack_Italian_Tutto <- bias_data %>% 
    filter(Dblack == 0) %>%
    lm.cluster(rating_T ~ DItalian + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonBlack_Italian_Tutto)

#Sample 2
  
  filtered_NonItalian_Black_All <- bias_data %>%
    filter(citizen != 'italian')
  
  print(nrow(filtered_NonItalian_Black_All))
  
  #Subsample - Italian - Parameter - Black - Newspaper - All
  NonItalian_Black_All <- bias_data %>% 
    filter(citizen != 'italian') %>%
    lm.cluster(rating_AVG ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonItalian_Black_All)

  #Subsample - Italian - Parameter - Black - Newspaper - Gazzetta
  NonItalian_Black_Gazzetta <- bias_data %>% 
    filter(citizen != 'italian') %>%
    lm.cluster(rating_G ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonItalian_Black_Gazzetta)
  
  #Subsample - Italian - Parameter - Black - Newspaper - Corriere
    NonItalian_Black_Corriere <- bias_data %>% 
    filter(citizen != 'italian') %>%
    lm.cluster(rating_C ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonItalian_Black_Corriere)
  
  #Subsample - Italian - Parameter - Black - Newspaper - Tutto
  NonItalian_Black_Tutto <- bias_data %>% 
    filter(citizen != 'italian') %>%
    lm.cluster(rating_T ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
  summary(NonItalian_Black_Tutto)
  
  
# Sample 3
  
  filtered_Italian_Black_All <- bias_data %>%
    filter(citizen == 'italian')
  
  print(nrow(filtered_Italian_Black_All))
  
  #Subsample - Non-Italian - Parameter - Black - Newspaper - All
  Italian_Black_All <- bias_data %>%
    filter(citizen == 'italian') %>%
    lm.cluster(rating_AVG ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                 PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                 Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                 Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                 InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
    summary(Italian_Black_All)
    
    #Subsample - Non-Italian - Parameter - Black - Newspaper - Gazzetta
    Italian_Black_Gazzetta <- bias_data %>%
      filter(citizen == 'italian') %>%
      lm.cluster(rating_G ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                   PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                   Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                   Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                   InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
    summary(Italian_Black_Gazzetta)
    
    #Subsample - Non-Italian - Parameter - Black - Newspaper - Corriere
    Italian_Black_Corriere <- bias_data %>%
      filter(citizen == 'italian') %>%
      lm.cluster(rating_C ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                   PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                   Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                   Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                   InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
    summary(Italian_Black_Corriere)
    
    #Subsample - Non-Italian - Parameter - Black - Newspaper - Tutto
    Italian_Black_Tutto <- bias_data %>%
      filter(citizen == 'italian') %>%
      lm.cluster(rating_T ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
                   PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
                   Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
                   Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
                   InAccCr + InAccCrn + TEAM + ROLE + SeasonStart, cluster = 'id_name', data = .)
    summary(Italian_Black_Tutto)
    
    
Black_Italian_Count <- bias_data %>%
  filter(Dblack == 1 & citizen == 'italian') %>%
  distinct(id_name) %>%
  nrow()
print(Black_Italian_Count)





#### Sensitivity Analysis 'Objective Rating' ####
objective_rating <- as.formula(paste('lnwage', '~', paste(independent_vars, collapse = '+')))
tb3_mod5 <- lm.cluster(tb3_form5, cluster = 'id_name', data = bias_data)


###### Ratings by Position ######

#Figuring out which roles which
  summary_position <- bias_data %>%
    group_by(ROLE) %>%
    summarize(mean_goals = mean(Goals),
              mean_tackles = mean(Tackles),
              mean_clear = mean(Clear))
  
  print(summary_position)

#Proper Coding of Positions
bias_data <- bias_data %>%
  mutate(ROLE = case_when(
    ROLE == 'A' ~ 'Forward',
    ROLE == 'C' ~ 'Midfielder',
    ROLE == 'D' ~ 'Defender'
  ))

filtered_Forward_All <- bias_data %>%
  filter(ROLE == 'Forward')

print(nrow(filtered_Forward_All))

filtered_Midfielder_All <- bias_data %>%
  filter(ROLE == 'Midfielder')

print(nrow(filtered_Midfielder_All))

filtered_Defender_All <- bias_data %>%
  filter(ROLE == 'Defender')

print(nrow(filtered_Defender_All))

#Subsample - Forward - Parameter - Black - All
Black_Forward_All <- bias_data %>% 
  filter(ROLE == 'Forward') %>%
  lm.cluster(rating_AVG ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_All)

#Subsample - Forward - Parameter - Black - Gazzetta
Black_Forward_Gazzetta <- bias_data %>% 
  filter(ROLE == 'Forward') %>%
  lm.cluster(rating_G ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_Gazzetta)

#Subsample - Forward - Parameter - Black - Corriere 
Black_Forward_Corriere <- bias_data %>% 
  filter(ROLE == 'Forward') %>%
  lm.cluster(rating_C ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_Corriere)

#Subsample - Forward - Parameter - Black - Tutto
Black_Forward_Tutto <- bias_data %>% 
  filter(ROLE == 'Forward') %>%
  lm.cluster(rating_T ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_Tutto)



#Subsample - Midfielders - Parameter - Black - All
Black_Midfielder_All <- bias_data %>% 
  filter(ROLE == 'Midfielder') %>%
  lm.cluster(rating_AVG ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Midfielder_All)

#Subsample - Midfielders - Parameter - Black - Gazzetta
Black_Midfielder_Gazzetta <- bias_data %>% 
  filter(ROLE == 'Midfielder') %>%
  lm.cluster(rating_G ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Midfielder_Gazzetta)

#Subsample - Midfielders - Parameter - Black - Corriere 
Black_Midfielder_Corriere <- bias_data %>% 
  filter(ROLE == 'Midfielder') %>%
  lm.cluster(rating_C ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Midfielder_Corriere)

#Subsample - Midfielders - Parameter - Black - Tutto
Black_Midfielder_Tutto <- bias_data %>% 
  filter(ROLE == 'Midfielder') %>%
  lm.cluster(rating_T ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Midfielder_Tutto)



#Subsample - Defenders - Parameter - Black - All
Black_Defender_All <- bias_data %>% 
  filter(ROLE == 'Defender') %>%
  lm.cluster(rating_AVG ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_All)

#Subsample - Defenders - Parameter - Black - Gazzetta
Black_Defender_Gazzetta <- bias_data %>% 
  filter(ROLE == 'Defender') %>%
  lm.cluster(rating_G ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Defender_Gazzetta)

#Subsample - Defenders - Parameter - Black - Corriere 
Black_Defender_Corriere <- bias_data %>% 
  filter(ROLE == 'Defender') %>%
  lm.cluster(rating_C ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Defender_Corriere)

#Subsample - Defenders - Parameter - Black - Tutto
Black_Defender_Tutto <- bias_data %>% 
  filter(ROLE == 'Defender') %>%
  lm.cluster(rating_T ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Defender_Tutto)

#Subsample - Forwards - Parameter - Black - Wage
Black_Forward_Wage <- bias_data %>% 
  filter(ROLE == 'Forward') %>%
  lm.cluster(lnwage ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Forward_Wage)

#Subsample - Midfielders - Parameter - Black - Wage
Black_Midfielder_Wage <- bias_data %>% 
  filter(ROLE == 'Midfielder') %>%
  lm.cluster(lnwage ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Midfielder_Wage)

#Subsample - Defenders - Parameter - Black - Wage
Black_Defender_Wage <- bias_data %>% 
  filter(ROLE == 'Defender') %>%
  lm.cluster(lnwage ~ Dblack + age + age2 + Goals + Assists + Yel + Red + SpG +
               PS + Aerialswon + Tackles + Interceptions + Fouls + Offsidesdefensive + Clear + 
               Drbdefensive + Blocks + KeyP + Drboffensive + Fouled + Off + UnsTch + AvgP + 
               Crosses + LongB + ThrB + OffTarget + OnPost + OnTarget + Blocked + Dispossessed + 
               InAccCr + InAccCrn + TEAM + SeasonStart, cluster = 'id_name', data = .)
summary(Black_Defender_Wage)




#### Data set accounting for 0 ratings ####
bias_data_without_zero <- bias_data %>%
  mutate(across(c('rating_G', 'rating_C', 'rating_T'), ~na_if(., 0)))

bias_data_without_zero <- bias_data_without_zero %>%
  mutate(rating_AVG = (rating_G + rating_C + rating_T)/3)

print(sum(is.na(bias_data_without_zero$rating_AVG)))

without_zeros1 <- as.formula(paste('rating_AVG', '~', paste(independent_vars, collapse = '+')))
without_zeros_model1 <- lm.cluster(without_zeros1, cluster = 'id_name', data = bias_data_without_zero)
summary(without_zeros_model1)

without_zeros2 <- as.formula(paste('rating_G', '~', paste(independent_vars, collapse = '+')))
without_zeros_model2 <- lm.cluster(without_zeros2, cluster = 'id_name', data = bias_data_without_zero)
summary(without_zeros_model2)

without_zeros3 <- as.formula(paste('rating_C', '~', paste(independent_vars, collapse = '+')))
without_zeros_model3 <- lm.cluster(without_zeros3, cluster = 'id_name', data = bias_data_without_zero)
summary(without_zeros_model3)

without_zeros4 <- as.formula(paste('rating_T', '~', paste(independent_vars, collapse = '+')))
without_zeros_model4 <- lm.cluster(without_zeros4, cluster = 'id_name', data = bias_data_without_zero)
summary(without_zeros_model4)

without_zeros5 <- as.formula(paste('lnwage', '~', paste(independent_vars, collapse = '+')))
without_zeros_model5 <- lm.cluster(without_zeros5, cluster = 'id_name', data = bias_data_without_zero)
summary(without_zeros_model5)












