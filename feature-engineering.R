# Load packages
library(pastecs)
library(skimr)
library(tidyverse)
library(car)
library(rpart.plot)
library(rattle)

# Casey Neubauer Resubmission for Assignment 3 and Assignment 4

################
# Assignment 03 -----------------------------------------------------------
################

# Pull in VS10 dataset as a dataframe
VS10 <- read.csv("VS10.csv")
vs10 <- as.data.frame(VS10) %>% select(-CSA2010)
options(max.print=1000000)

# Write to CSV for easier viewing to find top 6 correlations
export <- as.data.frame(round(cor(vs10),2))
write_csv(export, "view.csv")

# Select six variables with highest correlation
data <- select(vs10,
               "ID",
               owner_occ = "Percentage.of.Housing.Units.that.are.Owner.Occupied",
               vacant = "Percentage.of.Residential.Properties.that.are.Vacant.and.Abandoned",
               hs_absent = "Percent.of.9th.12th.Grade.Students.that.are.Chronically.Absent..Missing.at.least.20.days.",
               low_inc = "Percent.of.Households.Earning.Less.than..25.000",
               med_inc = "Median.Household.Income",
               employed = "Percent.Population.16.64.Employed",
               vio_crime = "Violent.Crime.Rate.per.1.000.Residents")

# Normalize (breaks at 0, minus 1 std dev, mean, plus 1 std dev, Inf)
stat.desc(data$owner_occ)
oo_brks <- c(0, 
             (mean(data$owner_occ) - sd(data$owner_occ)), 
             mean(data$owner_occ), 
             (mean(data$owner_occ) + sd(data$owner_occ)), 
             Inf)
data$oo_rng <- cut(data$owner_occ, breaks = oo_brks, include.lowest = T)
summary(data$oo_rng)

stat.desc(data$vacant)
v_brks <- c(0, 
            (mean(data$vacant) - sd(data$vacant)), 
            mean(data$vacant), 
            (mean(data$vacant) + sd(data$vacant)), 
            Inf)
data$v_rng <- cut(data$vacant, breaks = v_brks, include.lowest = T)
summary(data$v_rng)

stat.desc(data$hs_absent)
hs_brks <- c(0, 
             (mean(data$hs_absent) - sd(data$hs_absent)), 
             mean(data$hs_absent), 
             (mean(data$hs_absent) + sd(data$hs_absent)), 
             Inf)
data$hs_rng <- cut(data$hs_absent, breaks = hs_brks, include.lowest = T)
summary(data$hs_rng)

stat.desc(data$low_inc)
low_brks <- c(0, 
              (mean(data$low_inc) - sd(data$low_inc)), 
              mean(data$low_inc), 
              (mean(data$low_inc) + sd(data$low_inc)), 
              Inf)
data$low_rng <- cut(data$low_inc, breaks = low_brks, include.lowest = T)
summary(data$low_rng)

stat.desc(data$med_inc)
med_brks <- c(0, 
              (mean(data$med_inc) - sd(data$med_inc)), 
              mean(data$med_inc), 
              (mean(data$med_inc) + sd(data$med_inc)), 
              Inf)
data$med_rng <- cut(data$med_inc, breaks = med_brks, include.lowest = T)
summary(data$med_rng)

stat.desc(data$employed)
emp_brks <- c(0, 
              (mean(data$employed) - sd(data$employed)), 
              mean(data$employed), 
              (mean(data$employed) + sd(data$employed)), 
              Inf)
data$emp_rng <- cut(data$employed, breaks = emp_brks, include.lowest = T)
summary(data$emp_rng)

# Assign scores based on breakpoints (1-4 each)
data <- data %>% mutate(score_one =
                          case_when(oo_rng == "[0,42.9]" ~ 1,
                                    oo_rng == "(42.9,60.3]" ~ 2,
                                    oo_rng == "(60.3,77.6]" ~ 3,
                                    oo_rng == "(77.6,Inf]" ~ 4))

data <- data %>% mutate(score_two =
                          case_when(v_rng == "[-2.99,0]" ~ 1,
                                    v_rng == "(0,7.72]" ~ 2,
                                    v_rng == "(7.72,18.4]" ~ 3,
                                    v_rng == "(18.4,Inf]" ~ 4))

data <- data %>% mutate(score_three =
                          case_when(hs_rng == "[0,28.5]" ~ 1,
                                    hs_rng == "(28.5,37.8]" ~ 2,
                                    hs_rng == "(37.8,47.1]" ~ 3,
                                    hs_rng == "(47.1,Inf]" ~ 4))

data <- data %>% mutate(score_four =
                          case_when(low_rng == "[0,20.7]" ~ 1,
                                    low_rng == "(20.7,34.1]" ~ 2,
                                    low_rng == "(34.1,47.6]" ~ 3,
                                    low_rng == "(47.6,Inf]" ~ 4))

data <- data %>% mutate(score_five =
                          case_when(med_rng == "[0,2.48e+04]" ~ 4,
                                    med_rng == "(2.48e+04,4.19e+04]" ~ 3,
                                    med_rng == "(4.19e+04,5.9e+04]" ~ 2,
                                    med_rng == "(5.9e+04,Inf]" ~ 1))

data <- data %>% mutate(score_six =
                          case_when(emp_rng == "[0,51.7]" ~ 4,
                                    emp_rng == "(51.7,62.7]" ~ 3,
                                    emp_rng == "(62.7,73.8]" ~ 2,
                                    emp_rng == "(73.8,Inf]" ~ 1))

# Index (add score_one through score_six for index -- max of 24)
data <- data %>% rowwise() %>%
  mutate(index = sum(c_across(score_one:score_six)))

# Select original variables, index, and violent crime variables
index_data <- select(data, ID, owner_occ, vacant, hs_absent,
                     low_inc, med_inc, employed, index, vio_crime)


# Find descriptive statistics
stat.desc(index_data)

# Find correlations
round(cor(index_data),2)


################
# Assignment 04 --------------------------------------------------------------
################

# Prepare feature index to add to vs10 data
add_index <- select(index_data, ID, index)

# Join my feature index to vs10, remove part 1 crime data
tdata <- full_join(vs10, add_index) 

# Change name of violent crime variable for simplicity
names(tdata)[names(tdata) == "Violent.Crime.Rate.per.1.000.Residents"] <- "vio_crime"

# Check out distribution
hist(tdata$vio_crime,
     freq = FALSE,
     breaks = 30,
     col = "red",
     xlab = "Crime",
     main = "Histogram")

# Skim violent crime data to create quartiles
skim(tdata$vio_crime)

# Add variable "vcat" with violent crime quartile rank by neighborhood
tdata$vcat[tdata$vio_crime >= 22.3] <- "4th"
tdata$vcat[tdata$vio_crime >= 15.2 & tdata$vio_crime < 22.3] <- "3rd"
tdata$vcat[tdata$vio_crime >= 9.51 & tdata$vio_crime < 15.2] <- "2rd"
tdata$vcat[tdata$vio_crime < 9.51] <- "1st"

# Confirm distribution makes sense with a table
table(tdata$vcat)

# Remove ID and Violent Crime variables in prep for decision tree
violence <- tdata[c(2:18, 20:70, 72:73)]

# Create the model with rpart
fitv <- rpart(violence$vcat ~ .,
             data = violence,
             method = "class")

# Plot
windows()
rpart.plot::prp(fitv, type = 1, extra = 101, box.palette = "auto", branch.lty = 3,
                varlen = 0, shadow.col = "gray", nn = TRUE, fallen.leaves = TRUE)

# CP values/plot
printcp(fitv)
plotcp(fitv)

# Rules
rpart.rules(fitv, style = "tall", cover = TRUE, nn = TRUE, roundint = TRUE)

# Prepare dataframe for matrix and error rate
em_vcat <- 
  data.frame(violence$vcat,
             predict(fitv, type = "class"))

# Complexity matrix
rtab <- table(em_vcat)
rtab

# Overall error rate
sum(diag(rtab)) / sum(rtab)

# Prune with minsplit = 10
fitv_pruned <- rpart(violence$vcat ~ .,
              data = violence,
              method = "class",
              control = rpart.control(minsplit = 10)) 

# CP values/plot
printcp(fitv_pruned)
plotcp(fitv_pruned)

# Plot
windows()
rpart.plot::prp(fitv_pruned, type = 1, extra = 101, box.palette = "auto", branch.lty = 3,
                varlen = 0, shadow.col = "gray", nn = TRUE, fallen.leaves = TRUE)

# Prepare dataframe for pruned matrix and error rate
em_vcat_pruned <- 
  data.frame(violence$vcat,
             predict(fitv_pruned, type = "class"))

# Complexity matrix
rptab <- table(em_vcat_pruned)
rptab

# Overall error rate
sum(diag(rptab)) / sum(rptab)

##############
# Question 4 #
##############

# Pull in 2015 dataset
VS15 <- read.csv("VS15.csv")
vs15 <- as.data.frame(VS15) %>% select(-CSA2010)
vs15$Median.Price.of.Homes.Sold <- as.numeric(vs15$Median.Price.of.Homes.Sold)
vs15$Percent.of.Households.Earning..40.000.to..60.000 <- as.numeric(vs15$Percent.of.Households.Earning..40.000.to..60.000)

# Select original variables 
data2 <- select(vs15,
               "ID",
               owner_occ = "Percentage.of.Housing.Units.that.are.Owner.Occupied",
               vacant = "Percentage.of.Residential.Properties.that.are.Vacant.and.Abandoned",
               hs_absent = "Percent.of.9th.12th.Grade.Students.that.are.Chronically.Absent..Missing.at.least.20.days.",
               low_inc = "Percent.of.Households.Earning.Less.than..25.000",
               med_inc = "Median.Household.Income",
               employed = "Percent.Population.16.64.Employed",
               vio_crime = "Violent.Crime.Rate.per.1.000.Residents")

# Normalize (breaks at 0, minus 1 std dev, mean, plus 1 std dev, Inf)
stat.desc(data2$owner_occ)
oo_brks2 <- c(0, 
             (mean(data2$owner_occ) - sd(data2$owner_occ)), 
             mean(data2$owner_occ), 
             (mean(data2$owner_occ) + sd(data2$owner_occ)), 
             Inf)
data2$oo_rng2 <- cut(data2$owner_occ, breaks = oo_brks2, include.lowest = T)
summary(data2$oo_rng)

stat.desc(data2$vacant)
v_brks2 <- c(0, 
            (mean(data2$vacant) - sd(data2$vacant)), 
            mean(data2$vacant), 
            (mean(data2$vacant) + sd(data2$vacant)), 
            Inf)
data2$v_rng2 <- cut(data2$vacant, breaks = v_brks2, include.lowest = T)
summary(data2$v_rng)

stat.desc(data2$hs_absent)
hs_brks2 <- c(0, 
             (mean(data2$hs_absent) - sd(data2$hs_absent)), 
             mean(data2$hs_absent), 
             (mean(data2$hs_absent) + sd(data2$hs_absent)), 
             Inf)
data2$hs_rng2 <- cut(data2$hs_absent, breaks = hs_brks2, include.lowest = T)
summary(data2$hs_rng)

stat.desc(data2$low_inc)
low_brks2 <- c(0, 
              (mean(data2$low_inc) - sd(data2$low_inc)), 
              mean(data2$low_inc), 
              (mean(data2$low_inc) + sd(data2$low_inc)), 
              Inf)
data2$low_rng2 <- cut(data2$low_inc, breaks = low_brks2, include.lowest = T)
summary(data2$low_rng)

stat.desc(data2$med_inc)
med_brks2 <- c(0, 
              (mean(data2$med_inc) - sd(data2$med_inc)), 
              mean(data2$med_inc), 
              (mean(data2$med_inc) + sd(data2$med_inc)), 
              Inf)
data2$med_rng2 <- cut(data2$med_inc, breaks = med_brks2, include.lowest = T)
summary(data2$med_rng)

stat.desc(data2$employed)
emp_brks2 <- c(0, 
              (mean(data2$employed) - sd(data2$employed)), 
              mean(data2$employed), 
              (mean(data2$employed) + sd(data2$employed)), 
              Inf)
data2$emp_rng2 <- cut(data2$employed, breaks = emp_brks2, include.lowest = T)
summary(data2$emp_rng)

# Scores
data2 <- data2 %>% mutate(score_one2 =
                          case_when(oo_rng2 == "[0,38.8]" ~ 1,
                                    oo_rng2 == "(38.8,56.1]" ~ 2,
                                    oo_rng2 == "(56.1,73.4]" ~ 3,
                                    oo_rng2 == "(73.4,Inf]" ~ 4))

data2 <- data2 %>% mutate(score_two2 =
                          case_when(v_rng2 == "[-2.1,0]" ~ 1,
                                    v_rng2 == "(0,7.76]" ~ 2,
                                    v_rng2 == "(7.76,17.6]" ~ 3,
                                    v_rng2 == "(17.6,Inf]" ~ 4))

data2 <- data2 %>% mutate(score_three2 =
                          case_when(hs_rng2 == "[0,28]" ~ 1,
                                    hs_rng2 == "(28,37.7]" ~ 2,
                                    hs_rng2 == "(37.7,47.3]" ~ 3,
                                    hs_rng2 == "(47.3,Inf]" ~ 4))

data2 <- data2 %>% mutate(score_four2 =
                          case_when(low_rng2 == "[0,20.1]" ~ 1,
                                    low_rng2 == "(20.1,33.4]" ~ 2,
                                    low_rng2 == "(33.4,46.7]" ~ 3,
                                    low_rng2 == "(46.7,Inf]" ~ 4))

data2 <- data2 %>% mutate(score_five2 =
                          case_when(med_rng2 == "[0,2.43e+04]" ~ 4,
                                    med_rng2 == "(2.43e+04,4.52e+04]" ~ 3,
                                    med_rng2 == "(4.52e+04,6.61e+04]" ~ 2,
                                    med_rng2 == "(6.61e+04,Inf]" ~ 1))

data2 <- data2 %>% mutate(score_six2 =
                          case_when(emp_rng2 == "[0,49.5]" ~ 4,
                                    emp_rng2 == "(49.5,60.9]" ~ 3,
                                    emp_rng2 == "(60.9,72.4]" ~ 2,
                                    emp_rng2 == "(72.4,Inf]" ~ 1))

# Index
data2 <- data2 %>% rowwise() %>%
  mutate(index2 = sum(c_across(score_one2:score_six2)))

# Select original variables, index, and violent crime variables
index_data2 <- select(data2, ID, owner_occ, vacant, hs_absent,
                     low_inc, med_inc, employed, index2, vio_crime)



##############################################

# 10/23
# Find descriptive statistics
stat.desc(index_data)

# Find correlations
round(cor(index_data),2)

################
# Assignment 04 --------------------------------------------------------------
################

add_index <- select(index_data, ID, index)

tdata <- full_join(vs15, add_index) %>% 
  select(-Part.1.Crime.Rate.per.1.000.Residents)

names(tdata)[names(tdata) == "Violent.Crime.Rate.per.1.000.Residents"] <- "vio_crime"

glimpse(tdata)

# Check out distribution
hist(tdata$vio_crime,
     freq = FALSE,
     breaks = 30,
     col = "red",
     xlab = "Crime",
     main = "Histogram")

skim(tdata$vio_crime)

tdata$vcat[tdata$vio_crime >= 22.7] <- "4th"
tdata$vcat[tdata$vio_crime >= 15.5 & tdata$vio_crime < 22.7] <- "3rd"
tdata$vcat[tdata$vio_crime >= 10.2 & tdata$vio_crime < 15.5] <- "2rd"
tdata$vcat[tdata$vio_crime < 10.2] <- "1st"

table(tdata$vcat)

# Subest
violence <- tdata[c(2:69, 71:72)]


# Rules
rpart.rules(fitv, style = "tall", cover = TRUE, nn = TRUE, roundint = TRUE)

# Error Matrix
em_vcat <- data.frame(vcat = violence$vcat, 
                      pred = predict(fitv, type = "class"))

# Error Rate
rtab <- table(em_vcat)
rtab
sum(diag(rtab)) / sum(rtab)
table(em_vcat$vcat)


# CP values/plot
printcp(fitv_pruned)
plotcp(fitv_pruned)

# Plot
windows()
rpart.plot::prp(fitv_pruned, type = 1, extra = 101, box.palette = "auto", branch.lty = 3,
                varlen = 0, shadow.col = "gray", nn = TRUE, fallen.leaves = TRUE)

# Error Matrix
em_vcat_pruned <- data.frame(vcat = violence$vcat, 
                             pred = predict(fitv_pruned, type = "class"))

rptab <- table(em_vcat_pruned)
rptab

sum(diag(rptab)) / sum(rptab)
table(em_vcat_pruned$vcat)


full<-table(predict(fitv_pruned, violence, type="class"), violence[, "vcat"])
sum(diag(full)) / sum(full)









