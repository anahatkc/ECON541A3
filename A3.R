install.packages("summarytools")
install.packages('haven')
install.packages("psych")
install.packages("cli")
install.packages("fastDummies")
install.packages("hexbin")

library(dplyr)
library(summarytools)
library(stargazer)
library(haven)
library(psych)
library(tidyr)
library(gt)
library(ggplot2)
library(fixest)
library(fastDummies)
library(scales)
library(hexbin)

#setting working directory
setwd("/Users/anahat/Desktop/541_Data")

# uploading the new csv - contains non-residents who migrated only in wave 1 (MIG_STAT = 1), only in wave 2 (MIG_STAT = 2) and both in wave 1 and 2 (MIG_STAT = 3)
# only includes non residents from households who were interviewed in both wave 1 and 2 
# ID variable is concatenated STATEID, DISTID, PSUID, HHID, HHSPLITID
#X.VARNAME - info for wave1, VARNAME - info for wave2
non_res <- read.csv("https://raw.githubusercontent.com/anahatkc/ECON541A3/main/non_res05.csv")

# uploading women + non_res file
wmn_nonres <- read.csv("https://raw.githubusercontent.com/anahatkc/ECON541A3/main/wmn_nonres.csv")

# creating table 

# Filter the dataset to include only rows where X = 1
no_mig <- wmn_nonres %>% filter(MIG_STAT == 0)

# Create a frequency table for Y and convert it to a proportion table
# for 2005 waves responses
table_cook05 <- table(no_mig$XGR1A)
table_numchild05 <- table(no_mig$XGR3A)
table_ill05 <- table(no_mig$XGR7A)
table_purchase05 <- table(no_mig$XGR2A)
table_bank05 <- table(no_mig$XGR27B)
table_lease05 <- table(no_mig$XGR28)
table_healthcen05 <- table(no_mig$XGR9AY)
table_othershouse05 <- table(no_mig$XGR10AY)
table_kirana05 <- table(no_mig$XGR11AY)

# for 2011 wave responses
table_cook11 <- table(no_mig$GR1A)
table_numchild11 <- table(no_mig$GR3A) 
table_ill11 <- table(no_mig$GR7A) 
table_purchase11 <- table(no_mig$GR2A)
table_bank11 <- table(no_mig$GR27B)
table_lease11 <- table(no_mig$GR28)
table_healthcen11 <- table(no_mig$GR9AY)
table_othershouse11 <- table(no_mig$GR10AY)
table_kirana11 <- table(no_mig$GR11AY)

# converting to proportion
prop_table_cook1 <- prop.table(table_cook05) * 100 
prop_table_numchild1 <- prop.table(table_numchild05) * 100 
prop_table_ill1 <- prop.table(table_ill05) * 100 
prop_table_purchase1 <- prop.table(table_purchase05) * 100 
prop_table_bank1 <- prop.table(table_bank05) * 100 
prop_table_lease1 <- prop.table(table_lease05) * 100 
prop_table_healthcen1 <- prop.table(table_healthcen05) * 100 
prop_table_othershouse1 <- prop.table(table_othershouse05) * 100 
prop_table_kirana1 <- prop.table(table_kirana05) * 100 

prop_table_cook2 <- prop.table(table_cook11) * 100 
prop_table_numchild2 <- prop.table(table_numchild11) * 100 
prop_table_ill2 <- prop.table(table_ill11) * 100 
prop_table_purchase2 <- prop.table(table_purchase11) * 100 
prop_table_bank2 <- prop.table(table_bank11) * 100 
prop_table_lease2 <- prop.table(table_lease11) * 100 
prop_table_healthcen2 <- prop.table(table_healthcen11) * 100 
prop_table_othershouse2 <- prop.table(table_othershouse11) * 100 
prop_table_kirana2 <- prop.table(table_kirana11) * 100

# for hh that migrated not in 1 but did in 2

mig2only <- wmn_nonres %>% filter(MIG_STAT == 2)

# Create a frequency table for Y and convert it to a proportion table
# for 2005 responses
table_cook2m <- table(mig2only$XGR1A)
table_numchild2m <- table(mig2only$XGR3A)
table_ill2m <- table(mig2only$XGR7A)
table_purchase2m <- table(mig2only$XGR2A)
table_bank2m <- table(mig2only$XGR27B)
table_lease2m <- table(mig2only$XGR28)
table_healthcen2m <- table(mig2only$XGR9AY)
table_othershouse2m <- table(mig2only$XGR10AY)
table_kirana2m <- table(mig2only$XGR11AY)

#for 2011 responses
table_cook11m <- table(mig2only$GR1A)
table_numchild11m <- table(mig2only$GR3A)
table_ill11m <- table(mig2only$GR7A)
table_purchase11m <- table(mig2only$GR2A)
table_bank11m <- table(mig2only$GR27B)
table_lease11m <- table(mig2only$GR28)
table_healthcen11m <- table(mig2only$GR9AY)
table_othershouse11m <- table(mig2only$GR10AY)
table_kirana11m <- table(mig2only$GR11AY)

# converting to proportion table
prop_table_cook2m <- prop.table(table_cook2m) * 100 
prop_table_numchild2m <- prop.table(table_numchild2m) * 100 
prop_table_ill2m <- prop.table(table_ill2m) * 100 
prop_table_purchase2m <- prop.table(table_purchase2m) * 100 
prop_table_bank2m <- prop.table(table_bank2m) * 100 
prop_table_lease2m <- prop.table(table_lease2m) * 100 
prop_table_healthcen2m <- prop.table(table_healthcen2m) * 100 
prop_table_othershouse2m <- prop.table(table_othershouse2m) * 100 
prop_table_kirana2m <- prop.table(table_kirana2m) * 100 

prop_table_cook11m <- prop.table(table_cook11m) * 100 
prop_table_numchild11m <- prop.table(table_numchild11m) * 100 
prop_table_ill11m <- prop.table(table_ill11m) * 100 
prop_table_purchase11m <- prop.table(table_purchase11m) * 100 
prop_table_bank11m <- prop.table(table_bank11m) * 100 
prop_table_lease11m <- prop.table(table_lease11m) * 100 
prop_table_healthcen11m <- prop.table(table_healthcen11m) * 100 
prop_table_othershouse11m <- prop.table(table_othershouse11m) * 100 
prop_table_kirana11m <- prop.table(table_kirana11m) * 100 

print(prop_table_kirana2m)
print(prop_table_kirana11m)

#renaming variable outcomes
mig2_mod <- mig2only %>%
  mutate(NR8 = case_when(
    NR8 == 1 ~ "Same State",
    NR8 == 2 ~ "Another State",
    NR8 == 3 ~ "Abroad",
    TRUE ~ as.character(NR8)  # This line is optional; it keeps the original value for other cases
  ))

# only household that recieved remittances
mig2_mod<- mig2_mod %>% filter(NR12 == 2)

# Create a hexbin object from your x and y data
hb <- hexbin(mig2only$NR10, mig2only$NR13A)

# Plot the hexbin object
plot(hb, main="Binned Scatterplot of x vs. y", xlab="x", ylab="y")
ggplot(mig2_mod, aes(x=NR10, y=NR13A)) + 
  geom_hex() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + ggtitle("Scatterplot of Education vs. Remittances") +
  xlab("Education") +
  ylab("Remittances") + scale_y_continuous(labels = label_number()) # Customize colors if needed

# creating dummy vars
wmn_nonres_1 <- dummy_cols(wmn_nonres, select_columns = c("GR1A", "GR3A", "GR7A", "GR2A", "GR27B", "GR28",
                                                            "GR9AY", "GR10AY", "GR11AY", "GROUPS6","STATEID", "XID11", "MIG_STAT"), remove_selected_columns = FALSE)

# running lpm models
names(wmn_nonres_1)[names(wmn_nonres_1) == "XID11_(2) Muslim 2"] <- "Muslim"
names(wmn_nonres_1)[names(wmn_nonres_1) == "GROUPS6_(3) OBC 3"] <- "OBC"

#####
model1n <- lm(`GR1A_(1) Yes 1` ~ MIG_STAT_2 + Muslim +
               OBC , data = wmn_nonres_1)

model2n <- lm(`GR9AY_(0) No 0` ~ MIG_STAT_2 + Muslim +
               OBC, data = wmn_nonres_1)

model3n <- lm(`GR27B_(1) Yes 1` ~ MIG_STAT_2 , data = wmn_nonres_1)

model4n <- lm(`GR27B_(1) Yes 1` ~ MIG_STAT_2 + Muslim, data = wmn_nonres_1)
model5n <- lm(`GR27B_(1) Yes 1` ~ MIG_STAT_2 + Muslim +
               OBC , data = wmn_nonres_1)
colnames(wmn_nonres_1)




# Summary of the model

stargazer(model1n, model2n, model3n, model4n, model5n, type = "html", title = "Regression Results",
          out = "reg_table_with_controls2.html",
          omit = c("Muslim", "OBC"), # Corrected line
          add.lines = list(
            c("Muslim Control", "Yes", "Yes", "No", "Yes", "Yes"),
            c("OBC Control", "Yes", "Yes", "No", "No", "Yes")
          ))



