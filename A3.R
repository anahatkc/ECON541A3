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

# loading non-residents data from wave 1 and wave 2 
nonres05 <- read_sav('/Users/anahat/Desktop/541_Data/2005/DS0004_nonres/22626-0004-Data.sav') # 2005
load("/Users/anahat/Desktop/541_Data/2011_12/DS0007_nonres/nonres11.rda") # 2011-12

# loading individual level data from wave 1 and wave 2
ind05 <- read_sav("/Users/anahat/Desktop/541_Data/2005/DS0001_ind/22626-0001-Data.sav") # 2005
load("/Users/anahat/Desktop/541_Data/2011_12/DS0001_ind/36151-0001-Data.rda") # 2011-12

# creating a subset that has non-residents who are husbands 
non_res05 <- subset(nonres05, NR1 == 1 & NR4 == 2 & NR5 == 1) 
non_res11 <- subset(da36151.0007, NR1 == "(1) Spouse" & NR4 == "(02) Wife/Husband" & NR5 == "(1) Male")

# uploading the new csv - contains non-residents who migrated only in wave 1 (MIG_STAT = 1), only in wave 2 (MIG_STAT = 2) and both in wave 1 and 2 (MIG_STAT = 3)
# only includes non residents from households who were interviewed in both wave 1 and 2 
# ID variable is concatenated STATEID, DISTID, PSUID, HHID, HHSPLITID
#X.VARNAME - info for wave1, VARNAME - info for wave2
non_res <- read.csv("/Users/anahat/Desktop/541_Data/non_res05.csv")

# uploading the eligible women file (2005, 2011-12)
load("/Users/anahat/Desktop/541_Data/ICPSR_37382 2/DS0015/37382-0015-Data.rda")

# keeping only relevant variables
em_women <- select(da37382.0015, SURVEY, HHBASE, HHFAM2, PBASE, STATEID, DISTID, HHID2012, 
                       HHSPLITID, HHSPLITID2012, RO4, RO5, RO6, RO8, PSUID, PERSONID, EWWAVES, 
                       XGR1A, XGR1B, XGR1C, XGR1D, XGR1E, 
                       XGR1F, XGR1G, XGR3A, XGR3B, XGR3C, XGR3D, XGR3E, XGR3F, XGR3G, XGR7A, 
                       XGR7B, XGR7C, XGR7D, XGR7E, XGR7F, XGR7G, XGR2A, XGR2B, XGR2C, XGR2D, 
                       XGR2E, XGR2F, XGR2G, XGR27B, XGR28, XGR9AY, XGR10AY, XGR11AY, GR1A, 
                       GR1B, GR1C, GR1D, GR1E, GR1F, GR1G, GR3A, GR3B, GR3C, GR3D, GR3E, GR3F, 
                       GR3G, GR7A, GR7B, GR7C, GR7D, GR7E, GR7F, GR7G, GR2A, GR2B, GR2C, GR2D, 
                       GR2E, GR2F, GR2G, GR27B, GR28, GR9AY, GR10AY, GR11AY, GR12AY, GR18A, 
                       GR18B, GR18C, GR18D, GR19, MH7, GROUPS6, XID11)

# creating a new variable to merge on 
em_women$ID <- paste(em_women$STATEID, em_women$DISTID, em_women$PSUID, em_women$HHID2012, sep = ",")
non_res$ID <- paste(non_res$X.STATEID, non_res$X.DISTID, non_res$X.PSUID, non_res$X.HHID, sep = ",")

# uploading women + non_res file
wmn_nonres <- read.csv("/Users/anahat/Desktop/541_Data/wmn_nonres.csv")

# csv files
write.csv(em_women,'women1.csv')
write.csv(non_res, 'non_res_full1.csv')

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



