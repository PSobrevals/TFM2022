
#############################
#### Preproceso de los datos
############################

### ----------  Library
set.seed(42)
load <- function(packges){
  if ( any(! packges %in% installed.packages())) {
    install.packages(packges[! packges %in% installed.packages()], dependencies = TRUE)}
  
  sapply(packges, require, character.only = TRUE)
}

load(c("tcltk", "dplyr", "tidyr", "stats","ggplot2", "nnet", "caret", "broom"))

### ---------- Data loading

therapies <- "/Users/paulasa/Desktop/MASTER/1.TFM/Datos y applicaciones facilitados/temp/hash_therapies_renamed_curated.csv"
therapies <- read.csv2(therapies,header=T)

diagnoses <- "/Users/paulasa/Desktop/MASTER/1.TFM/Datos y applicaciones facilitados/temp/hash_diagnostics_renamed_curated.csv"
diagnoses <- read.csv2(diagnoses,header=T)

file <- "/Users/paulasa/Desktop/MASTER/1.TFM/Datos y applicaciones facilitados/temp/database.csv"
database <- read.csv(file,header=T)

### ----------  Data transformation
str(database)


## Factorise data
factor.variables <- c( "hospital","categorise_hospotalise_days","categorise_hospotalise_days2",
                       "categorise_age", "sex", "target_province"  , "home_province", "categorise_unit",
                       "reason_of_discharge","admission_types")
database[,factor.variables] <- lapply(database[,factor.variables], as.factor)

# Missing data 

database$categorise_age <- na_if(database$categorise_age, "")
database$age <- na_if(database$age, 0)
database$reason_of_discharge <- na_if(database$reason_of_discharge, "")
database$reason_of_discharge <- na_if(database$reason_of_discharge, "Unkown")
database$categorise_diagnostics <- na_if(database$categorise_diagnostics, "-")
database$categorise_unit <- na_if(database$categorise_unit, "Unspecified")


### ----------  Extract only the first & second diagnoses (the most important):
database$categorise_diagnostics_1st <- sapply(database$categorise_diagnostics, function(x) 
  strsplit(x,split=' ')[[1]][1])
database$categorise_diagnostics_1st <- as.factor(database$categorise_diagnostics_1st)

database$categorise_diagnostics_2nd <- sapply(database$categorise_diagnostics, function(x) 
  strsplit(x,split=' ')[[1]][2])
database$categorise_diagnostics_2nd <- as.factor(database$categorise_diagnostics_2nd)

database$categorise_diagnostics_1st <- na_if(database$categorise_diagnostics_1st, "-")
database$categorise_diagnostics_2nd <- na_if(database$categorise_diagnostics_2nd, "-")

### ----------  Extract only the first & second therapies (the most important):
database$categorise_therapies_1st <- sapply(database$categorise_therapies, function(x) 
  strsplit(x,split=' ')[[1]][1])
database$categorise_therapies_1st <- as.factor(database$categorise_therapies_1st)

database$categorise_therapies_2nd <- sapply(database$categorise_therapies, function(x) 
  strsplit(x,split=' ')[[1]][2])
database$categorise_therapies_2nd <- as.factor(database$categorise_therapies_2nd)



### ----------  Factorise continuous variables:

database$categorise_hospital_days_5 <- 0
database$categorise_hospital_days_5[database$hospitalise_days<=2] <- "Menos de 2 días"
database$categorise_hospital_days_5[database$hospitalise_days>2 & database$hospitalise_days<=7] <- "Entre dos días y una semana"
database$categorise_hospital_days_5[database$hospitalise_days>7 &database$hospitalise_days<=30] <- "Entre una semana y un mes"
database$categorise_hospital_days_5[database$hospitalise_days>30 &database$hospitalise_days<=60] <- "Entre un y dos meses"
database$categorise_hospital_days_5[database$hospitalise_days>60 &database$hospitalise_days<=180] <- "Entre dos y seis meses"
database$categorise_hospital_days_5[database$hospitalise_days>180] <- "Más de 6 meses"

database$categorise_age_1 <- 0
database$categorise_age_1[database$age<=10] <- "Less than 10 years"
database$categorise_age_1[database$age>10 & database$age<=20] <- "Between 10 and 20 years"
database$categorise_age_1[database$age>20 &database$age<=30] <- "Between 20 and 30 years"
database$categorise_age_1[database$age>30 &database$age<=40] <- "Between 30 and 40 years"
database$categorise_age_1[database$age>40 &database$age<=50] <- "Between 40 and 50 years"
database$categorise_age_1[database$age>50 &database$age<=60] <- "Between 50 and 60 years"
database$categorise_age_1[database$age>60 &database$age<=70] <- "Between 60 and 70 years"
database$categorise_age_1[database$age>70 &database$age<=80] <- "Between 70 and 80 years"
database$categorise_age_1[database$age>80] <- "More than 80 years"
database$categorise_age_1 <- as.factor(database$categorise_age_1 )


### ----------   Filter out patients with voluntary discharge

table(database$reason_of_discharge) #  3% of patients volunteer out 

database <- database[database$reason_of_discharge != "Discharge volunteer",]


### ----------   Merge similar diagnoses & therapies

db <- database
db$categorise_therapies_1st_less <- NA
db$categorise_therapies_1st_less <- sapply(db$categorise_therapies_1st,
                                           function(x) ifelse(grepl(".",x,perl=F),strsplit(as.character(x), split=".",perl=F,fixed=T)[[1]][1],x))
db$categorise_therapies_2nd_less <- "-"
db$categorise_therapies_2nd_less <- sapply(db$categorise_therapies_2nd,
                                           function(x) ifelse(grepl(".",x,perl=F),strsplit(as.character(x), split=".",perl=F,fixed=T)[[1]][1],x))
db$categorise_therapies_2nd_less[is.na(db$categorise_therapies_2nd_less)] <- "-"

db$categorise_diagnostics_1st_less <- NA
db$categorise_diagnostics_1st_less <- sapply(db$categorise_diagnostics_1st,
                                             function(x) ifelse(grepl(".",x,perl=F),strsplit(as.character(x), split=".",perl=F,fixed=T)[[1]][1],x))
db$categorise_diagnostics_2nd_less <- "-"
db$categorise_diagnostics_2nd_less <- sapply(db$categorise_diagnostics_2nd,
                                             function(x) ifelse(grepl(".",x,perl=F),strsplit(as.character(x), split=".",perl=F,fixed=T)[[1]][1],x))
db$categorise_diagnostics_2nd_less[is.na(db$categorise_diagnostics_2nd_less)] <- "-"


### ----------   Other filters and preparations

db <- db[which(db$hospitalise_days <= 365),]

db$age <- round(db$age)

db <- db[!is.na(db$categorise_diagnostics_1st_less),]
db <- db[!is.na(db$categorise_therapies_1st_less),]


### ----------   Prepare the data for algorithms

db.filterd <- db
db.filterd$categorise_hospital_days_5 <- 0
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days<=2] <- "1"
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days>2 & db.filterd$hospitalise_days<=7] <- "2"
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days>7 &db.filterd$hospitalise_days<=30] <- "3"
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days>30 &db.filterd$hospitalise_days<=60] <- "4"
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days>60 &db.filterd$hospitalise_days<=180] <- "5"
db.filterd$categorise_hospital_days_5[db.filterd$hospitalise_days>180] <- "6"
db.filterd$categorise_hospital_days_5 <- as.factor(db.filterd$categorise_hospital_days_5)

db.filterd <- db.filterd[, c("age","year","num_clinical_history","hospitalise_days","hospital","categorise_therapies_1st_less", "categorise_diagnostics_2nd_less",
                             "categorise_hospital_days_5", "categorise_age_1", "categorise_therapies_2nd_less",
                             "admission_types", "sex",  "categorise_diagnostics_1st_less"
)]

db.filterd$admission_types <- factor(db.filterd$admission_types, levels = unique(db.filterd$admission_types), labels = c("Urgente", "Programada"))

outlier_1st_diag <- names(table(db.filterd$categorise_diagnostics_1st_less))[which(table(db.filterd$categorise_diagnostics_1st_less)<=3)]
db.filterd <- db.filterd[-c(which(db.filterd$categorise_diagnostics_1st_less %in% outlier_1st_diag)),] 


outlier_2nd_diag <- names(table(db.filterd$categorise_diagnostics_2nd_less))[which(table(db.filterd$categorise_diagnostics_2nd_less)<=3)]
db.filterd <- db.filterd[-c(which(db.filterd$categorise_diagnostics_2nd_less %in% outlier_2nd_diag)),] 


colnames(therapies) <- c("categorise_therapies_1st_less", "categorise_therapies_1st_less_names")
db.filterd <- merge(db.filterd, therapies, by = "categorise_therapies_1st_less")
colnames(therapies) <- c("categorise_therapies_2nd_less", "categorise_therapies_2nd_less_names")
db.filterd <- merge(db.filterd, therapies, by = "categorise_therapies_2nd_less")

colnames(diagnoses) <- c("categorise_diagnostics_2nd_less", "categorise_diagnostics_2nd_less_names")
db.filterd <- merge(db.filterd, diagnoses, by = "categorise_diagnostics_2nd_less")
colnames(diagnoses) <- c("categorise_diagnostics_1st_less", "categorise_diagnostics_1st_less_names")
db.filterd <- merge(db.filterd, diagnoses, by = "categorise_diagnostics_1st_less")

write.csv2(db.filterd,"/Users/paulasa/Desktop/MASTER/1.TFM/Datos y applicaciones facilitados/paula/db_filtered.csv", row.names=F, quote=F )


