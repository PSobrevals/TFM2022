
#############################
#### Funciones de PreDiRecT
############################

### ---------- Librería
load <- function(packges){
  if ( any(! packges %in% installed.packages())) {
    install.packages(packges[! packges %in% installed.packages()], dependencies = TRUE)}
  
  sapply(packges, require, character.only = TRUE)
}

load(c("shinydashboard", "tableHTML", "ggthemes", "plotly", "fontawesome", 
       "e1071", "thematic", "bslib", "ggchicklet", "data.table", "shinythemes", 
       "shiny", "DT", "tcltk", "dplyr", "tidyr", "stats", "ggplot2", 
       "nnet", "caret", "broom", "randomForestSRC", "base64enc"))

gc()

### ---------- Datos
db.filterd.org <- read.csv2("/Users/paulasa/Desktop/MASTER/1.TFM/Datos y applicaciones facilitados/paula/db_filtered.csv",
                        header=T)
db.filterd <- db.filterd.org[,-c(1,2,3,4,5,6,7,8)]

for(col in 1:ncol(db.filterd)){
  db.filterd[,col] <- as.factor(db.filterd[,col])
}

GeneralData <<- db.filterd.org


### ---------- Funciones

classify_age <- function(age){
  if(age<=10){
    return("Less than 10 years")
  }else if (age>10 & age<=20 ){
    return("Between 10 and 20 years")
  }else if ( age>20 &  age<=30 ){
    return("Between 20 and 30 years")
  }else if ( age>30 &  age<=40 ){
    return("Between 30 and 40 years")
  }else if ( age>40 &  age<=50 ){
    return("Between 40 and 50 years")
  }else if ( age>50 &  age<=60 ){
    return("Between 50 and 60 years")
  }else if (  age>60 &  age<=70 ){
    return("Between 60 and 70 years")
  }else if ( age>70 &  age<=80){
    return("Between 70 and 80 years")
  }else{
    return("More than 80 years")
  }
  
  
}

classify_Htime <- function(predicted.time){
  coded.time <- c()
  for ( time.slot in predicted.time){
    if(as.numeric(time.slot) == 1){
      coded.time <- c(coded.time, "Menos de 2 días")
    }else if(as.numeric(time.slot) == 2){
      coded.time <- c(coded.time, "Entre dos días y una semana")
    }else if(as.numeric(time.slot) == 3){
      coded.time <- c(coded.time, "Entre una semana y un mes")
    }else if(as.numeric(time.slot) == 4){
      coded.time <- c(coded.time, "Entre un y dos meses")
    }else if(as.numeric(time.slot) == 5){
      coded.time <- c(coded.time, "Entre dos y seis meses")
    }else if(as.numeric(time.slot) == 6){
      coded.time <- c(coded.time, "Más de 6 meses")
    }
  }
  return(coded.time)
  
  
}

createSideBar <- function(id, title){
  
  hospitalID <- paste0("hospital", id)
  sexID <- paste0("sex", id)
  ageID <- paste0("age", id)
  admisionID <- paste0("admision", id)
  NombreID <- paste0("name", id)
  diagID <- paste0("diagnostico", id)
  diag2ID <- paste0("diagnostico2", id)
  
  res <- list(
    h3(title),
    h3(" "),
    textInput(NombreID, "Número de historial clínico del paciente:", value = ""),
    textInput(ageID, "Edad del paciente:", value = "", width = "60%"),
    selectInput(inputId = hospitalID, label="Selecciona el hospital:", multiple=F, choices = sort(unique(db.filterd$hospital))),
    radioButtons(inputId = sexID, label="Sexo del paciente:", choices = sort(unique(db.filterd$sex)), inline = T,
                 selected = character(0)),
    radioButtons(inputId = admisionID, label="Tipo de admisión:", inline = T,selected = character(0), choices = sort(unique(db.filterd$admission_types))),
    selectInput(inputId = diagID, label="Diagnostico primario:", multiple=F, choices = sort(unique(db.filterd$categorise_diagnostics_1st_less_names))),
    selectInput(inputId = diag2ID, label="Diagnostico secundario:", multiple=F, choices = sort(unique(db.filterd$categorise_diagnostics_2nd_less_names))),
    
    actionButton("goButton", tags$p(fa("arrow-alt-circle-up", height = "1em", width = "1em"), "   Buscar   ") ,style='padding:4px; font-size:110%'),
    h3(" ")
    
    
  )
  
  return(res)
}


find.all.therapies <- function(patient, db.therapies = db.filterd, only.Therapy = T){
  
  
  db.therapies <- db.therapies[which((db.therapies$categorise_diagnostics_1st_less_names == patient$categorise_diagnostics_1st_less_names[1]) |
                                       (db.therapies$categorise_diagnostics_2nd_less_names == patient$categorise_diagnostics_2nd_less_names[1]) |
                                       (db.therapies$categorise_diagnostics_2nd_less_names == patient$categorise_diagnostics_1st_less_names[1]) |
                                       (db.therapies$categorise_diagnostics_1st_less_names == patient$categorise_diagnostics_2nd_less_names[1])) ,]
  
  if(only.Therapy){
    pacient.therapies <- expand.grid(unique(db.therapies$categorise_therapies_1st_less_names),unique(db.therapies$categorise_therapies_2nd_less_names))
    
    colnames(pacient.therapies) <- c("categorise_therapies_1st_less_names", "categorise_therapies_2nd_less_names")
    return(pacient.therapies)
    
  }else{
    return(db.therapies)
  }
  
}

value.show <- function(age){
  age <- as.numeric(age)
  if((age -5) < 1){return(c(1,age+5))}
  else if( (age+5) > 102){ return(c(age-5,102))}
  else{return(c(age-5,age+5))}
}

GetInput <- function(x,Sex = F,Admision = F, Hospital = F, dbInput = DataSimilarCases$GeneralData){
  if( is.null(x)){
    if(Sex){
      return(c("Hombre","Mujer"))
    }else if (Admision){
      return(c("Programada","Urgente"))
    }else if( Hospital){
      return(unique(dbInput$hospital))
    }
  }else{
    return(x)
  }
}

time.anova <- function(general.data = GeneralData){
  
  general.data <- general.data[,-c(1,2,3,4,5,6,7,10)]
  
  colnames(general.data) <- c("Tiempo de hospitalización","Hospital",
                              "Edad", "Tipo de admisión", "Sexo","Primera Terapia",
                              "Segunda Terapia", "Primer diagnóstico", "Segundo diagnóstico")
  
  
  keep.colnames <- c("Tiempo de hospitalización")
  for( i in colnames(general.data)[-1]){
    if(length(unique(general.data[,i]))>1){
      keep.colnames <- c(keep.colnames,i)
    }
    
  }
  
  general.data <- general.data[,keep.colnames]
  
  
  for( i in colnames(general.data)[-1]){
    general.data[,i] <- as.factor(general.data[,i])
  }
  
  ## ANOVA
  
  general.data <- rbind(general.data,general.data)
  anova.result <- data.frame(summary(aov(general.data$`Tiempo de hospitalización` ~., data = general.data))[[1]])[,c(2,5)]
  anova.result$variables <- row.names(anova.result); anova.result <- anova.result[-nrow(anova.result),]
  anova.result <- anova.result[order(anova.result$Sum.Sq, decreasing = T),]
  anova.result.sig <- anova.result[which(anova.result$Pr..F. < 0.05), ]
  
  if(nrow(anova.result.sig) == 0){
    return("Estadísticamente no hay ninguna variable que tenga efecto sobre el número de días de hospitalización de este paciente.
           Aun así, la siguiente información puede ser útil para decidir cómo seguir con el tratamiento del nuevo paciente.")
  }else{
    
    
    return(paste("Hay ", nrow(anova.result.sig), " variables estadísticamente significantes que afectan al desarrollo de un paciente con un cuadro clínico parecido.", 
                 " Estas son, en orden de efecto, las siguientes: ", paste(gsub("`","",trimws(anova.result.sig$variables)),collapse = ", "), ".",
                 " Estas variables son interesantes a observar para estudiar el procedimiento a seguir con el paciente.", sep ="") )
    
  }
  
}


### ---------- Algoritmo de predicción 1
Hosp.Time.model <- nnet::multinom(categorise_hospital_days_5~., data = db.filterd, na.action =NULL,
                                  MaxNWts= 17000)

### ---------- Algoritmo de predicción 2
predict.future.diag <- function(general.data,db.use = db.filterd.org, test = NULL, predict = F){
  num.hist <- unique(general.data$num_clinical_history)
  
  
  for.rf <- unique(db.use[which(db.use$num_clinical_history %in% num.hist),])
  
  f.for.rf <- data.frame()
  f.for.rf.org <- data.frame()
  for( hist in num.hist){
    temp <- for.rf[which(for.rf$num_clinical_history == hist),]
    if(nrow(temp) ==1){
      f.for.rf.org <- rbind(f.for.rf.org,temp)
      temp$categorise_diagnostics_1st_less <- "No ha vuelto"
      temp$categorise_diagnostics_2nd_less <- "No ha vuelto"
      f.for.rf <- rbind(f.for.rf,temp)
    }else{
      
      when <- temp[which((temp$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) |
                           (temp$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),"year"]
      if(length(when)>1){
        when <- min(when)
      }
      f.for.rf.org <- rbind(f.for.rf.org,temp[which(temp$year == when),])
      temp <- temp[which(temp$year > when),]
      if(nrow(temp) !=0){
        
        f.for.rf <- rbind(f.for.rf,temp)
      }
    }
    
    
  }
  
  
  colnames(f.for.rf.org) 
  f.for.rf <- f.for.rf[,c("num_clinical_history","categorise_diagnostics_1st_less_names")]
  
  colnames(f.for.rf)[c(2)] <- c("New.diag.1")
  
  f.for.rf.org <- f.for.rf.org[,-c(1,2,3,4,5,6,8,10)]
  
  f.rf <- merge(f.for.rf.org,f.for.rf, by =  "num_clinical_history")
  
  f.rf <- f.rf[,-1]
  
  keep.colnames <- c("New.diag.1")
  for( i in colnames(f.rf)[-ncol(f.rf)]){
    if(length(unique(f.rf[,i]))>1){
      keep.colnames <- c(keep.colnames,i)
    }
    
  }
  
  f.rf <- f.rf[,keep.colnames]
  
  for( i in keep.colnames){
    f.rf[,i] <- as.factor(f.rf[,i])
  }
  
  
  
  
  
  Bayes_Model = naiveBayes(New.diag.1 ~., data=f.rf)
  
  bayes <- list()
  
  bayes$probable <- data.frame(sort(Bayes_Model$apriori,decreasing = T))
  colnames(bayes$probable) <- c("Diagnóstico", "Ocurrencia")
  bayes$probable$Color <- 1
  bayes$probable$Color[which(1:nrow(bayes$probable)%%2 ==0)] <- 2
  bayes$probable$Color <- as.factor(bayes$probable$Color)
  
  if(predict){
    
    test <- test[,keep.colnames[-1]]
    for( i in colnames(test)){
      test[,i] <- as.factor(test[,i])
    }
    
    
    multivar_fit<-rfsrc(New.diag.1~.
                        , data=f.rf,ntree = 300, 
                        importance = F, save.memory=T, seed=42) 
    
    preds.rf <- predict(multivar_fit, test)
    
    bayes$pred.rf <- as.character(preds.rf$class)
    
    bayes$pred.bay <- as.character(predict(Bayes_Model,test))
    
    if (bayes$pred.bay == bayes$pred.rf){
      bayes$pred <- paste("Teniendo en cuenta las terapias seleccionadas y el cuadro clínico del paciente, el diagnóstico más probable que se padezca en el futuro es: ", as.character(tolower(bayes$pred.rf)))
      
    }else{
      bayes$pred <- paste("Teniendo en cuenta las terapias seleccionadas y el cuadro clínico del paciente, los diagnósticos más probables que se padezcan en el futuro són: ", as.character(tolower(bayes$pred.rf)), "y", as.character(tolower(bayes$pred.rf)))
      
    }
    
    
    
  }
  
  return(bayes)
}


