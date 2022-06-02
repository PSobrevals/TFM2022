
#############################
#### Aplicación  PreDiRecT
############################

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source(paste(dirname(rstudioapi::getSourceEditorContext()$path),'/PreDiRecT_Funciones.R',sep=""))

Imagen_PredirectTrans <- base64enc::dataURI(file=paste(dirname(rstudioapi::getSourceEditorContext()$path),'/myimg/Imagen_PredirectTrans.png',sep=""), mime="image/png")
Predirect33 <- base64enc::dataURI(file=paste(dirname(rstudioapi::getSourceEditorContext()$path),'/myimg/Predirect33.png',sep=""), mime="image/png")
Grado_hospitalizacion <- base64enc::dataURI(file=paste(dirname(rstudioapi::getSourceEditorContext()$path),'/myimg/Grado_hospitalizacion.png',sep=""), mime="image/png")


server <- function(input, output, session) {
  
  ### ---------- First page: Description  
  {
    text_reactive <- eventReactive( input$goButton, {
      paste("Hoy día", as.character(Sys.Date()),  "ha ingresado en el", tolower(input$hospital1), "el/la paciente con número de historial clínico", toupper(input$name1), "; un(a)", tolower(input$sex1), "de", input$age1, "años." ,
            "El ingreso ha sido ",ifelse(input$admision1 == "Programada", "con cita previa.", "de manera urgente."),
            "El/la paciente presenta un diagnóstico primario de",
            tolower(input$diagnostico1), ifelse(input$diagnostico21 == "", "",paste("y un diagnóstico secundario de", tolower(input$diagnostico21))),"." )
      
    })
    
    # Description output
    output$descripcion <- renderText({
      text_reactive()
    })
    
    # Prev history
    DataprevPatient <- reactiveValues()
    
    observeEvent(input$PatientInfo, {
      
      DataprevPatient$PatientClinInfo <- db.filterd.org[which(as.integer(db.filterd.org$num_clinical_history) == as.integer(input$name1)),-c(1:4,7,10,11,13)]
      
      colnames(DataprevPatient$PatientClinInfo) <- c("Edad", "Año", "Días de hospitalización",
                                                     "Hospital", "Tipo de admissión", "Primera Terapia",
                                                     "Seguna Terapia",  "Segundo diagnóstico","Primer diagnóstico")
      
      DataprevPatient$PatientClinInfo <- DataprevPatient$PatientClinInfo[,c(2, 4, 1, 3, 5,9,8,6,7)]
      DataprevPatient$PatientClinInfo$`Días de hospitalización` <- as.integer(DataprevPatient$PatientClinInfo$`Días de hospitalización`)
      
      DataprevPatient$DDT <- datatable(  DataprevPatient$PatientClinInfo, extensions=c("Scroller","Buttons", "ColReorder", "Responsive"),  options = list(colReorder = TRUE, 
                                                                                                                                                          deferRender=TRUE, pageLength = 5,order = list(list(2, 'asc')), persistent = TRUE,dom = 'Bfrtip',
                                                                                                                                                          buttons = list(
                                                                                                                                                            list(extend = "csv", buttons= "csv",text = "Descargar Historial", filename = "Historial_Clínico",
                                                                                                                                                                 exportOptions = list(order= 'current',page= 'all')
                                                                                                                                                            ))),rownames= FALSE, style="bootstrap",selection = "multiple"
      ) %>% formatStyle(
        'Edad',
        target = 'row',
        backgroundColor = 'white')
      
      
      
    })
    
    output$table.ClinHistory <- renderDataTable({
      DataprevPatient$DDT
      
    },server = FALSE)
    
  }
  
  ### ---------- Second page: Prediction
  {
    # Predicción
    
    DataPredTiempo <- reactiveValues()
    DataPredTiempo$paciente <- data.frame(hospital = factor(),
                                          categorise_diagnostics_2nd_less_names = factor(),
                                          categorise_age_1 = factor(),
                                          admission_types = factor(),
                                          sex = factor(), categorise_diagnostics_1st_less_names = factor())
    
    observeEvent(input$goButton, {
      
      DataPredTiempo$paciente <- data.frame(hospital =input$hospital1,
                                            categorise_diagnostics_2nd_less_names = input$diagnostico21 ,
                                            categorise_age_1 =classify_age(as.integer(input$age1)) ,
                                            admission_types = input$admision1 ,
                                            sex = input$sex1 , 
                                            categorise_diagnostics_1st_less_names = input$diagnostico1
      )
      
      pacient.therapies <- find.all.therapies(  DataPredTiempo$paciente)
      
      ntherapies.combined <- nrow(pacient.therapies)
      DataPredTiempo$paciente <- data.frame(hospital = rep(  DataPredTiempo$paciente$hospital,ntherapies.combined),
                                            categorise_diagnostics_2nd_less_names = rep(  DataPredTiempo$paciente$categorise_diagnostics_2nd_less_names,ntherapies.combined),
                                            categorise_age_1 = rep(  DataPredTiempo$paciente$categorise_age_1,ntherapies.combined),
                                            admission_types = rep(  DataPredTiempo$paciente$admission_types,ntherapies.combined),
                                            sex = rep(  DataPredTiempo$paciente$sex,ntherapies.combined), 
                                            categorise_diagnostics_1st_less_names = rep(  DataPredTiempo$paciente$categorise_diagnostics_1st_less_names,ntherapies.combined),
                                            pacient.therapies)
      
      DataPredTiempo$predicted.classes <- Hosp.Time.model %>% predict(  DataPredTiempo$paciente)
      DataPredTiempo$predict.min.hospital <- cbind(  DataPredTiempo$paciente[, c("categorise_therapies_1st_less_names", "categorise_therapies_2nd_less_names")],  DataPredTiempo$predicted.classes)
      DataPredTiempo$predict.min.hospital <-   DataPredTiempo$predict.min.hospital[order(  DataPredTiempo$predict.min.hospital[,3]),]
      DataPredTiempo$predict.min.hospital$Tiempo.estimado <- classify_Htime(  DataPredTiempo$predict.min.hospital[,3])
      DataPredTiempo$predict.min.hospital <-   DataPredTiempo$predict.min.hospital[,-3]
      colnames(  DataPredTiempo$predict.min.hospital) <- c("Terapia 1", "Terapia 2", "Tiempo estimado" )
      DataPredTiempo$predict.min.hospital <- DataPredTiempo$predict.min.hospital[order(DataPredTiempo$predict.min.hospital[,3]),]
      
      # For ggplot
      DataPredTiempo$total.tiempo <- data.frame(table(  DataPredTiempo$predict.min.hospital$`Tiempo estimado`))
      DataPredTiempo$total.tiempo$Freq <- round((DataPredTiempo$total.tiempo$Freq/sum(DataPredTiempo$total.tiempo$Freq))*100,1)
      DataPredTiempo$total.tiempo$Var1 <- paste(DataPredTiempo$total.tiempo$Var1, " (",DataPredTiempo$total.tiempo$Freq, "%)", sep ="")
      
      
      
    })
    
   
    # First plot overview
    output$predicted.plot <- renderPlot({
      
      ggplot(data.frame(DataPredTiempo$total.tiempo), aes(y = Freq, x = Var1)) +
        geom_chicklet(radius = grid::unit(7, 'mm'),width = 0.1, fill = "#0C4433", size=0,color = "#0C4433")  +
        theme_minimal() + theme( axis.title = element_blank(),axis.text = element_text(size=15),
                                 axis.ticks.x = element_blank(), panel.grid = element_blank(),plot.background = element_rect(colour = "grey50")) + 
        
        scale_y_continuous(position = "right") + 
        coord_flip()
      
      
    })
    
    # Density hospital time overview
    
    output$density.similar <- renderPlot({
      
      ggplot(data.frame(GeneralData), aes(x = hospitalise_days)) +
        geom_density(aes(y = (..scaled..)*100 ),stat = "density", position = "identity", color ="#0C4433", size = 3)+ xlab("Días de hospitalización") +
        ylab("Density %") + 
        theme_minimal() + theme( axis.text = element_text(size=15),
                                 axis.ticks.x = element_blank(),plot.background = element_rect(colour = "grey50"))
      
      
    })
    
    # Predicted table overview
    output$table.pred <- renderDataTable({
      
      datatable(  DataPredTiempo$predict.min.hospital, extensions=c("Scroller","Buttons", "ColReorder", "Responsive"), options = list(colReorder = TRUE, 
                                                                                                                                      deferRender=TRUE, pageLength = 5,order = list(list(2, 'asc')), persistent = TRUE,dom = 'Bfrtip',
                                                                                                                                      buttons = list(
                                                                                                                                        list(extend = "csv", buttons= "csv",text = "Descargar Tabla", filename = "Terapias_Paciente",
                                                                                                                                             exportOptions = list(order= 'current',page= 'all')
                                                                                                                                        ))),rownames=FALSE, style="bootstrap"
      )  %>% formatStyle('Tiempo estimado',  color = 'black',  fontWeight = 'bold')
    }, server = FALSE)
    
    
    # Text prediction
    text_prediccion_react <- eventReactive( input$goButton, {
      paste("Visión general de las posibles terapias a tener en cuenta para ayudar al paciente, con el tiempo estimado de hospitalización de cada una. A la derecha, la escalera de tiempos en la cual están organizados. Según el diagnóstico del paciente con número de historial ", 
            toupper(input$name1), ", el tiempo mínimo estimado de hospitalización es de ",
            tolower(  DataPredTiempo$predict.min.hospital[1,3]),". \n")
    })
    
    output$tiempo.pred <- renderText({
      text_prediccion_react()
    })
    
    # Text anova
    text_prediccion_anova <- eventReactive( input$goButton, {
      time.anova()
    })
    
    output$anova.pred <- renderText({
      text_prediccion_anova()
    })
    
  }
  
  ### ---------- Third page: Similar cases (Time)
  {
    
    DataSimilarCases <<- reactiveValues()
    
    
    observeEvent(input$goButton, {
      DataSimilarCases$New.Pacient <<- DataPredTiempo$paciente[1,-c(grep("categorise_age_1|categorise_therapies_1st_less_names|categorise_therapies_2nd_less_names", colnames(DataPredTiempo$paciente) ))]
      DataSimilarCases$New.Pacient$age <<- input$age1
      
      GeneralData <<- db.filterd.org[which((db.filterd.org$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) |
                                             (db.filterd.org$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),]
      
      
      DataSimilarCases$GeneralData <<-  db.filterd.org[which(((db.filterd.org$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) |
                                                                (db.filterd.org$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)) &
                                                               (db.filterd.org$sex %in% c("Hombre","Mujer")) & (db.filterd.org$admission_types %in% c("Programada","Urgente")) &
                                                               ((db.filterd.org$age >= 1) & (db.filterd.org$age <= 102)) & (db.filterd.org$hospital %in% unique(db.filterd.org$hospital))),]
      
      
      
      DataSimilarCases$FirstPlot <<- data.frame(Diagnostic = c("Diagnóstico 1","Diagnóstico 2","Diagnóstico 1 y 2"),
                                                People = c(length(which(DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names)),
                                                           length(which(DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),
                                                           nrow(db.filterd.org[which((db.filterd.org$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) &
                                                                                       (db.filterd.org$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),]))) 
      DataSimilarCases$FirstPlot$Diagnostic <<- factor(DataSimilarCases$FirstPlot$Diagnostic,levels = c("Diagnóstico 1 y 2","Diagnóstico 2","Diagnóstico 1")) 
      
      DataSimilarCases$GeneralData.Diag1 <<- DataSimilarCases$GeneralData[which(DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names),]
      
      DataSimilarCases$GeneralData.Diag2 <<- DataSimilarCases$GeneralData[which(DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names),]
      
      DataSimilarCases$GeneralData.Diag12 <<- DataSimilarCases$GeneralData[which((DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names) & 
                                                                                   (DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) ),]
      
      
      
      colnames(DataSimilarCases$GeneralData.Diag1)[c(5, 13, 9,17,14,
                                                     8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                           "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      colnames(DataSimilarCases$GeneralData.Diag2)[c(5, 13, 9,17,14,
                                                     8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                           "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      
      colnames(DataSimilarCases$GeneralData.Diag12)[c(5, 13, 9,17,14,
                                                      8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                            "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      
      
    })
    
    observeEvent(input$ChangePlot, {
      
      DataSimilarCases$GeneralData <<- db.filterd.org[which(((db.filterd.org$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) |
                                                               (db.filterd.org$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)) &
                                                              (db.filterd.org$sex %in% GetInput(input$SexID, Sex = T)) & (db.filterd.org$admission_types %in% GetInput(input$AdmisionID, Admision = T)) &
                                                              ((db.filterd.org$age >= input$AgeID[1]) & (db.filterd.org$age <= input$AgeID[2])) & (db.filterd.org$hospital %in% GetInput(input$HospitalID, Hospital = T))),]
      
      
      DataSimilarCases$FirstPlot <<- data.frame(Diagnostic = c("Diagnóstico 1","Diagnóstico 2","Diagnóstico 1 y 2"),
                                                People = c(length(which(DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names)),
                                                           length(which(DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),
                                                           nrow(DataSimilarCases$GeneralData[which((DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) &
                                                                                                     (DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names %in% DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names)),] )))
      DataSimilarCases$FirstPlot$Diagnostic <<- factor(DataSimilarCases$FirstPlot$Diagnostic,levels = c("Diagnóstico 1 y 2","Diagnóstico 2","Diagnóstico 1")) 
      
      DataSimilarCases$GeneralData.Diag1 <<- DataSimilarCases$GeneralData[which(DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names),]
      
      DataSimilarCases$GeneralData.Diag2 <<- DataSimilarCases$GeneralData[which(DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names),]
      
      DataSimilarCases$GeneralData.Diag12 <<- DataSimilarCases$GeneralData[which((DataSimilarCases$GeneralData$categorise_diagnostics_2nd_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names) & 
                                                                                   (DataSimilarCases$GeneralData$categorise_diagnostics_1st_less_names == DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names) ),]
      
      
      colnames(DataSimilarCases$GeneralData.Diag1)[c(5, 13, 9,17,14,
                                                     8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                           "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      colnames(DataSimilarCases$GeneralData.Diag2)[c(5, 13, 9,17,14,
                                                     8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                           "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      
      colnames(DataSimilarCases$GeneralData.Diag12)[c(5, 13, 9,17,14,
                                                      8, 12, 16, 15)] <<- c("Edad", "Sexo", "Hospital", "Primer.Diagnóstico", "Primera.Terapia",
                                                                            "Días.de.hospitalización","Tipo.de.admisión", "Segundo.Diagnóstico", "Segunda.Terapia")
      
      
    })
    
    text_ppsimilarpeople <- eventReactive( input$goButton, {
      paste("Se han registrado un total de ", nrow(DataSimilarCases$GeneralData), "pacientes con diagnósticos parecidos. De los cuales, ",
            nrow(DataSimilarCases$GeneralData.Diag1), "de ellos comparten el diagnóstico", tolower(DataSimilarCases$New.Pacient$categorise_diagnostics_1st_less_names), ". \n",
            nrow(DataSimilarCases$GeneralData.Diag2), "de los cuales, comparten el diagnóstico", tolower(DataSimilarCases$New.Pacient$categorise_diagnostics_2nd_less_names), ". \n",
            "Y finalmente," , nrow(DataSimilarCases$GeneralData.Diag12), "de los cuales, tienen los dos diagnósticos.") 
      
    })
    
    # Description similar people
    output$Pp.SimilarPeople <- renderText({
      text_ppsimilarpeople()
    })
    
    
    output$PlotSimilarPeople <- renderPlot({
      req(nrow(DataSimilarCases$FirstPlot) > 0)
      ggplot(DataSimilarCases$FirstPlot, aes(x = People, y = Diagnostic))+
        geom_histogram(stat = "identity", width = 0.3, aes(fill = Diagnostic)) +
        geom_text( aes(x  = 1,label = People)) +
        theme_minimal() + scale_fill_manual(values = c("#75d191", "#75a6d1", "#75d1b5")) +
        theme( legend.position = "none",axis.title = element_blank(),axis.text = element_text(size=15),
               axis.ticks.x = element_blank(), panel.grid = element_blank(),plot.background = element_rect(colour = "grey50")) + scale_x_continuous(position = "top") +
        guides(fill = "none")
      
      
    })
    
    
    output$Leyenda <- renderText({
      paste("Los siguientes gráficos interactivos dan una visión más general de cada terapia ejercida. Podemos ver el tiempo promedio de hospitalización de los pacientes según la terapia ejercida. ",
            "Cada punto representa un individuo, y pasando el cursor por encima vemos de manera específica su descripción.", "El color de los puntos indica el sexo del paciente, amarillo y verde, representando mujeres y hombres correspondientemente; y",
            "la medida de los puntos corresponde a la edad del paciente. Las terapias están codificadas con números a en la parte inferior del gráfico; pasando en cursor por encima vemos de que terapia se trata." )
    })
    
    output$TreatTime.Diag1 <- renderPlotly({
      req(nrow(DataSimilarCases$GeneralData.Diag1) > 0)
      
      ggplotly(ggplot(DataSimilarCases$GeneralData.Diag1, aes( x = reorder(Primera.Terapia, Días.de.hospitalización, mean),
                                                               y = Días.de.hospitalización)) +
                 geom_boxplot(outlier.shape = NA, outlier.alpha = 0) +
                 geom_jitter(posiion = "jitter", aes(size = Edad, color = Sexo,
                                                     label7 = Edad, label1 =Sexo, label2 =Hospital, label3 = Días.de.hospitalización, label4 = Tipo.de.admisión, label5 = Segundo.Diagnóstico, label6 = Segunda.Terapia)) +
                 geom_text(data = data.frame(Num = seq(1, length(unique(DataSimilarCases$GeneralData.Diag1$Primera.Terapia)), by = 1),
                                             y = -0.5, Terapia = unique(reorder(DataSimilarCases$GeneralData.Diag1$Primera.Terapia, DataSimilarCases$GeneralData.Diag1$Días.de.hospitalización, mean))),
                           aes(x = Num, y =y, label = Num, textterapy = Terapia ))+
                 scale_size_continuous(range = c(0.1,2.5)) + scale_color_manual(values = c("#106e29", "#dde330")) +
                 ylab("Días de hospitalización") + xlab("Terapia") +
                 labs(title = "Primer diagnóstico ") +
                 theme_bw()+theme(legend.position = "none", axis.text.x = element_blank()), tooltip = c("label7", "label1", "label2", "label3", "label4", "label5", "label6","textterapy") )
      
      
      
      
    })
    
    output$TreatTime.Diag2 <- renderPlotly({
      req(nrow(DataSimilarCases$GeneralData.Diag2) > 0)
      
      ggplotly(ggplot(DataSimilarCases$GeneralData.Diag2, aes( x = reorder(Segunda.Terapia, Días.de.hospitalización, mean),
                                                               y = Días.de.hospitalización)) +
                 geom_boxplot(outlier.shape = NA, outlier.alpha = 0) +
                 geom_jitter(posiion = "jitter", aes(size = Edad, color = Sexo,
                                                     label7 = Edad, label1 =Sexo, label2 =Hospital, label3 = Días.de.hospitalización, label4 = Tipo.de.admisión, label5 = Primer.Diagnóstico, label6 = Primera.Terapia)) +
                 geom_text(data = data.frame(Num = seq(1, length(unique(DataSimilarCases$GeneralData.Diag2$Segunda.Terapia)), by = 1),
                                             y = -0.5, Terapia = unique(reorder(DataSimilarCases$GeneralData.Diag2$Segunda.Terapia, DataSimilarCases$GeneralData.Diag2$Días.de.hospitalización, mean))),
                           aes(x = Num, y =y, label = Num, textterapy = Terapia ))+
                 scale_size_continuous(range = c(0.1,2.5)) + scale_color_manual(values = c("#106e29", "#dde330")) +
                 ylab("Días de hospitalización") + xlab("Terapia") +
                 labs(title = "Segundo diagnóstico") +
                 theme_bw()+theme(legend.position = "none", axis.text.x = element_blank()), tooltip = c("label7", "label1", "label2", "label3", "label4", "label5", "label6","textterapy") )
      
      
      
      
    })
    
    output$TreatTime.Diag121 <- renderPlotly({
      req(nrow(DataSimilarCases$GeneralData.Diag12) > 0)
      
      ggplotly(ggplot(DataSimilarCases$GeneralData.Diag12, aes( x = reorder(Primera.Terapia, Días.de.hospitalización, mean),
                                                                y = Días.de.hospitalización)) +
                 geom_boxplot(outlier.shape = NA, outlier.alpha = 0) +
                 geom_jitter(posiion = "jitter", aes(size = Edad, color = Sexo,
                                                     label7 = Edad, label1 =Sexo, label2 =Hospital, label3 = Días.de.hospitalización, label4 = Tipo.de.admisión, label5 = Segundo.Diagnóstico, label6 = Segunda.Terapia)) +
                 geom_text(data = data.frame(Num = seq(1, length(unique(DataSimilarCases$GeneralData.Diag12$Primera.Terapia)), by = 1),
                                             y = -0.5, Terapia = unique(reorder(DataSimilarCases$GeneralData.Diag12$Primera.Terapia, DataSimilarCases$GeneralData.Diag12$Días.de.hospitalización, mean))),
                           aes(x = Num, y =y, label = Num, textterapy = Terapia))+
                 scale_size_continuous(range = c(0.1,2.5)) + scale_color_manual(values = c("#106e29", "#dde330")) +
                 ylab("Días de hospitalización") + xlab("Terapia") +
                 labs(title = "Primera Terapia") +
                 theme_bw()+theme(legend.position = "none", axis.text.x = element_blank()), tooltip = c("label7", "label1", "label2", "label3", "label4", "label5", "label6","textterapy") )
      
      
      
    })
    
    output$TreatTime.Diag122 <- renderPlotly({
      req(nrow(DataSimilarCases$GeneralData.Diag12) > 0)
      
      ggplotly(ggplot(DataSimilarCases$GeneralData.Diag12, aes( x = reorder(Segunda.Terapia, Días.de.hospitalización, mean),
                                                                y = Días.de.hospitalización)) +
                 geom_boxplot(outlier.shape = NA, outlier.alpha = 0) +
                 geom_jitter(posiion = "jitter", aes(size = Edad, color = Sexo,
                                                     label7 = Edad, label1 =Sexo, label2 =Hospital, label3 = Días.de.hospitalización, label4 = Tipo.de.admisión, label5 = Primer.Diagnóstico, label6 = Primera.Terapia)) +
                 geom_text(data = data.frame(Num = seq(1, length(unique(DataSimilarCases$GeneralData.Diag12$Segunda.Terapia)), by = 1),
                                             y = -0.5, Terapia = unique(reorder(DataSimilarCases$GeneralData.Diag12$Segunda.Terapia, DataSimilarCases$GeneralData.Diag12$Días.de.hospitalización, mean))),
                           aes(x = Num, y =y, label = Num, textterapy = Terapia ))+
                 scale_size_continuous(range = c(0.1,2.5)) + scale_color_manual(values = c("#106e29", "#dde330")) +
                 ylab("Días de hospitalización") + xlab("Terapia") +
                 labs(title = "Segunda Terapia") +
                 theme_bw()+theme(legend.position = "none", axis.text.x = element_blank()), tooltip = c("label7", "label1", "label2", "label3", "label4", "label5", "label6","textterapy") )
      
      
      
      
    })
    
    
    
  }
  
  ### ----------  Forth page: Future diagnoses
  {
    
    Data3rdDiag <- reactiveValues()
    
    observeEvent(input$goButton, {
      Data3rdDiag$New.Pacient <<- DataPredTiempo$paciente[1,-c(grep("categorise_age_1|categorise_therapies_1st_less_names|categorise_therapies_2nd_less_names", colnames(DataPredTiempo$paciente) ))]
      Data3rdDiag$New.Pacient$age <<- input$age1
      
      Data3rdDiag$GeneralData <<- db.filterd.org[which((db.filterd.org$categorise_diagnostics_1st_less_names %in% Data3rdDiag$New.Pacient$categorise_diagnostics_1st_less_names) |
                                                         (db.filterd.org$categorise_diagnostics_2nd_less_names %in% Data3rdDiag$New.Pacient$categorise_diagnostics_2nd_less_names)),]
      Data3rdDiag$GeneralDataorg <<- Data3rdDiag$GeneralData
      Data3rdDiag$FutureDiag <<- predict.future.diag(Data3rdDiag$GeneralData)$probable
      
    })
    
    observeEvent(input$ChangeFuture, {
      
      Data3rdDiag$GeneralData <<- db.filterd.org[which(((db.filterd.org$categorise_diagnostics_1st_less_names %in% Data3rdDiag$New.Pacient$categorise_diagnostics_1st_less_names) | 
                                                          (db.filterd.org$categorise_diagnostics_2nd_less_names %in% Data3rdDiag$New.Pacient$categorise_diagnostics_2nd_less_names)) &
                                                         (db.filterd.org$sex %in% GetInput(input$SexID4, Sex = T)) & (db.filterd.org$admission_types %in% GetInput(input$AdmisionID4, Admision = T)) &
                                                         ((db.filterd.org$age >= input$AgeID4[1]) & (db.filterd.org$age <= input$AgeID4[2])) & (db.filterd.org$hospital %in% GetInput(input$HospitalID4, Hospital = T))),]
      
      
      Data3rdDiag$FutureDiag <<- predict.future.diag(Data3rdDiag$GeneralData)$probable
      
    })
    
    output$FutureDiag <- renderPlotly({
      ggplotly(ggplot(Data3rdDiag$FutureDiag , aes(x = Diagnóstico, y = Ocurrencia))+
                 geom_histogram(stat = "identity", width = 0.75,aes(label = Diagnóstico, label1 = Ocurrencia, fill = Color)) +
                 theme_bw() + ylab("Número de ocurrencias \n")+ scale_fill_manual(values =c("#7ec9cf","#245559"))+
                 theme( legend.position = "none", axis.title.x = element_blank(),axis.text.y = element_text(size=10),
                        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
                 guides(fill = "none"), tooltip = c("label", "label1"))
      
      
    })
    
    output$diag3.presentation <- renderText({
      paste("Teniendo en cuenta otros pacientes que han sufrido de los mismos diagnósticos, podemos observar y predecir que otros diagnósticos puede padecer el paciente en el futuro.",
            "El tipo de terapia, a veces tiene un efecto sobre los futuros diagnósticos que se puedan observar,",
            "así pues, seleccionando las terapias a usar, podemos ver que futuros diagnósticos han estado predichos a padecer.")
    })
    
    observeEvent(input$ChangeTherapy, {
      
      Data3rdDiag$DataTest <- DataPredTiempo$paciente[1,]      
      Data3rdDiag$DataTest$categorise_therapies_2nd_less_names <- input$TherID2
      Data3rdDiag$DataTest$categorise_therapies_1st_less_names <- input$TherID1
      
      Data3rdDiag$FutureDiagPred <- predict.future.diag(Data3rdDiag$GeneralDataorg,test = Data3rdDiag$DataTest,predict = T)$pred
      
      Data3rdDiag$FutureDiagText <- Data3rdDiag$FutureDiagPred
    })
    
    output$Diag3Text <- renderText({
      Data3rdDiag$FutureDiagText
    })
    
    output$diag3otros <- renderText({
      paste("Observación de futuros diagnósticos que han sufrido pacientes parecidos. En este gráfico vemos que diagnósticos han sido mas concurrentes, podemos filtrar los datos para tener una visión más acotada al paciente.")
    })
    
    
    
    
  }
  
}


ui <- dashboardPage(skin = "black",
             
   ### ----------  Header      
  dashboardHeader(tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 120px}"),
                          tags$style(".main-header .logo {height: 120px}")
  ),
  title = tags$img(src = Predirect33, height = 110, width=330)),

  ### ---------- Sidebar
 dashboardSidebar(
   tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
   tags$style("#name1 {font-size:20px;}"),
   tags$style("#age1 {font-size:20px;}"),
   tags$style(HTML("
      .main-sidebar{
        width: 350px;
      }  .left-side, .main-sidebar {padding-top: 150px}
    ")),
    sidebarMenu(
      menuItem("  Datos del paciente", tabName = "zero", icon = icon("user-edit"),
               createSideBar(1, "Entrar datos del paciente:")),
      menuItem("  ", tabName = "start"),
      menuItem("  Cuadro clínico", tabName = "first", icon = icon("book-medical")),
      menuItem("  Pronóstico de Hospitalizacion", tabName = "second", icon = icon("calendar-plus")),
      menuItem("  Casos similares", tabName = "third", icon = icon("users")),
      menuItem("  Posibles futuros diagnósticos", tabName = "forth", icon = icon("hospital-user"))
      
    )
  ),
  dashboardBody(
    tags$style(tableHTML::make_css(list('.box', 
                                        c('font-size', 'font-family', 'color'), 
                                        c('17px', 'arial', '#282928')))),
    tags$head(tags$style(HTML('   .main-sidebar{    font-size: 20px;    width: 350px;      }      .main-header > .navbar {        margin-left: 350px;      }      .main-header .logo {         width: 350px;      }      .content-wrapper, .main-footer, .right-side {          margin-left: 350px;      }    '))),
    tabItems(

      ### ----------  First Page
      tabItem(tabName = "start",
              
              h2(span( icon("long-arrow-alt-left"), " Para empezar introduzca los datos del paciente")),
              fluidRow(h2("")),
              fluidRow(
                align = "center",
                column(12,
                img(src = Imagen_PredirectTrans, height = 300)
                )
              )
              ),
      
      tabItem(tabName = "first",
              fluidRow(
                column(2,),
                column(10,
                h2("Cuadro clínico del paciente"),
                
                box(status = "primary", textOutput("descripcion"), width = 14)
               
                )
                ),
            
              fluidRow(h3("")),
              fluidRow(
                align = "center",
                column(2,),
                column(10,
                       
                box(status = "warning", 
                    actionButton(inputId = "PatientInfo",
                                                      label = tags$p(fa("file-medical"), " Ver historial completo del paciente"),style='padding:4px; font-size:110%'),
                    
                    h3(" "),
                    DTOutput("table.ClinHistory"), width = 14)
                )
              )
             ),
      
      ### ---------- Second page
      tabItem(tabName = "second",
              fluidRow(
                column(2,),
                column(10,
                       h2("Predicción del tiempo de hospitalización del paciente")
                )
              ),
              fluidRow(
                column(2,),
                column(7,
                 box( status = "primary",width = 14,
                      textOutput("tiempo.pred"),
                      br(),
                       textOutput("anova.pred")
                 )
                ),
                column(3,
                       
                       img(src = Grado_hospitalizacion, height = 300),
                )),
              
              fluidRow(h3("")),
              
              fluidRow(
                
                column(2,),
                column(10,
                   box(status = "warning", width = 14, DTOutput("table.pred"))
                
                )),
             
              fluidRow(
                column(2,),
                column(10,
                h3("Distribuciones del tiempo de hospitalización")),
              ),
              
              fluidRow(
                column(2,),
                column(10,
                       
                box(title = "Distribución del tiempo predicho", solidHeader = T,status = "success", width = 6,plotOutput("predicted.plot")),
                box(title = "Distribución del tiempo en casos similares", solidHeader = T,status = "success", width = 6,plotOutput("density.similar")))
                
                
              )
      ),

      ### ----------  Third page
  tabItem(tabName = "third",
        fluidRow(
          column(2,),
          column(10,
                 h2("Visión general de casos similares")
          )
        ),
        fluidRow(
          column(2,),
          column(10,
                 box( status = "primary",width = 14,
                      textOutput("Pp.SimilarPeople"))
                 )
          ),
        
        fluidRow(h3("")),
        
        h3("Otros pacientes con los mismos diagnósticos"),
       
        fluidRow(
          column(2,),
          column(6,
  
                 box(title = "Diagnósticos compartidos", solidHeader=T, status = "success", width = 14,     plotOutput("PlotSimilarPeople"))
          ),
          column(4,
                 
                 box( status = "primary",width = 14, 
                      h3("Filtrar datos: "),
                 
                 checkboxGroupInput(inputId = "SexID", label = "",
                                    choices = c("Mujer" ,
                                                "Hombre" ),
                                    inline = TRUE),
                 
                 sliderInput(inputId = "AgeID", 
                             label= "Según edad: ", 
                             min = min(db.filterd.org$age, na.rm = T),
                             max = max(db.filterd.org$age, na.rm = T),
                             value =  c(min(db.filterd.org$age, na.rm = T),max(db.filterd.org$age, na.rm = T))),
                 
                 checkboxGroupInput(inputId = "AdmisionID", label = "Tipo de Admisión:",
                                    choices = c("Programada" ,
                                                "Urgente" ),
                                    inline = TRUE),
                 
                 selectInput(inputId = "HospitalID", 
                             label= "Según Hospital: ", 
                             choices = sort(unique(db.filterd$hospital)), 
                             multiple=T,
                             selected = character(0)),
                 actionButton(inputId = "ChangePlot",
                              label = tags$p(fa("sync"), " Actualizar gráficos"),style='padding:4px; font-size:110%')
                 
                            )
          )
        ),
        fluidRow(
          column(2,),
          
          column(10,
                 
                 h3("Terapias ejercidas en pacientes con un diagnóstico en común"),
                 box( status = "primary",width = 14,  textOutput("Leyenda")) )
          
        ),
        
        fluidRow(
          
          column(2,),
          column(5,
                 box(status = "success", width = 12,    plotlyOutput("TreatTime.Diag1"))),
          column(5,
                 box(status = "success", width = 12,   plotlyOutput("TreatTime.Diag2")))
 
          
        ),
        
        fluidRow(
          column(2,),
          
          column(10,
                 
                 h3("Terapias ejercidas en pacientes con los dos diagnósticos en común"))
        ),
        
        
        fluidRow(
          column(2,),
          column(5,
                 box(status = "success", width = 14,    plotlyOutput("TreatTime.Diag121"))),
          column(5,
                 box(status = "success", width = 14,   plotlyOutput("TreatTime.Diag122")))
    
          
        )
   
  ),

  ### ---------- Forth page
tabItem(tabName = "forth",
        fluidRow(
          column(2,),
          column(10,
                 h2("Posibles futuros diagnósticos"),
          )
        ),
        fluidRow(
          column(2,),
          column(10,
                 box( status = "primary",width = 14,
                      textOutput("diag3.presentation") )
                 )
          ),
        h4("Selecciona las terapias a llevar a cabo:"),
        
        fluidRow(
          align = "center",
          column(2,),
          box( 
           
              status = "primary",width = 7,
              fluidRow(
                align = "center",
               column(3,
                 selectInput(inputId = "TherID1", 
                             label= "Terapia 1: ",
                             width = "100%",
                             choices = sort(unique(GeneralData$categorise_therapies_1st_less_names)), 
                             multiple=F,
                             selected = character(0)),
                 actionButton(inputId = "ChangeTherapy",
                              label = tags$p(fa("search-plus"), " Predecir"),style='padding:4px; font-size:110%')),
          column(3,
                 selectInput(inputId = "TherID2", 
                             label= "Terapia 2: ", 
                             width = "100%",

                             choices = sort(unique(GeneralData$categorise_therapies_2nd_less_names)), 
                             multiple=F,
                             selected = character(0)))
              )
         
          ),
          
         column(4,
              box(    status = "primary",width = 14,
                 textOutput("Diag3Text")))
          
        ),
        h3("Visión general de otros pacientes"),
        box(   status = "primary", width = 14,
               textOutput("diag3otros")),
        fluidRow(
          column(2,),
          column(8,
        box(title ="Posibles futuros diagnósticos",solidHeader=T,status = "success", width = 14,       plotlyOutput("FutureDiag"))
          ),
          column(3,
              box( status = "primary",width = 14, 
                 h3("Filtrar datos: "),
                 
                 checkboxGroupInput(inputId = "SexID4", label = "",
                                    choices = c("Mujer" ,
                                                "Hombre" ),
                                    inline = TRUE),
                 
                 sliderInput(inputId = "AgeID4", 
                             label= "Según edad: ", 
                             min = min(db.filterd.org$age, na.rm = T),
                             max = max(db.filterd.org$age, na.rm = T),
                             value =  c(min(db.filterd.org$age, na.rm = T),max(db.filterd.org$age, na.rm = T))),
                 
                 checkboxGroupInput(inputId = "AdmisionID4", label = "Tipo de Admisión",
                                    choices = c("Programada" ,
                                                "Urgente" ),
                                    inline = TRUE),
                 
                 selectInput(inputId = "HospitalID4", 
                             label= "Según Hospital: ", 
                             choices = sort(unique(db.filterd$hospital)), 
                             multiple=T,
                             selected = character(0)),
                 actionButton(inputId = "ChangeFuture",
                              label = tags$p(fa("sync"), " Actualizar gráfico"),style='padding:4px; font-size:110%')
              )
              )
                 
          )
        )
      )
    )
)

shinyApp(ui = ui, server = server)


