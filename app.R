# BUGS (FIXED):
#  -an error has occurred. Check your logs or contact the app author for clarification. (shinyapps.io)
#   from logs:  Warning: Error in : Column 6 must be named. (used lme4 instead of lmerTest)
#   https://github.com/strengejacke/sjPlot/issues/392
#  -model results tab not working: Warning: Error in : No tidy method for objects of class lmerMod (broom.mixed library)
#  -summ function doesn't work (can't use pval=T)
#  -conditional panel for mouse boxplot breaks app on server 


# TO DO: 
#  -colorblind friendly palette for plots
#  -change variable inputs so nothing is selected when data is first uploaded
#  -should the type of the covariates be specified (if the variable name contains age or sex make it a factor?)

library(shiny)
library(lme4)
library(dplyr)
library(tidyr) #gather
library(ggplot2)
library(huxtable)
library(broom.mixed)
library(jtools) #export_summs
library(table1)
library(pbkrtest)

# Define UI ----
ui <- fluidPage(
    
    # application title
    titlePanel("EasyLME"),
    
    # globally hide errors
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    
    # layout of the GUI
    sidebarLayout(
        
        # define sidebar panel
        sidebarPanel(
            
            width = 3,
            uiOutput("messageSelectVars"),
            
            # greyed out panel
            wellPanel(
                
                uiOutput("demoData"),
                
                # selection of data file type
                selectInput(inputId="dataFileType",
                            label="Select type of data file:",
                            choices=c(" "=NA, "CSV template"='CSV',"Demo data"='Demo')),
                
                # show file upload control if user selects to upload a file
                conditionalPanel(
                    condition="input.dataFileType=='CSV'",
                    fileInput(inputId="dataFile",
                              label={h5("Upload data file:")},
                              multiple=FALSE,
                              accept=c("text/csv", ".csv"))),
                
                # selection of variables needed in the analysis
                uiOutput("selectResp"),
                uiOutput("selectTime"),
                uiOutput("selectGrouping"),
                uiOutput("selectCovar"),
                uiOutput("selectNestedRE"),
                uiOutput("expr"),
                uiOutput("expra"),
                uiOutput("exprb"))),
        
        mainPanel(
            tabsetPanel(
                tabPanel(title="Welcome", includeHTML("Welcome.html")),
                tabPanel(title="Data Summary",
                         uiOutput("summaries"), 
                         includeCSS("table1_defaults.css")),
                tabPanel(title="Exploratory Plots",
                         plotOutput("scatterplot", height = 400, width = 600),
                         plotOutput("avg_trend", height = 400, width = 600)),
                tabPanel(title="Model Results",
                         uiOutput("results_table"),
                         uiOutput("selectModel"),
                         conditionalPanel(
                             condition="input.model != ''",
                             verbatimTextOutput("model_summary"))),
                tabPanel(title="Fitted Lines",
                         uiOutput("selectModel2"),
                         plotOutput("trendlines", height = 500, width = 800),
                         plotOutput("trendlines2", height = 500, width = 800)))
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
    # source code for model functions
    source("Functions.R")
    
    # message - DEMO data
    output$demoData <- renderUI({
        if (input$dataFileType =="Demo") {h4("DEMO data")}
    })
    
    # create a dataset with the imported data
    inData <- reactive({
        observe(input$dataFile)
        
        if (input$dataFileType=="Demo") {
            location <- file.path("data", "data_long.csv")
            data <- read.csv(location, header = T, stringsAsFactors = T)
            return(data)
        }
        
        if(is.null(input$dataFile)) return()
        
        if (input$dataFileType=="CSV") {
            inFile <- input$dataFile
            data <- read.csv(inFile$datapath, header = T, comment.char = "#", stringsAsFactors = T)
            return(data)
        }
    })
    
    # SIDEBAR
    
    # GUI - selecting response variable
    output$selectResp <- renderUI({
        req(inData())
        varSelectInput(inputId="response",
                       label="Choose response variable:", 
                       data=inData(), 
                       if (input$dataFileType=="Demo"){selected="Perc.Weight"})
    })
    
    # GUI - selecting time variable
    output$selectTime <- renderUI({
        req(inData())
        varSelectInput(inputId="timeVar",
                       label="Choose time variable:", 
                       data=inData(),
                       if (input$dataFileType=="Demo"){selected="Time"})
    })
    
    # GUI - selecting grouping variable
    output$selectGrouping <- renderUI({
        req(inData())
        varSelectInput(inputId="groupVar",
                       label="Choose grouping variable:", 
                       data=inData(),
                       if (input$dataFileType=="Demo"){selected="Donor.Status"})
    })
    
    # GUI - selecting covariates
    output$selectCovar <- renderUI({
        req(inData())
        varSelectInput(inputId="Covar",
                       label="Choose covariates:", 
                       data=inData(), multiple=T,
                       if (input$dataFileType=="Demo"){selected=""})
    })
    
    # GUI - selecting nested random effects
    output$selectNestedRE <- renderUI({
        req(inData())
        checkboxInput(inputId="nestedRE",
                      label="Do you have a nested random effect model?",
                      if (input$dataFileType=="Demo"){
                          value=TRUE
                      } else {
                          value=FALSE
                      })
    })
    
    # GUI - selecting random effect if not nested
    re <- reactive({
        req(inData(), input$nestedRE==FALSE)
        if (input$nestedRE==FALSE){  
            varSelectInput(inputId="re",
                           label="Random Effect: ", 
                           data=inData(),
                           if (input$dataFileType=="Demo"){selected="Donor"}) 
        }
    })
    
    # GUI - selecting random effects if nested
    mouse <- reactive({
        req(inData(), input$nestedRE)
        if (input$nestedRE==TRUE){  
            varSelectInput(inputId="mouse",
                           label="This ", 
                           data=inData(),
                           if (input$dataFileType=="Demo"){selected="Mouse"})  
        }
    })
    
    donor <- reactive({
        req(inData(), input$nestedRE)
        if (input$nestedRE==TRUE){  
            varSelectInput(inputId="donor",
                           label="is nested within ", 
                           data=inData(),
                           if (input$dataFileType=="Demo"){selected="Donor"})  
        }
    })
    
    output$expr <- renderUI({
        mouse()
    })
    output$expra <- renderUI({
        donor()
    })
    output$exprb <- renderUI({
        re()
    })
    
    # specify the class types of each variable
    dataProcessed <- reactive({
        data = inData()
        
        data[[input$response]] = as.numeric(data[[input$response]])
        data[[input$timeVar]] = as.numeric(data[[input$timeVar]])
        data[[input$groupVar]] = as.factor(data[[input$groupVar]])
        
        if (input$nestedRE==TRUE){
            data[[input$mouse]] = as.factor(data[[input$mouse]])
            data[[input$donor]] = as.factor(data[[input$donor]])
        } else {
            data[[input$re]] = as.factor(data[[input$re]])
        }
        data
    })
    
    # TABS
    
    # message - used on all tabs
    output$messageSelectVars <- renderUI({
        if(is.null(inData())) {h4("Please use the menus below to upload data and select
                                    variables to analyze.")}
    })
    
    # Data Summary
    
    output$summaries <- renderUI({
        req(dataProcessed())
        data = dataProcessed()
        
        if (input$nestedRE==FALSE){
            data_sum = data %>% dplyr::select(!!input$response, !!input$timeVar, !!input$groupVar, !!input$re, !!!input$Covar)
        }
        if (input$nestedRE==TRUE){
            data_sum = data %>% dplyr::select(!!input$response, !!input$timeVar, !!input$groupVar, !!input$donor, !!input$mouse, !!!input$Covar)
        }
        vars = paste(names(data_sum), collapse="+")
        formula = as.formula(paste("~", vars))
        sum = print(table1(formula, data))
        return(sum)
    })
    
    # Exploratory Plots
    
    # scatterplot of overall data trends
    output$scatterplot <- renderPlot({
        req(dataProcessed(), input$timeVar, input$response)
        data = dataProcessed()
        
        if (input$nestedRE==TRUE){
            req(input$donor)
            color = input$donor
        } else {
            req(input$re)
            color = input$re
        }
        
        ggplot(data) + 
            geom_point(aes(x=!!input$timeVar, y=!!input$response, color=!!color), size=2.1) + 
            theme_bw(base_size=17) + labs(title="Overall Trends") + guides(size="none")
    })
    
    # trendlines per donor
    output$avg_trend <- renderPlot({
        req(dataProcessed())
        data = dataProcessed()
        
        if (input$nestedRE==TRUE){
            req(input$donor)
            donor = input$donor
        } else {
            req(input$re)
            donor = input$re
        }
        
        donors = levels(data[[donor]])
        avg_donors = c()
        
        for (i in 1:length(donors)) {
            
            avg_weight = data %>% filter(!!donor == donors[i]) %>% group_by(!!input$timeVar) %>% summarize(mean(!!input$response))
            colnames(avg_weight) = c("Time", donors[i])
            
            if (i==1) {
                avg_donors = avg_weight
            } else {
                avg_donors = full_join(avg_donors, avg_weight, by="Time")
            }
        }
        
        avg_donors2 = gather(avg_donors, donor, !!input$response, -Time)
        colnames(avg_donors2)[2] = "Donor"
        avg_donors2 = avg_donors2 %>% arrange(desc(Donor))
        
        group = data %>% dplyr::select(!!donor, !!input$groupVar) %>% distinct(!!donor, .keep_all = T)
        group[[donor]] = as.character(group[[donor]])
        group[[input$groupVar]] = as.character(group[[input$groupVar]])
        
        times = length(unique(data[[input$timeVar]]))
        
        group2 = as.data.frame(cbind(rep(group[[donor]], each=times), rep(group[[input$groupVar]], each=times)))
        colnames(group2) = c("Donor", "Group")
        group2 = group2 %>% arrange(desc(Donor))
        
        avg_donor_data = avg_donors2 %>% mutate(group2$Group)
        colnames(avg_donor_data)[4] = "Group"
        
        title = paste(donor, "Trendlines", sep=" ")
        
        ggplot() +
            geom_line(avg_donor_data, mapping=aes(x=Time, y=!!input$response, color=Donor, linetype=Group), lwd=1) +
            theme_bw(base_size=17) + labs(title=title)
    })
    
    # Model Results
    
    # fit models based on input selections (returns a list with 1-model name and 2-model)
    slopes <- reactive({
        if (input$nestedRE==TRUE){
            slopes_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$donor, input$mouse, input$Covar)
        }
    })
    mouse_slope <- reactive({
        if (input$nestedRE==TRUE){
            mouse_slope_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$donor, input$mouse, input$Covar)
        }
    })
    mouse_ <- reactive({
        if (input$nestedRE==TRUE){
            mouse_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$donor, input$mouse, input$Covar)
        } else {
            REslope_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$re, input$Covar)
        }
    })
    mouse_int <- reactive({
        if (input$nestedRE==TRUE){
            mouse_int_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$donor, input$mouse, input$Covar)
        } else {
            REint_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$re, input$Covar)
        }
    })
    noRE <- reactive({
        noRE_model(dataProcessed(), input$response, input$timeVar, input$groupVar, input$Covar)
    })
    
    Names <- reactive({
        if (input$nestedRE==TRUE){
            model.names = c(slopes()[[1]], mouse_slope()[[1]], mouse_()[[1]],
                            mouse_int()[[1]], noRE()[[1]])
        } else {
            model.names = c(mouse_()[[1]], mouse_int()[[1]], noRE()[[1]])
        }
        model.names
    })
    
    # create model results table
    output$results_table <- renderUI({
        
        predictors = names(coef(noRE()[[2]]))
        
        if (input$nestedRE==TRUE){
            
            lrt1v2 = anova(slopes()[[2]], mouse_slope()[[2]])[2,8]
            lrt2v3 = anova(mouse_slope()[[2]], mouse_()[[2]])[2,8]
            lrt3v4 = anova(mouse_()[[2]], mouse_int()[[2]])[2,8]
            lrt4v5 = anova(mouse_int()[[2]], noRE()[[2]])[2,8]
            
            pvalues = huxtable(Pvalue = c(lrt1v2, lrt2v3, lrt3v4, lrt4v5, ""))
            number_format(pvalues) = list(function(x) prettyNum(x, digits=3, scientific = T))
            
            results = export_summs(slopes()[[2]], mouse_slope()[[2]], mouse_()[[2]],
                                   mouse_int()[[2]], noRE()[[2]], statistics = c("logLik"),
                                   error_pos = 'same', align = "center", bold_signif = 0.05, 
                                   stars = NULL, number_format = "%.2f", model.names = Names(),
                                   coefs = c("Group*" = predictors[2], "Time*" = predictors[3], 
                                             "Interaction*" = predictors[4]))
            
        } else {
            
            lrt3v4 = anova(mouse_()[[2]], mouse_int()[[2]])[2,8]
            lrt4v5 = anova(mouse_int()[[2]], noRE()[[2]])[2,8]
            
            pvalues = huxtable(Pvalue = c(lrt3v4, lrt4v5, ""))
            number_format(pvalues) = list(function(x) prettyNum(x, digits=3, scientific = T))
            
            results = export_summs(mouse_()[[2]], mouse_int()[[2]], noRE()[[2]],
                                   statistics = c("logLik"), error_pos = 'same', 
                                   align = "center", bold_signif = 0.05, stars=NULL,
                                   number_format = "%.2f", model.names = Names(),
                                   coefs = c("Group*" = predictors[2], "Time*" = predictors[3], 
                                             "Interaction*" = predictors[4]))
        }
        all.results = t(results) %>% add_columns(pvalues, after=5) %>% theme_plain() %>%
            set_right_border(everywhere, 1, 0.4) %>% set_left_border(everywhere, 5, 0.4) %>%
            set_bottom_border(1, everywhere, 0.4) %>% set_bold(1, everywhere) %>%
            set_caption("Model Comparison Table") %>% set_caption_pos("topleft") %>%
            set_position("left") 
        
        all.results[1,5] = "Likelihood**"
        all.results[1,6] = "P-values***"
        align(all.results) = "center"
        
        all.results = all.results %>% 
            add_footnote("*Coefficient estimates with standard errors in parantheses (bolded entries are significant at a 0.05 level)") %>%
            add_footnote("**Log likelihoods (a larger likelihood indicates a better model fit)", border=0) %>%
            add_footnote("***P-values from the likelihood ratio test of the model with the nested model below it", border=0)
        
        n_row = nrow(all.results)
            
        all.results = all.results %>%  
            set_left_padding(everywhere, 1, 3) %>% set_right_padding(everywhere, 1, 3) %>%
            set_top_padding(everywhere, everywhere, 3) %>% set_bottom_padding(everywhere, everywhere, 3) %>%
            set_top_padding((n_row-1):n_row, everywhere, 0) %>% set_bottom_padding((n_row-2):n_row, everywhere, 0) %>%
            set_number_format(everywhere, 5, list(function(x) prettyNum(x, big.mark=","))) 
        
        valign(all.results) = "middle"
        HTML(huxtable::to_html(all.results))
    })
    
    # select a specific model
    output$selectModel <- renderUI({
        selectInput(inputId="model",
                    label="Choose a specific model:", 
                    choices=c('', Names()))
    })
    
    # print model summary
    output$model_summary <- renderPrint({
        req(dataProcessed(), input$model)
        
        match = which(Names()==input$model)
        
        if (input$nestedRE==TRUE){
            models = c(slopes()[[2]], mouse_slope()[[2]], mouse_()[[2]],
                       mouse_int()[[2]], noRE()[[2]])
        } else {
            models = c(mouse_()[[2]], mouse_int()[[2]], noRE()[[2]])
        }
        
        if (input$model=="No Random Effects"){
            return(summ(noRE()[[2]], groups.table=F))
        } else {
            return(summ(models[[match]], groups.table=F))
        }
    })
    
    # select a specific model
    output$selectModel2 <- renderUI({
        selectInput(inputId="model2",
                    label="Choose a specific model:", 
                    choices=c('', Names()))
    })
    
    # Trendlines
    
    output$trendlines <- renderPlot({
        req(dataProcessed(), input$model2)
        data = dataProcessed()
        
        # determine which model was selected from the dropdown menu
        match = which(Names()==input$model2)
        
        if (input$nestedRE==TRUE){ #model choices for nested data
            models = c(slopes()[[2]], mouse_slope()[[2]], mouse_()[[2]],
                       mouse_int()[[2]], noRE()[[2]])
        } else { #model choices for non-nested data
            models = c(mouse_()[[2]], mouse_int()[[2]], noRE()[[2]])
        }
        
        if ((match==3 || match==4) && input$nestedRE==TRUE){ #mouse plot for nested data with no donor models
            return(mouse_lines(models[[match]], data, input$response, input$timeVar, input$groupVar, input$donor, input$mouse))
            
        } else if (input$model2=="No Random Effects" && input$nestedRE==TRUE){ #plot for no random effects model (nested)
            return(donor_lines(noRE()[[2]], data, input$response, input$timeVar, input$groupVar, input$donor))
            
        } else if (input$model2=="No Random Effects" && input$nestedRE==FALSE){ #plot for no random effects model (non-nested)
            return(donor_lines(noRE()[[2]], data, input$response, input$timeVar, input$groupVar, input$re))
            
        } else if ((match==1 || match==2) && input$nestedRE==TRUE){ #donor plot for nested data with donor models
            return(donor_lines(models[[match]], data, input$response, input$timeVar, input$groupVar, input$donor))
            
        } else { #plot for non-nested data
            return(donor_lines(models[[match]], data, input$response, input$timeVar, input$groupVar, input$re))
        }
    })
    
    output$trendlines2 <- renderPlot({
        req(dataProcessed(), input$model2, input$nestedRE) #requires nested data
        data = dataProcessed()
        
        # determine which model was selected from the dropdown menu
        match = which(Names()==input$model2)
        
        # model choices for nested data
        models = c(slopes()[[2]], mouse_slope()[[2]], mouse_()[[2]],
                   mouse_int()[[2]], noRE()[[2]])
        
        if (match==3 || match==4 || match==5){ #don't show plot for nested data with no donor models
            return(NULL)
            
        } else { #mouse plot for nested data with donor models
            return(mouse_lines(models[[match]], data, input$response, input$timeVar, input$groupVar, input$donor, input$mouse))
        }
        
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)