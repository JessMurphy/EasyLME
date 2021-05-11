# BUGS (FIXED):
#  -an error has occurred. Check your logs or contact the app author for clarification. (shinyapps.io)
#   from logs:  Warning: Error in : Column 6 must be named. (used lme4 instead of lmerTest)
#   https://github.com/strengejacke/sjPlot/issues/392
#  -model results tab not working: Warning: Error in : No tidy method for objects of class lmerMod (broom.mixed library)
#  -summ function doesn't work (can't use pval=T)

# options(shiny.error = browser)


# TO DO: 
#  -colorblind friendly palette for plots
#  -change variable inputs so nothing is selected when data is first uploaded
#  -add covariates to summary table

library(shiny)
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr) #gather, drop_na
library(ggplot2)
library(plotly)
library(broom.mixed)
library(jtools) #export_summs, summ
library(kableExtra)

# Define UI ----
ui <- fixedPage(
    
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
                
                tabPanel(title="Data Summary", h3(""),
                         htmlOutput("summaries")),
                
                tabPanel(title="Exploratory Plots",
                         plotlyOutput("scatterplot"), h3(""),
                         h5(strong("Figure 1. Scatterplot of the response variable vs time."), br(),
                            "This plot can be used to verify the linearity assumption between the response 
                            variable and time before proceeding with a linear mixed effects model."),
                         plotlyOutput("avg_trend"), h3(""),
                         h5(strong(textOutput("figure2")), textOutput("legend2")), h3(""),
                         plotlyOutput("facet_plots", height="600px"),
                         conditionalPanel(condition="input.nestedRE",
                                          h5(strong("Figure 3. Trendlines faceted by the clustered random effect variable."), br(),
                                          "These plots can help determine if a random slope and/or random intercept would be 
                                          appropriate for the nested random effect variable."))),
                
                tabPanel(title="Model Results", h3(""),
                         htmlOutput("results_table"),
                         uiOutput("selectModel"),
                         conditionalPanel(
                             condition="input.model != ''",
                             verbatimTextOutput("model_summary"))),
                
                tabPanel(title="Diagnostic Plots",
                         plotlyOutput("resid_plot", height="800px"), h3(""),
                         h5(strong("Figure 4. Residual profile plots in increasing order of model complexity,"),
                            "where each line represents a single profile of the random effect variable.
                            These plots are useful to visually compare the different model fits. Models with a 
                            large, nonconstant variability in the residuals over time indicate a worse model fit, whereas 
                            models with a small, constant variability in the residuals over time indicate a better model fit.")),
                
                tabPanel(title="Fitted Lines", h3(""),
                         uiOutput("selectModel2"),
                         plotlyOutput("trendlines"), h3(""),
                         h5(strong(textOutput("figure5")), textOutput("legend5")),
                         conditionalPanel(condition="(input.model2 && input.nestedRE)",
                                          plotlyOutput("trendlines2"), h3(""),
                                          h5(strong(textOutput("figure6")), textOutput("legend6")))))
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
                           if (input$dataFileType=="Demo"){selected="Mouse"}) 
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
            
            # relevel donor variable by decreasing response variable
            max_donors = data %>% group_by(!!input$donor) %>% summarize(max=max(!!input$response, na.rm=T)) %>% arrange(desc(max))
            data[[input$donor]] = factor(data[[input$donor]], levels=max_donors[[input$donor]])
            
            # remove response na values & arrange by donor variable
            data2 = data %>% drop_na(!!input$response) %>% arrange(!!input$donor)
            
            # relevel mouse variable based on donor variable
            data2[[input$mouse]] = factor(data2[[input$mouse]], levels=unique(data2[[input$mouse]]))
            
        } else if (input$nestedRE==FALSE){
          
          req(input$re)
            
            # relevel random effect variable by decreasing response variable
            max_re = data %>% group_by(!!input$re) %>% summarize(max=max(!!input$response, na.rm=T)) %>% arrange(desc(max))
            data[[input$re]] = factor(data[[input$re]], levels=max_re[[input$re]])
            
            # remove response na values & arrange by random effect variable
            data2 = data %>% drop_na(!!input$response) %>% arrange(!!input$re)
        }
        data2
    })
    
    # TABS
    
    # message - used on all tabs
    output$messageSelectVars <- renderUI({
        if(is.null(inData())) {h4("Please use the menus below to upload data and select
                                    variables to analyze.")}
    })
    
    # Data Summary
    
    output$summaries <- renderText({
        req(dataProcessed())
        data = dataProcessed()
        
        if (input$nestedRE==FALSE){
            data_sum = summary_table(data, input$response, input$timeVar, input$groupVar, input$re, input$Covar)
        }
        if (input$nestedRE==TRUE){
            data_sum = summary_table(data, input$response, input$timeVar, input$groupVar, input$donor, input$Covar)
        }
        
        time = paste(input$timeVar)
        y = paste(input$response)
        
        data_sum %>% knitr::kable(format="html", align=c(rep('c', ncol(data_sum)))) %>% 
          kable_styling(bootstrap_options=c("striped", "hover", "responsive")) %>%
          add_header_above(c(" "=3, setNames(2, time), setNames(2, y)))
    })
    
    # Exploratory Plots
    
    # scatterplot of overall data trends
    output$scatterplot <- renderPlotly({
        req(dataProcessed(), input$timeVar, input$response)
        data = dataProcessed()
        
        if (input$nestedRE==TRUE){
            req(input$donor)
            color = input$donor
        } else {
            req(input$re)
            color = input$re
        }
        
        plot1 = ggplot(data) + 
            geom_point(aes(x=!!input$timeVar, y=!!input$response, color=!!color)) + 
            theme_bw(base_size=12) + theme(legend.title=element_blank()) + guides(size="none")
        
        ggplotly(plot1) %>%
            add_annotations(text=paste(color), xref="paper", yref="paper",
                             x=1.02, xanchor="left", y=0.9, yanchor="bottom", 
                             legendtitle=TRUE, showarrow=FALSE) %>%
            layout(legend=list(y=0.9, yanchor="top"))
    })
    
    # average trendlines per donor
    output$avg_trend <- renderPlotly({
      req(dataProcessed())
      data = dataProcessed()
      
      if (input$nestedRE==TRUE){
        req(input$donor)
        donor = input$donor
        
      } else {
        req(input$re)
        donor = input$re
      }
      
      avg_donors = data %>% group_by(!!donor, !!input$timeVar) %>% 
        summarize(Avg=mean(!!input$response), Group=first(!!input$groupVar))
      
      title = paste(donor, "Trendlines", sep=" ")
      
      plot3 = ggplot() +
        geom_line(avg_donors, mapping=aes(x=!!input$timeVar, y=Avg, color=!!donor, linetype=Group), lwd=0.75) +
        theme_bw(base_size=12) + labs(y=paste(input$response), linetype=paste(input$GroupVar)) +
        theme(legend.title=element_blank())
      
      ggplotly(plot3) %>%
        add_annotations(text=paste(donor), xref="paper", yref="paper",
                        x=1.02, xanchor="left", y=0.9, yanchor="bottom", 
                        legendtitle=TRUE, showarrow=FALSE) %>%
        layout(legend=list(y=0.9, yanchor="top"))
    })
    
    # Figure 2 legend
    text2 <- reactive({
      req(dataProcessed())
      
      if (input$nestedRE==TRUE){ #only show legend for nested data with donor models
        
        title = "Figure 2. Average trendlines for the clustered random effect variable."
        text = "This plot can help determine if a random slope and/or random intercept would be appropriate 
                for the clustered random effect variable. Missing data can also be identified by gaps in
                the lines or truncated lines."
      } else {
        
        title = "Figure 2. Trendlines for the random effect variable."
        text = "This plot can help determine if a random slope and/or random intercept would be appropriate 
                for the random effect variable. Missing data can also be identified by gaps in
                the lines or truncated lines."
      }
      
      list(title, text)
    })
    
    output$figure2 <- function() {
      text2()[[1]]
    }
    
    output$legend2 <- function() {
      text2()[[2]]
    }
    
    # mouse trendlines faceted by donor 
    output$facet_plots <- renderPlotly({
        req(dataProcessed(), input$timeVar, input$response, input$nestedRE) #requires nested data
        data = dataProcessed()
        
        plot2 = ggplot(data, aes(x=!!input$timeVar, y=!!input$response, color=!!input$groupVar)) + 
            geom_line(aes(group=!!input$mouse)) + facet_wrap(vars(!!input$donor)) +
            theme_bw(base_size=12) + theme(legend.title=element_blank()) + guides(size="none")
        
        ggplotly(plot2) %>%
            add_annotations(text=paste(input$groupVar), xref="paper", yref="paper",
                            x=1.02, xanchor="left", y=0.9, yanchor="bottom", 
                            legendtitle=TRUE, showarrow=FALSE) %>%
            layout(legend=list(y=0.9, yanchor="top"))
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
    
    Models <- reactive({
        
        if (input$nestedRE==TRUE){
            models = list(slopes()[[2]], mouse_slope()[[2]], mouse_()[[2]],
                            mouse_int()[[2]], noRE()[[2]])
        } else {
            models = list(mouse_()[[2]], mouse_int()[[2]], noRE()[[2]])
        }
        models
    })
    
    # create model results table
    output$results_table <- renderText({
      req(Models(), Names())
        
        predictors = names(coef(noRE()[[2]]))
        
        if (input$nestedRE==TRUE){
            
            lrt1v2 = anova(slopes()[[2]], mouse_slope()[[2]])[2,8]
            lrt2v3 = anova(mouse_slope()[[2]], mouse_()[[2]])[2,8]
            lrt3v4 = anova(mouse_()[[2]], mouse_int()[[2]])[2,8]
            lrt4v5 = anova(mouse_int()[[2]], noRE()[[2]])[2,8]
            
            pvalues = as.numeric(c(lrt1v2, lrt2v3, lrt3v4, lrt4v5, ""))
            pvalues = formatC(pvalues, format="e", digits=2)
            pvalues[length(pvalues)] = ""
            
            results = results_table(Models(), pvalues, Names())
            
        } else {
            
            lrt3v4 = anova(mouse_()[[2]], mouse_int()[[2]])[2,8]
            lrt4v5 = anova(mouse_int()[[2]], noRE()[[2]])[2,8]
            
            pvalues = as.numeric(c(lrt3v4, lrt4v5, ""))
            pvalues = formatC(pvalues, format="e", digits=2)
            pvalues[length(pvalues)] = ""
            
            results = results_table(Models(), pvalues, Names())
        }
        
        results[[1]][,5] = as.numeric(results[[1]][,5])
        results[[1]] %>% knitr::kable(format="html", format.args=list(big.mark = ','), align=c(rep('c', ncol(results[[1]])))) %>% 
          kable_styling(bootstrap_options=c("striped", "hover", "responsive"), full_width=F) %>%
            add_footnote(c(" ", "<small>*Coefficient estimates with standard errors in parantheses (bolded entries are significant at a 0.05 level)<small>",
                           "<small>**Log likelihoods (a larger likelihood indicates a better model fit)<small>", 
                           "<small>***P-values from the likelihood ratio test of the model with the nested model below it<small>"), 
                         notation="none", escape=F) %>%
            column_spec(c(2:6), width="5cm") %>% column_spec(1, width="10cm") %>% column_spec(2, bold=ifelse(results[[2]][,2]>0.05, FALSE, TRUE)) %>%
            column_spec(3, bold=ifelse(results[[2]][,3]>0.05, FALSE, TRUE)) %>% column_spec(4, bold=ifelse(results[[2]][,4]>0.05, FALSE, TRUE))
    })
    
    # select a specific model
    output$selectModel <- renderUI({
        selectInput(inputId="model",
                    label="Choose a specific model to see more detailed information:", 
                    choices=c('', Names()), width="400px")
    })
    
    # print model summary
    output$model_summary <- renderPrint({
        req(dataProcessed(), input$model)
        
        match = which(Names()==input$model)
        
        if (input$model=="No Random Effects"){
            sum.out = summ(noRE()[[2]], groups.table=F, pvals=F, confint=T)
        } else {
            sum.out = summ(Models()[[match]], groups.table=F, pvals=F, confint=T)
        }
        return(sum.out)
    })
    
    # Diagnostic Plots
    
    output$resid_plot <- renderPlotly({
        req(dataProcessed(), Models(), Names())
        data = dataProcessed()
        
        Time = model.frame(Models()[[1]])[[paste(input$timeVar)]]
        Model = rep(Names(), each=length(Time))
        
        if (input$nestedRE==TRUE){
            RE = model.frame(Models()[[1]])[[paste(input$mouse)]]
        } else {
            RE = model.frame(Models()[[1]])[[paste(input$re)]]
        }
        
        Residuals = lapply(Models(), FUN=residuals)
        Residuals = unlist(Residuals)
        
        resid_data = data.frame(Time, RE, Residuals, Model)
        resid_data$Model = factor(resid_data$Model, levels=rev(Names()))
        
        # time vs residuals to determine need for random effects
        ggplot(resid_data, aes(x=Time, y=Residuals)) + theme_bw(base_size=12) +
            geom_line(aes(group=RE)) + facet_wrap(~Model, ncol=2)
    })
    
    # Trendlines
    
    # select a specific model
    output$selectModel2 <- renderUI({
      selectInput(inputId="model2",
                  label="Choose a specific model to plot:", 
                  choices=c('', Names()), width="400px")
    })
    
    output$trendlines <- renderPlotly({
        req(dataProcessed(), input$model2, Names(), Models())
        data = dataProcessed()
        
        # determine which model was selected from the dropdown menu
        match = which(Names()==input$model2)
        model = Models()[[match]]
        
        if ((match==3 || match==4) && input$nestedRE==TRUE){ #mouse plot for nested data with no donor models
            plot5 = mouse_lines(model, data, input$response, input$timeVar, input$groupVar, input$donor, input$mouse)
  
        } else if (input$model2=="No Random Effects" && input$nestedRE==TRUE){ #plot for no random effects model (nested)
            plot5 = donor_lines(noRE()[[2]], data, input$response, input$timeVar, input$groupVar, input$donor)
            
        } else if (input$model2=="No Random Effects" && input$nestedRE==FALSE){ #plot for no random effects model (non-nested)
            plot5 = donor_lines(noRE()[[2]], data, input$response, input$timeVar, input$groupVar, input$re)
            
        } else if ((match==1 || match==2) && input$nestedRE==TRUE){ #donor plot for nested data with donor models
            plot5 = donor_lines(model, data, input$response, input$timeVar, input$groupVar, input$donor)
            
        } else { #plot for non-nested data
            plot5 = donor_lines(model, data, input$response, input$timeVar, input$groupVar, input$re)
        }
        
        ggplotly(plot5) %>%
            add_annotations(text=paste(input$donor), xref="paper", yref="paper",
                            x=1.02, xanchor="left", y=0.9, yanchor="bottom", 
                            legendtitle=TRUE, showarrow=FALSE) %>%
            layout(legend=list(y=0.9, yanchor="top"))
    })
    
    text5 <- reactive({
      req(dataProcessed(), input$model2, Names())
      
      # determine which model was selected from the dropdown menu
      match = which(Names()==input$model2)
      
      if ((match==3 || match==4) && input$nestedRE==TRUE){ #nested data & model with just one random effect
        
        title = "Figure 5. Fitted lines for the nested random effect variable."
        text = "This plot is useful for visualizing how the inclusion of a random slope and/or
                intercept affects the model fit for the nested random effect variable."
      
      } else if (input$model2=="No Random Effects"){ #model with no random effects
        
        title = "Figure 5. Fitted lines for the grouping variable."
        text = "This plot is useful for visualizing how the inclusion of a random slope and/o
                intercept affects the model fit for the grouping variable."
      
      } else if ((match==1 || match==2) && input$nestedRE==TRUE){ #nested data & model with both random effects
        
        title = "Figure 5. Fitted lines for the clustered random effect variable."
        text = "This plot is useful for visualizing how the inclusion of a random slope and/or
                intercept affects the model fit for the clustered random effect variable."
        
      } else { #non-nested data
        
        title = "Figure 5. Fitted lines for the random effect variable."
        text = "This plot is useful for visualizing how the inclusion of a random slope and/or
                intercept affects the model fit for the random effect variable."
      }
      list(title, text)
    })
    
    output$figure5 <- function() {
      text5()[[1]]
    }
    
    output$legend5 <- function() {
      text5()[[2]]
    }
      
    output$trendlines2 <- renderPlotly({
        req(dataProcessed(), input$model2, Names(), input$nestedRE) #requires nested data
        data = dataProcessed()
        
        # determine which model was selected from the dropdown menu
        match = which(Names()==input$model2)
        
        req(match)
        
        if (match==1 || match==2){ #only show plot for nested data with donor models
            
          plot6 = mouse_lines(Models()[[match]], data, input$response, input$timeVar, input$groupVar, input$donor, input$mouse)
            
          ggplotly(plot6) %>%
              add_annotations(text=paste(input$donor), xref="paper", yref="paper",
                              x=1.02, xanchor="left", y=0.9, yanchor="bottom", 
                              legendtitle=TRUE, showarrow=FALSE) %>%
              layout(legend=list(y=0.9, yanchor="top"))
          
        } else {
          
          plot6 = NULL
        }
    })
    
    text6 <- reactive({
      req(dataProcessed(), input$model2, Names(), input$nestedRE)
      
      # determine which model was selected from the dropdown menu
      match = which(Names()==input$model2)
      
      req(match)
      
      if (match==1 || match==2){ #only show legend for nested data with donor models
        
        title = "Figure 6. Fitted lines for the nested random effect variable."
        text = "This plot is useful for visualizing how the inclusion of a random slope and/or
                intercept affects the model fit for the nested random effect variable."
      } else {
        
        title = NULL
        text = NULL
      }
      
      list(title, text)
    })
    
    output$figure6 <- function() {
      text6()[[1]]
    }
    
    output$legend6 <- function() {
      text6()[[2]]
    }
    
    #outputOptions(output, "trendlines2", suspendWhenHidden = FALSE)
}

# Run the app ----
shinyApp(ui = ui, server = server)