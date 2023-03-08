#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui <- shinyUI(pageWithSidebar(
  
  # Title
  headerPanel(""),
  
  sidebarPanel(
    selectizeInput("typefunc",
                   "Displayed distance decay function(s):",
                   choices = c("Binary", 
                               "Gamma",
                               "Exponetial"),
                   selected = c("Binary"),
                   multiple = TRUE),
    sliderInput("tcrange", 
                label = "Travel cost range:", 
                min = 0, 
                max = 120,
                value = c(1,60),
                step=1),
    sliderInput("thres",
                "Threshold (binary):",
                min=0,
                max=120,
                value=15,
                step=1),
    sliderInput("rate1",
                "Rate parameter (exponential):",
                min=0,
                max=1.5,
                value=0.1,
                step=0.05),
    sliderInput("shape2",
                "Shape parameter (gamma):", 
                min=0,
                max=5,
                value=1.6,
                step=0.1),
    sliderInput("scale2",
                "Scale parameter (gamma):",
                min=0,
                max=1,
                value=0.1,
                step=0.1)
  ),
  
  # GGPLOT
  
  mainPanel(
    tabsetPanel(
      tabPanel("Exploring impedance functions",
               plotOutput("Plot_prob_dens"),
               h6("*Exponential and gamma probabiltiy density functions are rescaled such that the impedance function values range from 0 to 1")
      )  
    )
  )
  
))

server <- function(input,output){
  
  ################################# 
  # These are the reactive inputs # These do not get displayed 
  ################################# 
  
  rand_samp <- reactive({
    seq(input$tcrange[1], input$tcrange[2], 0.25)
    # rgamma((input$tcrange[2]-input$tcrange[1])/1,input$shape, input$scale)
  })
  
  min_quan <- reactive({
    min(rand_samp())
  })
  
    max_quan <- reactive({
    max(rand_samp())
  })
  
  quans <- reactive({
    seq(min_quan(), max_quan(), 0.01) 
  })
  
  prob_dens_bin <- reactive({
    ifelse(quans() >= input$thres, 0, 1) 
  })
  
  prob_dens_exp <- reactive({
    dexp(quans(), input$rate1) |> scales::rescale()
  })
  
  prob_dens_gamma <- reactive({
    dgamma(quans(), input$shape2, input$scale2) |> scales::rescale()
  })
  
  
  ################################## 
  # These are the reactive outputs # These get displayed
  ################################## 
  
  # Plot the probability density
  output$Plot_prob_dens <-renderPlot({
    fit_bin <- data.frame(f = prob_dens_bin(),
                          x = quans(),
                          type = "Binary")
    fit_dexp <- data.frame(f = prob_dens_exp(),
                           x = quans(),
                           type = "Exponetial")
    fit_dgamma <- data.frame(f = prob_dens_gamma(),
                             x = quans(),
                             type = "Gamma")

    plot.data <- rbind(fit_bin,fit_dexp,fit_dgamma)
    plot.data <- plot.data[plot.data$type %in% input$typefunc, ]
    
    ggplot(plot.data) + 
      geom_line(aes(x=x, y=f, color=type), size=0.6) + 
      scale_x_continuous(expand = c(0, 0), limits = input$tcrange) +
      theme_classic() +
      scale_color_manual(name = "Functions",
                         values = c("Red", "Green", "Blue")) +
      xlab(bquote(c[ij])) +
      ylab(bquote(f(c[ij])))
  },
  width=600,height=400
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)