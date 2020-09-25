#
# Esta app es  para entender la distribución en el muestreo
# de las medias muestrales
# Caso particular de la comparacion de dos medias
# 
#
#    
#

library(shiny)
source("sim_difmedias_muestreo.r")
# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinythemes::shinytheme("cerulean"),"Comparación de dos medias",

   
   # Application title
 #tabPanel("Test de de hipótesis",
   
   # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(width=3,
        h3("Población 1"),
         numericInput("mu1",
                     "Media",
                     value = 30),
         numericInput("n1",
                      "Tamaño de la muestra",
                     min=2,max=100,
                      value = 4),
         h3("Población 2"),
         numericInput("mu2",
                      "Media",
                      value = 30),
         numericInput("n2",
                     "Tamaño de la muestra",
                     min=2,max=100,
                     value = 4),
         h3(""),
         numericInput("sig",
                      "Desviación típica",
                      value = 10),
      
         numericInput("nsim",
                   "Simulaciones",
                   min=1,max=10000,
                   value = 5000),
        actionButton("Run","Ejecutar")
     ),
   
      # Show a plot of the generated distribution
      mainPanel(
         #htmlOutput("et1"),
         h4("Diferencia de medias muestrales"),
         plotOutput("distPlot",width="80%"),
         
         #h5("Valores teóricos"),
         h5(textOutput("et1")),
         textOutput("difmed.teo"),
         textOutput("sdmed.teo"),
         #h5("Valores empíricos"),
         h5(textOutput("et2")),
         textOutput("difmed.emp"),
         textOutput("sdmed.emp"),
         
         h4("Varianza muestral agregada"),
       
         plotOutput("Varianza",width="80%"),
         #h5("Valores teóricos"),
         h5(textOutput("et3")),
         textOutput("medvar.teo"),
         textOutput("sdvar.teo"),
         
         #h5("Valores empíricos"),
         h5(textOutput("et4")),
         textOutput("medvar.emp"),
         textOutput("sdvar.emp")
         
                  
         
      )
   )
# )
# tabPanel("Intervalo de Confianza", plotOutput("Interval")
# )        
          
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    result<-eventReactive(input$Run,{
      sal<-NULL
      # genera las diferencia de medias simuladas
      tt<-replicate(input$nsim,dif.medias(input$mu1,
          input$mu2,input$sig,input$n1,input$n2))
      sal$et1<-""
      sal$dmed<-tt[1,]
      xmin<-min(tt[1,])
      xmax<-max(tt[1,])
      sal$xmin<-xmin
      sal$xmax<-xmax
      sal$difmed.teo<-input$mu1-input$mu2
      sal$sdmed.teo<-input$sig*sqrt((1/input$n1)+(1/input$n2))
      sal$varp<-tt[2,]
      return(sal)
      })
   output$distPlot <- renderPlot({
      
      
      # draw the histogram 
      hist(result()$dmed,main="",freq=FALSE,breaks=30,
           xlab=expression(bar(Y)[1]-bar(Y)[2]),
           axes=FALSE,ylab="",xlim=c(result()$xmin,
           result()$xmax),col="skyblue1")
           #ylim=c(0,dnorm(result()$difmed.teo,
           # mean=result()$difmed.teo,sd=result()$sdmed.teo)))
      axis(1)
      
      #if (input$mu1==input$mu2) {
        curve(dnorm(x,mean=result()$difmed.teo,sd=result()$sdmed.teo),add=TRUE,
            col="red",lwd=2)
      abline(v=result()$difmed.teo,lty=2,col="red",lwd=2)
      #}
      abline(v=qnorm(0.975,mean=result()$difmed.teo,
                     sd=result()$sdmed.teo),lty=2
             ,col="red",lwd=2)
      
      abline(v=qnorm(0.025,mean=result()$difmed.teo,
                     sd=result()$sdmed.teo),lty=2
             ,col="red",lwd=2)
      
      
        
   })
   
   output$difmed.teo<-renderText({
     paste("Media = ",round(result()$difmed.teo,2))})

   output$sdmed.teo<-renderText({
      paste("Desv. Típica =",round(result()$sdmed.teo,2))})
   
   output$et1<-renderText({paste("Valores Teóricos",result()$et1)})
   output$et2<-renderText({paste("Valores Empíricos",result()$et1)})
   output$et3<-renderText({paste("Valores Teóricos",result()$et1)})
   output$et4<-renderText({paste("Valores Empíricos",result()$et1)})
   
   output$difmed.emp<-renderText({
      paste("Media = ",round(mean(result()$dmed),2))})
   
   output$sdmed.emp<-renderText({
      paste("Desv. Típica =",round(sd(result()$dmed),2))})
   
   output$Varianza<-renderPlot({
     hist(result()$varp,main="",xlab=expression(S[p]^2),ylab="",freq=FALSE,
          breaks=30,axes=F,col="springgreen")
   axis(1)
   lines(density(result()$varp),col="red",lwd=2)
   abline(v=input$sig**2,lty=2,col="red",lwd=2)
   
   output$medvar.teo<-renderText({
      paste("Media = ",round(input$sig**2,2))})
   
   output$sdvar.teo<-renderText({
      paste("Dev. Típica = ",round(sqrt(2*input$sig**4/(input$n1+input$n2-2)),2))})
   
   output$medvar.emp<-renderText({
      paste("Media = ",round(mean(result()$varp),2))})
   
   output$sdvar.emp<-renderText({
      paste("Dev. Típica = ",round(sd(result()$varp),2))})
   
 })
   

}   
# Run the application 
shinyApp(ui = ui, server = server)

