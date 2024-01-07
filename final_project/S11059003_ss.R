library(shiny)
library(tuneR)
library(seewave)
library(oce)
library(signal)
library(dplR)

ui <- fluidPage (
  titlePanel("S11059003のS&S Display"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file", "選擇Wav檔:", 
                multiple = FALSE, 
                accept = c("text/wav",
                           "text/comma-separated-values,text/plain",
                           ".wav")),
      
      actionButton("listen", "聆聽音檔"),
      
      tags$hr(),
      
      selectInput('filter', '濾波器選擇',
                  choices = list("Low-pass", "High-pass"),
                  selected = "Low-pass"),
      
      numericInput('frq', '截止頻率', 1000, min = 2, max = 3000),
      
      actionButton("submit", "送出截止頻率")
    ),
    
    mainPanel(
      
      plotOutput("time1"),
      plotOutput("frq1"),
      
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    readWave(input$file$datapath)
  })
  
  observeEvent(input$listen, {
    listen(data())
  })
  
  output$time1 <- renderPlot({
    snd <- data()@left
    fs <- data()@samp.rate
    dur <- length(snd)/data()@samp.rate
    snd <- snd - mean(snd)
    time <- time(data())
    plot(time,snd,type='l',xlab='Time',ylab='Amplitude', main = "時序圖")
  })
  
  output$frq1 <- renderPlot({
    nfft <- 1024
    window <- 256
    overlap <- 128
    snd <- data()@left
    fs <- data()@samp.rate
    
    spec = specgram(x = snd,
                    n = nfft,
                    Fs = fs,
                    window = window,
                    overlap = overlap)
    
    P <- abs(spec$S)
    P <- P/max(P)
    P <- 10*log10(P)
    t <- spec$t
    imagep(x = t,
           y = spec$f,
           z = t(P),
           col = oce.colorsViridis,
           ylab = 'Frequency(Hz)',
           xlab = 'Time(s)',
           drawPalette = T,
           decimate = F,
           main = "頻譜圖")
  })
  
  filtered_data <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    type <- input$filter
    
    if(type == "Low-pass") {
      filtered <- pass.filt(data()@left, 
                             W = input$frq, 
                             type = "low", 
                             method = "Butterworth", 
                             n = 6, 
                             Rp = 1)
    }
    else {
      filtered <- pass.filt(data()@left, 
                            W = input$frq, 
                            type = "high", 
                            method = "Butterworth", 
                            n = 6, 
                            Rp = 1)
    }
    normalized_data <- filtered_data() / max(abs(filtered))
    normalized_data <- normalized_data * 32767
    filtered_data(filtered)
  })
  
  observeEvent(filtered_data(), {
    if (!is.null(filtered_data())) {
      rounded_data <- round(filtered_data())
      rounded_data[rounded_data > 32767] <- 32767
      rounded_data[rounded_data < -32768] <- -32768
      writeWave(Wave(rounded_data, samp.rate = data()@samp.rate), "output.wav")
    }
  })
}

shinyApp(ui, server)