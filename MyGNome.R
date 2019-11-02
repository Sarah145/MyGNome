#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vcfR)
library(tidyr)
library(ggplot2)
library(scales)
library(VariantAnnotation)
library(matrixStats)
library(karyoploteR)
library(shinydashboard)

library(devtools)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)

# for the CRAN version
# install.packages("shinydashboardPlus")
# for the latest version
# devtools::install_github("RinteRface/shinydashboardPlus")
library(shinydashboardPlus)
# install.packages("shinyalert")
library(shinyalert)
# devtools::install_github("Sarah145/Sarah-Seq", force = T)
library(SarahSeq)
# detach("package:SarahSeq", unload=T)
# install.packages("paletteer")
library(paletteer)
# install.packages("ggforce")
library(ggforce)
# install.packages("cowplot")
library(cowplot)
# install.packages("gridExtra")
library(gridExtra)
# library()
library(plotly)

ui <- dashboardPage(# skin = "green",
    
    dashboardHeader(tags$li(class = "dropdown",
                            tags$style(".main-header {max-height: 200px}"),
                            tags$style(".main-header .logo {height: 70px}")
    ),
    title =h1(id ='head', "MyGNome"), 
    titleWidth = 250),
    
    dashboardSidebar(
        tags$style(".left-side, .main-sidebar {padding-top: 70px}"),                        width = 250,
        sidebarMenu( # Create sidebar options 
            menuItem("Upload File", tabName = "input_files", icon = icon("file-alt")),
            menuItem("What Does My Genome Look Like?", tabName = "W", icon = icon("search")),
            menuItem("Monogenic Traits", tabName = "Mon", icon=icon("dna")),
            menuItem("PRS", tabName = "PRS", icon = icon("heartbeat"),
                     menuSubItem("About", tabName = "Ab", icon=icon("question")),
                     menuSubItem("Calculate my PRS", tabName = "Calc", icon=icon("calculator"))),
            menuItem("Ancestry", tabName = "Ancestry", icon = icon("user-friends"),
                     menuSubItem("Ancestry Information", "AnInfo", icon = icon("user-friends")),
                     menuSubItem("Mitochondrial DNA", tabName = "Mito", icon=icon("circle")),
                     menuSubItem("Y Chromosome", tabName = "Y", icon = icon("y-combinator")),
                     menuSubItem("Whole Genome", tabName = "WG", icon=(icon("dna"))))
        )),
    
    dashboardBody(
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Bree+Serif|Cabin:400,700');
                    #head{color: #ffffff;
                    font-family: 'Bree Serif';}")),
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        
        tabItems(
            # Tab output - Input files (Tabs where the 6 files can be uploaded and previewed)
            
            ## First file = Clinical data (Also add in button that runs code when selected)
            
            # Second tab content
            tabItem(tabName = "input_files",
                    box(
                        width = 3,
                        status = "primary",
                        radioButtons("type", "Choose a file type", c('23andMe raw data', 'vcf'), selected = '23andMe raw data'),
                        tags$hr(),
                        fileInput("file1", "Choose file",
                                  multiple = TRUE,
                                  accept = c(".vcf", ".txt")),
                        tags$hr()
                    ),
                    
                    box(width = 9,
                        height = "500px",
                        status = "primary",
                        fluidRow(align = 'center',
                                 style = "border: 15px double white;",
                                 img(src='michael.gif', align = "center", border = "15cm"),
                                 tags$hr(),
                                 
                                 verbatimTextOutput("success", placeholder = F)))),
            
            #        tabItem(tabName = "QC",
            #               box(width = 4,
            #                   status = "primary",
            #                   selectInput("chr", "Choose a chromosome", c('All', seq(1:22), 'X', 'Y'), selected = 'All' ),
            #                   h5(tags$b("Number of variants")),
            #                   verbatimTextOutput("n_vars", placeholder = TRUE),
            #                   tags$hr(),
            #                   h4(tags$b("Filtering")),
            #                   h5("Include variants that have:"),
            #                   sliderInput("mq_fil", "Mapping quality scores between", min = 1, max = 60, value = c(1,60), step = 1),
            #                   sliderInput("dp_fil", "Read depth scores between", min = 1, max = 7000, value = c(1,7000), step = 1),
            #                   sliderInput("qual_fil", "Quality scores between", min = 1, max = 250000, value = c(1, 250000)),
            #                   tags$hr(),
            #                   fluidRow(align = 'center', actionButton("filter", tags$b("Update View")))
            #       ),
            #               box(width = 8,
            #                  status = "primary",
            #                  h4(tags$b("QC statistics:")),
            #                  tableOutput("qc_stats"),
            #                  tags$hr(),
            #                  h4(tags$b("QC plots:")),
            #                  plotOutput("QCPlot"))
            #   ),
            
            tabItem(tabName = "W",
                    tabBox(# The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "100%", width = "100%", 
                        tabPanel("Overview", 
                                 box(width =12,
                                     # status = "primary",
                                     height = "95%",
                                     img(src='genome.png', align = "center", width = "100%"),
                                     tags$hr(),
                                     plotOutput("karyoPlot")
                                 ),
                                 useShinyalert(),  # Set up shinyalert
                                 actionButton("preview1", "Did You Gnome?", icon("surprise"), 
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        
                        tabPanel("Zoom In",
                                 box(width = 3,
                                     #  status = "primary",
                                     height = "100%",
                                     h4("Choose a genomic region"),
                                     tags$hr(),
                                     selectInput('zoom_chr', 'Chromosome:',  c(seq(1:22), 'X', 'Y')),
                                     #fluidRow(align = 'center',
                                     #splitLayout( width = '50%',
                                     textInput('reg_min', 'from:'),
                                     textInput('reg_max', 'to:'),
                                     tags$hr(),
                                     h5("Number of variants in this region:"),
                                     verbatimTextOutput('n_vars', placeholder = T),
                                     tags$hr(),
                                     fluidRow(align = 'center',   
                                              actionButton('plot', tags$b('Plot')))
                                 ),
                                 
                                 box(width = 9,
                                     height = "100%",
                                     #status = "primary",
                                     plotOutput('single_chr_plot'),
                                     tags$hr(),
                                     plotlyOutput('zoom_plot'),
                                     conditionalPanel(
                                         condition = "input.plot > 0",
                                         img(src='legend.png', align = "center", width = "100%"),
                                         uiOutput("url")
                                     )
                                 ),
                                 
                                 useShinyalert(),  # Set up shinyalert
                                 actionButton("preview2", "Did You Gnome?", icon("surprise"), 
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ),
                        tabPanel("DNA Portrait", 
                                 box(width =12,
                                     # status = "primary",
                                     h2(tags$b('How does this work...?')),
                                     img(src='print_my_dna.png', align = "center", width = "100%"),
                                     tags$hr(),
                                     fluidRow(align = 'center',
                                              actionButton('dna_plot', tags$b('Print my DNA portrait'))),
                                     tags$hr(),
                                     plotOutput("dna_portrait")
                                 ),
                                 useShinyalert(),  # Set up shinyalert
                                 actionButton("preview5", "Did You Gnome?", icon("surprise"), 
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                        ))
            ), 
            
            tabItem(tabName = "Mon",
                    box(width = 12,
                        status = "primary",
                        fluidRow(align = 'center',
                                 img(src='gnome_construction.jpg', align = "center", width = "26%"),
                        h2("Under Construction")))),
            
            tabItem(tabName = "Ab",
                    box(width = 12,
                        status = "primary",
                        fluidRow(align = 'center',
                                 img(src='gnome_construction.jpg', align = "center", width = "26%"),
                                 h2("Under Construction")))),
            
            tabItem(tabName = "Calc",
                                 box(width =12,
                                     # status = "primary",
                                     height = "95%",
                                     status = "primary",
                                     img(src='prs_spread.png', align = "center", width = "100%"),
                                 ),
                                 useShinyalert(),  # Set up shinyalert
                                 actionButton("previewcalc", "Did You Gnome?", icon("surprise"), 
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            
            tabItem(tabName = "AnInfo",
                    box(width = 7,
                        status = "primary",
                        h2("Ancestry"),
                        img(src='fam_tree.png', align = "centre", height="50%", width="99%")),
                    box(width = 5,
                        status = "primary",
                        h3("What is ancestry?"),
                        br(),

h4("Your DNA is inherited from your parents, which is inherited from their parents and so on. Therefore, your genome is a result of your ancestors. Until recently, people didn’t move around very quickly, so your DNA can also be used to figure out the geographic affinity of your genome."),
br(),
h4("There are three different things we look at to investigate ancestry: mitochondrial DNA (mtDNA); Y chromosomes and your whole genome."),
h4(" - mtDNA is inherited from your mother only, so gives a picture of your maternal lineage"),
h4("- Y chromosomes are in males only. Therefore they give a picture of your male lineage"),
h4("- Your whole genome is much bigger and is inherited from both your parents. We look at a large number of markers chosen to give a better picture of your ancestry."),
br(),
h4("Mutations in mtDNA and Y chromosomes can be used to identify the type of DNA you have here (your haplogroup). This is what we report to you!")

)),
            
            tabItem(tabName = "Y",
                    box(width = 12,
                        height = "650px",
                        fill = T,
                        status="primary",
                        h3(strong("Y haplogroup sample results:")),
                        br(),
                        h4("Your Y haplogroup is within the R1b-M222 subclade!", useShinyalert(), actionButton("previewYhap", "?", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        br(),
                        # br(), 
                        img(src='Haplogroup-R1b-L21.jpg', align = "centre", height="430px", width="800px"),
                        br(),
                        br(),
                        useShinyalert(),  # Set up shinyalert
                        actionButton("preview6", "Did You Gnome?", icon("surprise"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    )),
                    
            tabItem(tabName = "Mito",
                    box(width = 12,
                        fill = T,
                        height = "650px",
                        status="primary",
                        h3(strong("Mitochondrial Haplogroup sample results:")),
                        br(),
                      
                        h4("Your mtDNA Haplogroup is J2b1a2a!",  
                        useShinyalert(), actionButton("previewmito", "?", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
            
                        br(),
                       # br(), 
                        img(src='mtDNA-J-map.jpg', align = "centre", height="430px", width="800px"),
                        br(),
                        br(),
                        # br(),
                        useShinyalert(),  # Set up shinyalert
                        actionButton("preview7", "Did You Gnome?", icon("surprise"), 
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    

            tabItem(tabName = "WG",
                    box(width = 12,
                    status = "primary",
                    fluidRow(align = 'center',
                     img(src='gnome_construction.jpg', align = "center", width = "26%"),
                     h2("Under Construction"))))
            
        )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    options(shiny.maxRequestSize=500*1024^2)
    
    output$progressBox <- renderInfoBox({
        infoBox(
            "Progress", paste0(25 + input$count, "%"), icon = icon("list"),
            color = "purple"
        )
    })
    
    dataInput <- reactive({
        req(input$file1)
        if(input$type == '23andMe raw data'){
            data    <- read_23andMe(input$file1$datapath)
        } else {
            data <- vcf_to_23(input$file1$datapath)
        } 
    })
    
    
    output$success <- renderText({
        req(input$file1)
        print(paste("File with ", nrow(dataInput()), " variants successfully uploaded!"))
    })     
    
    output$karyoPlot <- renderPlot({
        plot_karyotype(dataInput(), main  = 'Distribution of variants along each of your chromosomes')
    })
    
    output$zoom_plot <- renderPlotly({
        req(input$plot)
        zoom_plot(dataInput(), as.numeric(input$zoom_chr), c(as.numeric(input$reg_min), as.numeric(input$reg_max)))
    })
    
    output$single_chr_plot <- renderPlot({
        req(c(input$zoom_chr, input$reg_min, input$reg_max))
        sub <- subset(dataInput(), dataInput()$chr == input$zoom_chr)
        gr <- GRanges(seqnames = paste0('chr', sub$chr), ranges = IRanges(start = sub$pos, width = 1, names = sub$id))
        kp <- plotKaryotype("hg19", chromosomes = paste0('chr',input$zoom_chr), ideogram.plotter = NULL, labels.plotter = NULL)
        kp <- kpAddCytobandsAsLine(kp,lwd = 30)
        kp<-kpPlotDensity(kp, data=gr, col = '#FAA3A3')
        kpAxis(kp, ymax=kp$latest.plot$computed.values$max.density, cex=0.8)
        kpAbline(kp, h=mean(kp$latest.plot$computed.values$density), lty=2, ymax=kp$latest.plot$computed.values$max.density, r0=0.68, r1=1)
        kpAddBaseNumbers(kp)
        kpRect(kp, chr=paste0('chr', input$zoom_chr), x0=as.numeric(input$reg_min), x1=as.numeric(input$reg_max), y0=0, y1=1, col="indianred", data.panel="ideogram", border=2)
        title(main = paste('Chromosome', input$zoom_chr))
    })
    output$n_vars <- renderText({
        req(c(input$zoom_chr, input$reg_min, input$reg_max))
        nrow(subset(dataInput(), dataInput()$chr == input$zoom_chr & dataInput()$pos >= as.numeric(input$reg_min) & dataInput()$pos <= as.numeric(input$reg_max)))
    })
    
    observeEvent(input$plot,{output$url <-renderUI(a(href=paste0('https://genome-euro.ucsc.edu/cgi-bin/hgTracks?db=', input$assem,'&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr',
                                                                 input$zoom_chr, '%3A', input$reg_min, '%2D', input$reg_max, '&hgsid=233883892_09NIVN7vN7pBhYaot0qdf7matJfF'),
                                                     "Show in UCSC Genome Browser",target="_blank"))})
    
    output$dna_portrait <- renderPlot({
        req(input$dna_plot)
        print_DNA(dataInput())
    })
    
    observeEvent(input$preview, {
        # Show a modal when the button is pressed
        shinyalert("What is Ancestry?", "Ancestry is", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview1, {
        # Show a modal when the button is pressed
        shinyalert("", "", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview2, {
        # Show a modal when the button is pressed
        shinyalert("", "", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview3, {
        # Show a modal when the button is pressed
        shinyalert("", "", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview4, {
        # Show a modal when the button is pressed
        shinyalert("", "", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    observeEvent(input$preview5, {
        # Show a modal when the button is pressed
        shinyalert("", "Your DNA portrait is totally unique to YOU. Nobody else will have a DNA portrait that looks exactly like yours (unless you have an identical twin)!", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview6, {
        # Show a modal when the button is pressed
        shinyalert("Modern Distribution:", "

M222 is a subgroup of R1b-M269, which is very common in Europe, and at its most common in western Europe. R1b-M222 is most common in north-west Ireland. This is relatively geographically specific, as male reproductive success varied widely in history, so some males were much more likely to have children and pass on their Y haplogroups to their sons. 


", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$preview7, {
        # Show a modal when the button is pressed
        shinyalert("Modern Distribution", "Haplogroup J is relatively evenly spread across Europe, the Near East and North Africa.

mtDNA distribution is relatively even across wide geographical areas, because there wasn’t as much variance in reproductive success (children born to a woman) as there was in men.

", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$previewmito, {
        # Show a modal when the button is pressed
        shinyalert("Mitochondrial DNA is inherited from your mother only, so can give you some ideas about where your female ancestors originated.",
                   imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$previewYhap, {
        # Show a modal when the button is pressed
        shinyalert("Y chromosomes are inherited from father to son only, so are informative about the male line of descent.",
                   imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    observeEvent(input$previewcalc, {
        # Show a modal when the button is pressed
        shinyalert("", "", imageUrl = "https://cdn.shopify.com/s/files/1/2281/5369/products/802429_1_f6e2b31b-a747-4d43-9966-ccd472370b59_x700.jpg?v=1548874682", imageWidth = 350, imageHeight = 350)
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)

