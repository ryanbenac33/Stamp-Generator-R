library(shiny)
library(bslib)
library(colourpicker)
library(here)
library(htmltools)

font_choices <- sort(c(
  "Arial", "Calibri", "Candara", "Comic Sans MS", "Courier New", "Futura",
  "Garamond", "Gill Sans", "Georgia", "Helvetica", "Impact", "Lucida Console",
  "Palatino Linotype", "Segoe UI", "Tahoma", "Times New Roman", "Trebuchet MS",
  "Verdana", "Franklin Gothic Medium"
))

############## National Sites
parks_df <- read.csv(here::here("data", "national_sites.csv"), stringsAsFactors = FALSE)
parks <- parks_df$National.Park

format_date <- function(date) {
  toupper(format(as.Date(date), "%b %d %Y"))
}

make_stamp_svg <- function(text, date, color, font, state, circle_grunge = 2, text_grunge = 0.3) {
  sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="200" height="200" viewBox="0 0 200 200" role="img" aria-label="Stamp SVG">
      <defs>
        <filter id="circleGrunge">
          <feTurbulence type="turbulence" baseFrequency="0.8" numOctaves="2" result="noise"/>
          <feDisplacementMap in="SourceGraphic" in2="noise" scale="%f" xChannelSelector="R" yChannelSelector="G"/>
          <feGaussianBlur stdDeviation="0.4"/>
        </filter>
        <filter id="textGrunge">
          <feTurbulence type="turbulence" baseFrequency="0.5" numOctaves="1" result="noise"/>
          <feDisplacementMap in="SourceGraphic" in2="noise" scale="%f" xChannelSelector="R" yChannelSelector="G"/>
          <feGaussianBlur stdDeviation="0.15"/>
        </filter>
        <path id="circlePath" d="M 100,185 a 85,85 0 0,1 -85,-85 a 85,85 0 0,1 170,0 a 85,85 0 0,1 -85,85" />
      </defs>
      <circle cx="100" cy="100" r="85" stroke="%s" stroke-width="4" fill="none" filter="url(#circleGrunge)"/>
      
      <text dy="0.7em" filter="url(#textGrunge)">
        <textPath href="#circlePath" startOffset="50%%" text-anchor="middle"
          font-size="15" fill="%s" font-family="%s" font-weight="300" style="dominant-baseline: middle;">
          %s
        </textPath>
      </text>
      
      <text x="100" y="110" text-anchor="middle" font-size="14" font-family="%s" fill="%s" filter="url(#textGrunge)">%s</text>
      <text x="100" y="130" text-anchor="middle" font-size="12" font-family="%s" fill="%s" filter="url(#textGrunge)">%s</text>
    </svg>',
    circle_grunge,
    text_grunge,
    color,
    color, font, htmlEscape(text),
    font, color, format_date(date),
    font, color, htmlEscape(state)
  )
}


my_theme <- bs_theme(
  version = 5,
  bg = "#f5f7fa",
  fg = "white",
  primary = "#fbbf24",
  base_font = c("Segoe UI", "Arial", "sans-serif")
)

ui <- fluidPage(
  theme = my_theme,
  tags$head(
    tags$style(HTML("
      .navbar, .navbar-default {
        background-color: #003366 !important;
        border-bottom: 2px solid #fbbf24 !important;
      }
      .navbar-brand, .navbar-nav > li > a {
        color: white !important;
        font-weight: 700;
      }
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:focus,
      .navbar-nav > .active > a:hover {
        background-color: transparent !important;
        border-bottom: 3px solid #fbbf24 !important;
        color: #fbbf24 !important;
        box-shadow: none !important;
      }
      .navbar-nav > li > a:hover {
        background-color: rgba(251, 191, 36, 0.2) !important;
        border-radius: 4px;
        color: #fbbf24 !important;
      }
      @media (max-width: 767px) {
        .navbar-nav {
          flex-direction: row !important;
        }
        .navbar-collapse {
          overflow-x: auto;
          white-space: nowrap;
        }
      }
      footer {
        text-align: center;
        padding: 1rem;
        color: #000000 !important;
        font-size: 0.9rem;
      }
      #tab_content {
        color: #000000 !important;
      }
      .form-control, .form-select, .datepicker {
        color: #000000 !important;
        background-color: #ffffff !important;
      }
      input[type='text'].form-control {
        color: #000000 !important;
        background-color: #ffffff !important;
      }
      .color-row {
        display: flex;
        align-items: center;
      }
      .color-swatch-box {
        width: 30px;
        height: 30px;
        border: 1px solid #ccc;
        margin-left: 10px;
        margin-top: 18px;
        display: inline-block;
        vertical-align: middle;
      }
      .svg-container {
        display: flex !important;
        justify-content: center;
        align-items: center;
        height: 400px;
        background-color: #fff;
        border: 1px solid #ccc;
        padding: 10px;
      }
      .datepicker table tr td:hover {
        background-color: #fbbf24 !important;
        color: #000000 !important;
      }
    "))
  ),
  
  navbarPage(
    id = "main_nav",
    title = tags$span(style = "color:#fbbf24;", "Stamp SVG Generator"),
    inverse = TRUE,
    
    tabPanel("National Park Service Stamps",
             div(id = "tab_content",
                 fluidRow(
                   column(1),
                   column(3,
                          selectInput("nationalPark", "Select a National Park:",
                                      choices = c("Select a National Park" = "", parks)),
                          dateInput("visitDate", "Visit Date:", value = Sys.Date()),
                          div(style = "display:flex; align-items:center;",
                              colourInput("stampColor", "Stamp Color:", value = "#000000"),
                              uiOutput("colorSwatch")
                          ),
                          selectInput("fontSelect", "Select Font:", choices = font_choices),
                          sliderInput("circleGrungeSlider", "Circle Grunge Intensity:", min = 0, max = 5, value = 2, step = 0.1),
                          sliderInput("textGrungeSlider", "Text Grunge Intensity:", min = 0, max = 1, value = 0.3, step = 0.05),
                          
                   ),
                   column(5,
                          div(
                            id = "svg_viewer",
                            class = "svg-container",
                            uiOutput("svg_output")
                          )
                   ),
                   column(3)
                 ),
                 br(),
                 fluidRow(
                   column(12, align = "center",
                          downloadButton("downloadNPS", "Download Cancellation Stamp", class = "submit-button")
                   )
                 )
             )
    ),
    
    tabPanel("National Natural Landmarks", div(id = "tab_content", h3("Content for Tab 2"))),
    tabPanel("State Stamps", div(id = "tab_content", h3("Content for Tab 3"))),
    tabPanel("License Plates", div(id = "tab_content", h3("Content for Tab 4"))),
    tabPanel("Cities", div(id = "tab_content", h3("Content for Tab 5"))),
    
    footer = tags$footer(
      HTML("&copy; 2025 - My App")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$nationalPark, {
    default_col <- parks_df$default_color[parks_df$National.Park == input$nationalPark]
    if (length(default_col) != 1 || !grepl("^#", default_col)) {
      default_col <- "#000000"
    }
    updateColourInput(session, "stampColor", value = default_col)
  }, ignoreInit = TRUE)
  
  output$colorSwatch <- renderUI({
    req(input$stampColor)
    tags$span(
      class = "color-swatch-box",
      style = paste0("background-color:", input$stampColor, ";")
    )
  })
  
  output$svg_output <- renderUI({
    req(input$nationalPark, input$visitDate, input$stampColor, input$fontSelect)
    HTML(make_stamp_svg(
      text = input$nationalPark,
      date = input$visitDate,
      color = input$stampColor,
      font = input$fontSelect,
      state = parks_df$State[parks_df$National.Park == input$nationalPark],
      circle_grunge = input$circleGrungeSlider,
      text_grunge = input$textGrungeSlider
    ))
  })
  
  output$downloadNPS <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", tolower(input$nationalPark)), "_stamp.svg")
    },
    content = function(file) {
      svg_content <- make_stamp_svg(
        text = input$nationalPark,
        date = input$visitDate,
        color = input$stampColor,
        font = input$fontSelect,
        state = parks_df$State[parks_df$National.Park == input$nationalPark],
        circle_grunge = input$circleGrungeSlider,
        text_grunge = input$textGrungeSlider
      )
      writeLines(svg_content, con = file)
    },
    contentType = "image/svg+xml"
  )
  
}

shinyApp(ui, server)
