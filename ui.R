library(shiny)
library(bslib)
library(readxl)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(DT)
library(RColorBrewer)
library(tidyr)
library(scales)
library(shinymanager)
library(reactable)
library(shiny)
library(httr)
library(jsonlite)

# Define a custom theme (as per your original file)
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3498db",
  secondary = "#2c3e50",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Roboto")
)

ui <- page_fluid(
  theme = custom_theme,
  
  tags$head(
    tags$style(HTML("
      body { 
        overflow-y: auto;
        background-color: #ecf0f1; /* Original background */
      }

      /* --- Refined Keyframe Animations --- */
      @keyframes refinedFadeInSlideUp {
        from { opacity: 0; transform: translateY(10px); } /* Reduced distance */
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes refinedFadeInSlideDown {
        from { opacity: 0; transform: translateY(-10px); } /* Reduced distance */
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes refinedSlideInLeft {
        from { opacity: 0; transform: translateX(-15px); } /* Reduced distance */
        to { opacity: 1; transform: translateX(0); }
      }
      @keyframes refinedFadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      @keyframes refinedPopIn { /* For summary boxes */
        from { opacity: 0; transform: scale(0.98); } /* Subtle scale */
        to { opacity: 1; transform: scale(1); }
      }
      @keyframes refinedFadeInUpItem { /* For summary box items */
        from { opacity: 0; transform: translateY(5px); } /* Reduced distance */
        to { opacity: 1; transform: translateY(0); }
      }
      @keyframes refinedSlideInRightSummary { /* For summary total section */
        from { opacity: 0; transform: translateX(10px); } /* Reduced distance */
        to { opacity: 1; transform: translateX(0); }
      }

      /* Logo Animation */
      .layout_column_wrap > img {
        opacity: 0;
        animation: refinedFadeInSlideDown 0.5s ease-out forwards; /* Quicker */
        animation-delay: 0.1s;
      }

      /* Card Styling & Animation */
      .card {
        margin-bottom: 20px;
        border-radius: 10px; /* Original radius */
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); /* Original shadow */
        /* Refined transition for hover */
        transition: transform 0.25s ease-out, box-shadow 0.25s ease-out; 
        opacity: 0; 
        animation: refinedFadeInSlideUp 0.5s ease-out forwards; /* Quicker, less distance */
      }
      .card:hover {
        transform: translateY(-4px); /* Subtler lift */
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15); /* Subtler shadow */
      }
      .card-header { /* Original style from your file */
        background-color: gray; 
        color: white;
        font-weight: bold;
        border-top-left-radius: 10px;
        border-top-right-radius: 10px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .collapsed-card { /* Original class for collapse */
        height: auto !important;
        min-height: 0 !important;
      }

      /* Button Animations & Styles (as per first animation pass, refined transitions) */
      .btn-primary {
        background-color: gray; /* Original color */
        border-color: gray; /* Original border */
        transition: background-color 0.2s ease, border-color 0.2s ease, transform 0.15s ease;
      }
      .btn-primary:hover {
        background-color: #2980b9; /* Hover from first animation pass */
        border-color: #2980b9; /* Hover from first animation pass */
        transform: translateY(-2px); /* Original hover lift */
      }
      .btn-primary:active {
        transform: translateY(0px) scale(0.98); /* Original active press */
      }

      .btn-secondary {
        background-color: black; /* Original color */
        border-color: black; /* Original border */
        transition: background-color 0.2s ease, border-color 0.2s ease, transform 0.15s ease;
      }
      .btn-secondary:hover {
        background-color: #34495e; /* Hover from first animation pass */
        border-color: #34495e; /* Hover from first animation pass */
        transform: translateY(-2px); /* Original hover lift */
      }
      .btn-secondary:active {
        transform: translateY(0px) scale(0.98); /* Original active press */
      }
      
      .close-btn { /* Original style */
        color: white;
        background: transparent;
        border: none;
        font-size: 20px;
        cursor: pointer;
        padding: 0 10px;
        transition: color 0.2s ease;
      }
      .close-btn:hover {
        color: #f39c12; /* Original hover */
      }

      /* Sidebar elements animation */
      .sidebar > .form-group, 
      .sidebar > .card,       
      .sidebar > .btn,        
      .sidebar > hr,
      .sidebar > :nth-last-child(1), /* Targeting based on first animation pass */
      .sidebar > :nth-last-child(2) {
        opacity: 0;
        transform: translateX(-15px); /* Using refined value */
        animation: refinedSlideInLeft 0.4s ease-out forwards; /* Quicker */
      }
      /* Stagger animation for direct children of sidebar (slightly faster delays) */
      .sidebar > .form-group { animation-delay: 0.1s; } 
      .sidebar > .card       { animation-delay: 0.15s; } 
      .sidebar > .btn        { animation-delay: 0.2s; } 
      .sidebar > hr          { animation-delay: 0.25s; }
      .sidebar > :nth-last-child(2) { animation-delay: 0.3s; } 
      .sidebar > :nth-last-child(1) { animation-delay: 0.35s; }

      /* Plotly & Reactable output animation */
      .shiny-plotly-output, .rt-table {
        opacity: 0;
        animation: refinedFadeIn 0.6s ease-out forwards; /* Quicker fade */
        animation-delay: 0.2s; /* Slightly reduced delay */
      }
      
      /* Animation for summary boxes */
      .combined-currency-card { 
        opacity: 0;
        transform: scale(0.98); /* Initial state for popIn */
        animation: refinedPopIn 0.4s ease-out forwards; /* Quicker */
        animation-delay: 0.2s; 
      }
      .currency-item {
        opacity: 0;
        transform: translateY(5px); /* Initial state for item fade up */
        animation: refinedFadeInUpItem 0.3s ease-out forwards; /* Quicker */
      }
      /* Stagger animation for currency items (quicker delays) */
      .currency-breakdown .currency-item:nth-child(1) { animation-delay: 0.25s; }
      .currency-breakdown .currency-item:nth-child(2) { animation-delay: 0.3s; }
      .currency-breakdown .currency-item:nth-child(3) { animation-delay: 0.35s; }
      .currency-breakdown .currency-item:nth-child(4) { animation-delay: 0.4s; }
      .currency-breakdown .currency-item:nth-child(5) { animation-delay: 0.45s; }

      .total-summary {
        opacity: 0;
        transform: translateX(10px); /* Initial state for total slide in */
        animation: refinedSlideInRightSummary 0.4s ease-out forwards; /* Quicker */
        animation-delay: 0.5s; /* Reduced delay */
      }
    "))
  ),
  
  tags$script(HTML(" // Original JS for card collapse
    $(document).on('click', '.close-btn', function() {
      var cardBody = $(this).closest('.card').find('.card-body');
      if(cardBody.is(':visible')) {
        cardBody.hide();
        $(this).html('&#x25BC;'); // Down arrow
        $(this).closest('.card').addClass('collapsed-card');
      } else {
        cardBody.show();
        $(this).html('&#x2715;'); // X symbol
        $(this).closest('.card').removeClass('collapsed-card');
      }
    });
  ")),
  
  # Header (Original Structure)
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    img(src = "CinnaGen_Logo.png", height = 80, width = 160) # Original logo
  ),
  
  # Main layout (Original Structure)
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;", # Original style
      fileInput("file_3", "Upload your payment file",
                multiple = FALSE,
                accept = c(".xlsx")),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              "Filters",
              tags$button(class = "close-btn", HTML("&#x2715;"))
          )
        ),
        card_body(
          div(class = "live-stat",
              uiOutput("Year_ui_1"),
              uiOutput("date_range_selector"),
              uiOutput("Manufacturer_ui_1"),
              uiOutput("Country_ui_1"),
              uiOutput("Consignee_ui_1")
          )
        )
      ),
      actionButton("calcButton_1", "Calculate", class = "btn-primary mt-3 w-100"),
      hr(),
      "Author: Naser Ahmadi", 
      a(href = "mailto:Naserahmadi3002@gmail.com", "Email", style = "color: #3498db;")
    ),
    
    tabsetPanel(
      id = "main_tabs",
      tabPanel("Overall",
               card(
                 card_body(
                   layout_columns(
                     card(
                       height = "500px",
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             div(
                               tags$i(class = "fas fa-coins me-2"), 
                               "Overall Payment"
                             ),
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         div(class = "combined-currency-card", # This class will have animations applied from head
                             htmlOutput("combined_currency_summary_1")
                         ),
                         # Inline styles for LAYOUT of combined-currency-card (as per original structure)
                         tags$style(HTML("
                           .combined-currency-card { /* Layout styles */
                             display: grid; grid-template-columns: 1fr 1fr; gap: 20px; height: 100%;
                           }
                           .currency-breakdown { display: flex; flex-direction: column; gap: 15px; padding-right: 20px; border-right: 2px solid #eee; }
                           .total-summary { background: linear-gradient(135deg, #007bff, #0056b3); color: white; border-radius: 8px; padding: 10px; display: flex; align-items: center; margin-left: 1px; }
                           .currency-item { display: flex; align-items: center; gap: 10px; padding: 12px; background: #f8f9fa; border-radius: 8px; transition: transform 0.2s; }
                           .currency-item:hover { transform: translateX(5px); }
                           .total-icon { font-size: 2.5em; opacity: 0.9; }
                           .currency-icon { font-size: 1.5em; width: 30px; text-align: center; }
                           .currency-icon.usd { color: #28a745; } .currency-icon.euro { color: #007bff; }
                           .currency-icon.rub { color: #dc3545; } .currency-icon.dinar { color: #ffc107; }  .currency-icon.inr { color: #fd7e14; } /* Added INR for completeness if used */
                         "))
                       )
                     ),
                     card(
                       height = "500px",
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             div(
                               tags$i(class = "fas fa-coins me-2"),
                               "Paid Value"
                             ),
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         div(class = "combined-currency-card",
                             htmlOutput("combined_currency_summary")
                         ),
                         tags$style(HTML("
                           .combined-currency-card { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; height: 100%;}
                           .currency-breakdown { display: flex; flex-direction: column; gap: 15px; padding-right: 20px; border-right: 2px solid #eee; }
                           .total-summary { background: linear-gradient(135deg, #007bff, #0056b3); color: white; border-radius: 8px; padding: 10px; display: flex; align-items: center; margin-left: 1px; }
                           .currency-item { display: flex; align-items: center; gap: 10px; padding: 12px; background: #f8f9fa; border-radius: 8px; transition: transform 0.2s; }
                           .currency-item:hover { transform: translateX(5px); }
                           .total-icon { font-size: 2.5em; opacity: 0.9; }
                           .currency-icon { font-size: 1.5em; width: 30px; text-align: center; }
                           .currency-icon.usd { color: #28a745; } .currency-icon.euro { color: #007bff; }
                           .currency-icon.rub { color: #dc3545; } .currency-icon.dinar { color: #ffc107; } .currency-icon.inr { color: #fd7e14; }
                         "))
                       )
                     ),
                     card(
                       height = "500px",
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             div(
                               tags$i(class = "fas fa-coins me-2"),
                               "Overdue Payment"
                             ),
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         div(class = "combined-currency-card",
                             htmlOutput("combined_currency_summary_2")
                         ),
                         tags$style(HTML("
                           .combined-currency-card { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; height: 100%;}
                           .currency-breakdown { display: flex; flex-direction: column; gap: 15px; padding-right: 20px; border-right: 2px solid #eee; }
                           .total-summary { background: linear-gradient(135deg, #007bff, #0056b3); color: white; border-radius: 8px; padding: 10px; display: flex; align-items: center; margin-left: 1px; }
                           .currency-item { display: flex; align-items: center; gap: 10px; padding: 12px; background: #f8f9fa; border-radius: 8px; transition: transform 0.2s; }
                           .currency-item:hover { transform: translateX(5px); }
                           .total-icon { font-size: 2.5em; opacity: 0.9; }
                           .currency-icon { font-size: 1.5em; width: 30px; text-align: center; }
                           .currency-icon.usd { color: #28a745; } .currency-icon.euro { color: #007bff; }
                           .currency-icon.rub { color: #dc3545; } .currency-icon.dinar { color: #ffc107; } .currency-icon.inr { color: #fd7e14; }
                         "))
                       )
                     ),
                     col_widths = c(4,4,4)
                   )
                 )
               ),
               card(
                 card_body(
                   layout_columns(
                     card(
                       height = "500px",
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             "Total paid value in USD by year of export",
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         div(class = "live-stat",
                             plotlyOutput("plot_Year")
                         )
                       )
                     ),
                     card(
                       height = "500px", # Added height for consistency
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             "Paid value in USD by Value type",
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         plotlyOutput("plot_Paid_type")
                       )
                     )
                   )
                 )
               ),
               card(
                 card_body(
                   layout_columns(
                     card(
                       height = "500px", # Added height for consistency
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             "Paid value by manufacturer",
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         plotlyOutput('plot_Manufacturer_1')
                       )
                     ),
                     card(
                       height = "500px", # Added height for consistency
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             "Paid value in USD by manufacturer",
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         plotlyOutput("plot_Manufacturer_2")
                       )
                     )
                   )
                 )
               ),
               card(
                 card_body(
                   layout_columns(
                     card(
                       height = "600px", # Added height
                       card_header(
                         div(class = "d-flex justify-content-between align-items-center",
                             "Payments by payment type and currency type",
                             tags$button(class = "close-btn", HTML("&#x2715;"))
                         )
                       ),
                       card_body(
                         plotlyOutput('plot_currency', height="500px") # ensure plot has enough height
                       )
                     )
                   )
                 )
               )
      ),
      tabPanel("Tables",
               card(
                 card_header(
                   div(class = "d-flex justify-content-between align-items-center",
                       "Invoice data",
                       tags$button(class = "close-btn", HTML("&#x2715;"))
                   )
                 ),
                 card_body(
                   card(
                     card_body(
                       reactableOutput("data_1")
                     )
                   ),
                   card(
                     card_body(
                       div(style = "display: flex; justify-content: space-around; padding: 10px;",
                           downloadButton("downloadTable1_0", "Download Data", class = "btn-secondary"),
                           downloadButton("downloadTable1_1", "Download Overdue Data", class = "btn-secondary"),
                           downloadButton("downloadTable1_2", "Download One Week Remains Data", class = "btn-secondary")
                       )
                     )
                   )
                 )
               ),
               card(
                 card_header(
                   div(class = "d-flex justify-content-between align-items-center",
                       "Paid data",
                       tags$button(class = "close-btn", HTML("&#x2715;"))
                   )
                 ),
                 card_body(
                   card(
                     card_body(
                       reactableOutput("data_2")
                     )
                   ),
                   card(
                     card_body(
                       div(style = "display: flex; justify-content: space-around; padding: 10px;",
                           downloadButton("downloadTable2_0", "Download Data", class = "btn-secondary")
                       )
                     )
                   )
                 )
               )
      ),
      tabPanel("Plots",
               card(
                 height = "700px", # Original height
                 card_header(
                   div(class = "d-flex justify-content-between align-items-center",
                       "Paids by currency type and manufacturer",
                       tags$button(class = "close-btn", HTML("&#x2715;"))
                   )
                 ),
                 card_body(
                   plotlyOutput('plot_11', height="600px") 
                 )
               ),
               card(
                 height = "600px", # Original height
                 card_header(
                   div(class = "d-flex justify-content-between align-items-center",
                       "Average delay by consignee",
                       tags$button(class = "close-btn", HTML("&#x2715;"))
                   )
                 ),
                 card_body(
                   plotlyOutput('plot_delay', height="500px")
                 )
               )
      )
    )
  )
)

ui <- secure_app(ui)