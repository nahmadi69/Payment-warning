# ============================================================================
# ENHANCED UI.R - Payment Tracking Shiny App
# Author: Naser Ahmadi  
# Enhanced version with properly resizable sidebar
# Date: November 2025
# ============================================================================

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
library(httr)
library(jsonlite)

# ============================================================================
# CUSTOM THEME
# ============================================================================

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

# ============================================================================
# MAIN UI DEFINITION
# ============================================================================

ui <- page_fluid(
  theme = custom_theme,
  
  # =========================================================================
  # CUSTOM CSS STYLES (Including properly resizable sidebar)
  # =========================================================================
  
  tags$head(
    tags$style(HTML("
      /* Base Styles */
      body {
        overflow-y: auto;
        background-color: #ecf0f1;
      }

      /* ===================================================================
         RESIZABLE SIDEBAR STYLES
         =================================================================== */

      .bslib-sidebar-layout {
        position: relative;
        display: grid;
        grid-template-columns: var(--_sidebar-width, 350px) 1fr;
        gap: var(--bslib-sidebar-layout-gap, 1.5rem);
        transition: grid-template-columns 0.1s ease;
      }

      .sidebar {
        position: relative;
        min-width: 250px !important;
        max-width: 600px !important;
        box-shadow: 2px 0 8px rgba(44, 62, 80, 0.08);
      }

      .sidebar-resize-handle {
        position: absolute;
        right: -5px;
        top: 0;
        bottom: 0;
        width: 10px;
        cursor: ew-resize;
        background-color: transparent;
        transition: background-color 0.2s;
        z-index: 1000;
      }

      .sidebar-resize-handle:hover {
        background-color: rgba(52, 152, 219, 0.4);
        box-shadow: 0 0 5px rgba(52, 152, 219, 0.5);
      }

      .sidebar-resize-handle:active {
        background-color: #3498db;
      }

      .sidebar-resize-handle::after {
        content: '';
        position: absolute;
        left: 50%;
        top: 50%;
        transform: translate(-50%, -50%);
        width: 3px;
        height: 40px;
        background-color: rgba(52, 152, 219, 0.3);
        border-radius: 2px;
        transition: background-color 0.2s;
      }

      .sidebar-resize-handle:hover::after {
        background-color: rgba(52, 152, 219, 0.8);
      }

      /* ===================================================================
         KEYFRAME ANIMATIONS
         =================================================================== */

      @keyframes refinedFadeInSlideUp {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes refinedFadeInSlideDown {
        from { opacity: 0; transform: translateY(-10px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes refinedSlideInLeft {
        from { opacity: 0; transform: translateX(-15px); }
        to { opacity: 1; transform: translateX(0); }
      }

      @keyframes refinedFadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }

      @keyframes refinedPopIn {
        from { opacity: 0; transform: scale(0.98); }
        to { opacity: 1; transform: scale(1); }
      }

      @keyframes refinedFadeInUpItem {
        from { opacity: 0; transform: translateY(5px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes refinedSlideInRightSummary {
        from { opacity: 0; transform: translateX(10px); }
        to { opacity: 1; transform: translateX(0); }
      }

      /* ===================================================================
         LOGO ANIMATION
         =================================================================== */

      .layout_column_wrap > img {
        opacity: 0;
        animation: refinedFadeInSlideDown 0.5s ease-out forwards;
        animation-delay: 0.1s;
      }

      /* ===================================================================
         CARD STYLING & ANIMATION
         =================================================================== */

      .card {
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: transform 0.25s ease-out, box-shadow 0.25s ease-out;
        opacity: 0;
        animation: refinedFadeInSlideUp 0.5s ease-out forwards;
      }

      .card:hover {
        transform: translateY(-4px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
      }

      .card-header {
        background-color: #34495e;
        color: white;
        font-weight: bold;
        border-top-left-radius: 10px;
        border-top-right-radius: 10px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 15px;
      }

      .collapsed-card {
        height: auto !important;
        min-height: 0 !important;
      }

      /* ===================================================================
         BUTTON STYLES
         =================================================================== */

      .btn-primary {
        background-color: #3498db;
        border-color: #3498db;
        transition: background-color 0.2s ease, border-color 0.2s ease, 
                    transform 0.15s ease;
        font-weight: 500;
      }

      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #2980b9;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(52, 152, 219, 0.3);
      }

      .btn-primary:active {
        transform: translateY(0px) scale(0.98);
      }

      .btn-primary:disabled {
        background-color: #95a5a6;
        border-color: #95a5a6;
        cursor: not-allowed;
        opacity: 0.6;
      }

      .btn-secondary {
        background-color: #34495e;
        border-color: #34495e;
        transition: background-color 0.2s ease, border-color 0.2s ease, 
                    transform 0.15s ease;
        font-weight: 500;
      }

      .btn-secondary:hover {
        background-color: #2c3e50;
        border-color: #2c3e50;
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(52, 73, 94, 0.3);
      }

      .btn-secondary:active {
        transform: translateY(0px) scale(0.98);
      }

      .close-btn {
        color: white;
        background: transparent;
        border: none;
        font-size: 20px;
        cursor: pointer;
        padding: 0 10px;
        transition: color 0.2s ease, transform 0.2s ease;
      }

      .close-btn:hover {
        color: #f39c12;
        transform: rotate(90deg);
      }

      /* ===================================================================
         FILE INPUT ENHANCEMENT
         =================================================================== */

      .form-group {
        margin-bottom: 20px;
      }

      .file-upload-help {
        font-size: 0.85em;
        color: #7f8c8d;
        margin-top: 5px;
        font-style: italic;
      }

      /* ===================================================================
         SIDEBAR ELEMENTS ANIMATION
         =================================================================== */

      .sidebar > .form-group,
      .sidebar > .card,
      .sidebar > .btn,
      .sidebar > hr,
      .sidebar > :nth-last-child(1),
      .sidebar > :nth-last-child(2) {
        opacity: 0;
        transform: translateX(-15px);
        animation: refinedSlideInLeft 0.4s ease-out forwards;
      }

      /* Stagger animation */
      .sidebar > .form-group { animation-delay: 0.1s; }
      .sidebar > .card { animation-delay: 0.15s; }
      .sidebar > .btn { animation-delay: 0.2s; }
      .sidebar > hr { animation-delay: 0.25s; }
      .sidebar > :nth-last-child(2) { animation-delay: 0.3s; }
      .sidebar > :nth-last-child(1) { animation-delay: 0.35s; }

      /* ===================================================================
         PLOTLY & TABLE ANIMATIONS
         =================================================================== */

      .shiny-plotly-output, .rt-table {
        opacity: 0;
        animation: refinedFadeIn 0.6s ease-out forwards;
        animation-delay: 0.2s;
      }

      /* ===================================================================
         CURRENCY SUMMARY BOXES
         =================================================================== */

      .combined-currency-card {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 20px;
        height: 100%;
        opacity: 0;
        transform: scale(0.98);
        animation: refinedPopIn 0.4s ease-out forwards;
        animation-delay: 0.2s;
      }

      .currency-breakdown {
        display: flex;
        flex-direction: column;
        gap: 15px;
        padding-right: 20px;
        border-right: 2px solid #eee;
      }

      .total-summary {
        background: linear-gradient(135deg, #3498db, #2980b9);
        color: white;
        border-radius: 8px;
        padding: 20px;
        display: flex;
        align-items: center;
        margin-left: 1px;
        box-shadow: 0 4px 10px rgba(52, 152, 219, 0.3);
      }

      .currency-item {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 12px;
        background: #f8f9fa;
        border-radius: 8px;
        transition: transform 0.2s, box-shadow 0.2s;
        opacity: 0;
        transform: translateY(5px);
        animation: refinedFadeInUpItem 0.3s ease-out forwards;
      }

      .currency-item:hover {
        transform: translateX(5px);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
      }

      /* Stagger currency items */
      .currency-breakdown .currency-item:nth-child(1) { animation-delay: 0.25s; }
      .currency-breakdown .currency-item:nth-child(2) { animation-delay: 0.3s; }
      .currency-breakdown .currency-item:nth-child(3) { animation-delay: 0.35s; }
      .currency-breakdown .currency-item:nth-child(4) { animation-delay: 0.4s; }
      .currency-breakdown .currency-item:nth-child(5) { animation-delay: 0.45s; }

      .total-summary {
        opacity: 0;
        transform: translateX(10px);
        animation: refinedSlideInRightSummary 0.4s ease-out forwards;
        animation-delay: 0.5s;
      }

      .total-icon {
        font-size: 2.5em;
        opacity: 0.9;
      }

      .currency-icon {
        font-size: 1.5em;
        width: 30px;
        text-align: center;
      }

      /* ===================================================================
         RESPONSIVE DESIGN
         =================================================================== */

      @media (max-width: 768px) {
        .combined-currency-card {
          grid-template-columns: 1fr;
        }

        .currency-breakdown {
          border-right: none;
          border-bottom: 2px solid #eee;
          padding-bottom: 20px;
        }

        .bslib-sidebar-layout {
          grid-template-columns: 1fr !important;
        }

        .sidebar {
          width: 100% !important;
        }

        .sidebar-resize-handle {
          display: none;
        }
      }

      /* ===================================================================
         ACCESSIBILITY IMPROVEMENTS
         =================================================================== */

      /* Focus states for keyboard navigation */
      .btn:focus,
      input:focus,
      select:focus {
        outline: 3px solid #3498db;
        outline-offset: 2px;
      }

      /* Better contrast for links */
      a {
        color: #2980b9;
        text-decoration: none;
        transition: color 0.2s ease;
      }

      a:hover {
        color: #3498db;
        text-decoration: underline;
      }

      /* ===================================================================
         TABLE ENHANCEMENTS
         =================================================================== */

      .rt-table {
        border: 1px solid #dee2e6;
        border-radius: 8px;
        overflow: hidden;
      }

      .rt-thead {
        background-color: #34495e;
        color: white;
      }

      .rt-tbody .rt-tr:hover {
        background-color: #f8f9fa;
      }

      /* ===================================================================
         NOTIFICATION STYLING
         =================================================================== */

      .shiny-notification {
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
        font-size: 14px;
      }

      .shiny-notification-message {
        background-color: #3498db;
        color: white;
      }

      .shiny-notification-warning {
        background-color: #f39c12;
        color: white;
      }

      .shiny-notification-error {
        background-color: #e74c3c;
        color: white;
      }

      /* ===================================================================
         PROGRESS BAR STYLING
         =================================================================== */

      .shiny-progress {
        border-radius: 8px;
        overflow: hidden;
      }

      .shiny-progress-bar {
        background-color: #3498db;
        transition: width 0.3s ease;
      }
    "))
  ),
  
  # =========================================================================
  # JAVASCRIPT FOR CARD COLLAPSE AND RESIZABLE SIDEBAR
  # =========================================================================
  
  tags$script(HTML("
    // Card collapse functionality
    $(document).on('click', '.close-btn', function() {
      var cardBody = $(this).closest('.card').find('.card-body');
      if(cardBody.is(':visible')) {
        cardBody.slideUp(300);
        $(this).html('▼');
        $(this).closest('.card').addClass('collapsed-card');
      } else {
        cardBody.slideDown(300);
        $(this).html('✕');
        $(this).closest('.card').removeClass('collapsed-card');
      }
    });

    // Resizable sidebar functionality
    $(document).ready(function() {
      // Add resize handle if not already present
      if ($('.sidebar-resize-handle').length === 0) {
        $('.sidebar').append('<div class=\"sidebar-resize-handle\"></div>');
      }

      var isResizing = false;
      var sidebarLayout = $('.bslib-sidebar-layout');
      var minWidth = 250;
      var maxWidth = 600;

      // Mouse down on resize handle
      $(document).on('mousedown', '.sidebar-resize-handle', function(e) {
        isResizing = true;
        $('body').css({
          'cursor': 'ew-resize',
          'user-select': 'none'
        });
        e.preventDefault();
      });

      // Mouse move - resize sidebar and adjust main content
      $(document).on('mousemove', function(e) {
        if (!isResizing) return;
        
        var containerOffset = sidebarLayout.offset().left;
        var pointerRelativeX = e.clientX - containerOffset;
        
        // Constrain width between min and max
        var newWidth = Math.min(Math.max(pointerRelativeX, minWidth), maxWidth);
        
        // Update CSS custom property which controls grid layout
        sidebarLayout.css('--_sidebar-width', newWidth + 'px');
        
        // Trigger window resize event for plotly/DT to adjust
        $(window).trigger('resize');
      });

      // Mouse up - stop resizing
      $(document).on('mouseup', function(e) {
        if (isResizing) {
          isResizing = false;
          $('body').css({
            'cursor': 'default',
            'user-select': 'auto'
          });
          
          // Final resize trigger for all plots
          setTimeout(function() {
            $(window).trigger('resize');
          }, 100);
        }
      });
    });
  ")),
  
  # =========================================================================
  # HEADER
  # =========================================================================
  
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    img(src = "CinnaGen_Logo.png", height = 80, width = 160, alt = "CinnaGen Logo")
  ),
  
  # =========================================================================
  # MAIN LAYOUT
  # =========================================================================
  
  layout_sidebar(
    
    # =======================================================================
    # SIDEBAR
    # =======================================================================
    
    sidebar = sidebar(
      width = 350,
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;",
      
      # File upload with enhanced help text
      fileInput(
        "file_3",
        "Upload your payment file",
        multiple = FALSE,
        accept = c(".xlsx"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      div(
        class = "file-upload-help",
        HTML("📄 Required sheets: <b>Paid</b>, <b>PI</b>, <b>Invoice</b>")
      ),
      
      # Filters card
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              tags$i(class = "fas fa-filter me-2"),
              "Filters"
            ),
            tags$button(class = "close-btn", HTML("✕"))
          )
        ),
        card_body(
          div(
            class = "live-stat",
            uiOutput("Year_ui_1"),
            uiOutput("date_range_selector"),
            uiOutput("Manufacturer_ui_1"),
            uiOutput("Country_ui_1"),
            uiOutput("Consignee_ui_1")
          )
        )
      ),
      
      # Calculate button
      actionButton(
        "calcButton_1",
        "Calculate",
        class = "btn-primary mt-3 w-100",
        icon = icon("calculator")
      ),
      
      hr(),
      
      # Exchange rate inputs (conditional)
      conditionalPanel(
        condition = "input.file_3",
        card(
          card_header("Exchange Rates"),
          card_body(
            uiOutput("multiplier_EUR"),
            uiOutput("multiplier_RUB"),
            uiOutput("multiplier_IQD"),
            uiOutput("multiplier_INR")
          )
        )
      ),
      
      hr(),
      
      # Author info
      div(
        style = "text-align: center; padding: 10px;",
        p("Author: Naser Ahmadi", style = "margin: 5px 0;"),
        a(
          href = "mailto:Naserahmadi3002@gmail.com",
          icon("envelope"),
          "Email",
          style = "color: #3498db; margin-right: 10px;"
        ),
        br(),
        a(
          href = "https://nahmadi69.quarto.pub/naser-ahmadi/",
          target = "_blank",
          icon("globe"),
          "Portfolio",
          style = "color: #3498db;"
        )
      )
    ),
    
    # =======================================================================
    # MAIN CONTENT AREA
    # =======================================================================
    
    tabsetPanel(
      id = "main_tabs",
      
      # =====================================================================
      # TAB: OVERALL
      # =====================================================================
      
      tabPanel(
        "Overall",
        icon = icon("chart-line"),
        
        # Summary cards for overall statistics
        card(
          card_body(
            layout_columns(
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    div(
                      tags$i(class = "fas fa-coins me-2"),
                      "Overall Payment"
                    ),
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  div(
                    class = "combined-currency-card",
                    htmlOutput("combined_currency_summary_1")
                  )
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    div(
                      tags$i(class = "fas fa-check-circle me-2"),
                      "Paid value based on invoice date"
                    ),
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  div(
                    class = "combined-currency-card",
                    htmlOutput("combined_currency_summary_2")
                  )
                )
              ),
              col_widths = c(6, 6)
            )
          )
        ),
        
        # Overdue and future payments
        card(
          card_body(
            layout_columns(
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    div(
                      tags$i(class = "fas fa-exclamation-triangle me-2"),
                      "Overdue Payment"
                    ),
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  div(
                    class = "combined-currency-card",
                    htmlOutput("combined_currency_summary_3")
                  )
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    div(
                      tags$i(class = "fas fa-clock me-2"),
                      "Future Payment"
                    ),
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  div(
                    class = "combined-currency-card",
                    htmlOutput("combined_currency_summary_4")
                  )
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    div(
                      tags$i(class = "fas fa-plus-circle me-2"),
                      "Additional Paid"
                    ),
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  div(
                    class = "combined-currency-card",
                    htmlOutput("combined_currency_summary_5")
                  )
                )
              ),
              col_widths = c(4, 4, 4)
            )
          )
        ),
        
        # Year-based visualizations
        card(
          card_body(
            layout_columns(
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total paid value in USD by year",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Year")
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total paid value in USD by manufacturer",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Manufacturer_2")
                )
              )
            )
          )
        ),
        
        # Payment value visualizations
        card(
          card_body(
            layout_columns(
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total payment value in USD by year",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Year_1")
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total payment value in USD by manufacturer",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Manufacturer_1")
                )
              )
            )
          )
        ),
        
        # Overdue payment visualizations
        card(
          card_body(
            layout_columns(
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total overdue payment in USD by year",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Year_3")
                )
              ),
              card(
                height = "500px",
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    "Total overdue payment in USD by manufacturer",
                    tags$button(class = "close-btn", HTML("✕"))
                  )
                ),
                card_body(
                  plotlyOutput("plot_Manufacturer_3")
                )
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB: TABLES
      # =====================================================================
      
      tabPanel(
        "Tables",
        icon = icon("table"),
        
        # Invoice data table
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$i(class = "fas fa-file-invoice me-2"),
                "Invoice data"
              ),
              tags$button(class = "close-btn", HTML("✕"))
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
                div(
                  style = "display: flex; justify-content: space-around; padding: 10px;",
                  downloadButton("downloadTable1_0", "Download All Data", class = "btn-secondary"),
                  downloadButton("downloadTable1_1", "Download Overdue", class = "btn-secondary"),
                  downloadButton("downloadTable1_2", "Download One Week Remaining", class = "btn-secondary")
                )
              )
            )
          )
        ),
        
        # Paid data table
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$i(class = "fas fa-check-circle me-2"),
                "Paid data"
              ),
              tags$button(class = "close-btn", HTML("✕"))
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
                div(
                  style = "display: flex; justify-content: center; padding: 10px;",
                  downloadButton("downloadTable2_0", "Download Paid Data", class = "btn-secondary")
                )
              )
            )
          )
        ),
        
        # Additional paid table
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$i(class = "fas fa-plus-circle me-2"),
                "Additional Paid"
              ),
              tags$button(class = "close-btn", HTML("✕"))
            )
          ),
          card_body(
            card(
              card_body(
                reactableOutput("data_3")
              )
            ),
            card(
              card_body(
                div(
                  style = "display: flex; justify-content: center; padding: 10px;",
                  downloadButton("downloadTable3_0", "Download Additional Paid Data", class = "btn-secondary")
                )
              )
            )
          )
        )
      ),
      
      # =====================================================================
      # TAB: PLOTS
      # =====================================================================
      
      tabPanel(
        "Plots",
        icon = icon("chart-bar"),
        
        # Currency type and manufacturer plot
        card(
          height = "700px",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$i(class = "fas fa-chart-pie me-2"),
                "Payments by currency type"
              ),
              tags$button(class = "close-btn", HTML("✕"))
            )
          ),
          card_body(
            plotlyOutput("plot_currency", height = "600px")
          )
        ),
        
        # Average delay by consignee
        card(
          height = "600px",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$i(class = "fas fa-clock me-2"),
                "Average payment delay by consignee"
              ),
              tags$button(class = "close-btn", HTML("✕"))
            )
          ),
          card_body(
            plotlyOutput("plot_delay", height = "500px")
          )
        )
      )
    )
  )
)

# ============================================================================
# SECURE APP WRAPPER
# ============================================================================

ui <- secure_app(ui)
