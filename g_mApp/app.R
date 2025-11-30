# g_mApp - full app.R
# Includes:
# - two tabs: About + App
# - PDF report download (rmarkdown)
# - Download Excel template (hard-coded path)
# - All original calculations and plots preserved
#
# Author: Matija Pavlovic
# NOTE: Template path is hard-coded as requested:
# "D:/Shiny App no1/g_mApp/g_mApp_template.xlsx"

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(bslib)
library(readxl)
library(plotly)
library(tibble)
library(rmarkdown)
library(knitr)
library(tinytex)

# ============================================================
# 1) Khamis–Roche tables
# ============================================================
kr_male <- tribble(
  ~age,~b0,~b_stat,~b_wt,~b_mid,
  4.0,-10.2567,1.23812,-0.0087235,0.50286,
  5.0,-11.0213,1.10674,-0.0064778,0.53919,
  6.0,-11.1138,1.05923,-0.0052947,0.52513,
  7.0,-10.9984,1.05877,-0.0048144,0.48538,
  8.0,-11.0696,1.06853,-0.0046778,0.44469,
  9.0,-11.1571,1.05166,-0.0045254,0.42776,
  10.0,-11.0380,0.97135,-0.0039981,0.45932,
  11.0,-10.4917,0.81239,-0.0029050,0.54781,
  12.0,-9.3522,0.68325,-0.0020076,0.60927,
  13.0,-7.8632,0.60818,-0.0013895,0.62407,
  14.0,-6.4299,0.59151,-0.0009776,0.58762,
  15.0,-5.1282,0.63757,-0.0006988,0.49536,
  16.0,-3.9292,0.75069,-0.0004795,0.34271,
  17.0,-3.2830,0.93520,-0.0002470,0.12510
)

kr_female <- tribble(
  ~age,~b0,~b_stat,~b_wt,~b_mid,
  4.0,-8.1325,1.24768,-0.019435,0.44774,
  5.0,-5.13582,1.19932,-0.017530,0.38467,
  6.0,-3.51039,1.15866,-0.015400,0.34105,
  7.0,-2.87645,1.11342,-0.013184,0.31748,
  8.0,-2.45559,1.05135,-0.011019,0.31457,
  9.0,-1.87098,0.96020,-0.009044,0.33291,
  10.0,0.33468,0.82771,-0.007397,0.37312,
  11.0,3.50436,0.67173,-0.006136,0.42042,
  12.0,4.84365,0.64452,-0.004894,0.39490,
  13.0,3.21417,0.72260,-0.003661,0.31163,
  14.0,0.32425,0.85062,-0.002500,0.20235,
  15.0,-2.35055,0.97319,-0.001477,0.09880,
  16.0,-3.17885,1.03496,-0.000655,0.03272,
  17.0,-0.65579,0.98054,-0.000100,0.03584
)

# ============================================================
# 2) Helper functions
# ============================================================
decimal_age <- function(dob, dom) {
  as.numeric(difftime(dom, dob, units = "days")) / 365.25
}

mirwald_full <- function(sex, age, height, sitting, weight) {
  leg <- height - sitting
  if (tolower(sex) %in% c("male","m")) {
    offset <- -9.236 +
      0.0002708 * (leg * sitting) -
      0.001663  * (age * leg) +
      0.007216  * (age * sitting) +
      0.02292   * ((weight / height) * 100)
  } else {
    offset <- -9.376 +
      0.0001882 * (leg * sitting) +
      0.0022    * (height * age) +
      0.005841  * (age * sitting) -
      0.002658  * (age * leg) +
      0.07693   * ((weight / height) * 100)
  }
  list(offset = offset, age_phv = age - offset)
}

khamis_roche <- function(sex, age, height_cm, weight_kg, mother_cm, father_cm) {
  height_in <- height_cm / 2.54
  weight_lb <- weight_kg * 2.20462
  mid_in    <- ((mother_cm + father_cm) / 2) / 2.54
  
  tab <- if (tolower(sex) %in% c("male","m")) kr_male else kr_female
  row <- tab[which.min(abs(tab$age - age)), ]
  
  PAH_in <- row$b0 +
    row$b_stat * height_in +
    row$b_wt   * weight_lb +
    row$b_mid  * mid_in
  
  PAH_cm <- as.numeric(PAH_in * 2.54)
  list(PAH = PAH_cm, Perc = 100 * height_cm / PAH_cm)
}

maturity_status <- function(offset) {
  dplyr::case_when(
    offset < -1 ~ "Pre-PHV",
    offset <= 1 ~ "At-PHV",
    offset > 1  ~ "Post-PHV",
    TRUE ~ NA_character_
  )
}

# ============================================================
# 3) UI - two tabs: About + App
# ============================================================
# Hard-coded template path (as requested)
TEMPLATE_PATH <- "g_mApp_template.xlsx"

ui <- navbarPage(
  title = NULL,
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bg = "#0f172a",
    fg = "#e2e8f0",
    base_font = font_google("Inter"),
    primary = "#38bdf8"
  ),
  tabPanel("About",
           fluidRow(
             column(8,
                    tags$div(style = "padding:18px;",
                             tags$h3("Growth & Maturation App"),
                             tags$p("A tool for coaches and practitioners to estimate biological maturity (Mirwald) and Predicted Adult Height (Khamis–Roche)."),
                             tags$ol(
                               tags$li(tags$b("What the app does:"), " Calculates maturity offset, age at PHV, predicted adult height and displays growth visualisations."),
                               tags$li(tags$b("Data input:"), " Manual entry or upload an Excel (.xlsx) file using the provided template."),
                               tags$li(tags$b("PDF report:"), " Download a PDF summary of the athlete's current data and plots."),
                               tags$li(tags$b("Author:"), " Matija Pavlovic")
                             ),
                             tags$h4("Links"),
                             tags$ul(
                               tags$li(tags$a(href="https://github.com/matijapavlovic", "GitHub — matijapavlovic", target="_blank")),
                               tags$li(tags$a(href="http://linkedin.com/in/matija-pavlovic", "LinkedIn — Matija Pavlovic", target="_blank")),
                               tags$li(tags$a(href="https://x.com/MatijaPavlovic", "X / Twitter — @MatijaPavlovic", target="_blank")),
                               tags$li(tags$a(href="https://linktr.ee/matijapav", "Linktree", target="_blank"))
                             ),
                             tags$p("Note: This app is designed to support coaches and practitioners in tracking growth and maturation, but it should not replace proper understanding of the underlying science. Please interpret all outputs with reference to the relevant literature and your own professional judgment. The tool is not meant to offer definitive conclusions. If you have questions, feedback, or need clarification, feel free to reach out anytime using the contact information provided above")
                    )
             ),
             column(4,
                    tags$div(style="padding:18px;",
                             tags$h5("Quick tips"),
                             tags$ul(
                               tags$li("Use the Excel template if you have multiple timepoints."),
                               tags$li("Template is pre-filled with correct column names for the app.")
                             )
                    )
             )
           )
  ),
  tabPanel("App",
           fluidRow(
             column(
               3,
               div(class="sidebar-block",
                   h5("Athlete input"),
                   radioButtons("mode", NULL,
                                c("Manual"="manual","Upload File"="excel"), "manual"),
                   conditionalPanel(
                     "input.mode=='excel'",
                     fileInput("file","Excel (.xlsx)",accept=".xlsx"),
                     uiOutput("excel_athlete")
                   ),
                   conditionalPanel(
                     "input.mode=='manual'",
                     textInput("athlete","Athlete","Athlete 01"),
                     selectInput("gender","Gender",c("Male","Female")),
                     dateInput("dob","Date of birth", value = Sys.Date() - 12*365),
                     dateInput("dom","Measurement date", value = Sys.Date()),
                     numericInput("height","Height (cm)",152,80,220),
                     numericInput("sitting","Sitting height (cm)",80,40,140),
                     numericInput("weight","Weight (kg)",46,20,150),
                     h6("Parents (cm)"),
                     numericInput("mother_h","Mother",162,120,210),
                     numericInput("father_h","Father",170,120,220)
                   )
               )
             ),
             
             column(
               9,
               tags$head(tags$style(HTML("
          .kpi-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:10px;margin-bottom:14px;}
          .kpi-card{background:#1f2937;border-radius:14px;padding:12px;min-height:78px;box-shadow:0 4px 10px rgba(0,0,0,.25);}
          .kpi-label{font-size:0.9rem;font-weight:600;color:#9CA3AF;}
          .kpi-value{font-size:1.6rem;font-weight:700;color:#e2e8f0;}
          .plot-panel{background:#1f2937;border-radius:14px;padding:10px;margin-bottom:10px;min-height:180px;}
        "))),
               
               div(class="kpi-grid",
                   div(class="kpi-card", div(class="kpi-label","Height"), div(class="kpi-value", textOutput("kpi_height"))),
                   div(class="kpi-card", div(class="kpi-label","Weight"), div(class="kpi-value", textOutput("kpi_weight"))),
                   div(class="kpi-card", div(class="kpi-label","Chronological age"), div(class="kpi-value", textOutput("kpi_chrono"))),
                   div(class="kpi-card", div(class="kpi-label","Biological age"), div(class="kpi-value", textOutput("kpi_bio"))),
                   div(class="kpi-card", div(class="kpi-label","Predicted adult height"), div(class="kpi-value", textOutput("kpi_pah"))),
                   div(class="kpi-card", div(class="kpi-label","Maturity offset"), div(class="kpi-value", textOutput("kpi_offset"))),
                   div(class="kpi-card", div(class="kpi-label","Age at PHV"), div(class="kpi-value", textOutput("kpi_aphv"))),
                   div(class="kpi-card", div(class="kpi-label","Maturity status"), div(class="kpi-value", textOutput("kpi_status")))
               ),
               
               fluidRow(
                 column(4, div(class="plot-panel", uiOutput("gauge_pah"))),
                 column(4, div(class="plot-panel", uiOutput("gauge_growth"))),
                 column(4, div(class="plot-panel", plotOutput("plot_offset", height = 180)))
               ),
               
               fluidRow(
                 column(6, div(class="plot-panel", plotOutput("plot_pah_zone", height = 210))),
                 column(6, div(class="plot-panel", plotOutput("plot_growth", height = 210)))
               ),
               
               br(),
               fluidRow(
                 column(4, downloadButton("report", "Download PDF Report", class = "btn btn-primary")),
                 column(4, downloadButton("template", "Download Excel template", class = "btn btn-primary"))
               ),
               br()
             )
           )
  )
)

# ============================================================
# 4) SERVER
# ============================================================
server <- function(input, output, session) {
  
  # --------------------------- MANUAL PATH ------------------------------
  manual_data <- reactive({
    dob <- as.Date(input$dob); dom <- as.Date(input$dom)
    age <- decimal_age(dob, dom)
    
    kr <- khamis_roche(input$gender, age, input$height, input$weight,
                       input$mother_h, input$father_h)
    mir <- mirwald_full(input$gender, age, input$height,
                        input$sitting, input$weight)
    
    tibble(
      Athlete = input$athlete,
      Age = age,
      Height = input$height,
      Weight = input$weight,
      PAH = kr$PAH,
      Perc = kr$Perc,
      Offset = mir$offset,
      AgePHV = mir$age_phv,
      BioAge = age - mir$offset,
      Status = maturity_status(mir$offset),
      GrowthTempo = NA_real_
    )
  })
  
  # --------------------------- EXCEL PATH -------------------------------
  excel_raw <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  output$excel_athlete <- renderUI({
    req(excel_raw())
    selectInput(
      "excel_name", "Select athlete",
      choices = unique(excel_raw()$Athlete)
    )
  })
  
  excel_processed <- reactive({
    req(excel_raw(), input$excel_name)
    
    df <- excel_raw() %>%
      filter(Athlete == input$excel_name) %>%
      mutate(
        DOB    = as.Date(`Date of Birth`),
        DOM    = as.Date(`Testing Date`),
        Age    = decimal_age(DOB, DOM),
        Height = `Height (CM)`,
        Weight = `Weight (KG)`,
        Sitting = `Sitting Height (CM)`,
        Mother  = `Mothers Height (CM)`,
        Father  = `Fathers Height (CM)`
      ) %>%
      arrange(DOM)
    
    df <- df %>%
      rowwise() %>%
      mutate(
        tmp_kr  = list(khamis_roche(Gender, Age, Height, Weight, Mother, Father)),
        PAH     = tmp_kr$PAH,
        Perc    = tmp_kr$Perc,
        tmp_mir = list(mirwald_full(Gender, Age, Height, Sitting, Weight)),
        Offset  = tmp_mir$offset,
        AgePHV  = tmp_mir$age_phv,
        BioAge  = Age - Offset
      ) %>%
      ungroup() %>%
      select(-tmp_kr, -tmp_mir)
    
    df <- df %>%
      arrange(Age) %>%
      mutate(
        GrowthTempo = if_else(
          row_number() == 1,
          NA_real_,
          (Height - lag(Height)) / (Age - lag(Age))
        )
      ) %>%
      mutate(Status = maturity_status(Offset))
    
    df
  })
  
  excel_latest <- reactive({
    excel_processed() %>% slice_tail(n = 1)
  })
  
  # Choose dataset
  current <- reactive({
    if (input$mode == "manual") manual_data() else excel_latest()
  })
  
  growth_df <- reactive({
    if (input$mode == "manual") manual_data() else excel_processed()
  })
  
  # KPI outputs
  output$kpi_height  <- renderText({ paste0(round(current()$Height,1)," cm") })
  output$kpi_weight  <- renderText({ paste0(round(current()$Weight,1)," kg") })
  output$kpi_chrono  <- renderText({ paste0(round(current()$Age,2)," y") })
  output$kpi_bio     <- renderText({ paste0(round(current()$BioAge,2)," y") })
  output$kpi_pah     <- renderText({ paste0(round(current()$PAH,1)," cm") })
  output$kpi_offset  <- renderText({ paste0(round(current()$Offset,2)," y") })
  output$kpi_aphv    <- renderText({ paste0(round(current()$AgePHV,2)," y") })
  output$kpi_status  <- renderText({ current()$Status })
  
  # % PAH donut gauge (UI)
  output$gauge_pah <- renderUI({
    d <- current(); p <- d$Perc[1]; if (is.na(p)) p <- 0
    p <- max(min(p, 110), 0)
    ang <- p / 100 * 360
    div(style="width:100%;height:100%;display:flex;flex-direction:column;align-items:center;justify-content:center;",
        div(style=paste0("width:150px;height:150px;border-radius:50%;
                background:conic-gradient(#38bdf8 ",ang,"deg,#1f2937 0);
                display:flex;align-items:center;justify-content:center;"),
            div(style="background:#0f172a;border-radius:50%;width:90px;height:90px;display:flex;align-items:center;justify-content:center;",
                span(style="font-size:1.4rem;font-weight:700;", paste0(round(p,1),"%"))
            )
        ),
        div(style="margin-top:6px;font-weight:600;","% Predicted adult height")
    )
  })
  
  # Growth Tempo gauge
  output$gauge_growth <- renderUI({
    div(
      style="
        width:100%;
        height:100%;
        display:flex;
        flex-direction:column;
        align-items:center;
        justify-content:space-between;
        padding-top:8px;
        padding-bottom:8px;
      ",
      div(style="width:90%; display:flex; justify-content:center;",
          plotlyOutput("growthGaugePlot", height='140px', width='100%')
      ),
      div(style="text-align:center;font-weight:600;margin-top:6px;margin-bottom:4px;",
          "Growth Tempo")
    )
  })
  
  output$growthGaugePlot <- renderPlotly({
    df <- growth_df()
    
    if (nrow(df) < 2 || all(is.na(df$GrowthTempo))) {
      fig <- plot_ly(type="indicator", mode="number", value=NULL) %>%
        layout(
          annotations=list(
            x=0.5, y=0.5,
            text="<b>Growth Tempo requires<br>at least 2 measurements</b>",
            showarrow=FALSE,
            font=list(color="#e2e8f0", size=13)
          ),
          paper_bgcolor="rgba(0,0,0,0)",
          plot_bgcolor="rgba(0,0,0,0)",
          margin=list(l=0,r=0,t=0,b=0)
        )
      return(fig)
    }
    
    gt <- tail(df$GrowthTempo[!is.na(df$GrowthTempo)], 1)
    gt_clamped <- max(min(gt, 12), 0)
    
    fig <- plot_ly(
      type="indicator",
      mode="gauge+number",
      value=gt_clamped,
      number=list(suffix=" cm/y", font=list(size=16,color="#e2e8f0")),
      gauge=list(
        axis=list(range=list(0,12)),
        steps=list(
          list(range=c(0,3.6), color="rgba(34,197,94,.8)"),
          list(range=c(3.6,7.2), color="rgba(250,204,21,.8)"),
          list(range=c(7.2,12), color="rgba(239,68,68,.8)")
        ),
        threshold=list(
          line=list(color="#ffffff", width=4),
          thickness=0.8,
          value=gt_clamped
        ),
        bar=list(color="rgba(0,0,0,0)")
      )
    ) %>% layout(
      margin=list(l=10,r=10,b=10,t=10),
      paper_bgcolor="rgba(0,0,0,0)",
      plot_bgcolor="rgba(0,0,0,0)",
      font=list(color="#e2e8f0")
    )
    
    fig
  })
  
  # OFFSET bar
  output$plot_offset <- renderPlot({
    d <- current(); mo <- d$Offset[1]
    validate(need(!is.na(mo), "Need offset"))
    
    ggplot(data.frame(mo=mo), aes(x="offset", y=mo)) +
      geom_col(fill="#16a34a", width=0.5) +
      geom_hline(yintercept=0, linetype="dashed", color="#e2e8f0") +
      geom_text(aes(label=round(mo,2)), hjust=-0.2, color="white", size=4) +
      coord_flip(ylim=c(min(mo,-1.5),0.1)) +
      labs(title="Maturity offset (yrs from PHV)", x=NULL, y="Years") +
      theme_minimal(base_size=14) +
      theme(
        panel.grid=element_blank(),
        panel.background=element_rect(fill="#1f2937",color=NA),
        plot.background=element_rect(fill="#1f2937",color=NA),
        axis.text=element_text(color="#e2e8f0"),
        axis.title=element_text(color="#e2e8f0"),
        plot.title=element_text(color="white",face="bold")
      )
  })
  
  # % Adult height zone
  output$plot_pah_zone <- renderPlot({
    p <- current()$Perc[1]; if (is.na(p)) p <- 0
    
    ggplot() +
      annotate("rect", xmin=85, xmax=95, ymin=0, ymax=1,
               fill="#22c55e", alpha=0.12) +
      annotate("rect", xmin=88, xmax=92, ymin=0, ymax=1,
               fill="#22c55e", alpha=0.35) +
      geom_vline(xintercept=p, color="white", linetype="dashed") +
      geom_point(aes(x=p, y=0.5), color="white", size=4) +
      annotate("text", x=p, y=0.82,
               label=paste0(round(p,1),"%"),
               color="white") +
      scale_x_continuous(limits=c(80,105)) +
      labs(title="% adult height & PHV zones", x="% adult height", y=NULL) +
      theme_minimal(base_size=14) +
      theme(
        panel.grid=element_blank(),
        panel.background=element_rect(fill="#1f2937",color=NA),
        plot.background=element_rect(fill="#1f2937",color=NA),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(color="#e2e8f0"),
        axis.title.x=element_text(color="#e2e8f0"),
        plot.title=element_text(color="white",face="bold")
      )
  })
  
  # GROWTH CURVE — PHV LINE + LEGEND RESTORED
  output$plot_growth <- renderPlot({
    df_all <- growth_df()
    validate(need(nrow(df_all) >= 1, "No data"))
    
    aphv <- suppressWarnings(as.numeric(current()$AgePHV[1]))
    show_phv <- !is.na(aphv)
    
    all_ages <- df_all$Age
    if (show_phv) all_ages <- c(all_ages, aphv)
    
    x_min <- max(4, min(all_ages) - 1)
    x_max <- min(18, max(all_ages) + 1)
    
    y_min <- floor((min(df_all$Height) - 5) / 10) * 10
    y_max <- ceiling((max(df_all$Height) + 5) / 10) * 10
    
    g <- ggplot() +
      geom_line(data = df_all, aes(x = Age, y = Height, color = "Current height"), size = 1) +
      geom_point(data = df_all, aes(x = Age, y = Height, color = "Current height"), size = 3)
    
    if (show_phv) {
      g <- g + geom_vline(aes(xintercept = aphv, linetype = "Age at PHV"),
                          color = "#22c55e", size = 1)
    }
    
    g +
      scale_color_manual(values = c("Current height" = "#38bdf8"), name = NULL) +
      scale_linetype_manual(values = c("Age at PHV" = "dotted"), name = NULL) +
      scale_x_continuous(limits = c(x_min, x_max)) +
      scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = 10)) +
      labs(title = "Growth Curve", x = "Chronological age (y)", y = "Height (cm)") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#1f2937",color=NA),
        plot.background  = element_rect(fill="#1f2937",color=NA),
        axis.text  = element_text(color="#e2e8f0"),
        axis.title = element_text(color="#e2e8f0"),
        plot.title = element_text(color="white",face="bold"),
        legend.position = c(0.92, 0.92),
        legend.justification = c(1,1),
        legend.background = element_rect(fill="#1f2937",color=NA),
        legend.text = element_text(color="#e2e8f0", size=10)
      )
  })
  
  # ============================================================
  # PDF REPORT EXPORT (same functioning code as you had)
  # ============================================================
  output$report <- downloadHandler(
    filename = function() {
      nm <- current()$Athlete
      if (is.null(nm) || nm == "") nm <- "athlete"
      paste0(gsub("\\s+","_", nm), "_maturity_report.pdf")
    },
    content = function(file) {
      # Capture reactive values now
      cur <- current()
      df_all <- growth_df()
      
      # temp image files
      growth_png <- tempfile(fileext = ".png")
      pah_png    <- tempfile(fileext = ".png")
      offset_png <- tempfile(fileext = ".png")
      
      # --- create growth plot png (same code as output$plot_growth) ---
      aphv <- suppressWarnings(as.numeric(cur$AgePHV[1]))
      show_phv <- !is.na(aphv)
      all_ages <- df_all$Age
      if (show_phv) all_ages <- c(all_ages, aphv)
      x_min <- max(4, min(all_ages) - 1)
      x_max <- min(18, max(all_ages) + 1)
      y_min <- floor((min(df_all$Height) - 5) / 10) * 10
      y_max <- ceiling((max(df_all$Height) + 5) / 10) * 10
      
      g <- ggplot() +
        geom_line(data = df_all, aes(x = Age, y = Height, color = "Current height"), size = 1) +
        geom_point(data = df_all, aes(x = Age, y = Height, color = "Current height"), size = 3)
      if (show_phv) {
        g <- g + geom_vline(xintercept = aphv, color = "#22c55e", linetype = "dotted", size = 1)
      }
      g <- g +
        scale_color_manual(values = c("Current height" = "#38bdf8"), name = NULL) +
        scale_x_continuous(limits = c(x_min, x_max)) +
        scale_y_continuous(limits = c(y_min, y_max), breaks = seq(y_min, y_max, by = 10)) +
        labs(title = "Growth Curve", x = "Chronological age (y)", y = "Height (cm)") +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold")
        )
      ggsave(growth_png, g, width = 7, height = 3.5, dpi = 150)
      
      # --- create %PAH png ---
      pval <- cur$Perc[1]; if (is.na(pval)) pval <- 0
      g2 <- ggplot() +
        annotate("rect", xmin=85, xmax=95, ymin=0, ymax=1, fill="#22c55e", alpha=0.12) +
        annotate("rect", xmin=88, xmax=92, ymin=0, ymax=1, fill="#22c55e", alpha=0.35) +
        geom_vline(xintercept=pval, color="black", linetype="dashed") +
        geom_point(aes(x=pval, y=0.5), color="black", size=4) +
        annotate("text", x=pval, y=0.82, label=paste0(round(pval,1),"%"), color="black") +
        scale_x_continuous(limits=c(80,105)) +
        labs(title="% Predicted adult height", x="% adult height", y=NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid=element_blank(), plot.title = element_text(face = "bold"))
      ggsave(pah_png, g2, width = 7, height = 2.2, dpi = 150)
      
      # --- create offset png ---
      mo <- cur$Offset[1]
      df_mo <- data.frame(mo = mo)
      g3 <- ggplot(df_mo, aes(x="offset", y=mo)) +
        geom_col(fill="#16a34a", width=0.5) +
        geom_hline(yintercept=0, linetype="dashed") +
        coord_flip(ylim=c(min(mo, -1.5), 0.1)) +
        labs(title="Maturity offset (yrs from PHV)", x=NULL, y="Years") +
        theme_minimal(base_size = 12) +
        theme(panel.grid=element_blank(), plot.title = element_text(face="bold"))
      ggsave(offset_png, g3, width = 5, height = 2.5, dpi = 150)
      
      # --- write temporary Rmd that embeds the images and text ---
      tempReport <- tempfile(fileext = ".Rmd")
      rmd_lines <- c(
        "---",
        "title: \"Maturity Report\"",
        "output: pdf_document",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```",
        "",
        "## Athlete information",
        "",
        paste0("**Name:** ", ifelse(is.null(cur$Athlete) || cur$Athlete=="", "Athlete", cur$Athlete)),
        "",
        paste0("**Chronological age:** ", round(cur$Age,2), " y"),
        "",
        paste0("**Biological age:** ", round(cur$BioAge,2), " y"),
        "",
        paste0("**Maturity offset (yrs from PHV):** ", round(cur$Offset,2)),
        "",
        paste0("**Age at PHV:** ", round(cur$AgePHV,2)),
        "",
        "",
        "## % Predicted adult height",
        "",
        paste0("`", round(cur$Perc,1), "%`"),
        "",
        "",
        "## Growth curve",
        "",
        paste0("![](", growth_png, "){width=100%}"),
        "",
        "## % Predicted adult height (zones)",
        "",
        paste0("![](", pah_png, "){width=100%}"),
        "",
        "## Maturity offset",
        "",
        paste0("![](", offset_png, "){width=60%}"),
        ""
      )
      writeLines(rmd_lines, tempReport)
      
      # render to pdf
      rmarkdown::render(tempReport, output_file = file, envir = new.env(parent = globalenv()))
    }
  )
  
  # ============================================================
  # DOWNLOAD EXCEL TEMPLATE (hard-coded path)
  # ============================================================
  output$template <- downloadHandler(
    filename = function() {
      basename(TEMPLATE_PATH)
    },
    content = function(file) {
      # copy the hard-coded original template into the download file path
      file.copy(TEMPLATE_PATH, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
}

# ============================================================
# LAUNCH APP
# ============================================================
shinyApp(ui, server)







