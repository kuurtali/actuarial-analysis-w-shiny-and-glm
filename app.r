if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shiny")) install.packages("shiny")
if (!require("bslib")) install.packages("bslib")
if (!require("DT")) install.packages("DT")
if (!require("caret")) install.packages("caret")
if (!require("pROC")) install.packages("pROC")
if (!require("plotly")) install.packages("plotly")
if (!require("pscl")) install.packages("pscl")

library(tidyverse)
library(shiny)
library(bslib)
library(DT)
library(caret)
library(pROC)
library(plotly)
library(pscl)

df <- read_csv("Car_Insurance_Claim.csv")

df_clean <- df %>%
  select(-ID) %>%
  mutate(
    CREDIT_SCORE = ifelse(is.na(CREDIT_SCORE), mean(CREDIT_SCORE, na.rm = TRUE), CREDIT_SCORE),
    ANNUAL_MILEAGE = ifelse(is.na(ANNUAL_MILEAGE), mean(ANNUAL_MILEAGE, na.rm = TRUE), ANNUAL_MILEAGE)
  ) %>%
  filter(
    !(AGE == "16-25" & DRIVING_EXPERIENCE %in% c("10-19y", "20-29y", "30y+")),
    !(AGE == "26-39" & DRIVING_EXPERIENCE %in% c("20-29y", "30y+")),
    !(AGE == "40-64" & DRIVING_EXPERIENCE == "30y+")
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(OUTCOME = as.factor(OUTCOME))

model_risk <- glm(OUTCOME ~ AGE + GENDER + DRIVING_EXPERIENCE + VEHICLE_TYPE + VEHICLE_YEAR + CREDIT_SCORE, 
                  data = df_clean, 
                  family = "binomial")

df_clean$Risk_Skoru <- predict(model_risk, type = "response") * 100

roc_obj <- roc(df_clean$OUTCOME, predict(model_risk, type = "response"))
auc_score <- round(auc(roc_obj), 3)
mcfadden <- round(pR2(model_risk)["McFadden"], 3)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cyborg"),
  
  titlePanel(div(HTML("<i class='fa fa-shield-alt'></i> Kasko Risk Yonetim Platformu"))),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Musteri Profili"),
      hr(),
      selectInput("AGE", "Surucu Yasi:", choices = levels(df_clean$AGE)),
      selectInput("GENDER", "Cinsiyet:", choices = levels(df_clean$GENDER)),
      selectInput("DRIVING_EXPERIENCE", "Surus Tecrubesi:", choices = levels(df_clean$DRIVING_EXPERIENCE)),
      selectInput("VEHICLE_TYPE", "Arac Tipi:", choices = levels(df_clean$VEHICLE_TYPE)),
      selectInput("VEHICLE_YEAR", "Arac Yili:", choices = levels(df_clean$VEHICLE_YEAR)),
      sliderInput("CREDIT_SCORE", "Kredi Skoru (0-1):", min = 0, max = 1, value = 0.5, step = 0.01),
      hr(),
      actionButton("hesapla", "RISKI HESAPLA", class = "btn-primary btn-lg", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(type = "pills",
        
        tabPanel("Risk Dashboard", 
                 br(),
                 fluidRow(
                   column(5, 
                          uiOutput("sonuc_kutusu"),
                          br(),
                          div(style = "background-color: #222; padding: 15px; border-radius: 10px;",
                              h4("Yonetici Ozeti:"),
                              p("Bu panel, anlik musteri verisine dayali olarak Hasar Olasiligini (Probability of Default) hesaplar."),
                              p("Arka planda calisan GLM algoritmasi, 10.000 policelik tarihsel veri ile egitilmistir.")
                          )
                   ),
                   column(7, 
                          h4("Risk Metre"),
                          plotlyOutput("risk_gauge", height = "250px"),
                          br(),
                          h4("Portfoy Icindeki Konumu"),
                          plotlyOutput("risk_dagilim", height = "250px")
                   )
                 )
        ),
        
        tabPanel("Veri Kesfi (EDA)", 
                 br(),
                 fluidRow(
                   column(4, 
                          wellPanel(
                            h4("Degisken Analizi"),
                            p("Hangi faktorun hasar uzerindeki etkisini gormek istiyorsunuz?"),
                            selectInput("eda_x", "Incelenecek Degisken:", 
                                        choices = names(df_clean)[!names(df_clean) %in% c("OUTCOME", "Risk_Skoru")],
                                        selected = "AGE")
                          )
                   ),
                   column(8, 
                          plotlyOutput("eda_plot")
                   )
                 )
        ),
        
        tabPanel("Istatistiksel Kanit", 
                 br(),
                 fluidRow(
                   column(6, 
                          h4("ROC Egrisi (Model Basarisi)"),
                          div(style = "background-color: #303030; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                              p(strong("AUC Skoru:"), auc_score),
                              p(style="font-size: 12px; color: #aaa;", 
                                "Bu egri, modelin 'Hasarli' ve 'Hasarsiz' musterileri ne kadar iyi ayirt ettigini gosterir. 0.8 uzerindeki bir AUC, modelin ayirt etme gucunun 'Mukemmel' seviyede oldugunu kanitlar.")
                          ),
                          plotOutput("roc_plot_static")
                   ),
                   column(6,
                          h4("Risk Faktorleri Etki Duzeyi"),
                          div(style = "background-color: #303030; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
                              p("Asagidaki grafik, hangi degiskenin model uzerinde en fazla agirliga sahip oldugunu gosterir.")
                          ),
                          plotOutput("var_imp_plot")
                   )
                 )
        ),

        tabPanel("Ham Veri", 
                 br(),
                 DTOutput("veri_tablosu")
        ),
        
        tabPanel("Model Detaylari", 
                 br(),
                 h3("Regresyon Analizi Ciktilari"),
                 verbatimTextOutput("model_ozeti"),
                 br(),
                 h3("Odds Ratios (Risk Carpanlari)"),
                 verbatimTextOutput("model_yorumlari")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$AGE, {
    yas_grubu <- input$AGE
    secenekler <- levels(df_clean$DRIVING_EXPERIENCE)
    valid_choices <- secenekler
    if(yas_grubu == "16-25") { valid_choices <- c("0-9y") } 
    else if(yas_grubu == "26-39") { valid_choices <- c("0-9y", "10-19y") } 
    else if(yas_grubu == "40-64") { valid_choices <- c("0-9y", "10-19y", "20-29y") }
    
    selected <- input$DRIVING_EXPERIENCE
    if(!(selected %in% valid_choices)) selected <- valid_choices[1]
    updateSelectInput(session, "DRIVING_EXPERIENCE", choices = valid_choices, selected = selected)
  })

  risk_hesapla <- eventReactive(list(input$hesapla, input$AGE), {
    yeni_musteri <- data.frame(
      AGE = input$AGE,
      GENDER = input$GENDER,
      DRIVING_EXPERIENCE = input$DRIVING_EXPERIENCE,
      VEHICLE_TYPE = input$VEHICLE_TYPE,
      VEHICLE_YEAR = input$VEHICLE_YEAR,
      CREDIT_SCORE = input$CREDIT_SCORE
    )
    predict(model_risk, newdata = yeni_musteri, type = "response")
  }, ignoreInit = FALSE)

  output$sonuc_kutusu <- renderUI({
    olasilik <- risk_hesapla()
    yuzde <- round(olasilik * 100, 1)
    renk <- if(yuzde > 50) "#d9534f" else "#5cb85c"
    baslik <- if(yuzde > 50) "YUKSEK RISK!" else "DUSUK RISK"
    
    div(style = paste0("text-align: center; background-color: ", renk, "; padding: 30px; border-radius: 15px; color: white; box-shadow: 0 4px 8px rgba(0,0,0,0.5);"),
        h4("TAHMIN EDILEN KAZA IHTIMALI", style = "opacity: 0.9; margin-bottom: 20px;"),
        h1(paste0("%", yuzde), style = "font-size: 70px; font-weight: bold; margin: 0;"),
        hr(style = "border-top: 2px solid white; opacity: 0.5;"),
        h2(baslik, style = "font-weight: bold;")
    )
  })
  
  output$risk_gauge <- renderPlotly({
    olasilik <- risk_hesapla()
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = olasilik * 100,
      title = list(text = "Risk Metre"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(NULL, 100)),
        bar = list(color = "black"),
        steps = list(
          list(range = c(0, 50), color = "#5cb85c"),
          list(range = c(50, 100), color = "#d9534f")
        ),
        threshold = list(
          line = list(color = "white", width = 4),
          thickness = 0.75,
          value = olasilik * 100
        )
      )
    ) %>% layout(margin = list(l=20,r=20,t=30,b=20), paper_bgcolor = "rgba(0,0,0,0)", font = list(color = "white"))
  })

  output$risk_dagilim <- renderPlotly({
    kullanici_riski <- risk_hesapla() * 100
    
    p <- ggplot(df_clean, aes(x = Risk_Skoru)) +
      geom_histogram(fill = "#4b4b4b", color = "white", bins = 30, alpha = 0.7) +
      geom_vline(xintercept = kullanici_riski, color = "red", size = 1.5, linetype = "dashed") +
      annotate("text", x = kullanici_riski, y = 100, label = "Sen Buradasin", color = "red", vjust = -1) +
      labs(x = "Risk Skoru (%)", y = "Kisi Sayisi", title = "Tum Musteriler Icindeki Konumunuz") +
      theme_minimal() +
      theme(
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid = element_line(color = "#444")
      )
    
    ggplotly(p)
  })

  output$eda_plot <- renderPlotly({
    req(input$eda_x)
    p <- ggplot(df_clean, aes(x = .data[[input$eda_x]], fill = OUTCOME)) +
      geom_bar(position = "fill") +
      labs(title = paste(input$eda_x, "Degiskenine Gore Hasar Oranlari"), 
           y = "Oran", x = input$eda_x, fill = "Hasar Durumu") +
      scale_fill_manual(values = c("#5cb85c", "#d9534f"), labels = c("Yok", "Var")) +
      theme_minimal() +
      theme(text = element_text(color = "white"), axis.text = element_text(color = "white"))
    
    ggplotly(p)
  })

  output$model_ozeti <- renderPrint({ summary(model_risk) })
  
  output$model_yorumlari <- renderPrint({
    katsayilar <- exp(coef(model_risk))
    print(round(katsayilar, 2))
  })
  
  output$var_imp_plot <- renderPlot({
    varImp_obj <- varImp(model_risk)
    ggplot(varImp_obj, aes(x = reorder(rownames(varImp_obj), Overall), y = Overall)) +
      geom_bar(stat = "identity", fill = "#007bff") +
      coord_flip() +
      labs(x = "Degiskenler", y = "Etki Gucu") +
      theme_minimal() +
      theme(text = element_text(color = "white"), axis.text = element_text(color = "white"))
  }, bg = "transparent")
  
  output$roc_plot_static <- renderPlot({
    plot(roc_obj, main = "ROC Egrisi", col = "#00C851", lwd = 3, col.main="white", col.axis="white", col.lab="white")
  }, bg = "transparent")

  output$veri_tablosu <- renderDT({
    datatable(head(df_clean, 100), options = list(pageLength = 10, scrollX = TRUE), style = "bootstrap4")
  })
}

shinyApp(ui = ui, server = server)