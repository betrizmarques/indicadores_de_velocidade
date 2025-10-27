library(shiny)
library(bs4Dash)
library(tidyverse)
library(plotly)
library(fresh)
library(markdown)

theme <- create_theme(
  bs4dash_color (
       yellow = "#f49725"
  ),
  bs4dash_status(
    primary = '#f49725',
    info = "#bdbdbd"
  )
)

base <- read_csv("base_referencia_radares.csv") %>% 
  mutate(cluster_porte = case_when(
    cluster_junto == "Clusters 1 e 3" & porte == "Menor porte" ~ "Menor Potencial de Mobilização - Menor porte",
    cluster_junto == "Clusters 1 e 3" & porte == "Médio porte" ~ "Menor Potencial de Mobilização - Médio porte",
    cluster_junto == "Clusters 1 e 3" & porte == "Maior porte" ~ "Menor Potencial de Mobilização - Maior porte",
    cluster_junto == "Cluster 2" & porte == "Menor porte" ~ "Maior Potencial de Mobilização - Menor porte",
    cluster_junto == "Cluster 2" & porte == "Médio porte" ~"Maior Potencial de Mobilização - Médio porte",
    cluster_junto == "Cluster 2" & porte == "Maior porte" ~ "Maior Potencial de Mobilização - Maior porte"
  ), 
  potencial = case_when(
    cluster_junto == "Clusters 1 e 3" ~ "Menor Potencial de Mobilização",
    cluster_junto == "Cluster 2" ~ "Maior Potencial de Mobilização"
  ),
  porte_com_numeros = case_when(
    porte == "Menor porte" ~ "Menor porte (<20 mil)",
    porte == "Médio porte" ~ "Médio porte (>20 mil e <100 mil)",
    porte == "Maior porte" ~ "Maior porte (>100 mil)"
  )) %>% filter(!is.na(valor_q3))

ui <- dashboardPage(
  
  
  freshTheme = theme,
  title = "ONSV - Indicador de velocidade ideal",

  fullscreen = T,
  dark = NULL,
  help = NULL,
  scrollToTop = T,

  header = dashboardHeader(
    title = dashboardBrand(
      title = "ONSV",
      image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s"
      
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Início",
        tabName = "inicio",
        icon = icon("home")
      ),
      menuItem(
        "Relatório",
        tabName = "introducao",
        icon = icon("info")
      ),
      menuItem(
        "Nível de fiscalização",
        tabName = "indicadores",
        icon = icon("bar-chart")
      )
    )
  ),
  footer = dashboardFooter(
    left = "ONSV",
    right = "2025"
  ),
  body = dashboardBody(
    tags$head(
      tags$link(
        rel = "icon",
        href = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s"
      )
    ),  
    tags$head(
      tags$style(HTML("
        .jumbotron {
          background-color: #c0c0c0 !important;
          color: #000000 !important;
        }
        .jumbotron .btn {
          background-color: #555555 !important;
          color: white !important;
          border: none !important;
        }
      "))
    ),
    tags$head(
      tags$style(HTML("
        .nav-pills .nav-link.active,
        .nav-pills .show>.nav-link {
          background-color: #007bff !important;
          color: white !important;  
        }
        
        .nav-sidebar .nav-item>.nav-link.active {
          background-color: #f49725 !important;
          color: white !important;
        }
      "))
    ),
    tabItems(
      
      tabItem(
        tabName = "inicio",
        
        jumbotron(
          title = "Qual o nível de fiscalização eletrônica de velocidade que um município deve ter?",
          lead = "Um estudo sobre o cenário da fiscalização eletrônica de velocidade nos municípios brasileiros",
          href = "https://www.onsv.org.br/estudos-pesquisas/estudos-e-pesquisas",
          btnName = "ONSV",
          "Confira outras pesquisas e estudos no link abaixo:",
          status = "info"
          
        ),
        
        fluidRow(
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Observatório Nacional de Segurança Viária",
              image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTiUGcruSnnUaYj84CofuRj9oRE1ZX7K-JpcQ&s",
              type = 1
            ),
            status = "primary",
            HTML("<br>"),
            "O Observatório Nacional de Segurança Viária é uma instituição social
            sem fins lucrativos, dedicada a desenvolver ações que contribuam efetivamente
            para a redução dos elevados índices de ocorrências no trânsito brasileiro. Com 
            esse objetivo, um grupo de profissionais multidisciplinares decidiu reunir todo 
            o seu conhecimento, experiência e motivação em um único projeto grandioso e desafiador: 
            mobilizar a sociedade em prol de um trânsito mais seguro."
          ),
          
          box(
            title = "Objetivo",
            width = 6,
            collapsible = TRUE,
            blockQuote(
              "Este estudo tem como objetivo estabelecer um nível de fiscalização
              eletrônica de velocidade ideal para os municípios brasileiros. ",
              color = "primary"
            )
          )
        )
      ),
      tabItem(
        tabName = "introducao",
        fluidRow(
          bs4Card(
            title = "Relatório",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            includeMarkdown("introducao.Rmd")
          )
        )
      ),
      tabItem(
        tabName = "indicadores",
        
        bs4Card(
          title = "Filtros de Seleção",
          solidHeader = T,
          width = 12,
          collapsible = FALSE,
          
          fluidRow(
            column(
              width = 4,
              
              selectInput(
                inputId = "filtro_estado",
                label = tags$strong("Selecione a UF:"),
                choices = c("Selecione..." = "", unique(sort(base$uf))),
                selected = ""
              )
            ),
            
            column(
              width = 4,
              
              selectInput(
                inputId = "filtro_municipio",
                label = tags$strong("Selecione o município:"),
                choices = c("Primeiro selecione uma UF" = ""),
                selected = ""
              )
            ),
            
            column(
              width = 4,
              br(),
              
              actionButton(
                inputId = "limpar_filtros",
                label = "Limpar Filtros",
                icon = icon('eraser'),
                class = "btn-warning",
                style = "margin-top: 5px;"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Nome do município",
              value = textOutput("municipio_text", inline = TRUE),
              icon = icon("city"),
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "UF",
              value = textOutput("uf_text", inline = T),
              icon = icon("map-marker-alt"),
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Frota de veículos",
              value = textOutput("frota_text", inline = TRUE),
              icon = icon("car")
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "População",
              value = textOutput("populacao_text", inline = T),
              icon = icon("users")
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Porte",
              value = textOutput("porte_text", inline = T),
              icon = icon("chart-bar")
            )
          ),
          column(
            width = 4,
            infoBox(
              width = 12,
              title = "Cluster",
              value = textOutput("cluster_text", inline = T),
              icon = icon("sitemap")
            )
          )
        ),
        fluidRow(
          box(
            title = strong("Fiscalização Eletrônica de Velocidade"),
            solidHeader = T,
            width = 4,
            status = "primary",
            height = "440px",
            uiOutput("fiscalizacao")
          ),
          box(
            title = strong("Situação do município em seu cluster"),
            solidHeader = T,
            status = "primary",
            width = 8,
            plotlyOutput("boxplot", height = "400px")
          )
        )
      )
    )
  )
)


server <- function(input, output, session){
  
  
  dados_selecionados <- reactive({
    if (input$filtro_estado != "" && input$filtro_municipio != ""){
      base %>% 
        filter(uf == input$filtro_estado, nome == input$filtro_municipio)
    } else{
      NULL
    }
  })
  
  dados_grupo <- reactive({
    dados <- dados_selecionados()
    if (!is.null(dados) && nrow(dados) > 0){
      base %>% 
        filter(cluster_porte == dados$cluster_porte, radares_10mil_veiculos > 0)
    } else {
      NULL
    }
  })
  
  
  
  observeEvent(input$filtro_estado,{
    if (input$filtro_estado != ""){
      municipios_filtrados <- base %>% 
        filter(uf == input$filtro_estado) %>% 
        mutate(nome = sort(nome)) %>% 
        pull(nome) %>% 
        unique()
      
      updateSelectInput(
        session = session,
        inputId = "filtro_municipio",
        label = "Selecione o Município:",
        choices = c("Selecione..." = "", municipios_filtrados),
        selected = ""
      )
    } else {
      updateSelectInput(
        session = session,
        inputId = "filtro_municipio",
        label = "Selecione o Município:",
        choices = c("Primeiro selecione um estado" = ""),
        selected = ""
      )
    }
  }
  )
  
  observeEvent(input$limpar_filtros,{
    updateSelectInput(
      session = session,
      inputId = "filtro_estado",
      selected = ""
      
    )
    
    updateSelectInput(
      session = session,
      inputId = "filtro_municipio",
      choices = c("Primeiro selecione um estado" = ""),
      selected = ""
    )
  })
  
  output$municipio_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      print(dados$nome)
    } else {
      "---"
    }
  })
  
  output$uf_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      print(dados$sigla)
    } else{
      "---"
    }
  })
  
  
  output$frota_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      print(format(dados$frota_23, big.mark = ".", decimal.mark = ","))
    } else{
      "---"
    }
  })
  
  
  output$populacao_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      format(dados$populacao_23, big.mark =".", decimal.mark = ",")
    } else{
      "---"
    }
  })
  
  output$porte_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      print(dados$porte_com_numeros)
    } else {
      "---"
    }
  })
    
  output$cluster_text <- renderText({
    dados <- dados_selecionados()
    if (!is.null(dados)){
      print(dados$potencial)
    } else {
      "---"
    }
  })
  
  output$fiscalizacao <- renderUI({
    dados <- dados_selecionados()
    dados_grupo <- dados_grupo() 
    
    
    if (is.null(dados) || is.null(dados_grupo)) {
      return(
        p("Selecione um município para ver os detalhes.", 
          style = "text-align: center; color: gray; padding-top: 20px;")
      )
    }
    
    
    valor_atual <- dados$radares_10mil_veiculos
    
    valor_ideal_q3 <- quantile(dados_grupo$radares_10mil_veiculos, na.rm = TRUE, probs = 0.75)
    
    cameras_atual <- dados$total_radares
    
    cameras_ideal <- round(dados$valor_abs)
  
    valor_atual_txt <- format(round(valor_atual, 2), big.mark = ".", decimal.mark = ",")
    valor_ideal_txt <- format(round(valor_ideal_q3, 2), big.mark = ".", decimal.mark = ",")
    cameras_atual_txt <- format(round(cameras_atual, 2), big.mark = ".", decimal.mark = ",")
    cameras_ideal_txt <- format(round(cameras_ideal, 2), big.mark = ".", decimal.mark = ",")
    
  
    if (valor_atual >= valor_ideal_q3) {
      comparacao_ui <- div(
        style = "color: #28a745; font-weight: bold; margin-top: 15px; text-align: center; font-size: 1.1em;", 
        icon("check-circle", style = "font-size: 1.5em;"),
        br(),
        "O município está no nível ideal ou acima."
      )
    } else {
      comparacao_ui <- div(
        style = "color: #dc3545; font-weight: bold; margin-top: 15px; text-align: center; font-size: 1.1em;", 
        icon("times-circle", style = "font-size: 1.5em;"),
        br(),
        "O município está abaixo do nível ideal."
      )
    }
    
    if (valor_atual >= valor_ideal_q3){
      div_cor <- div(
        style = "padding: 5px; margin-top: 15px; font-size: 1.2em; ",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          strong("Nível de Fiscalização Atual:"),
          span(
            valor_atual_txt, 
            style = "font-size: 1.4em; font-weight: bold; color: #28a745;"
          )
        ),
        
        
        div(
          style = "font-size: 0.5em; color: #6c757d; margin-top: 0px;", 
          "(câmeras de segurança/10 mil veículos)"
        )
      )
      div_cor_2 <- div(
        style = "padding: 5px; margin-top: 10px;font-size: 1.2em; display: flex; justify-content: space-between; align-items: center;",
        strong("Número Atual de Câmeras:"), 
        span(cameras_atual_txt, 
             style = "float: right; font-size: 1.4em; font-weight: bold; color: #28a745;")
      )} else {
        div_cor <- div(
          style = "padding: 5px; margin-top: 15px; font-size: 1.2em; ",
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            strong("Nível de Fiscalização Atual:"),
            span(
              valor_atual_txt, 
              style = "font-size: 1.4em; font-weight: bold; color: #dc3545;"
            )
          ),
          
          
          div(
            style = "font-size: 0.5em; color: #6c757d; margin-top: 0px;", 
            "(câmeras de segurança/10 mil veículos)"
          )
        )
        div_cor_2 <- div(
          style = "padding: 5px; margin-top: 10px;font-size: 1.2em; display: flex; justify-content: space-between; align-items: center;",
          strong("Número Atual de Câmeras:"), 
          span(cameras_atual_txt, 
               style = "float: right; font-size: 1.4em; font-weight: bold; color: #dc3545;")
        )
      }
    
    tagList(
      div(
        style = "text-align: center; border-bottom: 1px solid #ddd; padding-bottom: 10px; margin-bottom: 15px;",
        h4("Comparativo de Nível")
      ),
      
      
      
      div_cor,
      
      div_cor_2,
      
      div(
        style = "padding: 5px; margin-top: 15px; font-size: 1.2em; ",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          strong("Nível de Fiscalização Ideal:"),
          span(
            valor_ideal_txt, 
            style = "font-size: 1.4em; font-weight: bold; color: #007bff;"
          )
        ),
        
        
        div(
          style = "font-size: 0.5em; color: #6c757d; margin-top: 0px;", 
          "(câmeras de segurança/10 mil veículos)"
        )
      ),
      div(
        style = "padding: 5px; margin-top: 10px;font-size: 1.2em; display: flex; justify-content: space-between; align-items: center;",
        strong("Número Ideal de Câmeras:"), 
        span(cameras_ideal_txt, 
             style = "float: right; font-size: 1.4em; font-weight: bold; color: #007bff;")
      ),
      
      
      comparacao_ui
    )
  })
  
  output$boxplot <- renderPlotly({
    dados <- dados_selecionados()
    if (is.null(dados)){
      plot_ly() %>%
        layout(
          annotations = list(
            text = "Selecione um município para visualizar o boxplot do seu grupo",
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 15, color = "gray50")
          ),
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    } else{
      dados_grupo <- dados_grupo()
      p <- plot_ly(
        data = dados_grupo,
        x = ~cluster_porte,
        y = ~radares_10mil_veiculos,
        type = "box",
        name = "Distribuição do Grupo",
        hoveron = "points", 
        boxpoints = "all",
        jitter = 0.3,
        pointpos = -1.8,
        customdata = ~nome,
        hovertemplate = paste(
          "<b>Município:</b> %{customdata}<br>",
          "<b>Valor:</b> %{y:.2f}<br>",
          "<extra></extra>"
        ),
        marker = list(
          color = "rgba(70, 130, 180, 0.6)",
          size = 6
        ),
        fillcolor = "rgba(70, 130, 180, 0.4)",
        line = list(color = "rgba(70, 130, 180, 1)")
        
      )
      
      p <- p %>% 
        add_markers(
          data = dados,
          x = ~ cluster_porte,
          y = ~ radares_10mil_veiculos,
          name = "Município Selecionado",
          marker = list(
            color = "red",
            size = 12,
            symbol = "diamond",
            line = list(color = "darkred", width = 2)
          ),
          customdata = ~nome, 
          hovertemplate = paste(
            "<b>MUNICÍPIO SELECIONADO</b><br>",
            "<b>Nome:</b> %{customdata}<br>",
            "<b>Valor:</b> %{y:.2f}<br>",
            "<extra></extra>"
          )
        )
      
      valor_q3 <- quantile(dados_grupo$radares_10mil_veiculos, na.rm = TRUE, probs = 0.75)
      p <- p %>%
        layout(
          title = list(
            text = paste("Distribuição do Indicador -", dados$cluster_porte),
            x = 0.5,
            font = list(size = 16, color = "black")
          ),
          xaxis = list(
            title = "Grupo",
            titlefont = list(size = 12),
            tickfont = list(size = 10)
          ),
          yaxis = list(
            title = "Valor do Indicador (câmeras de segurança / 10 mil veículos)",
            titlefont = list(size = 10),
            tickfont = list(size = 8)
          ),
          showlegend = TRUE,
          legend = list(
            x = 0.76,
            y = 0.98,
            bgcolor = "rgba(255,255,255,0.8)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ),
          hovermode = "closest",
          shapes = list(
            list(
              type = "line",
              line = list(color = "red", dash = "dash", width = 2),
              x0 = 0,    
              x1 = 1,    
              xref = "paper", 
              y0 = valor_q3, 
              y1 = valor_q3, 
              yref = "y"     # 
            )
          ),
          annotations = list(
            list(
              text = "Nível de fiscalização eletrônica ideal",
              x = 0.95,   
              y = valor_q3, 
              xref = "paper",
              yref = "y",
              showarrow = FALSE,
              font = list(color = "red", size = 12),
              xanchor = "right", 
              yanchor = "bottom" 
            )
          )
        )
      p
      
    }
    
      
  })

}

shinyApp(ui, server)
