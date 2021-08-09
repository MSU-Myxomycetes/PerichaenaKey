library(shiny)

perichaena_table <- read.csv("perichaena_tbl.csv")

inputMyx <- function(vector, input, y = 2, facultative = FALSE) {
  l_vector <- vector %in% input
  n_vector <- sum(l_vector) & !l_vector
  if (facultative) {
    n_vector <- n_vector - !sum(l_vector)
  }
  n_vector <- y * n_vector
  names(n_vector) <- vector
  return(n_vector)
}

ui <- function(req) {
  navbarPage("Perichaena",
  tabPanel("Ключ до видов",
    fluidPage(

      column(3,
        h3("Спорангий"),
        br(),
        checkboxGroupInput("checkSporophoreType",
          label = "Тип спорофора", 
          choices = c(
            "Сидячие" = "sessile", 
            "На ножке" = "stalked",
            "Плазмодиокарпы" = "plasmodiocarps")),
        br(),
        checkboxGroupInput("checkSporangiumShape", 
          label = "Форма спорангия",
          choices = c(
            "Сильно уплощенное" = "flat", 
            "Подушковидное" = "pillow",
            "Округлое на зауженном основании" = "spherical",
            "С углублением в нижней части" = "deep_bottom")),
        br(),
        sliderInput("rangeSporangiumDiameter",
          label = "Диаметр спорангия (мм)",
          min = 0,
          max = 1.5,
          value = c(0, 1.5),
          step= 0.05),
        hr(),
        h3("Ножка"),
        br(),
        sliderInput("rangeStalk",
          label = "Длина ножки (мм)",
          min = 0,
          max = 0.7,
          value = c(0, 0.7),
          step= 0.05),
        br(),
        checkboxGroupInput("checkStalkSporangiaRatio",
          label = "Соотношение длины ножки и диаметра спорангия",
          choices = c(
            "Длина ножки больше диаметра спорангия" = "more",
            "Длина ножки меньше диаметра спорангия" = "less")),
        br(),
        checkboxGroupInput("checkStalkColour",
          label = "Цвет ножки и наличие в ней извести",
          choices = c(
            "Темная, извести нет" = "dark", 
            "Светлая, покрыта известью" = "light"))
      ),

      column(3,
        h3("Перидий"),
        br(),
        checkboxGroupInput("checkPeridiumLayers",
          label = "Перидий",
          choices = c(
            "Однослойный" = "single",
            "Двуслойный" = "double")),
        br(),
        checkboxGroupInput("checkPeridiumThickness",
          label = "Толщина наружнего слоя перидия",
          choices = c(
            "Тонкий, мембраноподобный" = "thick",
            "Толстый, плотный, хрящеватый" = "thin")),
        br(),
        checkboxGroupInput("checkPeridiumSurface",
          label = "Гранулярный материал или известь на поверхности перидия",
          choices = c(
            "Присутствует в заметных количествах" = "granular",
            "Отсутствует в заметных количествах" = "absent")),
        br(),
        checkboxGroupInput("checkPeridiumDehiscence", 
          label = "Тип растрескивания",
          choices = c(
            "По кругу с образованием крышечки" = "circle",
            "На отдельные пластинки по заранее сформированным линиям" = "plates",
            "Неправильно" = "irregular")),
        hr(),
        h3("Гипоталлус"),
        br(),
        checkboxGroupInput("checkHypothallus",
          label = "Выраженность гипоталлуса",
          choices = c(
            "Незаметный, слабо развитый" = "inconspicuous",
            "Хорошо развитый, распростертый" = "conspicuous")),
        br(),
        checkboxGroupInput("checkHypothallusColour",
          label = "Цвет гипоталлуса",
          choices = c(
            "Прозрачный" = "transparent",
            "Светлоокрашенный" = "light",
            "Тёмный" = "dark"))
      ),

      column(3,
        h3("Капиллиций"),
        br(),
        checkboxGroupInput("checkCapillitum",
          label = "Наличие капиллиция",
          choices = c(
            "Хорошо развит" = "abundant",
            "Представлен немногочисленными, часто короткими, слабо ветвящимися нитями" = "scanty",
            "Отсутствует" = "absent")),
        br(),
        sliderInput("rangeCapillitumDiameter",
          label = "Диаметр нитей капиллиция",
          min = 0,
          max = 8,
          value = c(0, 8),
          step= 0.5),
        br(),
        checkboxGroupInput("checkCapillitumOrnamentation",
          label = "Орнаментация нитей капиллиция",
          choices = c(
            "Мелкие шипики или бородавочки" = "warted",
            "Крупные шипики (> 1 мкм дл.)" = "spiny",
            "Мелкая сеточка" = "net",
            "Полукольцевые, коралловидные или неправильные утолщения" = "irregular")),
        hr(),
        h3("Споры"),
        br(),
        sliderInput("rangeSporeDiameter",
          label = "Диаметр спор (мкм)",
          min = 7,
          max = 25,
          value = c(7, 25),
          step= 0.5),
        br(),
        checkboxGroupInput("checkSporeAggregation",
          label = "Агрегированность спор",
          choices = c(
            "Свободные" = "free",
            "Собранные в группы" = "clusters")),
        br(),
        checkboxGroupInput("checkSporeShape",
          label = "Форма спор",
          choices = c(
            "Округлые" = "spherical",
            "Многогранные" = "polygonal")),
        br(),
          checkboxGroupInput("checkSporeSurface",
            label = "Орнаментация поверхности спор",
            choices = c(
              "Мелкобородавчатая" = "small_warted",
              "Крупнобородавчатая" = "large_warted",
              "Мелкобородавчатая с несколькими выделяющимися более крупными бородавками" = "small_warted_with_large",
              "Шиповатая" = "spiny",
              "Сетчатая" = "reticulate"))
      ),

      column(3,
        h3("Список подходящих видов"),
        htmlOutput("value"))

    ))
  )
}

server <- function(input, output, session) {

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  output$value <- renderText({
    vSporophoreType <- inputMyx(c("sessile", "stalked", "plasmodiocarps"), input$checkSporophoreType)
    vSporangiumShape <- inputMyx(c("flat", "pillow", "spherical", "deep_bottom"), input$checkSporangiumShape)
    vStalkSporangiaRatio <- inputMyx(c("more", "less"), input$checkStalkSporangiaRatio, facultative = T)
    vStalkColour <- inputMyx(c("dark", "light"), input$checkStalkColour, facultative = T)
    vPeridiumLayers <- inputMyx(c("single", "double"), input$checkPeridiumLayers)
    vPeridiumThickness <- inputMyx(c("thick", "thin"), input$checkPeridiumThickness)
    vPeridiumSurface <- inputMyx(c("granular", "absent"), input$checkPeridiumSurface, facultative = T)
    vPeridiumDehiscence <- inputMyx(c("circle", "plates", "irregular"), input$checkPeridiumDehiscence)
    vHypothallus <- inputMyx(c("inconspicuous", "conspicuous"), input$checkHypothallus)
    vHypothallusColour <- inputMyx(c("transparent", "light", "dark"), input$checkHypothallusColour, facultative = T)
    vCapillitum <- inputMyx(c("abundant", "scanty", "absent"), input$checkCapillitum)
    vCapillitumOrnamentation <- inputMyx(c("warted", "spiny", "net", "irregular"), input$checkCapillitumOrnamentation, facultative = T)
    vSporeShape <- inputMyx(c("spherical", "polygonal"), input$checkSporeShape)
    vSporeAggregation <- inputMyx(c("free", "clusters"), input$checkSporeAggregation)
    vSporeSurface <- inputMyx(c("small_warted", "large_warted", "small_warted_with_large", "spiny", "reticulate"), input$checkSporeSurface)

    filtered_perichaena_table <- do.call(
      rbind,
      lapply(
        1:nrow(perichaena_table),
        function(i, df) {
          perichaena_table <- df[i, ]
          if (
            all(
              any(
                perichaena_table$ts_ses > vSporophoreType["sessile"],
                perichaena_table$ts_st > vSporophoreType["stalked"],
                perichaena_table$ts_pl > vSporophoreType["plasmodiocarps"]
              ),
              any(
                perichaena_table$sh_flat > vSporangiumShape["flat"],
                perichaena_table$sh_pil > vSporangiumShape["pillow"],
                perichaena_table$sh_sph > vSporangiumShape["spherical"],
                perichaena_table$sh_dp > vSporangiumShape["deep_bottom"]
              ),
              all(
                perichaena_table$ds_lmin <= input$rangeSporangiumDiameter[2],
                perichaena_table$ds_lmax >= input$rangeSporangiumDiameter[1]
              ),
              all(
                perichaena_table$ls_min <= input$rangeStalk[2],
                perichaena_table$ls_max >= input$rangeStalk[1]
              ),
              any(
                perichaena_table$ls_more > vStalkSporangiaRatio["more"],
                perichaena_table$ls_less > vStalkSporangiaRatio["less"]
              ),
              any(
                perichaena_table$cs_d > vStalkColour["dark"],
                perichaena_table$cs_l > vStalkColour["light"]
              ),
              any(
                perichaena_table$hy_in > vHypothallus["inconspicuous"],
                perichaena_table$hy_co > vHypothallus["conspicuous"]
              ),
              any(
                perichaena_table$hc_t > vHypothallusColour["transparent"],
                perichaena_table$hc_l > vHypothallusColour["light"],
                perichaena_table$hc_d > vHypothallusColour["dark"]
              ),
              any(
                perichaena_table$p_1 > vPeridiumLayers["single"],
                perichaena_table$p_2 > vPeridiumLayers["double"]
              ),
              any(
                perichaena_table$pt_thn > vPeridiumThickness["thick"],
                perichaena_table$pt_tck > vPeridiumThickness["thin"]
              ),
              any(
                perichaena_table$ps_gr > vPeridiumSurface["granular"],
                perichaena_table$ps_ab > vPeridiumSurface["absent"]
              ),
              any(
                perichaena_table$pd_cir > vPeridiumDehiscence["circle"],
                perichaena_table$pd_pl > vPeridiumDehiscence["plates"],
                perichaena_table$pd_ir > vPeridiumDehiscence["irregular"]
              ),
              all(
                perichaena_table$sd_lmin <= input$rangeSporeDiameter[2],
                perichaena_table$sd_lmax >= input$rangeSporeDiameter[1]
              ),
              any(
                perichaena_table$ss_cir > vSporeShape["spherical"],
                perichaena_table$ss_pol > vSporeShape["polygonal"]
              ),
              any(
                perichaena_table$sa_fr > vSporeAggregation["free"],
                perichaena_table$sa_cl > vSporeAggregation["clusters"]
              ),
              any(
                perichaena_table$so_smw > vSporeSurface["small_warted"],
                perichaena_table$so_lw > vSporeSurface["large_warted"],
                perichaena_table$so_slw > vSporeSurface["small_warted_with_large"],
                perichaena_table$so_sp > vSporeSurface["spiny"],
                perichaena_table$so_ret > vSporeSurface["reticulate"]
              ),
              any(
                perichaena_table$ca_w > vCapillitum["abundant"],
                perichaena_table$ca_sc > vCapillitum["scanty"],
                perichaena_table$ca_ab > vCapillitum["absent"]
              ),
              all(
                perichaena_table$cd_lmin <= input$rangeCapillitumDiameter[2],
                perichaena_table$cd_lmax >= input$rangeCapillitumDiameter[1]
              ),
              any(
                perichaena_table$co_sw > vCapillitumOrnamentation["warted"],
                perichaena_table$co_ls > vCapillitumOrnamentation["spiny"],
                perichaena_table$co_ret > vCapillitumOrnamentation["net"],
                perichaena_table$co_ir > vCapillitumOrnamentation["irregular"]
              )
            )
          ) {
            df[i, ]
          } else {
            NULL
          }
        },
        perichaena_table
      )
    )

    paste0("<i>", filtered_perichaena_table$sp, "</i>", collapse = "<br>")

  })
}

enableBookmarking("url")

shinyApp(ui, server)
