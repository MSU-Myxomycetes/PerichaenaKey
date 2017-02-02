#ui.R

library(shiny)
library(shinythemes)
library(shinyjs)

shinyUI(navbarPage("Perichaena",
  theme = shinytheme("cerulean"),
  tabPanel("Ключ до видов",
    fluidPage(

      column(3,
        shinyjs::useShinyjs(),
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
          label = "Аггрегированность спор",
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
              "Мелкобородавчатые" = "small_warted",
              "Крупнобородавчатые" = "large_warted",
              "Мелкобородавчатые с несколькими выделяющимися более крупными бородавками" = "small_warted_with_large",
              "Шиповатые" = "spiny",
              "Сетчатые" = "reticulate"))
      ),

      column(3,
        h3("Список подходящих видов"),
        htmlOutput("value"))

  ))
))
