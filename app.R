library(shiny)

# --------------------------
# UI
# --------------------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .btn {
        background-color: lightpink !important;
        border-color: lightpink !important;
        color: black !important;
      }
      .btn:hover, .btn:focus, .btn:active {
        background-color: lightpink !important;
        border-color: lightpink !important;
        color: black !important;
        box-shadow: none !important;
      }
    "))
  ),
  
  titlePanel("Taller: Acceso a la Justicia en Ciencias Sociales"),
  
  tabsetPanel(
    
    # =====================================================
    # PANEL 1 — ACTIVIDADES
    # =====================================================
    
    tabPanel("Actividades",
             
             h3("Taller Interactivo"), br(),
             
             h4("Actividad 1: Quiz de comprensión"),
             radioButtons("quiz1",
                          "¿Cuál puede ser una unidad de análisis?",
                          choices = c("Un tribunal",
                                      "Una encuesta de hogares",
                                      "Un barrio periférico",
                                      "Todas las anteriores"),
                          selected = character(0)),
             actionButton("submit_quiz1", "Responder"),
             textOutput("quiz1_feedback"), br(), br(),
             
             h4("Actividad 2: Relaciona problema y unidad"),
             selectInput("assoc_problem", "Problema:",
                         choices = c("Brecha digital",
                                     "Desigualdad educativa",
                                     "Participación comunitaria"),
                         selected = NULL),
             selectInput("assoc_unit", "Unidad:",
                         choices = c("Estudiantes",
                                     "Organizaciones barriales",
                                     "Encuestas de hogares"),
                         selected = NULL),
             actionButton("submit_assoc", "Relacionar"),
             textOutput("assoc_feedback"), br(), br(),
             
             h4("Actividad 3: Verdadero/Falso"),
             radioButtons("vf1",
                          "La población es siempre igual a la muestra.",
                          choices = c("Verdadero", "Falso"),
                          selected = character(0)),
             actionButton("submit_vf1", "Responder"),
             textOutput("vf1_feedback"), br(), br(),
             
             h4("Actividad 4: Casos prácticos"),
             selectInput("case", "Caso:",
                         choices = c("Acceso diferencial a servicios comunitarios",
                                     "Participación juvenil en política local",
                                     "Brecha salarial de género"),
                         selected = NULL),
             selectInput("unit_case", "Unidad de análisis:",
                         choices = c("Jóvenes",
                                     "Trabajadores/as",
                                     "Usuarios de servicios comunitarios"),
                         selected = NULL),
             actionButton("submit_case", "Responder"),
             textOutput("case_feedback"), br(), br(),
             
             h4("Actividad 5: Reflexión"),
             textAreaInput("reflection",
                           "Escribe tu reflexión:",
                           width = "100%", height = "120px"),
             downloadButton("download_reflection", "Descargar reflexión"), br(), br(),
             
             h4("Actividad 6: Autoevaluación"),
             textAreaInput("self_eval",
                           "Escribe tu autoevaluación:",
                           width = "100%", height = "120px"),
             downloadButton("download_self_eval", "Descargar autoevaluación")
    ),
    
    
    # =====================================================
    # PANEL 2 — UNIDAD Y VARIABLES
    # =====================================================
    
    tabPanel("Unidad de análisis y variables",
             
             h3("Del marco conceptual al marco operativo"), br(),
             
             p("La unidad de análisis responde a: ¿de qué unidades habla la hipótesis?"),
             p("Las variables responden a: ¿qué características se observan en esas unidades?"),
             
             radioButtons("ua1",
                          "Si analizamos la confianza institucional de jóvenes urbanos, la unidad es:",
                          choices = c("Confianza institucional",
                                      "Jóvenes urbanos",
                                      "Instituciones públicas"),
                          selected = character(0)),
             actionButton("submit_ua1", "Responder"),
             textOutput("ua1_feedback"), br(), br(),
             
             radioButtons("ua2",
                          "En un estudio sobre hogares, ¿cuál es una variable?",
                          choices = c("Hogares",
                                      "Nivel de ingresos",
                                      "Barrio"),
                          selected = character(0)),
             actionButton("submit_ua2", "Responder"),
             textOutput("ua2_feedback")
    ),
    
    
    # =====================================================
    # PANEL 3 — DISEÑO DE ESTUDIO
    # =====================================================
    
    tabPanel("Diseño de estudio",
             
             h3("Unidad de análisis, población y muestra"), br(),
             
             radioButtons("pob1",
                          "La población es:",
                          choices = c("Todos los habitantes de Montevideo",
                                      "50 entrevistados",
                                      "La hipótesis"),
                          selected = character(0)),
             actionButton("submit_pob1", "Responder"),
             textOutput("pob1_feedback"), br(), br(),
             
             radioButtons("muestra1",
                          "La muestra es:",
                          choices = c("Todos los habitantes",
                                      "50 personas seleccionadas",
                                      "El concepto teórico"),
                          selected = character(0)),
             actionButton("submit_muestra1", "Responder"),
             textOutput("muestra1_feedback")
    ),
    
    
    # =====================================================
    # PANEL 4 — EVALUACIÓN INTEGRAL
    # =====================================================
    
    tabPanel("Evaluación integral",
             
             h3("Evaluación final"), br(),
             
             radioButtons("eval1",
                          "La unidad de análisis determina:",
                          choices = c("El enfoque metodológico",
                                      "Las variables del estudio",
                                      "La razón  del estudio"),
                          selected = character(0)),
             
             radioButtons("eval2",
                          "Si estudio organizaciones, la unidad es:",
                          choices = c("Individuos",
                                      "Organizaciones",
                                      "Conceptos"),
                          selected = character(0)),
             
             radioButtons("eval3",
                          "El acceso a la justicia puede analizarse como fenómeno:",
                          choices = c("Individual",
                                      "Estructural",
                                      "Multinivel"),
                          selected = character(0)),
             
             actionButton("submit_eval_total", "Calcular resultado"),
             textOutput("eval_total_feedback")
    )
  )
)

# --------------------------
# SERVER
# --------------------------
server <- function(input, output, session) {
  
  # PANEL 1
  observeEvent(input$submit_quiz1, {
    output$quiz1_feedback <- renderText(
      if (input$quiz1 == "Todas las anteriores") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  observeEvent(input$submit_assoc, {
    if (input$assoc_problem == "Desigualdad educativa" && input$assoc_unit == "Estudiantes") {
      output$assoc_feedback <- renderText("✅ Correcto.")
    } else if (input$assoc_problem == "Participación comunitaria" && input$assoc_unit == "Organizaciones barriales") {
      output$assoc_feedback <- renderText("✅ Correcto.")
    } else if (input$assoc_problem == "Brecha digital" && input$assoc_unit == "Encuestas de hogares") {
      output$assoc_feedback <- renderText("✅ Correcto.")
    } else {
      output$assoc_feedback <- renderText("❌ Incorrecto.")
    }
  })
  
  observeEvent(input$submit_vf1, {
    output$vf1_feedback <- renderText(
      if (input$vf1 == "Falso") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  observeEvent(input$submit_case, {
    if (input$case == "Acceso diferencial a servicios comunitarios" &&
        input$unit_case == "Usuarios de servicios comunitarios") {
      output$case_feedback <- renderText("✅ Correcto.")
    } else if (input$case == "Participación juvenil en política local" &&
               input$unit_case == "Jóvenes") {
      output$case_feedback <- renderText("✅ Correcto.")
    } else if (input$case == "Brecha salarial de género" &&
               input$unit_case == "Trabajadores/as") {
      output$case_feedback <- renderText("✅ Correcto.")
    } else {
      output$case_feedback <- renderText("❌ Incorrecto.")
    }
  })
  
  output$download_reflection <- downloadHandler(
    filename = function() "reflexion.txt",
    content = function(file) writeLines(input$reflection, file)
  )
  
  output$download_self_eval <- downloadHandler(
    filename = function() "autoevaluacion.txt",
    content = function(file) writeLines(input$self_eval, file)
  )
  
  # PANEL 2
  observeEvent(input$submit_ua1, {
    output$ua1_feedback <- renderText(
      if (input$ua1 == "Jóvenes urbanos") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  observeEvent(input$submit_ua2, {
    output$ua2_feedback <- renderText(
      if (input$ua2 == "Nivel de ingresos") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  # PANEL 3
  observeEvent(input$submit_pob1, {
    output$pob1_feedback <- renderText(
      if (input$pob1 == "Todos los habitantes de Montevideo") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  observeEvent(input$submit_muestra1, {
    output$muestra1_feedback <- renderText(
      if (input$muestra1 == "50 personas seleccionadas") "✅ Correcto." else "❌ Incorrecto."
    )
  })
  
  # PANEL 4
  observeEvent(input$submit_eval_total, {
    puntaje <- 0
    if (input$eval1 == "El enfoque metodológico") puntaje <- puntaje + 1
    if (input$eval2 == "Organizaciones") puntaje <- puntaje + 1
    if (input$eval3 == "Multinivel") puntaje <- puntaje + 1
    
    output$eval_total_feedback <- renderText(
      paste("Resultado final:", puntaje, "/ 3")
    )
  })
}

shinyApp(ui = ui, server = server)
