ui <- fluidPage(

    fluidRow(
      h1('Page title')
    ),
    fluidRow(
      column(width = 6,
             wellPanel(
               p('Hello world')
             )
             ),
      column(width = 6,
             wellPanel(
               p('Panel 2')
             )
             )
    ),
    tabsetPanel(
      tabPanel(title = 'Inputs',
               wellPanel(
              textInput(inputId = 'my_input',label = 'Input',width = '200px',placeholder = 'Enter text here')
               )),
      tabPanel(title = 'Outputs',
               wellPanel(
                 plotlyOutput(outputId = 'my_output')
               ))
    )

  )



mytheme <- bs_theme(version = 5,
                    bootswatch = 'sandstone',
                    bg = '#000',
                    fg = '#FFF')

  ui <- navbarPage(title = 'Patent analytics',
                   theme = mytheme,
                   tabPanel(title = 'Home',

                            wellPanel(
                              p('Some text here')
                            )
                   ),
                   tabPanel(title = 'Charts',
                   ),
                   tabPanel(title = 'tab2',
                   )
  )
