#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "What's behind Netflix ?"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Home Page",tabName = "home_page",icon = icon("igloo")),
      menuItem("Netflix Analysis",tabName = "netflixAnalysis",icon = icon("drafting-compass"),
        menuItem("Generality", tabName = "generality", icon = icon("font")),     
        menuItem("Netflix and countries",tabName = "mapping",icon = icon("globe-europe"),
            menuSubItem("Plotting",tabName = "plotting_countries",icon=icon("map-signs")),
            menuSubItem("Static map",tabName = "static_map",icon=icon("map")),
            menuSubItem("Interactive map",tabName = "interactive_map",icon = icon("map-marked-alt")),
            menuItem("Top", tabName = "top", icon = icon("list-ol"))
        ),
        menuItem("Duration analysis", tabName = "plotting_duration",icon=icon("hourglass")),
        menuItem("Data", tabName = "data",icon=icon("table"))
      ),
      
      menuItem( "Netflix vs Others",tabName = "netflixvs",icon = icon("chess-knight"),
        menuItem("Market Share",tabName = "market_share",icon = icon("shopping-cart")),
        menuItem("Age of content", tabName = "age_of_content",icon = icon("chart-line")),
        menuItem("Rating analysis",tabName = "rating_analysis",icon = icon("diagnoses"),
            menuSubItem("IMDb analysis",tabName = "IMDb_analysis",icon = icon("chart-bar")),
            menuSubItem("Rotten Tomatoes analysis",tabName = "RT_analysis",icon = icon("chart-bar")),
            menuSubItem("IMDb/RT linear model",tabName = "imdb_RT_lm",icon = icon("chart-line")),
            menuSubItem("Rating/Year linear model",tabName = "rating_year_lm",icon = icon("chart-line"))
            ),
        menuItem("Top 20",tabName = "top20",icon = icon("award"))
        ),
      
      menuItem("Search engine",tabName = "search",icon = icon("search"))
    )),
    
    dashboardBody(
        tags$head(tags$style(
        HTML('
            .main-header .logo {
                font-family: "Chalkduster", Comic Sans MS, Times, "Times New Roman", serif;
                font-weight: bold;
                font-size: 15px;
                <!--position: fixed;-->
            }
      
            /* body */
            .content-wrapper, .right-side {
                background-color: black;
            }
            
            .sidebar {
                position: fixed;
                width: 220px;

            }
            
            h3 { 
              color: white;
              font-family: "Chalkduster", Comic Sans MS, Times, "Times New Roman", serif; 
            }
            
            h4 { 
              color: grey;
              font-family: "Chalkduster", Comic Sans MS, Times, "Times New Roman", serif; 
              font-style: italic;
            }
            
            a {
              color: red;
              font-weight: bold;
            }
            
            
      '))),
      

        tabItems(
            tabItem(tabName="home_page",
                    fluidRow(
                      box(width=12,
                          status = "danger",
                          background = "black",
                          solidHeader = TRUE,
                      h3("This application was designed as part of our data visualisation course.", br()),
                      h3("If you want to learn more about Netflix you should check the video bellow : ", br())),

                          HTML('<p style="text-align:center;">
                               <iframe width="560" height="315" align="middle" src="https://www.youtube.com/embed/BrpEHssa_gQ" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
                               </p>
                              ')
                     ,
                      box(width=12,
                          status = "danger",
                          background = "black",
                          solidHeader = TRUE,
                          HTML('
                          <h3>We have written a notice that present our application, you can <a href="README.pdf" target="_blank">click here</a> if you want !</h3>
                          <h4>The pdf is in French, English version coming soon.</h4>
                          ')
                      )
                       )),
            
            tabItem(tabName = "static_map",
                    fluidRow(
                        box(
                            width = 12,
                            title = "Inputs",
                            status = "danger",
                            solidHeader = TRUE,
                            background = "black",
                            selectInput(
                                "genre_choice_c1_c2",
                                "Select a genre ?",
                                liste_genre,
                                selected = "Dramas"
                            ),
                            radioButtons(
                                "type_choice_c1_c2",
                                "Select a type ?",
                                c("TV Show", "Movie"),
                                selected = "Movie"
                            ),
                            radioButtons(
                                inputId = "scale_c1",
                                label = "Choose a scale for the map bellow :",
                                inline = TRUE,
                                choices = c("normal", "sqrt"),
                                selected = "sqrt"
                            ),
                            actionButton("go_staticmap", "Update !"),
                            
                            downloadButton("downloadData", "Télécharger la carte !"),
                        ),
                        box(width = 12,
                            title = textOutput("title_c1_c2"),
                            status = "danger",
                            solidHeader = TRUE,
                            #background = "white",
                            plotOutput("c1_c2")),
                    )),
            
            tabItem(tabName = "interactive_map",
                    fluidRow(
                        box(
                            width = 12,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            selectInput("genre_choice_c3", "Select the genre ?", liste_genre, selected = liste_genre[2]),
                            radioButtons(
                                "type_choice_c3",
                                "Select a type ?",
                                c("TV Show", "Movie"),
                                selected = "Movie"
                            ),
                            actionButton("go_interactive_map", "Update !")
                        ),
                        box(width = 12,
                            title = textOutput("title_c3"),
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            leafletOutput("c3")),
                    )),
            tabItem(tabName = "plotting_countries",
                fluidRow(
                  box(width = 4,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "red",
                      selectInput("genre_choice_g4", "Select a genre ?", liste_genre, selected = liste_genre[2]),
                      radioButtons(
                        "type_choice_g4",
                        "Select a type ?",
                        c("TV Show", "Movie"),
                        selected = "Movie"
                      ),
                      actionButton("go_g4", "Update !"),
                  ),
                  
                  box(width = 8,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "red",
                      amChartsOutput("g4")),
                  
                  box(width = 12,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      plotlyOutput("g14")),
                )),
         
            tabItem(
              tabName = "generality",
              fluidRow(
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "red",
                    amChartsOutput("g1")),
                
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g2")),
                
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g7")),
            
              )),
            
            tabItem(
                tabName = "plotting_duration",
                fluidRow(
                    box(
                        width = 3,
                        status = "danger",
                        solidHeader = TRUE,
                        background = "red",
                        selectInput("genre_choice_g3", "Select a genre ?", liste_genre, selected = liste_genre[2])
                    ),
                    
                    box(width = 9,
                        status = "danger",
                        solidHeader = TRUE,
                        background = "red",
                        amChartsOutput("g3")),
                    
                    box(
                      width = 3,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      numericInput(
                        inputId =  "year_choice_g6", 
                        label = "Select a year :", 
                        min = min_realise_year,
                        max = max_realise_year,
                        step = 1,
                        value = 2017
                      ),
                      selectInput("genre_choice_g6", "Select a genre :", liste_genre, selected = "Dramas"),
                      actionButton("go_g6", "Update !")
                    ),
                    
                    box(width = 9,
                        status = "danger",
                        solidHeader = TRUE,
                        background = "black",
                        plotlyOutput("g6")),


                    box(width = 12,
                        status = "danger",
                        solidHeader = TRUE,
                        background = "black",
                        plotlyOutput("g8")),
                    
                    box(width = 12,
                        status = "danger",
                        solidHeader = TRUE,
                        background = "black",
                        plotlyOutput("g9")),
                )
            ),

            tabItem(tabName = "top",
                    fluidRow(
                        box(
                            width = 3,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            numericInput(
                                "n_g11",
                                "Select the lenght of the top :",
                                value = 10,
                                min = 5,
                                max = 45,
                                step = 2.5
                            )
                        ),
                        box(width = 9,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            plotlyOutput("g11")),
                        
                        box(
                            width = 3,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "black",
                            numericInput(
                                "n_g12",
                                "Select the lenght of the top :",
                                value = 10,
                                min = 5,
                                max = 45,
                                step = 2.5
                            ),
                            radioButtons(
                              "type_choice_g12",
                              "Select a type ?",
                              c("TV Show", "Movie"),
                              selected = "Movie"
                            ),
                            selectInput(
                              "country_choice_g12",
                              "Select a country ?",
                              liste_countries,
                              selected = "United States"
                            ),
                            actionButton("go_g12", "Update !"),
                        ),
                        box(width = 9,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "black",
                            plotlyOutput("g12")),
                        
                        box(
                            width = 3,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            numericInput(
                                "n_g13",
                                "Select the lenght of the top :",
                                value = 10,
                                min = 5,
                                max = 45,
                                step = 2.5
                            ),
                            radioButtons(
                                "type_choice_g13",
                                "Select type ?",
                                c("TV Show", "Movie"),
                                selected = "Movie"
                            ),
                            selectInput(
                                "country_choice_g13",
                                "Select a country ?",
                                liste_countries,
                                selected = "United States"
                            ),
                            actionButton("go_g13", "Update !"),
                            
                        ),
                        box(width = 9,
                            status = "danger",
                            solidHeader = TRUE,
                            background = "red",
                            plotlyOutput("g13")),
                    )),
            
            tabItem(
              tabName = "data",
              fluidRow(
                      box(width=12,
                          title = "Explore the database :",
                          status = "danger",
                          solidHeader = TRUE,
                          DTOutput("data_netflix_title"))
              )),
            
            tabItem(
              tabName = "market_share",
              fluidRow(    
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    radioButtons(
                      inputId = "type_g15_g16_g17", 
                      label = "Select a type for the histogram bellow :",
                      inline = TRUE,
                      choices = c("Total" = "Total", "Movies" = "Movies","TV Shows" = "TVShows"),
                      selected = "Total"
                    )),
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g15"))                
              )),
            
            tabItem(
              tabName = "age_of_content",
              fluidRow(
                box(width = 12,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    sliderInput("period_choice_age_oc", "Year period :",
                                min = date_deb_aoc, max = date_fin_aoc, value = c(date_deb_aoc,date_fin_aoc),sep="")),
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g18")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g19")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g20")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g21"))
              )),
            
            tabItem(
              tabName = "RT_analysis",
              fluidRow(
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g22")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g23")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g24")),
                
                box(width = 6,
                    status = "danger",
                    solidHeader = TRUE,
                    background = "black",
                    plotlyOutput("g25"))
              )),
            
              tabItem(
                  tabName = "IMDb_analysis",
                  fluidRow(
                      box(width = 6,
                          status = "danger",
                          solidHeader = TRUE,
                          background = "black",
                          plotlyOutput("g26")),
                      
                      box(width = 6,
                          status = "danger",
                          solidHeader = TRUE,
                          background = "black",
                          plotlyOutput("g27")),
                      box(width = 6,
                          status = "danger",
                          solidHeader = TRUE,
                          background = "black",
                          plotlyOutput("g28")),
                      
                      box(width = 6,
                          status = "danger",
                          solidHeader = TRUE,
                          background = "black",
                          plotlyOutput("g29"))
                  )),
            
              tabItem(tabName = "imdb_RT_lm",
                fluidRow(
                  box(width = 12,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      plotlyOutput("r1")),
                  
                  box(width = 12,
                      title = "Summary",
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      verbatimTextOutput("summary_r1"))
                )),
            
              tabItem(tabName = "rating_year_lm",
                fluidRow(
                  box(width = 12,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      radioButtons(
                        inputId = "rating_method_r2",
                        label = "Choose a rating method for the linear regression bellow :",
                        inline = TRUE,
                        choices = c("Rotten Tomatoes", "IMDb"),
                        selected = "IMDb"
                      )),
                  box(width = 12,
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      plotlyOutput("r2")),
                  ),
                  
                  box(width = 12,
                      title="Summary",
                      status = "danger",
                      solidHeader = TRUE,
                      background = "black",
                      collapsible = TRUE,
                      collapsed = TRUE,
                      verbatimTextOutput("summary_r2")
                  )
              ),
            
            tabItem(
              tabName = "top20",
              fluidRow(  
                box(width = 12,
                    title = "Top 20 Netflix",
                    status = "danger",
                    solidHeader = TRUE,
                    DTOutput("top20_netflix")),
                
                box(width = 12,
                    title = "Top 20 Prime Video",
                    status = "danger",
                    solidHeader = TRUE,
                    DTOutput("top20_pv")),
                
                box(width = 12,
                    title = "Top 20 Hulu",
                    status = "danger",
                    solidHeader = TRUE,
                    DTOutput("top20_hulu")),
                
                box(width = 12,
                    title = "Top 20 Disney +",
                    status = "danger",
                    solidHeader = TRUE,
                    DTOutput("top20_disney"))
              )),
            
            tabItem(
                tabName = "search",
                fluidRow(
                  box(width = 12,
                      title = "Search Engine :",
                      status = "danger",
                      solidHeader = TRUE,
                      selectizeInput(inputId = "search_engine_title" ,
                                     label ="Enter the title of the movie or TV Show you want to watch:",
                                     choices = title_list, selected = "Inception", multiple = FALSE,
                                     options = NULL),
                      radioButtons(
                        "type_choice_se",
                        "Select a type ?",
                        c("TV Show" = 1, "Movie" = 0),
                        selected = 0
                      ),
                      sliderInput("period_choice_SE", "Year period :",
                                  min = min_year_SE, max = max_year_SE, value = c(min_year_SE,max_year_SE),sep=""),
                      actionButton("go_search", "Search !"),
                      
                  ),
                  box(width = 12,DTOutput("search_engine"))
            ))
        )
    ))
    