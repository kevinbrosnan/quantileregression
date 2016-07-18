library(shiny)
library(shinydashboard)

dashboardPage(title = "Quantile Regression",
              skin = ("red"),
              dashboardHeader(title = "Quantile Regression",
                              
                              # Email Sharing link
                              tags$li(class = "dropdown",
                                      tags$a(href = "mailto:?Subject=Quantile Regression Methods&Body=Quantile Regression methods applied to the identification of reference ranges for thyroid disease in pregnancy. http://significantstats.org/shiny/quantileregression/ ",
                                             tags$img(height = "18px", 
                                                      src = "images/email.png")
                                      )
                              ),
                              
                              # Twitter Sharing Link
                              tags$li(class = "dropdown",
                                      tags$a(href = "http://twitter.com/share?url=http://significantstats.org/shiny/quantileregression/&text=Quantile Regression methods applied to the identification of reference ranges for thyroid disease in pregnancy", 
                                             target = "_blank_", 
                                             tags$img(height = "18px", 
                                                      src = "images/twitter.png")
                                      )
                              ),
                              
                              # Facebook Sharing link
                              tags$li(class = "dropdown",
                                      tags$a(href = "http://www.facebook.com/sharer.php?u=http://significantstats.org/shiny/responsetimes/", 
                                             target = "_blank_", 
                                             tags$img(height = "18px", 
                                                      src = "images/facebook.png")
                                      )
                              ),
                              
                              # LinkedIn Sharing link
                              tags$li(class = "dropdown",
                                      tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=http://significantstats.org/shiny/quantileregression/", 
                                             target = "_blank_", 
                                             tags$img(height = "18px", 
                                                      src = "images/linkedin.png")
                                      )
                              )
              ),
              
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Introduction", tabName = "intro", icon = icon("home")),
                  menuItem("Data", tabName = "datafile", icon = icon("table")),
                  menuItem("Analysis", tabName = "analysis", icon = icon("binoculars")),
                  menuItem("Report", tabName = "report", icon = icon("file-pdf-o")),
                  menuItem("Presentation", tabName = "present", icon = icon("microphone")),
                  menuItem("About", tabName = "about", icon = icon("info")),
                  hr(),
                  sidebarUserPanel(name = a("Kevin Brosnan", target = "_blank_",
                                            href = "http://significantstats.org"), 
                                   subtitle = "Applied Statistician",
                                   image = "images/kevinbrosnan.png"),
                  sidebarUserPanel(name = a("Dr. Kevin Hayes", target = "_blank_",
                                            href = "http://www.ulsites.ul.ie/macsi/kevin-hayes-profile"), 
                                   subtitle = "Applied Statistician",
                                   image = "images/kevinhayes.png"),
                  sidebarUserPanel(name = a("Dr. Norma Bargary", target = "_blank_",
                                            href = "https://ulris.ul.ie/live/!W_VA_CV_BUILDER.SHOW_ALL?user=norma.bargary@ul.ie"), 
                                   subtitle = "Applied Statistician",
                                   image = "images/normabargary.png"),
                  hr(),
                  menuItem("Source code", icon = icon("file-code-o"), 
                           href = "https://github.com/significantstats/quantileregression"),
                  menuItem("Bug Reports", icon = icon("bug"),
                           href = "https://github.com/significantstats/quantileregression/issues")
                )
              ),
              
              dashboardBody(
                tags$head(includeScript("www/js/google-analytics.js"),
                          HTML('<link rel="apple-touch-icon" sizes="57x57" href="icons/apple-icon-57x57.png">
                                <link rel="apple-touch-icon" sizes="60x60" href="icons/apple-icon-60x60.png">
                                <link rel="apple-touch-icon" sizes="72x72" href="icons/apple-icon-72x72.png">
                                <link rel="apple-touch-icon" sizes="76x76" href="icons/apple-icon-76x76.png">
                                <link rel="apple-touch-icon" sizes="114x114" href="icons/apple-icon-114x114.png">
                                <link rel="apple-touch-icon" sizes="120x120" href="icons/apple-icon-120x120.png">
                                <link rel="apple-touch-icon" sizes="144x144" href="icons/apple-icon-144x144.png">
                                <link rel="apple-touch-icon" sizes="152x152" href="icons/apple-icon-152x152.png">
                                <link rel="apple-touch-icon" sizes="180x180" href="icons/apple-icon-180x180.png">
                                <link rel="icon" type="image/png" sizes="192x192"  href="icons/android-icon-192x192.png">
                                <link rel="icon" type="image/png" sizes="32x32" href="icons/favicon-32x32.png">
                                <link rel="icon" type="image/png" sizes="96x96" href="icons/favicon-96x96.png">
                                <link rel="icon" type="image/png" sizes="16x16" href="icons/favicon-16x16.png">
                                <link rel="manifest" href="icons/manifest.json">
                                <meta name="msapplication-TileColor" content="#ffffff">
                                <meta name="msapplication-TileImage" content="icons/ms-icon-144x144.png">
                                <meta name="theme-color" content="#ffffff">')),
                tabItems(
                  # Introduction Tab
                  tabItem(tabName = "intro", includeHTML("intro.html")),
                  
                  # Data Tab
                  tabItem(tabName = "datafile",
                          style = "overflow-y:scroll;",
                          box(width = 12, 
                              title = "Thyroid Data - Select a column for Validation", 
                              DT::dataTableOutput("thyroid.tab")
                          ),
                          box(width = 4, 
                              DT::dataTableOutput("thyroid.tab.val")
                          ),
                          box(width = 8, 
                              plotOutput("thyroid.plot.val")
                          )
                  ),
                  
                  # Analysis Tab
                  tabItem(tabName = "analysis",
                          style = "overflow-y:scroll;",
                          box(width = 6, title = "User Inputs",
                              HTML("Choose a method for Quantile Regression:"),
                              radioButtons("method", label = "",
                                           choices = c("Frequentist",
                                                       "Bayesian",
                                                       "Linear Mixed Models"),
                                           inline = TRUE),
                              HTML("Select the dependent variable of interest for the regression model."),
                              radioButtons("dep_var", label = "",
                                           choices = c("T4", "T3", "TSH"),
                                           inline = TRUE)
                          ),
                          box(width = 6,
                              title = "Model Fit",
                              DT::dataTableOutput("modeltab")
                          ),
                          box(width = 6,
                              title = "Descriptive Statistics",
                              plotOutput("analdesc", height = "350px")
                          ),
                          box(width = 6,
                              title = "Results",
                              plotOutput("modelplot", height = "350px")
                          )
                  ),
                  
                  # Report Tab
                  tabItem(tabName = "report",
                          tags$iframe(style = "height:calc(100vh - 80px); width:100%", 
                                      src = "docs/report.pdf")  
                  ),
                  
                  # Presentation Tab
                  tabItem(tabName = "present", 
                          tags$iframe(style = "height:calc(100vh - 80px); width:100%", 
                                      src = "docs/presentation.pdf")
                  ),
                  
                  # About Tab
                  tabItem("about", includeMarkdown("about.md"))
                )
              )
)