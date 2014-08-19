library(markdown)

shinyUI(
    navbarPage(
        "Type 2 diabetes prediction",
        tabPanel("Instructions",
                 includeMarkdown("about.md")
        ),
        tabPanel("Prediction",
                 sidebarLayout(
                     sidebarPanel(
                         h4("Please input your data"),
                         # Setting up the input controls
                         sliderInput("age", "Age:", 
                                     min=0, max=100, value=50),
                         selectInput("gender", "Sex:", 
                                     choices=c(Choose='', "Male" = "M", 'Female' = "F"), selectize=TRUE),
                         selectInput("descent", "Are you of any of the following descents?", 
                                     choices=c(Choose='', "Asian"="Asian", "Aboriginal"="Aboriginal", 
                                               "Pacific Islander"="Pacific Islander", "Maori"="Maori", 
                                               "Torres Strait Islander"="Torres Islander", "American Indian"="American Indian",
                                               "Inuit"="Inuit", 'Other' = 'Other'), selectize=TRUE),
                         selectInput("born", "Where were you born?", 
                                     choices=c(Choose='', "Asia"="Asia", "Australia"="Australia", "North America"="North America", 
                                               "South America"="South America", "Northern Europe"="Northern Europe", "Southern Europe"="Southern Europe",
                                               "Middle East"="Middle East", "Africa - north of Sahara"="Africa - north of Sahara",
                                               "Africa - south of Sahara"="Africa - south of Sahara", "Other"="Other"), selectize=TRUE),
                         selectInput("family", "Have any of your parents or siblings been diagnosed with type 1 diabetes or type 2 diabetes?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         selectInput("history", "Have you ever been found to have high blood sugar levels, e.g. during illness, pregnancy or a health examination?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         selectInput("bpmed", "Are you currently taking medications for high blood pressure?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         selectInput("smoke", "Do you currently smoke any tobacco product on a daily basis?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         selectInput("fruit", "Do you eat fruit or vegetables every day?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         selectInput("exercise", "On average, do you perform at least 2.5 hours of physical activity per week?",
                                     choice=c(Choose='', "Yes" = "Yes", 'No' = 'No'), selectize=TRUE),
                         textInput("waist", "Please indicate your waist measurement (taken below the ribs while standing):",
                                   value='30'),
                         selectInput("waistunit", "Please select the unit of your measurement:",
                                     choice=c("inches" = "in", 'cm' = 'cm')),
                         actionButton("goButton", "Submit")
                         
                     ),
                     mainPanel(
                         h3('Predicted risk of type 2 diabetes'),
                         
                         # Show the risk assessment plot
                         plotOutput("score")
                         
                     )
                 )
        )
        
        
    )
)