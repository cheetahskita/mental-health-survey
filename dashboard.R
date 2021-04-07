library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)

results = read_csv('results.csv', col_types = 'ddccccccc')
results$year = as.factor(results$year)
results$company_size = factor(results$company_size, levels = c('1-25', '26-500', '>500'))
results$interferes = factor(results$interferes, levels = c('never', 'rarely', 'sometimes', 'often'))

top_countries = c('united states of america', 'united kingdom', 'canada', 'germany', 'netherlands', 'india', 'australia', 'france', 'ireland', 'spain')

choice_names = c("Do you currently have a mental illness?",
                 "Does your employer provide mental health benefits as part of healthcare coverage?",
                 "How often do you feel your mental illness interferes with your work when being treated effectively",
                 "Would you feel comfortable discussing a mental health issue with your coworkers?")

choices = c("current_disorder",
            "company_benefits",
            "interferes",
            "comfortable")

question_names = setNames(choice_names, choices)

# USER INTERFACE #####################################################

ui = dashboardPage(
  
  dashboardHeader(
    title = "Exploring Mental Health in the Workplace",
    titleWidth = 400
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon('home')),
      menuItem("Global", tabName = "global", icon = icon("globe")),
      menuItem("Age/Gender", tabName = "age_gender", icon=icon('user')),
      menuItem("Data Source", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "home",
        
        h2("Home"),
        p("The purpose of this dashboard is to analyze trends in mental health in the workplace."),
        p("A multi-year survey was collected online by Open Sourcing Mental Illness, Ltd., a non-profit with a mission to raise awareness and providing resources to support mental wellness in the tech and open source communities. This data was downloaded from the OSMI website, cleaned, and transformed into a dashboard to clearly visualize the data."),
        p("The survey consisted of 100 questions about how mental illness impacts professional workplaces. Over the past seven years, the survey has gathered 4,400 responses. The variety of questions and the number of responses made the data difficult to understand on it's own. By visualizing the data, it is easier to find relevant trends about mental health in the workplace."),
        p(HTML('&nbsp;')),
        img(src = "images/osmi.png", height = "200px", width = "200px")
      ),
      
      tabItem(
        tabName = "global",
        fluidPage(
          h2("Global Survey Responses"),
          p("The survey was administered online, so respondands from all across the globe were able to fill it out. In total, there were people over 50 different countries who responded. Many of these countries had less than 10 responses, so only the top ten countries are visualized below."),
          p("The survey also collected information about company size. These companies were broken into categories of small business (1-25 employees), medium business (25-500), and large business (>500)."),
          fluidRow(
            column(6,
                   selectizeInput(inputId = "country_q",
                                    "Compare results by country:",
                                    choices = setNames(choices, choice_names)),
                   plotOutput(outputId = "country_graph")
                   ),
            column(6,
                 selectizeInput(inputId = "company_q",
                                "Compare results by company size:",
                                choices = setNames(choices, choice_names)),
                 plotOutput(outputId = "company_graph")
                 )
            ))
      ),
      
      tabItem(
        tabName = "age_gender",
        fluidPage(
          h2("Age / Gender Responses"),
          p("The survey collected information about participant's age and gender. These questions can also be compared by selecting from the dropdown below."),

          fluidRow(
            column(6,
                   selectizeInput(inputId = "age_q",
                                  "Compare results by age:",
                                  choices = setNames(choices, choice_names)),
                   plotOutput(outputId = "age_bar_graph"),
                   plotOutput(outputId = "age_line_graph")
                   ),
            column(6,
                   selectizeInput(inputId = "gender_q",
                                  "Compare results by gender:",
                                  choices = setNames(choices, choice_names)),
                   plotOutput(outputId = "gender_graph")
                  )
          ))
        ),
      
      tabItem(
        tabName = "data",
        fluidPage(
          h4("Sources of Data"),
          p("Survey results were downloaded from the OSMI website. These works are licensed under a Creative Commons Attribution-Share 4.0 International."),
          tags$a(href = "https://osmihelp.org/research", "OSMI Website"),
          # p("National statistics we downloaded from the Department of Health and Human Services 2018
          #   National Survey on Druge Use and Health, which can be downloaded from their website:"),
          # tags$a(href = "https://www.samhsa.gov/data/sites/default/files/cbhsq-reports/NSDUHNationalFindingsReport2018/NSDUHNationalFindingsReport2018.pdf",
          #        "Access Report")
          )
        )
      )
    )
  )

# SERVER ##########################################################

server = function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$n))
  })
  
  output$text = reactive({question_names[[input$country_q]]})
  
  country_order = reactive({
    if(input$country_q == 'interferes'){
      results %>%
        filter(country %in% top_countries) %>%
        group_by(country, .data[[input$country_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(interferes=="often") %>%
        arrange(desc(rel_perc)) %>%
        pull(country)
    }else{
      results %>%
        filter(country %in% top_countries) %>%
        group_by(country, .data[[input$country_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(.data[[input$country_q]]=="yes") %>%
        arrange(desc(rel_perc)) %>%
        pull(country)
    }
  })
  
  output$country_graph <- renderPlot(
    results %>% 
      filter(country %in% top_countries) %>% 
      filter(!is.na(.data[[input$country_q]])) %>% 
      mutate(country = factor(country, country_order())) %>% 
      ggplot(aes_string(x="country", fill=input$country_q)) + geom_bar(position="fill") + labs(x = 'Country', y = 'Percent', title = question_names[[input$country_q]], fill = 'Responses:') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(face = 'bold'), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  )
  
  company_order = reactive({
    if(input$company_q == 'interferes'){
      results %>%
        filter(!is.na(.data[[input$company_q]])) %>% 
        group_by(company_size, .data[[input$company_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(interferes=="often") %>%
        arrange(desc(rel_perc)) %>%
        pull(company_size)
    }else {
      results %>%
        filter(!is.na(.data[[input$company_q]])) %>% 
        group_by(company_size, .data[[input$company_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(.data[[input$company_q]]=="yes") %>%
        arrange(desc(rel_perc)) %>%
        pull(company_size)
    }
  })
  
  output$company_graph <- renderPlot(
    results %>% 
      filter(!is.na(.data[[input$company_q]])) %>% 
      mutate(company_size = factor(company_size, company_order())) %>% 
      ggplot(aes_string(x="company_size", fill=input$company_q)) + geom_bar(position="fill") + labs(x = 'Company Size', y = 'Percent', title = question_names[[input$company_q]], fill = 'Responses:') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(face = 'bold'))
  )
  
  output$age_bar_graph <- renderPlot(
    results %>% 
      filter(age > 21, age < 55) %>% 
      filter(!is.na(.data[[input$age_q]])) %>% 
      ggplot(aes_string(x="age", fill=input$age_q)) + geom_bar(position="stack") + labs(x = 'Age', y = 'Count', title = question_names[[input$company_q]], fill = 'Responses:') + theme(plot.title = element_text(face = 'bold'))
    )
  
  age_percent = reactive({
    if(input$age_q == 'interferes'){
      results %>%
        filter(age > 21, age < 55, !is.na(.data[[input$age_q]])) %>%
        group_by(age, .data[[input$age_q]]) %>%
        summarise(count = n()) %>%
        mutate(percent = count/sum(count)) %>%
        filter(.data[[input$age_q]] == "often")
    }else {
      results %>%
        filter(age > 21, age < 55, !is.na(.data[[input$age_q]])) %>%
        group_by(age, .data[[input$age_q]]) %>%
        summarise(count = n()) %>%
        mutate(percent = count/sum(count)) %>%
        filter(.data[[input$age_q]] == "yes")
    }
  })
    
  output$age_line_graph <- renderPlot(
    age_percent() %>% 
      filter(age > 21, age < 55, !is.na(.data[[input$age_q]])) %>% 
      ggplot(aes(x = age, y = percent, color='Yes')) + geom_smooth() + scale_color_manual(name = "Response:", values = '#619CCF') + labs(x = 'Age', y = 'Percent', title = "Avg. Percent Who Answered 'Yes'", fill = '') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(face = 'bold'))  )
  
  gender_order = reactive({
    if(input$gender_q == 'interferes'){
      results %>%
        filter(!is.na(.data[[input$gender_q]])) %>% 
        group_by(gender, .data[[input$gender_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(.data[[input$gender_q]]=="often") %>%
        arrange(desc(rel_perc)) %>%
        pull(gender)
    }else {
      results %>%
        filter(!is.na(.data[[input$gender_q]])) %>% 
        group_by(gender, .data[[input$gender_q]]) %>%
        summarise(count = n()) %>%
        mutate(rel_perc = count/sum(count)) %>%
        filter(.data[[input$gender_q]]=="yes") %>%
        arrange(desc(rel_perc)) %>%
        pull(gender)
    }
  })
  
  output$gender_graph <- renderPlot(
    results %>% 
      filter(!is.na(.data[[input$gender_q]])) %>% 
      mutate(gender = factor(gender, gender_order())) %>% 
      ggplot(aes_string(x="gender", fill=input$gender_q)) + geom_bar(position="fill") + labs(x = 'Gender', y = 'Percent', title = question_names[[input$gender_q]], fill = 'Responses:') + scale_y_continuous(labels = scales::percent) + theme(plot.title = element_text(face = 'bold'))
  )
}

shinyApp(ui = ui, server = server)