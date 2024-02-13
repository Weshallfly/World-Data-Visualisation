#setwd("C:\\Users\\VISHAL MAURYA\\Desktop\\R\\Visualisation_app")
World_data <- read.csv("WorldData2023.csv",header=T)

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(gridExtra)


# UI
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title = div(strong("SphereVista"), style = "text-align: center;"),
    titleWidth = "100%"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro"),
      menuItem("Univariate Analysis", tabName = "univar"),
      menuItem("Bivariate Analysis", tabName = "bivar"),
      menuItem("Multivariate Analysis", tabName = "multi"),
      menuItem("Conclusion", tabName = "concl")
    )
  ),
  
  dashboardBody(
    tabItems(
      # Introduction section
      tabItem(tabName = "intro",
              h2("About the project"),
              htmlOutput("about_project"),
              h2("About Dataset"),
              p("This comprehensive dataset provides a wealth information of 31 attributes about all countries worldwide, covering a wide range of indicators and attributes."),
              h4("Discription of Variables Used:"),
              htmlOutput("dataset_variables"),
              h2("Why named SphereVista !"),
              htmlOutput("about_name")
      ),
      
      # Attribute-wise analysis section 
      tabItem(tabName = "univar",
              h2("Attribute-wise Analysis"),
              htmlOutput("about_univar"),
              selectInput("attribute", "Select a Attribute", choices = names(World_data)[-1]),
              selectInput("transformation", "Select a transformation if histogram of numerical attribute is exponentially decreasing", choices = c("Without transformation", "Log transformation")),
              plotOutput("attribute_plot"),
              plotOutput("attribute_plot2"),
              p(""),
              p("The countries will be ranked based on the selected attribute, with the option to display up to a choosen number below."),
              selectInput("upto","Choose a number", choices = c(5,10,20,30,50,100,194)),
              tableOutput("top_countries_table"),
              h2("Interpretations from plots"),
              htmlOutput("univar_interpretation")
      ),
      
      #  Pair-wise Attribute Analysis section
      tabItem(tabName = "bivar",
              h2("Pair-wise Attribute Analysis"),
              htmlOutput("about_bivar"),
              selectInput("pair1", "Select first Attribute", choices = names(World_data)[-1][-14][-14]),
              selectInput("pair2", "Select second Attribute", choices = names(World_data)[-1][-14][-14]),
              plotOutput("pair_plot"),
              plotOutput("pair_plot2"),
              h2("Interpretations from plots"),
              htmlOutput("bivar_interpretation")
      ),
      
      # Multivariate analysis section
      tabItem(tabName = "multi",
              h2("Multivariate Analysis"),
              htmlOutput("about_multi"),
              selectInput("multi", "Select a analysis combination", choices = c("(A) Population versus Land Area with Life expectancy and Continents","(B) Agriculture Land versus GDP with Population and Continents","(C) Forest Area per and Agriculture Area per with Land area and Continents","(D) Population density vs Life Expectancy with Area and Continents")),
              plotOutput("multi_plot"),
              h2("Interpretations from plots"),
              htmlOutput("multi_interpretation")
      ),
              
      # Conclusion section
      tabItem(tabName = "concl",
              h2("Conclusion"),
              htmlOutput("concl"),
              h3("Acknowlegement"),
              p("I am thankful to Mr. Sourish Das, professor at Chennai Mathematical Institute for guiding me through this project and helping me whenever needed."),
              p(""),
              HTML("<strong>Contact</strong> - vishalmaurya0906@gmail.com")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$about_project <- renderText({
    HTML("Hi, I am Vishal Maurya, student of Chennai Mathematical Institute. I have created this project on the World Data Analysis named 'SphereVista' using RStudio with the help of R programming language in November-December 2023.<br><br>
    The main goal of this project is to explore and make sense of various indicators and characteristics of countries around the world. The website is designed to assist users in analyzing a dataset by visualizing and understanding different aspects of each country, such as their distribution and dependencies. <br><br>
    To achieve this, the site provides tools for three types of analysis: <br> 
    <ul>
           <li>Univariate Analysis</li>
           <li>Bivariate Analysis </li>
           <li>Multivariate Analysis </li>
           </ul>
    The website employs various visualization tools such as histograms, bar plots, pie charts, and box plots for univariate analysis, scatter plots for bivariate analysis, and multivariate analysis to offer users a comprehensive and accessible way to explore and gain insights into global country data.
    ")
  })
    
  # Introduction section
  output$dataset_variables <- renderText({
    HTML("
    <ul>
      <li>Country: Name of the country.</li>
      <li>Population: Total population of the country.</li>
      <li>Continent: Continent in which the country belongs.</li>
      <li>Land Area: Total land area in square kilometers.</li>
      <li>GDP: Gross Domestic Product.</li>
      <li>Life Expectancy: Average life expectancy in years.</li>
      <li>Agricultural Land Per: Percentage of the total land area used for agricultural purposes.</li>
      <li>Urban Population: Total population residing in urban areas.</li>
      <li>Population Density: Population density measured in individuals per square kilometer.</li>
      <li>Forested Area: Land area covered by forests or wooded land.</li>
      <li>Fertility Rate: Average number of children born per woman.</li>
      <li>Birth Rate: Number of live births per 1,000 individuals.</li>
      <li>Unemployment Rate: Percentage of the labor force unemployed and seeking employment.</li>
      <li>CO2 Emissions: Carbon dioxide emissions in metric tons.</li>
      <li>Latitude: North-south geographic coordinate.</li>
      <li>Longitude: East-west geographic coordinate.</li>
    </ul>
    Country and Continent are the only object type attributes.
  ")
  })
  
  output$about_name <- renderText({
    HTML("I chose 'SphereVista' because it perfectly captures what my website is all about. The word 'Sphere' reflects the idea that I want to provide a complete and detailed view of global information. The term 'Vista' makes it clear that users can expect a wide and expansive visualisation when exploring the data. Together, it symbolizes that my platform is like a window to a broad and insightful world of information about countries worldwide.")
  })
  
  # Attribute-wise analysis section
  output$about_univar<- renderText({
    HTML("This segment is dedicated to attribute-wise analysis.
    <ul>
      <li>If the chosen attribute is numerical, the result will include a histogram and boxplot illustrating the distribution of that attribute.Additionally, the counties will be ranked based on the selected attribute, with the option to display up to a specified number.</li>
      <li>If the attribute is of the object type, the outcome will feature a bar plot and a pie chart, providing a visual representation of counts across different categories.</li>
      <li>If one attribute is numerical and the other is of object type, the outcome will show an aggregate bar plot of average of numerical attribute a boxplot of numerical attribute among the groups of Object type attribute.</li>
    </ul>")
  })
  
  # For plot 1 in univariate analysis
  output$attribute_plot <- renderPlot({
    if (input$tabs == "univar" && input$transformation == "Without transformation") {
      attribute_data <- World_data[, input$attribute]
      
      if (is.numeric(attribute_data)) {
        ggplot(World_data, aes(x = !!sym(input$attribute))) +
          geom_histogram(fill = "#27AE60", color = "white",alpha= 0.7) +
          labs(title = paste("Histogram of", input$attribute)) +
          theme_minimal()
      } else {
        ggplot(World_data, aes(x = factor(attribute_data))) +
          geom_bar(fill = "#27AE60", color = "white", alpha = 0.7) +
          labs(title = paste("Bar Plot of", input$attribute),
               x= "Continents") +
          theme_minimal()
      }
    } else {
      attribute_data <- World_data[, input$attribute]
      
      if (is.numeric(attribute_data)) {
        ggplot(World_data, aes(x = !!sym(input$attribute))) +
          geom_histogram(fill = "#27AE60", color = "white",alpha= 0.7) +
          scale_x_log10() +
          labs(title = paste("Histogram of Log(", input$attribute,")")) +
          theme_minimal()
      } else {
        ggplot(World_data, aes(x = factor(attribute_data))) +
          geom_bar(fill = "#27AE60", color = "white", alpha = 0.7) +
          labs(title = paste("Bar Plot of", input$attribute),
               x= "Continents") +
          theme_minimal()
      }
    }
  })
  
  # For plot 2 in Univariate analysis
  output$attribute_plot2 <- renderPlot({
    if (input$tabs == "univar" && input$transformation == "Without transformation") {
      attribute_data <- World_data[, input$attribute]
      
      if (is.numeric(attribute_data)) {
        ggplot(World_data, aes(x = !!sym(input$attribute))) +
          geom_boxplot(fill = "#87CEFA", color = "#2980B9") +
          labs(title = paste("Boxplot of", input$attribute)) +
          theme_classic()
      } else {
        aggregated_data <- table(attribute_data)
        ggplot(as.data.frame(aggregated_data), aes(x = "", y = Freq, fill = factor(attribute_data))) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +  # Use coord_polar to create a pie chart
          labs(title = paste("Pie Chart of", input$attribute),
               fill= "Continents") +
          theme_void()
      } 
      } else {
        attribute_data <- World_data[, input$attribute]
        
        if (is.numeric(attribute_data)) {
          ggplot(World_data, aes(x = !!sym(input$attribute))) +
            geom_boxplot(fill = "#87CEFA", color = "#2980B9") +
            labs(title = paste("Boxplot of Log(", input$attribute, ")"),
                 x=paste("Log(", input$attribute, ")")) +
            scale_x_log10() +
            theme_classic()
        } else {
          aggregated_data <- table(attribute_data)
          ggplot(as.data.frame(aggregated_data), aes(x = "", y = Freq, fill = factor(attribute_data))) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar(theta = "y") +  # Use coord_polar to create a pie chart
            labs(title = paste("Pie Chart of", input$attribute),
                 fill = "Continents") +
            theme_void()
        }
      }
  })
  
  output$top_countries_table <- renderTable({
    if(is.numeric(World_data[[input$attribute]])) {
    # Select only the "Country" column and the chosen attribute
    filtered_data <- World_data %>%
      select(Country, !!sym(input$attribute)) %>%
      arrange(desc(get(input$attribute))) %>%
      head(n = as.numeric(input$upto))
    
    # Return the top countries based on the selected attribute
    filtered_data
    }
  })
  
  output$univar_interpretation <- renderText({
    HTML("
    <ul>
      <li>Population has a symmetric distribution about 10 million people after log transformation.</li>
      <li>Land_Area has a left-skewed distribution with a peak around a million square kilometers after log transformation.</li>
      <li>GDP has a symmetric distribution about a billion dollars after log transformation with some outliers.</li>
      <li>Life_Expectancy is Platykurtically spread over the range 53 to 88 years.</li>
      <li>Agricultural_Land_Per is approximately uniformly distributed between 0 and 82%.</li>
      <li>Urban Population has a symmetric distribution about a million after log transformation.</li>
      <li>Population Density has a symmetric distribution about 100 people per square kilometer after log transformation.</li>
      <li>Forest Area Percentage has a triangular distribution between 0 and 100% with 0% as the peak.</li>
      <li>Fertility Rate has a bimodal right-tailed distribution with peaks at 1.6 and 4.6.</li>
      <li>Birth Rate has a right-tailed distribution with a peak at 10.</li>
      <li>Unemployment Rate has a right-tailed distribution with a peak at 0.4.</li>
      <li>CO2 Emission has a symmetric distribution about 10,000 after log transformation.</li>
      <li>More than 3/4th of the total countries belong to Asia, Africa, and Europe only.</li>
    </ul>
  ")
  })
  
  
    
  # Pair-wise Analysis section
  output$about_bivar <- renderText({
    HTML("This segment is dedicated to pair-wise attribute analysis.
    <ul>
      <li>If both attributes are the same, then univariate analysis will be displayed.</li>
      <li>If both attributes are numerical, the visualization will include four scatter plots, each representing a different combination of log-transformed attributes.</li>
      <li>If one attribute is numerical and the other is categorical, the result will display an aggregate bar plot showing the average of the numerical attribute and a boxplot illustrating the distribution of the numerical attribute across groups of the categorical attribute.</li>
    </ul>
  ")
  })
  
  
  # Pair-wise Analysis section (1st plot)
  output$pair_plot <- renderPlot({
    if (input$tabs == "bivar") {
      pair_data <- World_data[, c(input$pair1, input$pair2)]
      
      if (is.numeric(pair_data[[1]]) && is.numeric(pair_data[[2]]) && identical(pair_data[[1]], pair_data[[2]])) {
        # Case 1: Both attributes are numerical and same
        ggplot(pair_data, aes(x = !!sym(input$pair1))) +
          geom_histogram(fill = "#27AE60", color = "white",alpha = 0.8) +
          labs(title = paste("Histogram of", input$pair1))+
          theme_minimal()
      } else if (is.character(pair_data[[1]]) && is.character(pair_data[[2]]) && identical(pair_data[[1]], pair_data[[2]])) {
        # Case 2: Both attributes are categorical and same
        ggplot(pair_data, aes(x = !!sym(input$pair1))) +
          geom_bar(fill ="#27AE60", color = "white",alpha = 0.8) +
          labs(title = paste("Count Bar Plot of", input$pair1))+
          theme_minimal()
      } else if (is.numeric(pair_data[[1]]) && is.numeric(pair_data[[2]])) {
        # Case 3: Both attributes are numerical
        p1=ggplot(pair_data, aes(x = !!sym(input$pair1), y = !!sym(input$pair2))) +
          geom_point(color ="#27AE60",alpha = 0.8) +
          labs(title = paste("Four Scatter plots between", input$pair1, "and", input$pair2,"each representing a different combination of log-transformed attributes"))+
          theme_minimal()
        p2=ggplot(pair_data, aes(x = !!sym(input$pair1), y = !!sym(input$pair2))) +
          geom_point(color ="#2980B9",alpha = 0.8) +
          labs(y= paste("Log(",input$pair2, ")"))+
          scale_y_log10() +
          theme_minimal()
        
        grid.arrange(p1,p2,ncol=2)
      } else if (is.character(pair_data[[1]]) && is.numeric(pair_data[[2]])) {
        # Case 4: First attribute is of object type and second is numerical
        aggregated_data <- aggregate(pair_data[[2]], by = list(pair_data[[1]]), FUN = mean)
        ggplot(aggregated_data, aes(x = factor(Group.1), y = x)) +
          geom_bar(fill ="#27AE60", stat = "identity",alpha = 0.8) +
          labs(x = "Continents",
               y = paste("Average of",input$pair2))+
          theme_minimal()
      } else if (is.character(pair_data[[2]]) && is.numeric(pair_data[[1]])) {
        # Case 5: Second attribute is of object type and first is numerical
        aggregated_data <- aggregate(pair_data[[1]], by = list(pair_data[[2]]), FUN = sum)
        ggplot(aggregated_data, aes(x = factor(Group.1), y = x)) +
          geom_bar(fill = "#2980B9", stat = "identity",alpha = 0.8) +
          labs(title = paste("Aggregated Barchart of", input$pair1, "and", input$pair2),
               x = "Continents",
               y = paste("Average of",input$pair1))+
          theme_minimal()
      } 
    }
  }, width = 800, height = 400)
  
  # Pair-wise Analysis section (2nd plot)
  output$pair_plot2 <- renderPlot({
    if (input$tabs == "bivar") {
      pair_data <- World_data[, c(input$pair1, input$pair2)]
      
      if (is.numeric(pair_data[[1]]) && is.numeric(pair_data[[2]]) && identical(pair_data[[1]], pair_data[[2]])) {
        # Case 1: Both attributes are numerical and same
        ggplot(pair_data, aes(x = !!sym(input$pair1))) +
          geom_boxplot(fill = "#87CEFA", color = "#2980B9") +
          labs(title = paste("Boxplot of", input$pair1)) +
          theme_minimal()
      } else if (is.character(pair_data[[1]]) && is.character(pair_data[[2]]) && identical(pair_data[[1]], pair_data[[2]])) {
        # Case 2: Both attributes are categorical and same
        attribute_data <- World_data$Continent
        aggregated_data <- table(attribute_data)
        adf <- as.data.frame(aggregated_data)
        ggplot(adf, aes(x = "", y = Freq, fill = attribute_data)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(title = paste("Number of countries among continents"),
               fill = "Continents") +
          theme_void()
      }else if (is.numeric(pair_data[[1]]) && is.numeric(pair_data[[2]])) {
        # Case 3: Both attributes are numerical
        p3=ggplot(pair_data, aes(x = !!sym(input$pair1), y = !!sym(input$pair2))) +
          geom_point(color ="#8E44AD",alpha = 0.8) +
          labs(x= paste("Log(",input$pair1, ")"))+
          scale_x_log10() +
          theme_minimal()
        p4=ggplot(pair_data, aes(x = !!sym(input$pair1), y = !!sym(input$pair2))) +
          geom_point(color ="#E63946",alpha = 0.8) +
          labs(x= paste("Log(",input$pair1, ")"),
               y= paste("Log(",input$pair2, ")"))+
          scale_y_log10() +
          scale_x_log10() +
          theme_minimal()
        
        grid.arrange(p3,p4,ncol=2)
        }else if (is.character(pair_data[[1]]) && is.numeric(pair_data[[2]])) {
        # Case 4: First attribute is of object type and second is numerical
          ggplot(World_data, aes(y = pair_data[[2]], x = pair_data[[1]], col= pair_data[[1]])) +
            geom_boxplot(alpha = 0.8) + 
            labs(title = paste("Boxplot of", input$pair1, "and", input$pair2),
                 x = "Continents",
                 y = paste(input$pair2)) +
            theme_minimal() +
            theme(legend.position = "none")  # Set legend.position = "none" to remove the legend
          
      }else if (is.character(pair_data[[2]]) && is.numeric(pair_data[[1]])) {
        # Case 5: Second attribute is of object type and first is numerical
        ggplot(World_data, aes(y = pair_data[[1]], x = pair_data[[2]], col= pair_data[[2]])) +
          geom_boxplot(alpha = 0.8) +
          labs(title = paste("Boxplot of", input$pair2, "among", input$pair1),
               x = "Continents",
               y = paste(input$pair1))+
          theme_minimal()+
          theme(legend.position = "none")
      } 
    }
  }, width = 800, height = 400)
  
  output$bivar_interpretation <- renderText({
    HTML("<strong>Significant linear relationships in Numerical-Numerical attribute pairs</strong>
    <ul>
      <li>Very Strong linear relationship between Log(GDP) and Log(Urban_population).</li>
      <li>Very Strong linear relationship between Log(GDP) and Log(CO2_Emission).</li>
      <li>Very Strong positive linear relationship between Log(Population) and Log(Urban_population).</li>
      <li>Strong positive linear relationship between Log(Population) and Log(Land_Area).</li>
      <li>Strong positive linear relationship between Log(Population) and Log(GDP).</li>
      <li>Strong positive linear relationship between Log(Urban_Population) and Log(CO2_Emission).</li>
      <li>Strong positive linear relationship between Fertility_Rate and Birth Rate.</li>
      <li>Strong linear relationship between Log(Population) and Log(CO2_Emission).</li>
      <li>Strong positive linear relationship between Log(Land_Area) and Log(Urban_population).</li>
      <li>Negative linear relationship between Log(Land_Area) and Log(Population_density).</li>
      <li>Negative linear relationship between Log(Life_Expectancy) and Log(Fertility Rate) and also with Log(Birth_Rate). Here, we can observe that all the 4 scatter-plots have a strong positive relation. This is because both variables have a very small scale, i.e., in tens.</li>
      <li>Positive linear relationship between Log(Land_Area) and Log(CO2_Emission).</li>
      <li>Positive linear relationship between Log(Land_Area) and Log(GDP).</li>
      <li>Weak positive linear relationship between Log(GDP) and Log(Life_Expectancy).</li>
      <li>Weak negative linear relationship between Agricultural_Land_Per and Log(Forest_Area_Per).</li>
      <li>However, Population Density depends on Population, but there is no clear relationship between them.</li>
      <li>However, CO2_Emission and Life_Expectancy must have a negative linear relationship, but surprisingly, there is a weak positive linear relationship between Log(Life_Expectancy) and Log(CO2_Emission).</li>
    </ul>
    <strong>Numerical- Categorical analysis </strong> (Continent is the only categorical valriable)
    <ul>
      <li>Asia have highest average population among its countries. </li>
      <li>Austrelia have highest average land area amongs its countries. </li>
      <li>North America have highest average GDP amongs its countries, followed by Asia and Australia.</li>
      <li>Austrelia and Africa have highest average agricultural land percentage amongs its countries.</li>
      <li>Asia have highest average CO2 Emission among its countries. </li>
      <li>Africa and Oceania have highest average birth rate amongs its countries.</li>
    </ul>
  ")
  })
  
  
  # Multivariate Analysis
  output$multi_plot <- renderPlot({
      if (input$multi == "(A) Population versus Land Area with Life expectancy and Continents") {
        ggplot(World_data, aes(y = Land_Area, x = Population, col = Life_Expectancy, size = Continent)) +
          labs(title = "Population versus Land Area with Life expectancy and Continents",
               size = "Continents",
               col = "Life Expectancy",
               y = "Log(Land Area)",
               x = "Log(Population)") +
          geom_point() +
          scale_x_log10() +
          scale_y_log10() +
          theme_classic()
      } else if (input$multi == "(B) Agriculture Land versus GDP with Population and Continents") {
        ggplot(World_data, aes(y = Agricultural_Land_Per, x = GDP, size = Land_Area, col = Continent)) +
          coord_cartesian(xlim = c(100000000, 20000000000000)) +
          labs(title = "Agriculture Land versus GDP with Population and Continents",
               size = "Land Area (%)",
               col = "Continents",
               y = "Log(Agricultural Land (%))",
               x = "Log(GDP)") +
          geom_point() +
          scale_y_log10() +
          scale_x_log10() +
          theme_classic()
      }else if (input$multi == "(C) Forest Area per and Agriculture Area per with Land area and Continents") {
        ggplot(World_data , aes(y=Forest_Area_Per,
                        x=Agricultural_Land_Per,
                        size=Land_Area,
                        col=Continent))+
          coord_cartesian()+
          geom_point()+
          theme_classic()+
          labs(title="Forest Area per and Agriculture Area per with Land area and Continents",
               x="Agriculture Area (%)",
               y="Forest Area (%)",
               col="Continents",
               size="Land Area")
      }else if (input$multi == "(D) Population density vs Life Expectancy with Area and Continents") {
        ggplot(World_data, aes(y=Life_Expectancy,
                        x=Population_Density,
                        size=Land_Area,
                        col=Continent))+
          coord_cartesian(xlim=c(0,500))+
          geom_point()+
          labs(title="Population density vs Life Expectancy with Area and Continents",
               size="Area",
               y="Life Expectancy",
               x="Population Density")+
          theme_classic()
      }
    })
  output$multi_interpretation <- renderText({
    HTML("
    <ul>
      <li>For graph A</li>
      <ul>
        <li>The scatter plot is condensed along a line with slope 1. So, Land area and Population are significantly positively correlated when both are transformed with log10.</li>
        <li>All the small circles are condensed to higher Population and higher Land Area. So, African and Asian countries have a higher chance to have higher Land area and higher population.</li>
      </ul>

      <li>For graph B</li>
      <ul>
        <li>The graph without log transformation was very condensed to lower GDP and lower Agricultural land. This happens in most of the cases when both the variables are exponentially decreasing. So, we had changed the scale of X and Y by log10 transformation to visualize it in a better way.</li>
        <li>From the scatter plot, the lower right has small circles and circles gradually increase when we move to higher GDP and higher Agricultural land.</li>
        <li>This scatter plot clearly shows that there is a significant positive correlation among Land Area, GDP, and Agricultural Land.</li>
      </ul>

      <li>For graph C</li>
      <ul>
        <li>The scatter plot is condensed along a line with slope -1. So, Forest Area Percent and Agricultural area percent are significantly negatively correlated.</li>
        <li>There are some countries with negligible forest area, but there are only 1-2 countries with zero agricultural land.</li>
      </ul>

      <li>For graph D</li>
      <ul>
        <li>Most of the larger countries by land are condensed to higher Life Expectancy and lower Population Density.</li>
        <li>Most of the African countries are condensed at lower Life Expectancy and lower Population Density.</li>
      </ul>
    </ul>
  ")
  })
  
  
  output$concl<- renderText({
    HTML("In conclusion, the analysis of the dataset reveals interesting patterns in various distributions. The population distribution, after log transformation, exhibits a symmetric pattern around 10 million people, while land area shows a left-skewed distribution peaking at around a million square kilometers post log transformation. The GDP distribution, also after log transformation, is symmetric around a billion dollars with some outliers. Life expectancy is platykurtically spread over the range of 53 to 88 years, and Agricultural Land Percentage is approximately uniformly distributed between 0 and 82%. These insights provide a comprehensive understanding of the demographic and economic landscape of the countries in the dataset.<br><br>
          Moving on to linear relationships between variables, the analysis highlights several strong and weak correlations. For instance, there is a very strong linear relationship between Log(GDP) and Log(Urban_population) and Log(GDP) and Log(CO2_Emission). Positive linear relationships are observed between Log(Population) and Log(Urban_population), Log(Population) and Log(GDP), and Fertility_Rate and Birth Rate. Additionally, the relationship between Log(Land_Area) and Log(Urban_population) is strongly positive, while there is a negative linear relationship between Log(Land_Area) and Log(Population_density). These findings shed light on the interconnectedness of various demographic and economic factors within the dataset.<br><br>
          Multivariate analysis further enriches our understanding, as evidenced by the graphical representations. In the first graph, a condensed scatter plot along a line with slope 1 suggests a significant positive correlation between Land Area and Population after log transformation. The second graph, after log transformation, illustrates a positive correlation among Land Area, GDP, and Agricultural Land. The third graph indicates a condensed scatter plot along a line with slope -1, revealing a significant negative correlation between Forest Area Percent and Agricultural Area Percent. Lastly, the fourth graph portrays larger countries with higher life expectancy and lower population density, offering insights into the spatial distribution of these demographic indicators. These visualizations enhance the interpretability of the data, providing a nuanced perspective on the relationships between different variables.<br><br>")
  })
}

# To run the app
shinyApp(ui = ui, server = server)

