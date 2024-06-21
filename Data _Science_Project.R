library(shiny) #to create a web application with GUI
library(dplyr) #that library contains group_by function
library(arules)#to use apriori function 
library(DT)   #to create tables 


#sum(duplicated(dataa))#display sum of duplicateds
#dataa<-unique(dataa)#remove the duplicateds
#sum(duplicated(dataa))#check 
#sum(is.na(dataa))#display the empty cells
#dataa<-dataa[,c("items","city","customer","paymentType","count","total","rnd","age")]#rearrange cells

#boxplot(dataa[,5:8])
#outlier<- boxplot(dataa$count)$out#display outliers in count field
#dataa[which(dataa$count%in%outlier),]



ui <- fluidPage(
  
  # App title ----
  titlePanel(
    div(class = "myTitle", "Welcome To Our Grocery store"),  # Add a class
  ),
  # ... rest of our UI code ...
  tags$head(
    tags$style(HTML("
      .myTitle {
        color:#4F9FF0;  /* Baby blue color (replace with hex code if needed) */
        font-family: 'Comic Sans MS', cursive;
        
      }
      
    ")))
  ,
  # Sidebar panel for inputs ----
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file", "Choose CSV File"),# Input: Select a file ----
      selectInput("vusal","Select The visualisation",choices = c("Cash and Credit"=1," age and sum of total spending"=2,
                                                                 "Cities' total spending"=3,
                                                                 "Distribution of total spending"=4,
                                                                 "the dash board"=5)),
      numericInput("nCluster", "Number of clusters", min = 2, max = 4, value = 2),
      numericInput("minSupport", "Minimum Support", 0.001, min = 0.001, max = 1, step = 0.01),
      numericInput("minConfidence", "Minimum Confidence", 0.02, min = 0.001, max = 1, step = 0.01)
      , checkboxInput("display_apriori", "Display apriori", value = F),
      checkboxInput("display_data_table", "Display Kmean table", value = F)
      
      
    ),
    mainPanel(
      textOutput("plotText"),
      plotOutput("plot"), #to display visualization graphs 
      textOutput("aprioriText"),   # Add text output as a title 
      tableOutput("apriori_table"),  # Add table output for apriori rules
      plotOutput("plot2"),       # associatioin 
      textOutput("kmeanText"),   
      DTOutput("data_table_output")  # kmean table
      ,
      HTML('<style type="text/css">
      .well {
        background-color: #99CCFF; /* Set background color */
      }
      .shiny-html-output {
        font-size: 30px; /* Set font size */
        line-height: 25px; /* Adjust line height */
      }
    </style>')   
    )
  )
)
###########################################################################################
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, sep = ",")# store dataSet in a variable 
  })
  
  
  output$plotText<-renderText({
    req(input$file)
    "The Visualisations"
    
  })
  output$plot <- renderPlot({  #displaying graphs 
    big_data <- data()        # loading or generating a dataset that will be used and assigning it in a variable
    if(input$vusal==1){
      
      p<- table(big_data$paymentType)
      percentage <- sprintf("%.2f%%", 100 * p / sum(p))  # this is to calculate the percentage of each the cash and 
      #credit and display two digits after decimal place and follow it by "%" 
      pie(p, labels = percentage, main = "Compare cash and credit totals", col = c("#4F9FF0", "pink")) #create a pie chart to compare the 
      #totals of cash and credit payments
      legend("bottomleft", legend = c("Cash", "Credit"), fill = c("#4F9FF0", "pink")) #to add a legend to the pie chart to explain the
      # colors used:light blue for cash and pink for credit
      
      
      
    }else if(input$vusal==2){
      grp_tbl<-big_data %>%  #grouping the age col with sum of total spending 
  group_by(age)  #in here the function got the ages collected into a group of all the diffrent ages and how many times they had repeatred
      agg_tbl<-grp_tbl%>%
 summarise(sum(total))  #this part collected the total sum of spendings , and associated with the previous part to show how much did each age group spend 
      plot(x=agg_tbl$age , y=agg_tbl$`sum(total)`, main= "Compare",xlab="age",ylab="total sum",type='b',col="#4F9FF0") # create scatter plot
  
    
    }else if(input$vusal==3){
      grp_tbl<-big_data %>% group_by(city)
      grp_tbl    #this part grouped the different cities there were in the data and collected their amounts
      agg_tbl<-grp_tbl%>% summarise(sum(total))
   agg_tbl  #in here , the sum total of spending was collected , got associated with the different cities from the previous part 
   #to show each city's spending
      agg_tbl_dec<-agg_tbl[order(-agg_tbl$`sum(total)`),]    #descending order
 barplot(agg_tbl_dec$`sum(total)`,names.arg = agg_tbl_dec$city, main = "Cities' total spending" , xlab="city",ylab ="total sum", col='lightblue')
      # create a box plot of the 'total' column of the 'data' data frame to show the distribution of the total spending 
      
      
      
      
    }else if(input$vusal==4){
      boxplot(x=big_data$total, main="The distribution of total spending", xlab="Total spending",col="pink")
      # create a box plot of the 'total' column of the 'data' data frame to show the distribution of the total spending
      
      
      
    }else{                  #if the user choosed dashboard choice
      par(mfrow=c(2,2))
      
      p<- table(big_data$paymentType)
      percentage <- sprintf("%.2f%%", 100 * p / sum(p))
      pie(p, labels = percentage, main = "Compare cash and credit totals", col = c("#4F9FF0", "pink"))
      legend("bottomleft", legend = c("Cash", "Credit"), fill = c("#4F9FF0", "pink"))
      #########################################
      grp_tbl<-big_data %>%  #grouping the age col with sum of total spending 
        group_by(age)
      agg_tbl<-grp_tbl%>%
        summarise(sum(total))
      plot(x=agg_tbl$age , y=agg_tbl$`sum(total)`, main= "Compare",xlab="age",ylab="total sum",type='b',col="blue")
      
      ########################################################################################3
      grp_tbl<-big_data %>% group_by(city)
      grp_tbl
      agg_tbl<-grp_tbl%>% summarise(sum(total))
      agg_tbl
      agg_tbl_dec<-agg_tbl[order(-agg_tbl$`sum(total)`),]
      barplot(agg_tbl_dec$`sum(total)`,names.arg = agg_tbl_dec$city, main = "Cities' total spending" , xlab="city",ylab ="total sum", col='lightblue')
      
      ###########################################################################################  
      boxplot(x=big_data$total, main="The distribution of total spending", xlab="Total spending",col='pink')
    }})
  ########################################################################################################
  #apriori:
  
  
  output$aprioriText<-renderText({
    req(input$file)     # Check if a file is uploaded
    "The Apriori Graph"   # Display the message "The Apriori Graph"
  })
  # Render the plot and table for Apriori results
  output$plot2<- renderPlot({
    if(input$display_apriori){   # Check if the checkbox for displaying Apriori results is checked
      req(input$file)            # Read the data from the uploaded file and convert it into transactions 
      big_data<- data()          # Get the data (Note: You should define 'data()' somewhere
      i=big_data$items          # Extract the items from the dat
      t=read.transactions(textConnection(i),sep=",")   # Convert items into transactions
      inspect(head(t))                              # Show the first few transactions for inspection
      # Apply the Apriori algorithm to find association rules:
      tdata<-apriori(t,parameter = list(supp= input$minSupport,conf= input$minConfidence,minlen=2))
      # Generate a plot showing the frequency of items:
      itemFrequencyPlot(t ,topN=6,type="absolute",col='pink')  # Plot the top 6 most frequent item
      
    output$apriori_table <- renderTable({     # Render the Apriori results in a table
       big_data<-data()
       # Extract items and convert them into transactions
       x<-big_data $items  # to access only at the items column
      y <- read.transactions(textConnection(x), sep = ",")  # Convert items into transactions
      
      # Apply the Apriori algorithm to find association rules:
      tdata <- apriori(y, parameter = list(supp= input$minSupport,conf= input$minConfidence,minlen=2))
      
      # Convert the results into a data frame to display:
      rules<-as(tdata,"data.frame")
      print(head(rules))    })
  
  }  })
  
     
      
      
  ######################################################################
  #k means:

  output$kmeanText<-renderText({  #displaying a text as a title
    req(input$file)              # check if the file is uploaded
    "Kmean Table"
  })
  output$data_table_output <- renderDT({  #displaying a table for k-mean    
    if (input$display_data_table) {
      data<- read.csv(input$file$datapath)
      # Grouping and summarizing data
      data_grouped<-data %>%           #this operator to perform grouped operations on the data set
        group_by(age,customer)        #grouping the data for k-means: customers names and ages
      data_totalspending<-data_grouped %>%      #assigning the grouped data: names and ages in a new variable
        summarize(total_spending=sum(total))    #to calculate the sum of each customers total spending and assign it as anew column
      
      data_for_kmean<-data_totalspending[,-c(2)]   #preparing the data for the kmean operation by removing the character column
      if (input$nCluster < 2 || input$nCluster > 4) {   #checking for ncluster numbers
        showModal(modalDialog(                        # it displays a modal dialog box with the title "Invalid Input" and the message "Number of clusters must be between 2 and 4."
          title = "Invalid Input",
          "Number of clusters must be between 2 and 4.",
          easyClose = TRUE                      # specifies that the modal dialog box can be closed by clicking outside of it or by pressing the escape key
        ))
        return(NULL)
      }
      kmeanss<-kmeans(data_for_kmean,centers = input$nCluster) #performing the k-mean operation 
      kmeanss
      data_table<-data.frame(data_totalspending$customer,   #creating a data frame and adding the cluster vector of each customer to it
                             data_totalspending$age,
                             data_totalspending$total_spending,
                             kmeanss$cluster)
      data_table
      
    }})
  
}
shinyApp(ui = ui, server = server)