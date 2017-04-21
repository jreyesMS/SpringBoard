#Phase 1: Upload & binding of the dataset

  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(psych)
  
  URL1 <- "https://docs.google.com/spreadsheets/d/1hcH2Q9pXcJmAxBU82FZTdug_w9X-pTjkC8UjHnxdmyU/pub?gid=1931092666&single=true&output=csv"
  URL1
  URL2 <- "https://docs.google.com/spreadsheets/d/17-7d0oymVuVM-4s5jZAVz28RumprMq5etQiUwxIyP1o/pub?gid=468560914&single=true&output=csv"
  URL2
  URL3 <- "https://docs.google.com/spreadsheets/d/1XS0v1EaO2W402i04VrFUeym6T2oQd2LF1vHCsZmaamQ/pub?gid=2091533647&single=true&output=csv"
  URL3
  
  #retrieving the 3 data sets
  test1 <- read.csv(url(URL1))
  test2 <- read.csv(url(URL2))
  test3 <- read.csv(url(URL3))

  #Merging the 3 data sets  
  raw_data <- rbind(test1, test2, test3)
  data <- raw_data

#Phase 2:  Data Set Inspection
  
  #Viewing the structure of the data
  str(raw_data)
  
  #Viewing the characteristics of each individual variable
  summary(raw_data)
  
  #Analyzing the patient ages
  range(raw_data$Age)
  sort(unique(raw_data$Age))
  count(filter(raw_data, Age == 0)) #Patient with Age value equal to zero
  
  #Analyzing the patient gender
  table(raw_data$Gender)

  #Analyzing the AppointmentDay variable
  table(raw_data$DayOfTheWeek)
    
    #Visualization of the 
  
  #Analyzing the Status values
  table(raw_data$Status)
  
    #Visualization of the Appointment Status
    ggplot(raw_data, aes(x=Status, fill = Status)) + geom_bar() +
      labs(title = "Appointment Status")

    #Percentage of Status "No-Show Values  
    round(count(filter(raw_data, Status == "No-Show")) / (count(filter(raw_data, Status == "No-Show")) + count(filter(raw_data, Status == "Show-Up")))* 100,1)
  
  #Analyzing the Diabetes values
  table(raw_data$Diabetes)
  
  
    #Percentage of Diabetes True Values  
    round(count(filter(raw_data, Diabetes == 1)) / (count(filter(raw_data, Diabetes == 1)) + count(filter(raw_data, Diabetes == 0))) * 100,2)
  
  #Analyzing the Acoolism variables - NOTE:  variable is mispelled
  table(raw_data$Alcoolism)
  
    #Percentage of Alcoolism True Values  
    round(count(filter(raw_data, Alcoolism == 1)) / (count(filter(raw_data, Alcoolism == 1)) + count(filter(raw_data, Alcoolism == 0))) * 100,1)
  
  #Analyzing the HiperTension variables - NOTE:  variable is mispelled
  table(raw_data$HiperTension)
  
    #Percentage of HiperTension True Values  
    round(count(filter(raw_data, HiperTension == 1))/ 
             (count(filter(raw_data, HiperTension == 1)) + count(filter(raw_data, HiperTension == 0)))*100,1)
  
  #Analyzing the Handcap values - NOTE:  variable is mispelled
  table(raw_data$Handcap)

    #Percentage of Handcap True Values  
    round((count(filter(raw_data, Handcap == 1)) + count(filter(raw_data, Handcap == 2)) +
             count(filter(raw_data, Handcap == 3)) + count(filter(raw_data, Handcap == 4)))/ 
            (count(filter(raw_data, Handcap == 1)) + count(filter(raw_data, Handcap == 2)) +
               count(filter(raw_data, Handcap == 3)) + count(filter(raw_data, Handcap == 4)) +
               count(filter(raw_data, Handcap == 0)))*100,2)
  
  #Analyzing the Smokes values
  table(raw_data$Smokes)
  
    #Percentage of Smokes True Values  
    round(count(filter(raw_data, Smokes == 1)) / (count(filter(raw_data, Smokes == 1)) + count(filter(raw_data, Smokes == 0))) * 100,1)
  
  #Analyzing the Tuberculosis values
  table(raw_data$Tuberculosis)

    #Percentage of Tuberculosis True Values  
    round(count(filter(raw_data, Tuberculosis == 1)) / (count(filter(raw_data, Tuberculosis == 1)) + count(filter(raw_data, Tuberculosis == 0))) *100, 5)
  
  #Analyzing the SMS_Reminder values
  table(raw_data$Sms_Reminder)
  
    round((count(filter(raw_data, Sms_Reminder == 1)) + count(filter(raw_data, Sms_Reminder == 2)))/count(raw_data)*100, 2)
    
  #Looking at the AwaitingTime variable
  sort(unique(raw_data$AwaitingTime))
  range(raw_data$AwaitingTime)
  
  #Looking at the Scholarship variable
  table(raw_data$Scholarship)
  
    #Percentage of Scholarship True Values  
    round(count(filter(raw_data, Scholarship == 1)) / (count(filter(raw_data, Scholarship == 1)) + count(filter(raw_data, Scholarship == 0))) * 100)

    
#Phase 3:  Data Wrangling
    
  #Correction of the variable names
  names(data) <- c("Age","Gender", "AppointmentRegistration","ApointmentData","AppointmentDay","Status","Diabetes",
                   "Alcoholism","Hypertension","Handicap","Smokes","Scholarship","Tuberculosis","SMS_Reminder"
                   ,"AwaitingTime")  
  
  #Turning the Deay into a factor
  data$AppointmentDay <- factor(data$AppointmentDay, levels= c("Sunday", "Monday", 
                                                               "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  #Correcting the Handicap column to binary.  NOTE:  No key to suggest what multiple values mean, I have decided to make the column a simple binary column.
  data$Handicap <- ifelse(data$Handicap == 1, 1
                          ,ifelse(data$Handicap == 0, 0, 1 ))
  
  
  #Correcting the Age Columm
  #I will convert all negative values to positive values and 
  #I will add a random value between the min and max age values.
  #There will be some outlier age values but I will remove them when I construct the training/test sets. 
  set.seed(123)
  data$Age <- ifelse(data$Age == 0, round(runif(1, 1, max(data$Age))),
                     ifelse(data$Age < 0, data$Age * -1, data$Age))
  
  #Correcting the SMS_Reminder value of 2.
  #Looking at the quantities for value 2, it constitutes less that 1% of the records, therefore I will correct this column to be a binary column.
  data$SMS_Reminder <- ifelse(data$SMS_Reminder == 2, 1, data$SMS_Reminder)
  
  
  #Converting the AwaitingTime variable from negative values to positive values
  data$AwaitingTime <- ifelse(data$AwaitingTime < 0, data$AwaitingTime * -1, data$AwaitingTime)
  range(data$AwaitingTime)
  sort(unique(data$AwaitingTime))


  #Converting the Age, Handicap, SMS_Reminder, and AwaitingTime variables to 
  data$Age <- as.integer(data$Age)
  data$Handicap <- as.integer(data$Handicap)
  data$AwaitingTime <- as.integer(data$AwaitingTime)
  str(data)
  
  
  #Adding a new behavioral condition column.
  data1 <- mutate(data, Behavioral_condition = ifelse(data$Alcoholism == 1 & data$Smokes == 0, "Alcoholic",
                                                       ifelse(data$Alcoholism == 0 & data$Smokes == 1, "Smoker",
                                                              ifelse(data$Alcoholism == 0 & data$Smokes == 0, "None","Alcoholic/Smoker"))))
  

  
  #Adding a new chronic condition column.
  data2 <- mutate(data1, Chronic_condition = ifelse(data1$Diabetes == 1 & data1$Tuberculosis == 1 & data1$Hypertension == 1, "DM/TB/HT",
                                                    ifelse(data1$Diabetes == 1 & data1$Tuberculosis == 1 & data1$Hypertension == 0, "DM/TB",
                                                           ifelse(data1$Diabetes == 0 & data1$Tuberculosis == 1 & data1$Hypertension == 1, "TB/HT",
                                                                  ifelse(data1$Diabetes == 0 & data1$Tuberculosis == 0 & data1$Hypertension == 1, "HT",
                                                                         ifelse(data1$Diabetes == 0 & data1$Tuberculosis == 1 & data1$Hypertension == 0, "TB",
                                                                                ifelse(data1$Diabetes == 1 & data1$Tuberculosis == 0 & data1$Hypertension == 0, "DM", "None")))))))
  
  
  
  #Changing the SMS reminder from Binary to Yes/No
  data2$SMS_Reminder <- ifelse(data2$SMS_Reminder == 1, "Yes", "No")

    
  #Changing the Gender Column from M/F to Male/Female
  data2$Gender <- ifelse(data2$Gender == "F", "Female", "Male")
  data2$Gender <- factor(data2$Gender, levels = c("Male","Female"))

  
  #Adding an ID column
  data3 <- mutate(data2, id = row.names(data2))
  data3$id <- as.integer(data3$id)

  
  #Outlier Check
    #Age Outlier Check
    AgePlot <- ggplot(data3, aes(x = Gender, y = Age))
    AgePlot +
      geom_point(aes(col = Gender), position = "jitter", alpha = 1/50) +
      scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
      theme(legend.position="none") +
      labs(title = "Gender Age Distribution")
    
      #Outliers for age appear at ages greater than 95.
    
    #AwaitingTime Outlier Check
    AwPlot <- ggplot(data3, aes(x = Status, y = AwaitingTime))
    AwPlot +
      geom_point(aes(col = Status), position = "jitter", alpha = .5) +
      scale_y_continuous() +
      theme(legend.position="none") +
      labs(title = "Awaiting Time Distribution")
    
      #Outliers for AwaitingTime appear to occur at 150 days.
  
  #Removing outliers
    Outliers_Age <- filter(data3, data3$Age > 95)
    Outliers_Awaiting <- filter(data3, data3$AwaitingTime > 150)
    Outliers <- rbind(Outliers_Age, Outliers_Awaiting)
    
  #Cleansed Data Set
  data4 <- filter(data3, !data3$id %in% Outliers$id)
  

#Phase 5:  Exploratory Data Analysis
  
  #Analysis of Appointment Status by Gender
  gender_stat <- table(data4$Gender, data4$Status)
  addmargins(gender_stat)
  
  ggplot(data4, aes(x= Gender, fill = Gender)) + geom_bar() +
    labs(title = "Dataset Gender Count") +
    theme(legend.position="none")
  
  
  ggplot(data4, aes(x = Gender, fill = Status)) + geom_bar(position = "fill")
  
  
  #Appointment Day Analysis
  DoW <- table(data4$AppointmentDay, data4$Status)
  addmargins(DoW)

    #Appointment Day Visualization
    ggplot(data4, aes(x = AppointmentDay, fill = Status)) + geom_bar() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplot(data4, aes(x = AppointmentDay, fill = Status)) + geom_bar(position = "fill") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #Analysis of effectiveness of SMS Messaging
    #Percentage of No-Show Members without an SMS Reminder
    count(filter(data4, SMS_Reminder == "No" & Status == "No-Show")) /
      count(filter(data4, Status == "No-Show")) * 100
    
    #Percentage of No-Show Members with an SMS Reminder
    count(filter(data4, SMS_Reminder == "Yes" & Status == "No-Show")) /
      count(filter(data4, Status == "No-Show")) * 100
    
    #Visualization of SMS Effectiveness
    ggplot(data4, aes(x = SMS_Reminder, fill = Status)) +
      geom_bar() +
      #facet_grid(. ~ Status) +
      scale_y_continuous(breaks = c(0, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000)) +
      labs(title = "Effectiveness of SMS Reminders on No-Shows") +
      labs(x = "SMS Reminder", y = "Count") 
    
    
    ggplot(data4, aes(x = SMS_Reminder, fill = Status)) +
      geom_bar(position = "fill") +
      #facet_grid(. ~ Status) +
      #scale_y_continuous(breaks = c(0, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000, 180000, 200000)) +
      labs(title = "Effectiveness of SMS Reminders on No-Shows") +
      labs(x = "SMS Reminder", y = "Count") 
  
    
  #Analysis of Wait Time
    
    #Analysis of Appointment Status by Waiting Time and Gender
    ggplot(data4, aes(x = Status, y = AwaitingTime, fill = factor(Gender))) + 
      geom_boxplot() +
      facet_grid(Gender ~ .) +
      scale_y_continuous(breaks = AwaitingTime_Axis) +
      theme(legend.position="none") +
      labs(title = "Analysis of Appointment Status by Waiting Time and Gender") +
      labs(x = "Appointment Status", y = "Waiting Time (Days)")
    quantile(data4$AwaitingTime)
    
    
    quantile(data4$AwaitingTime, c(.75))
    count(filter(data4, AwaitingTime <= quantile(data4$AwaitingTime, c(.75))))/
      count(data4) * 100
                 
    
#Phase 6:  Creating the test and training sets
  
  #Filtering the male patients 
  males <- filter(data4, data4$Gender == 'Male')
  
  #Filtering the female patients
  females <- filter(data4, data4$Gender == 'Female')
  
  #Training set for males only - 10% of Males
  set.seed(123)
  males$id <- 1:nrow(males)
  train_males <- males %>% dplyr::sample_frac(.10)
  
  #Training set for females only - 10% of Females
  set.seed(123)
  females$id <- 1:nrow(females)
  train_females <- females %>% dplyr::sample_frac(.10)
  
  #combining the Male and Female training sets to create a Male-Female sample
  mf_sample <- rbind(train_males, train_females)
  
  #Creating the Training Set
  train <- filter(data4, data4$id %in% mf_sample$id)
  
  #Creating the Testing Set
  test <- filter(data4, !data4$id %in% mf_sample$id)

#Phase 7 - Data Analysis/Visualization of training set
  
  #Identifying the No-Show populations
  Train_M <- filter(train, train$Gender == 'Male')
  Train_F <- filter(train, train$Gender == 'Female')
  TrainNS <- filter(train, train$Status == 'No-Show')
  TrainNS_M <- filter(train, train$Gender == 'Male' & train$Status == 'No-Show')
  TrainNS_F <- filter(train, train$Gender == 'Female' & train$Status == 'No-Show')
  
  Age_Axis <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  AwaitingTime_Axis <- c(0,20,40,60,80,100,120,140,160)
 
  
  #Analysis of waiting time
  count(filter(data4, AwaitingTime >= 4 & AwaitingTime <= 22)) /count(unique(data4))
  
  #Analysis of Appointment Status by Age, Chronic Condition, Gender, and Behavioral Habits
  ggplot(train, aes(x = Behavioral_condition, y = Age, fill = Status)) +
    geom_boxplot() +
    facet_grid(Gender ~ Chronic_condition) +
    scale_y_continuous(breaks = Age_Axis) +
    labs(title = "Analysis of Appointment Status by Age, Chronic Condition, Gender, and Behavioral Habits") +
    labs(x = "Chronic Condition", y = "Age") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
    #Males No Show Age Quantiles
        #Healthy - None
        NS_M_None <- filter(train, Status == "No-Show" & Gender == "Male" & 
                              Chronic_condition == "None" & Behavioral_condition == "None")
        
        #Healthy - Smoker
        NS_M_Smoker <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                Chronic_condition == "None" & Behavioral_condition == "Smoker")
        
        #Healthy - Alcoholic
        NS_M_Alcoholic <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                   Chronic_condition == "None" & Behavioral_condition == "Alcoholic")
  
        #Healthy - Smoker/Alcoholic
        NS_M_Both <- filter(train, Status == "No-Show" & Gender == "Male" & 
                              Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker")
        
        #Hypertension - None
        NS_M_HT_None <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                 Chronic_condition == "HT" & Behavioral_condition == "None")
        
        #Hypertension - Smoker
        NS_M_HT_Smoker <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                   Chronic_condition == "HT" & Behavioral_condition == "Smoker")
        
        #Hypertension - Alcoholic
        NS_M_HT_Alcoholic <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                      Chronic_condition == "HT" & Behavioral_condition == "Alcoholic")
        
        #Hypertension - Smoker/Alcoholic
        NS_M_HT_Both <- filter(train, Status == "No-Show" & Gender == "Male" & 
                                 Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker")
        
        #DM - Non-Smokers 
        NS_M_DM <- filter(train, Status == "No-Show" & Gender == "Male" & 
                            Chronic_condition == "DM" & Behavioral_condition == "None")
  
    #Females No Show Age Quantiles
        #Healthy - None
        NS_F_None <- filter(train, Status == "No-Show" & Gender == "Female" & 
                            Chronic_condition == "None" & Behavioral_condition == "None")
        
        #Healthy - Smoker
        NS_F_Smoker <- filter(train, Status == "No-Show" & Gender == "Female" & 
                              Chronic_condition == "None" & Behavioral_condition == "Smoker")
        
        #Healthy - Alcoholic
        NS_F_Alcoholic <- filter(train, Status == "No-Show" & Gender == "Female" & 
                                Chronic_condition == "None" & Behavioral_condition == "Alcoholic")
        
        #Healthy - Smoker/Alcoholic
        NS_F_Both <- filter(train, Status == "No-Show" & Gender == "Female" & 
                                   Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker")
        
        #Hypertension - None
        NS_F_HT_None <- filter(train, Status == "No-Show" & Gender == "Female" & 
                            Chronic_condition == "HT" & Behavioral_condition == "None")
        
        #Hypertension - Smoker
        NS_F_HT_Smoker <- filter(train, Status == "No-Show" & Gender == "Female" & 
                            Chronic_condition == "HT" & Behavioral_condition == "Smoker")
        
        #Hypertension - Alcoholic
        NS_F_HT_Alcoholic <- filter(train, Status == "No-Show" & Gender == "Female" & 
                                   Chronic_condition == "HT" & Behavioral_condition == "Alcoholic")
        
        #Hypertension - Smoker/Alcoholic
        NS_F_HT_Both <- filter(train, Status == "No-Show" & Gender == "Female" & 
                                      Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker")
        
        #DM - Non-Smokers 
        NS_F_DM <- filter(train, Status == "No-Show" & Gender == "Female" & 
                            Chronic_condition == "DM" & Behavioral_condition == "None")
        
        #DM - Smokers 
        NS_F_DM_Smoker <- filter(train, Status == "No-Show" & Gender == "Female" & 
                            Chronic_condition == "DM" & Behavioral_condition == "Smoker")
        
        
        #Male Age Quantiles
        round(quantile(NS_M_None$Age))
        round(quantile(NS_M_Smoker$Age))
        round(quantile(NS_M_Alcoholic$Age))
        round(quantile(NS_M_Both$Age))
        round(quantile(NS_M_HT_None$Age))
        round(quantile(NS_M_HT_Smoker$Age))
        round(quantile(NS_M_HT_Alcoholic$Age))
        round(quantile(NS_M_HT_Both$Age))
        round(quantile(NS_M_DM$Age))
        
        #Female Age Quantiles
        round(quantile(NS_F_None$Age))
        round(quantile(NS_F_Smoker$Age))
        round(quantile(NS_F_Alcoholic$Age))
        round(quantile(NS_F_Both$Age))
        round(quantile(NS_F_HT_None$Age))
        round(quantile(NS_F_HT_Smoker$Age))
        round(quantile(NS_F_HT_Alcoholic$Age))
        round(quantile(NS_F_HT_Both$Age))
        round(quantile(NS_F_DM$Age))
        round(quantile(NS_F_DM_Smoker$Age))
      

#Phase 8 - No Show Indicators
  
  #Males (9 cases)
    #Healthy-None between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Smoker between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Alcoholic between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Both between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_None between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Smoker between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Alcoholic between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Both between 25th and 75th Waiting Day & Age Quartiles
    #Diabetic_None between 25th and 75th Waiting Day & Age Quartiles
  
  #Females (10 cases)
    #Healthy-None between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Smoker between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Alcoholic between 25th and 75th Waiting Day & Age Quartiles
    #Healthy-Both between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_None between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Smoker between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Alcoholic between 25th and 75th Waiting Day & Age Quartiles
    #Hypertension_Both between 25th and 75th Waiting Day & Age Quartiles
    #Diabetic_None between 25th and 75th Waiting Day & Age Quartiles
    #Diabetic_Smoker between 25th and 75th Waiting Day & Age Quartiles
  
  
#Phase 9 - Applying the training cases to the testing set
  
  Train_prediction <- mutate(train, NS_Prediction = 
                         #Case 1: Male - Healthy-None
                         ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "None" &
                                                        (Age >= round(quantile(NS_M_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_None$Age, c(.75)))) &
                                                       (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                
                                #Case 2: Male - Healthy-Smoker
                                ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Smoker" &
                                         (Age >= round(quantile(NS_M_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_M_Smoker$Age, c(.75)))) &
                                         (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                       
                                       #Case 3: Male - Healthy-Alcoholic
                                       ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic" &
                                                (Age >= round(quantile(NS_M_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_M_Alcoholic$Age, c(.75)))) &
                                                (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                              
                                              #Case 4: Male - Healthy-Both
                                              ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker" &
                                                       (Age >= round(quantile(NS_M_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_None$Age, c(.75)))) &
                                                       (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                     
                                                     #Case 5: Male - Hypertension-None
                                                     ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "None" &
                                                              (Age >= round(quantile(NS_M_HT_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_None$Age, c(.75)))) &
                                                              (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                            
                                                            #Case 6: Male - Hypertension-Smoker
                                                            ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Smoker" &
                                                                     (Age >= round(quantile(NS_M_HT_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Smoker$Age, c(.75)))) &
                                                                     (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                   
                                                                   #Case 7: Male - Hypertension-Alcoholic
                                                                   ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic" &
                                                                            (Age >= round(quantile(NS_M_HT_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Alcoholic$Age, c(.75)))) &
                                                                            (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                          
                                                                          #Case 8: Male - Hypertension-Both
                                                                          ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                   (Age >= round(quantile(NS_M_HT_Both$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Both$Age, c(.75)))) &
                                                                                   (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                 
                                                                                 #Case 9: Male - Diabetic-None
                                                                                 ifelse(Gender == "Male" & Chronic_condition == "DM" & Behavioral_condition == "None" &
                                                                                          (Age >= round(quantile(NS_M_DM$Age, c(.25))) & train$Age <= round(quantile(NS_M_DM$Age, c(.75)))) &
                                                                                          (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                        
                                                                                        #Case 10: Female - Healthy-None
                                                                                        ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "None" &
                                                                                                 (Age >= round(quantile(NS_F_None$Age, c(.25))) & train$Age <= round(quantile(NS_F_None$Age, c(.75)))) &
                                                                                                 (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                               
                                                                                               #Case 11: Female - Healthy-Smoker
                                                                                               ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Smoker" &
                                                                                                        (Age >= round(quantile(NS_F_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_Smoker$Age, c(.75)))) &
                                                                                                        (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                      
                                                                                                      #Case 12: Female - Healthy-Alcoholic
                                                                                                      ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic" &
                                                                                                               (Age >= round(quantile(NS_F_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_F_Alcoholic$Age, c(.75)))) &
                                                                                                               (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                             
                                                                                                             #Case 13: Female - Healthy-Both
                                                                                                             ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                                                      (Age >= round(quantile(NS_F_Both$Age, c(.25))) & train$Age <= round(quantile(NS_F_Both$Age, c(.75)))) &
                                                                                                                      (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                    
                                                                                                                    #Case 14: Female - Hypertension-None
                                                                                                                    ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "None" &
                                                                                                                             (Age >= round(quantile(NS_F_HT_None$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_None$Age, c(.75)))) &
                                                                                                                             (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                           
                                                                                                                           #Case 15: Female - Hypertension-Smoker
                                                                                                                           ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Smoker" &
                                                                                                                                    (Age >= round(quantile(NS_F_HT_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Smoker$Age, c(.75)))) &
                                                                                                                                    (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                  
                                                                                                                                  #Case 16: Female - Hypertension-Alcoholic
                                                                                                                                  ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic" &
                                                                                                                                           (Age >= round(quantile(NS_F_HT_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Alcoholic$Age, c(.75)))) &
                                                                                                                                           (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                         
                                                                                                                                         #Case 17: Female - Hypertension-Both
                                                                                                                                         ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                                                                                  (Age >= round(quantile(NS_F_HT_Both$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Both$Age, c(.75)))) &
                                                                                                                                                  (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                
                                                                                                                                                #Case 18: Male - Diabetic-None
                                                                                                                                                ifelse(Gender == "Female" & Chronic_condition == "DM" & Behavioral_condition == "None" &
                                                                                                                                                         (Age >= round(quantile(NS_F_DM$Age, c(.25))) & train$Age <= round(quantile(NS_F_DM$Age, c(.75)))) &
                                                                                                                                                         (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                       
                                                                                                                                                       #Case 19: Female - Diabetic-Smoker
                                                                                                                                                       ifelse(Gender == "Female" & Chronic_condition == "DM" & Behavioral_condition == "Smoker" &
                                                                                                                                                                (Age >= round(quantile(NS_F_DM_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_DM_Smoker$Age, c(.75)))) &
                                                                                                                                                                (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                              "Show-Up"))))))))))))))))))))
  
                                       
  Train_Results <- mutate(Train_prediction, Result = ifelse(Status == "No-Show" & NS_Prediction == "No-Show", "NS_Correct",
                                                                 ifelse(Status == "No-Show" & NS_Prediction == "Show-Up", "NS_FalsePositive", 
                                                                        ifelse(Status == "Show-Up" & NS_Prediction == "No-Show", "NS_FalseNegative", "SU_Correct"))))
  
  View(Train_Results)
  #Visualizing the Predictions
  count_train <- c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000)
  ggplot(Train_Results, aes(x = Result)) +
    geom_bar(aes(col = Result, fill = Result)) + 
    scale_y_continuous(breaks = count_train) +
    labs(title = "Training Set Prediction Results") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  count(Train_Results, Result)

  # Handicap No-Shows
  HandicapNS <- filter(Train_Results, Handicap == 1)
  HandicapNS_scale <- c(1)
  ggplot(HandicapNS, aes(x = Handicap, fill = Status)) +
    geom_bar() +
    labs(title = "Handicap No-Show") +
    scale_x_continuous(breaks = HandicapNS_scale)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ##SMS ineffectiveness
  ggplot(Train_Results, aes(x = Status, fill = SMS_Reminder)) +
    geom_bar() +
    labs(title = "SMS Effectivess")
  
  #Measuring Training Set - Correct No-Show predictions
  round(count(filter(Train_Results, Result == "NS_Correct"))/
    count(filter(Train_Results, Status == "No-Show"))*100,2)
  
  
  #Measuring Training Set - InCorrect No-Show predictions
  round(count(filter(Train_Results, Result ==  "NS_FalsePositive"))/
    count(filter(Train_Results, Status == "No-Show"))*100,2)
  
  
  #Measuring Training Set - Correct Show-Up predictions
  round(count(filter(Train_Results, Result == "SU_Correct"))/
    count(filter(Train_Results, Status == "Show-Up"))*100,2)
  
  
  #Measuring Training Set - InCorrect Show-Up predictions
  round(count(filter(Train_Results, Result ==  "NS_FalseNegative"))/
    count(filter(Train_Results, Status == "Show-Up"))*100,2)
  
  
  #Measuring the overall success of predictions
  round((count(filter(Train_Results, Result ==  "NS_Correct")) + 
           count(filter(Train_Results, Result ==  "SU_Correct"))) /
          count(unique(Train_Results))*100,2)

  
  #Phase 10 - Applying the test cases to the training set
  
  Test_prediction <- mutate(test, NS_Prediction = 
                               #Case 1: Male - Healthy-None
                               ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "None" &
                                        (Age >= round(quantile(NS_M_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_None$Age, c(.75)))) &
                                        (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                      
                                      #Case 2: Male - Healthy-Smoker
                                      ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Smoker" &
                                               (Age >= round(quantile(NS_M_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_M_Smoker$Age, c(.75)))) &
                                               (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                             
                                             #Case 3: Male - Healthy-Alcoholic
                                             ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic" &
                                                      (Age >= round(quantile(NS_M_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_M_Alcoholic$Age, c(.75)))) &
                                                      (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                    
                                                    #Case 4: Male - Healthy-Both
                                                    ifelse(Gender == "Male" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker" &
                                                             (Age >= round(quantile(NS_M_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_None$Age, c(.75)))) &
                                                             (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                           
                                                           #Case 5: Male - Hypertension-None
                                                           ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "None" &
                                                                    (Age >= round(quantile(NS_M_HT_None$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_None$Age, c(.75)))) &
                                                                    (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                  
                                                                  #Case 6: Male - Hypertension-Smoker
                                                                  ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Smoker" &
                                                                           (Age >= round(quantile(NS_M_HT_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Smoker$Age, c(.75)))) &
                                                                           (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                         
                                                                         #Case 7: Male - Hypertension-Alcoholic
                                                                         ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic" &
                                                                                  (Age >= round(quantile(NS_M_HT_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Alcoholic$Age, c(.75)))) &
                                                                                  (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                
                                                                                #Case 8: Male - Hypertension-Both
                                                                                ifelse(Gender == "Male" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                         (Age >= round(quantile(NS_M_HT_Both$Age, c(.25))) & train$Age <= round(quantile(NS_M_HT_Both$Age, c(.75)))) &
                                                                                         (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                       
                                                                                       #Case 9: Male - Diabetic-None
                                                                                       ifelse(Gender == "Male" & Chronic_condition == "DM" & Behavioral_condition == "None" &
                                                                                                (Age >= round(quantile(NS_M_DM$Age, c(.25))) & train$Age <= round(quantile(NS_M_DM$Age, c(.75)))) &
                                                                                                (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                              
                                                                                              #Case 10: Female - Healthy-None
                                                                                              ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "None" &
                                                                                                       (Age >= round(quantile(NS_F_None$Age, c(.25))) & train$Age <= round(quantile(NS_F_None$Age, c(.75)))) &
                                                                                                       (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                     
                                                                                                     #Case 11: Female - Healthy-Smoker
                                                                                                     ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Smoker" &
                                                                                                              (Age >= round(quantile(NS_F_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_Smoker$Age, c(.75)))) &
                                                                                                              (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                            
                                                                                                            #Case 12: Female - Healthy-Alcoholic
                                                                                                            ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic" &
                                                                                                                     (Age >= round(quantile(NS_F_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_F_Alcoholic$Age, c(.75)))) &
                                                                                                                     (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                   
                                                                                                                   #Case 13: Female - Healthy-Both
                                                                                                                   ifelse(Gender == "Female" & Chronic_condition == "None" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                                                            (Age >= round(quantile(NS_F_Both$Age, c(.25))) & train$Age <= round(quantile(NS_F_Both$Age, c(.75)))) &
                                                                                                                            (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                          
                                                                                                                          #Case 14: Female - Hypertension-None
                                                                                                                          ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "None" &
                                                                                                                                   (Age >= round(quantile(NS_F_HT_None$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_None$Age, c(.75)))) &
                                                                                                                                   (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                 
                                                                                                                                 #Case 15: Female - Hypertension-Smoker
                                                                                                                                 ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Smoker" &
                                                                                                                                          (Age >= round(quantile(NS_F_HT_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Smoker$Age, c(.75)))) &
                                                                                                                                          (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                        
                                                                                                                                        #Case 16: Female - Hypertension-Alcoholic
                                                                                                                                        ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic" &
                                                                                                                                                 (Age >= round(quantile(NS_F_HT_Alcoholic$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Alcoholic$Age, c(.75)))) &
                                                                                                                                                 (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                               
                                                                                                                                               #Case 17: Female - Hypertension-Both
                                                                                                                                               ifelse(Gender == "Female" & Chronic_condition == "HT" & Behavioral_condition == "Alcoholic/Smoker" &
                                                                                                                                                        (Age >= round(quantile(NS_F_HT_Both$Age, c(.25))) & train$Age <= round(quantile(NS_F_HT_Both$Age, c(.75)))) &
                                                                                                                                                        (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                      
                                                                                                                                                      #Case 18: Male - Diabetic-None
                                                                                                                                                      ifelse(Gender == "Female" & Chronic_condition == "DM" & Behavioral_condition == "None" &
                                                                                                                                                               (Age >= round(quantile(NS_F_DM$Age, c(.25))) & train$Age <= round(quantile(NS_F_DM$Age, c(.75)))) &
                                                                                                                                                               (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                             
                                                                                                                                                             #Case 19: Female - Diabetic-Smoker
                                                                                                                                                             ifelse(Gender == "Female" & Chronic_condition == "DM" & Behavioral_condition == "Smoker" &
                                                                                                                                                                      (Age >= round(quantile(NS_F_DM_Smoker$Age, c(.25))) & train$Age <= round(quantile(NS_F_DM_Smoker$Age, c(.75)))) &
                                                                                                                                                                      (AwaitingTime  >=  quantile(TrainNS_M$AwaitingTime, c(.25)) & AwaitingTime  <=  quantile(TrainNS_M$AwaitingTime, c(.75))), "No-Show",
                                                                                                                                                                    "Show-Up"))))))))))))))))))))
  
  
  Test_Results <- mutate(Test_prediction, Result = ifelse(Status == "No-Show" & NS_Prediction == "No-Show", "NS_Correct",
                                                            ifelse(Status == "No-Show" & NS_Prediction == "Show-Up", "NS_FalsePositive", 
                                                                   ifelse(Status == "Show-Up" & NS_Prediction == "No-Show", "NS_FalseNegative", "SU_Correct"))))
  
                                
  #Visualizing the Predictions
  ggplot(Test_Results, aes(x = Result)) +
    geom_bar(aes(col = Result, fill = Result)) + 
    labs(title = "Testing Set Prediction Results - 25th to 75th quartile") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  table(Test_Results$Result)
  
  # Handicap No-Shows
  HandicapNS <- filter(Test_Results, Handicap == 1)
  HandicapNS_scale <- c(1)
  ggplot(HandicapNS, aes(x = Handicap, fill = Status)) +
    geom_bar() +
    labs(title = "Handicap No-Show") +
    scale_x_continuous(breaks = HandicapNS_scale)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ##SMS ineffectiveness
  ggplot(Test_Results, aes(x = Status, fill = SMS_Reminder)) +
    geom_bar() +
    labs(title = "SMS Effectivess")
  
  #Measuring Testing Set - Correct No-Show predictions
  round(count(filter(Test_Results, Result == "NS_Correct"))/
    count(filter(Test_Results, Status == "No-Show")) * 100,2)
  
  
  #Measuring Testing Set - InCorrect No-Show predictions
  round(count(filter(Test_Results, Result ==  "NS_FalsePositive"))/
    count(filter(Test_Results, Status == "No-Show")) * 100,2)
  
  
  #Measuring Testing Set - Correct Show-Up predictions
  round(count(filter(Test_Results, Result == "SU_Correct"))/
    count(filter(Test_Results, Status == "Show-Up")) * 100,2)
  
  
  #Measuring Testing Set - InCorrect Show-Up predictions
  round(count(filter(Test_Results, Result ==  "NS_FalseNegative"))/
    count(filter(Test_Results, Status == "Show-Up")) * 100,2)
  
  
  #Measuring the overall success of predictions
 round((count(filter(Test_Results, Result ==  "NS_Correct")) + 
      count(filter(Test_Results, Result ==  "SU_Correct"))) /
    count(unique(Test_Results))*100,2)
                        


  

  

  
  



  
  
