library(dplyr)
library(ggplot2)
library(swimplot)
library(lubridate)
library(grid)
library(gridExtra)
library(ppsr)

setwd("/Users/philipplange/Desktop/Arbeit/moitb")

data <- readxl::read_xlsx("20230122_MolTB.xlsx")

#data <- data[,1:24]

data$id <- as.numeric(1)

datacov <- filter(data, Therapy_Coverage_Insurance == 1)

# frist date diff
data$days_to_discuss_date <- as.Date(data$Registration_Date)-
                             as.Date(data$MTB_Discussion_Date)

data$days_to_discuss_date <- abs(data$days_to_discuss_date)
data$days_to_discuss_date <- as.numeric(data$days_to_discuss_date)

data$tdd_death_fu <- as.Date(data$MTB_Discussion_Date)-
                     as.Date(data$`Date_Death_vs_Follow-Up`)
data$tdd_death_fu <- abs(data$tdd_death_fu)
data$tdd_death_fu <- as.numeric(data$tdd_death_fu)

# age
data$age <- as.Date(data$Registration_Date)-
            as.Date(data$Date_Birth)

data$age <- as.numeric(data$age/365)




# create identifier 


               
data <- as.data.frame(data)
data$Diagnosis_Category <- as.factor(data$Diagnosis_Category)
data$Metastasis_Registration <- as.factor(data$Metastasis_Registration)
data$Status <- as.factor(data$Status)

swimmer_plot(datacov,id='LMU_ID',end="days_to_discuss_date",,
             id_order='days_to_discuss_date',fill="steelblue",alpha=0.75,width=.8
             , stratify = c("Metastasis_Registration", "Status"))

swimmer_plot(datacov,id='LMU_ID',end="days_to_discuss_date",name_fill = "quarters",
             id_order='days_to_discuss_date',fill="steelblue",alpha=0.75,width=.8)

swimmer_plot(datacov,id='LMU_ID',end="tdd_death_fu",,
             id_order='tdd_death_fu',fill="steelblue",alpha=0.75,width=.8
             , stratify = c("Metastasis_Registration", "Status"))

swimmer_plot(datacov,id='LMU_ID',end="tdd_death_fu", name_col  = "Status",
             id_order='tdd_death_fu',fill="steelblue",alpha=0.75,width=.8)


#Prepare for export option 1 using grid
myTable <- tableGrob(
  tibble2, 
  rows = NULL, 
  theme = ttheme_default(core = list(bg_params = list(fill = "grey99")))
)
#Export to pdf
pdf('datacov.pdf',width = 10)
grid.draw(myTable)
dev.off()



data$quarters <- paste0(year(data$Registration_Date),         # Convert dates to quarters
                             "/0",
                             quarter(data$Registration_Date))

tibble <- data %>%
  group_by(quarters) %>% 
  summarise(n = sum(id))

tibble2 <- datacov %>% 
  group_by(LMU_ID) %>% 
  select(Systemic_Therapy_Lines_Registration, Local_Therapy_Lines_Registration, age, Sex,
         Primary_Metastasis)



ggplot(data=tibble, aes(x = , y = n)) +
  geom_bar(stat = "identity",fill = "steelblue") +
  labs(title = "Sum of Registrations (N=92)",
       y = "Count",
       x = "Date") + theme_bw(base_size = 15)+   
  theme(axis.text.x=element_text(angle=60, hjust=1))



swimmer_plot(databaskets,id='Diagnosis_Category',end="",name_fill = "Alterat_Basket"
             id_order='Diagnosis_Category',fill="steelblue",alpha=0.75,width=.8
             )


databaskets <- data %>% 
  select(c(Treatment_Basket, Diagnosis_Category, Alteration_Basket, LMU_ID))

databaskets <- na.omit(databaskets)

datatbaskets <- data %>% 
  select(c(Treatment_Basket, Diagnosis_Category, LMU_ID))

datatbaskets <- na.omit(databaskets)




data <- data %>% 
  rename(death_fu = 'Date_Death_vs_Follow-Up')
