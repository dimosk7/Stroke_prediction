
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(caret)
library(randomForest)

# loading data

data = read.csv("C:/Users/30694/Desktop/healthcare-dataset-stroke-data/healthcare-dataset-stroke-data.csv", na = "N/A")

#first, we check data structure

str(data)

# converting columns $hypertension, $heart_disease and $stroke from numeric to factor

data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease)

#we check if our categorical variables have values that don;t affect our prediction 

data %>% 
  group_by(gender) %>%
  summarise(count = n())

data %>% 
  group_by(smoking_status) %>%
  summarise(count = n())

data %>% 
  group_by(work_type) %>%
  summarise(count = n())

data %>% 
  group_by(Residence_type) %>%
  summarise(count = n())

# we delete recordings whose gender is "Other"

data = data[!(data$gender == "Other"),]

## Missing values

#we check for NAs in every column

na = data %>% sapply(function(x) is.na(x) %>% sum())
names(na) = colnames(data)
na

#we want to see if these NAs are related to strokes

 data %>% 
  mutate("bmi_na" = 
           case_when(is.na(bmi) ~ "BMI_NA", 
                     !is.na(bmi) ~ "BMI_VALUE")) %>% 
  group_by(bmi_na) %>%
  summarise("Percentage of stroke" = mean(stroke))   
 

#we notice that "BMI_NA" is related with a significant percentage of stoke. Therefore we decide to add column "bmi_na" to our dataframe
 
data = data %>% 
   mutate(bmi_na = case_when(is.na(bmi) ~ 'BMI - NA' , !is.na(bmi) ~ 'BMI - Value'))

## Data Description
 
#  we observe that 4.9% of our records have "stroke" attribute 
 
(data[data$stroke ==1,] %>% nrow() / length(data$stroke)) %>% round(3) %>% paste0("%")
 

## We examine the relationship between stroke and every variable of our dataset. 
 
# Gender
 
data %>%
   group_by(gender) %>%
   summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3))

# Age

g1 = ggplot(data, aes(x = age, fill = as.factor(stroke))) +
  geom_density(alpha = 0.75)+
  scale_fill_manual(values = c("darkblue", "darkred"))+
  labs(fill = "Stroke") + theme_bw() 

g2 = ggplot(data, aes(y = age, x = as.factor(stroke) ,fill = as.factor(stroke))) +
    geom_boxplot() + scale_fill_manual(values = c("darkblue", "darkred")) +
    labs(x = "Stroke")+ theme_bw() +theme(legend.position = "none") 

grid.arrange(g1 , g2)

# Hypertension

data %>% 
  group_by(hypertension) %>% 
  summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3))

# Heart Disease

data %>% 
  group_by(heart_disease) %>% 
  summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3))

# Ever Married

data %>% 
  group_by(ever_married) %>% 
  summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3))
  
# Work Type

data %>% 
  group_by(work_type) %>% 
  summarise(Count = n(), avg_age = mean(age), stroke_percentage = round(mean(stroke), 3)) %>% 
  arrange(desc(stroke_percentage)

# Residence Type

data %>% 
  group_by(Residence_type) %>% 
  summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3))

# Average glucose level

g3 = ggplot( data , aes(x = avg_glucose_level, fill = as.factor(stroke))) +
    eom_density(alpha = 0.75) + scale_fill_manual(values = c("darkblue", "darkred")) +
    abs(x = "Average glucose level" , fill = "Stroke") + theme_bw()

g4 = ggplot(data , aes(y = avg_glucose_level, x = stroke, fill = as.factor(stroke))) +
    geom_boxplot() + scale_fill_manual(values = c("darkblue", "darkred")) +
    labs(y = "Average glucose level", x = "Stroke") + theme_bw() + theme(legend.position = "none")
  
grid.arrange(g3, g4)

# BMI

g3 = ggplot( data , aes(x = bmi, fill = as.factor(stroke))) +
  geom_density(alpha = 0.75) + scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(x = "BMI" , fill = "Stroke") + theme_bw()

g4 = ggplot(data , aes(y = bmi, x = stroke, fill = as.factor(stroke))) +
  geom_boxplot() + scale_fill_manual(values = c("darkblue", "darkred")) +
  labs(y = "BMI", x = "Stroke") + theme_bw() + theme(legend.position = "none")

grid.arrange(g3, g4)

# Smoking Status

data %>% 
  group_by(smoking_status) %>% 
  summarise(Count = n(), "Percentage of Strokes" = round(mean(stroke), 3)) %>% 
  arrange(desc("Percentage of Strokes"))

data %>% 
  group_by(smoking_status) %>% 
  summarise(count = n(), Avg_stroke = round(mean(stroke), 3))  %>%
    ggplot(aes(x = reorder(smoking_status, desc(Avg_stroke)), y = Avg_stroke, fill = as.factor(smoking_status))) + 
    geom_col() + geom_text(aes(label = count), vjust = 2, size = 5) + labs(x = "", y = "Percentage of Strokes") + theme_bw() + 
    theme(legend.position = "none") + theme(axis.title = element_text(face = 'bold'))

# Ever_married & Age

#It is reasonable to assume that the ever_married status is strongly related with age. 
#We apply logistic regression in order to understand the relationship between the two variables. 

data %>% 
  group_by(ever_married) %>%
  summarise("mean age" = round(mean(age), 1), "Percentage of Strokes" = round(mean(stroke), 3))

      

married = ifelse(data$ever_married == "Yes", 1, 0)

a = glm(married ~ age, data = data, family = binomial())

#Finally, we decide not to include columns $residence_type, $gender and $ever_married



## Dealing with missing values

# Remove bmi column

data1 = data %>% select(-bmi)
    
# Replace NAs with mean value

data2 = data
data2$bmi[is.na(data2$bmi)] = mean(data2$bmi, na.rm = T)

# Replace NAs with linear regression estimation

ln_reg = lm(log(bmi) ~ age + hypertension + heart_disease + avg_glucose_level + smoking_status, data = data[!is.na(data$bmi),] )
summary(ln_reg)

data3 = data

data3$bmi[is.na(data3$bmi)] = exp(predict(ln_reg, newdata =  filter(data3, is.na(bmi))))

# we plot the distribution of imputed values to visually evaluate the imputation

ggplot(data3, aes(x = bmi, fill = bmi_na)) + 
    geom_density(alpha = 0.7) + labs(fill = "", title = "Linear Regression Imputation") +
    scale_fill_manual(values = c("navyblue", "palegreen"), labels = c("Imputed Values", "Non-missing values")) +
    scale_x_continuous(breaks = seq(0,80,10)) + xlim(c(0,80))
  
# Replace NAs with K-Nearest Neighbors

# first we need to normalize data

normalize = function(x , na.rm = TRUE) { (x - min(x , na.rm = na.rm))/(max(x , na.rm = na.rm) - min(x , na.rm = na.rm)) }

data_temp = data
data_temp$hypertension = as.integer(data_temp$hypertension)
data_temp$heart_disease = as.integer(data_temp$heart_disease)

knn_data = data_temp %>% select(bmi, age, hypertension, heart_disease, avg_glucose_level) %>% sapply(FUN = normalize)

knn_data = as.data.frame(knn_data)

knn_data = knn_data %>% mutate(bmi_na = case_when( is.na(bmi) ~ "BMI_NA",
                                        !is.na(bmi) ~ "BMI_VALUE"))

#first we split our dataset into two datasets 

train_knn = knn_data[!is.na(knn_data$bmi), ] 
test_knn = knn_data[is.na(knn_data$bmi), ]
        

#then we split train_knn into training_set and validation_set

set.seed(100)
index = sample(1:nrow(train_knn), 0.7*nrow(train_knn), replace = F) 
training_set = train_knn[index,]
validation_set = train_knn[-index,]

val_mse = c()
d = 1

for (i in 10:100) {
  knn = knnreg(bmi ~ age + hypertension + heart_disease + avg_glucose_level, data = training_set, k = i)
  val_mse[d] = sum((validation_set$bmi - predict(knn, newdata = validation_set))^2)
  d = d + 1
}

qplot(x = 10:100, y = val_mse, geom = "line") + labs(x = "k", y =  "Validation MSE") + 
  theme_bw()

k = 10:100
error = as.data.frame(cbind(k , val_mse))
error[error$val_mse == min(error$val_mse),]

#we find that validation error is minimum for k = 69. But we chose to use k = 40 to avoid underfitting

knn = knnreg(bmi ~ age + hypertension + heart_disease + avg_glucose_level , data = training_set, k = 40)
knn_data$bmi[is.na(knn_data$bmi)] = predict(knn,  newdata = knn_data[is.na(knn_data$bmi),])


ggplot(knn_data, aes(x = bmi, fill = bmi_na)) + geom_density(alpha = 0.7) + 
  scale_fill_manual(values = c("navyblue", "palegreen"), labels = c("Imputed Values", "Non-missing values")) +
  labs(fill = "", title = "KNN Imputation")  + theme_bw() 
  
data_temp$bmi = knn_data$bmi
data_temp$hypertension = as.factor(data_temp$hypertension)
data_temp$heart_disease = as.factor(data_temp$heart_disease)
data4 = data_temp



## Random Forest

# we combine the four datasets into one. That will help us make changes fast in all selected columns

data1 = data1 %>% mutate(bmi = NA, dataset = "data1") %>% select(age:heart_disease, avg_glucose_level, bmi, smoking_status, dataset, stroke)
data2 = data2 %>% mutate(dataset = "data2") %>% select(age:heart_disease, avg_glucose_level, bmi, smoking_status, dataset, stroke)
data3 = data3 %>% mutate(dataset = "data3") %>% select(age:heart_disease, avg_glucose_level, bmi, smoking_status, dataset, stroke)
data4 = data4 %>% mutate(dataset = "data4") %>% select(age:heart_disease, avg_glucose_level, bmi, smoking_status, dataset, stroke)

data_combined = rbind(data1,data2,data3, data4)


# label encoding for smoking_status

data_combined$smoking_status = case_when(
                                          data_combined$smoking_status == "Unknown" ~ 1,
                                          data_combined$smoking_status == "never smoked" ~ 2,
                                          data_combined$smoking_status == "formerly smoked" ~ 3,
                                          data_combined$smoking_status == "smokes" ~ 4)

# columns transformation

data_combined$stroke = as.factor(ifelse(data_combined$stroke == 1, "Stroke", "No Stroke"))
data_combined$hypertension = as.integer(data_combined$hypertension) - 1
data_combined$heart_disease = as.integer(data_combined$heart_disease) - 1



# we save the edited datasets in a list 

data_combined = split(select(data_combined, - dataset), data_combined$dataset)


#we split our datasets into training and testing 

set.seed(100)
index = sample(1:nrow(data_combined$data1), 0.75*nrow(data_combined$data1))
logical_index = (1:nrow(data_combined$data1) %in% index)

training_list = lapply(data_combined, function(x) filter(x, logical_index))
test_list = lapply(data_combined, function(x) filter(x, !logical_index))

# applying Random Forest classification for data4

rf = randomForest(stroke ~ ., data = training_list$data4, ntree = 1000, mtry = 3)

preds_4 = predict(rf, newdata = test_list$data4, type = "prob") %>% as.data.frame()
                   
recall = c()
precision = c()
f1_score = c()
i = 1
threshold = seq(0 ,0.5, 0.01)

for (t in threshold){
  
  pred = ifelse(preds_4$Stroke > t, "Stroke", "No Stroke")
  actual = test_list$data4$stroke
  metrics = case_when(
                      actual == "Stroke" & pred == "Stroke" ~ "tp",
                      actual == "Stroke" & pred == "No Stroke" ~ "fn",
                      actual == "No Stroke" & pred == "Stroke" ~ "fp"
  )
  
  metr_table = metrics %>% table()
  precision[i] = metr_table[3]/(metr_table[3] + metr_table[2])
  recall[i] = metr_table[3]/(metr_table[3] + metr_table[1])
  f1_score[i] = 2 * precision[i] * recall[i]/(precision[i] + recall[i])
  i = i + 1

}
                   
## Metrics plot

data.frame(threshold, precision, recall, f1_score) %>% 
  gather(key = "Metric", value = "Score", 2:4) %>%
    ggplot(aes(x = threshold, y = Score, col = Metric)) + geom_line() + geom_point() +
    scale_y_continuous(breaks = seq(0,1, 0.05))
  
