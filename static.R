## Task 1: Read in the Data and Modify

library(tidyverse)
library(stringr)
library(ggcorrplot)
library(ggdensity)
library(ggpie)

#Read in the cell phone user behavior data
usrbhvr_data <- read_csv("user_behavior_dataset.csv")

#Fix variable names since these have spaces in some cases
names(usrbhvr_data) <- gsub("\\([^\\)]+\\)", "",
                            str_replace_all(names(usrbhvr_data),
                                       c(" " = "")))

# Convert the data into a tibble with factor variables.
# User Behavior Class (ubc)
# MB/day 0 <= x < 300:  ubc = 1
# 300 <= x < 600:  ubc = 2
# 600 <= x < 1000:  ubc = 3
# 1000 <= x < 1500:  ubc = 4
# 1500 <= x:  ubc = 5

usrbhvr_data <- usrbhvr_data |> 
  mutate(
    AgeF = dplyr::case_when(
      Age >= 18 & Age < 25 ~ "18-24",
      Age >= 25 & Age < 35 ~ "25-34",
      Age >= 35 & Age < 45 ~ "35-44",
      Age >= 45 & Age < 55 ~ "45-54",
      Age >= 55            ~ "55+"
    ),
    # Convert to factor
    AgeF = factor(
      AgeF,
      level = c("18-24", "25-34","35-44","45-54","55+")
    )
  ) |>
  mutate(UserBehaviorClass = 
           factor(UserBehaviorClass, 
                  levels=c(1,2,3,4,5), 
                  labels=c("Between 0 and 300 MB/day", 
                             "Between 300 and 600 MB/day",
                             "Between 600 and 1,000 MB/day",
                             "Between 1,000 and 1,500 MB/day",
                             "More than 1,500 MB/day")),
         OperatingSystem = 
           factor(OperatingSystem,
                      levels=c("Android",
                               "iOS")),
         DeviceModel = 
           factor(DeviceModel,
                  levels=c("Google Pixel 5",
                           "OnePlus 9",
                           "Samsung Galaxy S21",
                           "Xiaomi Mi 11",
                           "iPhone 12")),
         Gender = 
           factor(Gender,
                  levels=c("Female","Male"))
         ) |>
  mutate(AppUsageTime = as.double(AppUsageTime))

usrbhvr_data
  

### Produce numerical and graphical summaries to investigate the data.

#### One-way and Two-way contingency tables
# User_Behavior_Class
table(usrbhvr_data$UserBehaviorClass,dnn=list("UserBehaviorClass"))

# Operating_System
table(usrbhvr_data$OperatingSystem,dnn=list("OperatingSystem"))

# Device_Model
table(usrbhvr_data$DeviceModel,dnn=list("DeviceModel"))

# Gender
table(usrbhvr_data$Gender,dnn=list("Gender"))

#User_Behavior_Class + Operating_System
table(usrbhvr_data$UserBehaviorClass,
      usrbhvr_data$OperatingSystem,
      dnn=list("UserBehaviorClass","OperatingSystem"))

#User_Behavior_Class + Device_Model
table(usrbhvr_data$UserBehaviorClass,
      usrbhvr_data$DeviceModel,
      dnn=list("UserBehaviorClass","DeviceModel"))

#User_Behavior_Class + Gender
table(usrbhvr_data$UserBehaviorClass,
      usrbhvr_data$Gender,
      dnn=list("UserBehaviorClass","Gender"))

#Operating_System + Device_Model
table(usrbhvr_data$OperatingSystem,
      usrbhvr_data$DeviceModel,
      dnn=list("OperatingSystem","DeviceModel"))

#Operating_System + Gender
table(usrbhvr_data$OperatingSystem,
      usrbhvr_data$Gender,
      dnn=list("OperatingSystem","Gender"))

#Device_Model + Gender
table(usrbhvr_data$DeviceModel,
      usrbhvr_data$Gender,
      dnn=list("DeviceModel","Gender"))

#### Numerical summaries (means, medians, sds, etc.) for quantitative variables 
#### at levels of categorical variables
#csg = center spread at levels of categorical variables
usrbhvr_data_csg <- usrbhvr_data |>
  group_by(DeviceModel,OperatingSystem,Gender,UserBehaviorClass) |>
  drop_na(DeviceModel,OperatingSystem,Gender,UserBehaviorClass) |>
  summarize(across(c(AppUsageTime,
                     ScreenOnTime,
                     BatteryDrain,
                     NumberofAppsInstalled,
                     DataUsage,
                     Age),
            list("mean" = mean,
                 "median" = median,
                 "st_dev" = sd,
                 "variance" = var,
                 "IQR" = IQR),
            .names = "{.fn}_{.col}"))

usrbhvr_data_csg

#### Create at least six plots

# (1) Histogram for BatteryDrain across OperatingSystem

gkdm <- ggplot(usrbhvr_data, aes(x = BatteryDrain))
gkdm + geom_histogram(alpha=0.5,
                    aes(fill = OperatingSystem, 
                        col=I("black")),
                      position = "identity") + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
  xlab("Battery Drain (mAh/day)") + 
  ylab("Number of Phones") + 
  ggtitle(
    str_wrap(
      "Number of Phones with Battery Drain Rate, by Operating System",
      45)) + 
  theme(plot.title = element_text(hjust = 0.5))

# (2) Histogram for BatteryDrain across Gender

gkdm <- ggplot(usrbhvr_data, aes(x = BatteryDrain))
gkdm + geom_histogram(alpha=0.5,
                      aes(fill = Gender, 
                          col=I("black")),
                      position = "identity") + 
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) + 
  xlab("Battery Drain (mAh/day)") + 
  ylab("Number of Phones") + 
  ggtitle(
    str_wrap(
      "Number of Phones with Battery Drain Rate, by Gender",
      45)) + 
  theme(plot.title = element_text(hjust = 0.5))

# (3) Boxplot for BatteryDrain across Gender

gbpm <- ggplot(usrbhvr_data)
gbpm + 
  geom_boxplot(
    aes(x = DeviceModel, 
        y = BatteryDrain, 
        fill = Gender)) + 
  scale_fill_manual(
    values=c("#E69F00", "#56B4E9")) + 
  xlab("Gender") + 
  ylab("Battery Drain (mAh/day)") + 
  ggtitle(
    str_wrap(
      "Box Plot of Battery Drain Per Day, by Gender",
      45)) + 
  theme(plot.title = element_text(hjust = 0.5))

# (4) Scatterplot for BatteryDrain versus DataUsage, by Operating System

g1 <- ggplot(usrbhvr_data |> drop_na(DataUsage, BatteryDrain, OperatingSystem),
             aes(x = DataUsage, y = BatteryDrain, color = OperatingSystem))
g1 + geom_point() +
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  geom_jitter(width = 0.2, alpha = 0.3)

# (5) Scatterplot for ScreenOnTime versus DataUsage, by Gender

g1 <- ggplot(usrbhvr_data |> drop_na(DataUsage, ScreenOnTime, Gender),
             aes(x = DataUsage, y = ScreenOnTime, color = Gender))
g1 + geom_point() +
  geom_jitter(width = 0.2, alpha = 0.3) +
  facet_wrap(~ Gender)

# (6) Correlation plot (ggcorrplot)
# Compute a correlation matrix for numeric variables
usrbhvr_numdata <- usrbhvr_data |> filter(OperatingSystem=="Android") |> select(
  -c(UserID,Age,AgeF,DeviceModel,OperatingSystem,Gender,UserBehaviorClass))
corr <- round(cor(usrbhvr_numdata), 1)
ggcorrplot(corr)

# (7) Correlation plot (ggdensity)
ggplot(usrbhvr_data, 
       aes(DataUsage, AppUsageTime, fill = AgeF)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21) +
  facet_wrap(vars(OperatingSystem))

# (8) Pie charts
pf <- ggpie(usrbhvr_data |> filter(Gender=="Female"), 
            group_key = "DeviceModel", 
            count_type = "full",
            label_info = "all", 
            label_type = "horizon",
            label_size = 4, 
            label_pos = "out" )
pm <- ggpie(usrbhvr_data |> filter(Gender=="Male"), 
            group_key = "DeviceModel",
            count_type = "full",
            label_info = "all", 
            label_type = "horizon",
            label_size = 4, 
            label_pos = "out" )
cowplot::plot_grid(pf,pm,ncol = 2)

#Numerical Variables
num_vars <- c("User ID" = "UserID",
              "App Usage Time (min/day)" = "AppUsageTime",
              "Screen On Time (hrs/day)" = "ScreenOnTime",
              "Battery Drain (mAh/day)" = "BatteryDrain",
              "Number of Apps Installed" = "NumberofAppsInstalled",
              "Data Usage (MB/day)" = "DataUsage",
              "Age (years)" = "Age")

#Categorical Variables
cat_vars <- c("Device Model" = "DeviceModel",
              "Operating System" = "OperatingSystem",
              "Gender" = "Gender",
              "User Behavior Class" = "UserBehaviorClass",
              "Age Ranges" = "AgeF")