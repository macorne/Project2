# Read in the data
#my_sample <- read_csv("user_behavior_dataset.csv")

#Fix variable names since these have spaces in some cases
#names(usrbhvr_data) <- gsub("\\([^\\)]+\\)", "",
#                            str_replace_all(names(usrbhvr_data),
#                                            c(" " = "")))

#usrbhvr_data <- usrbhvr_data |> 
#  mutate(
#    AgeF = dplyr::case_when(
#      Age >= 18 & Age < 25 ~ "18-24",
#      Age >= 25 & Age < 35 ~ "25-34",
#      Age >= 35 & Age < 45 ~ "35-44",
#      Age >= 45 & Age < 55 ~ "45-54",
#      Age >= 55            ~ "55+"
#    ),
    # Convert to factor
#    AgeF = factor(
#      AgeF,
#      level = c("18-24", "25-34","35-44","45-54","55+")
#    )
#  ) |>
#  mutate(UBCfac = 
#           factor(UserBehaviorClass, 
#                  levels=c(1,2,3,4,5), 
#                  labels=c("Between 0 and 300 MB/day", 
#                           "Between 300 and 600 MB/day",
#                           "Between 600 and 1,000 MB/day",
#                           "Between 1,000 and 1,500 MB/day",
#                           "More than 1,500 MB/day")),
#         OpSysfac = 
#           factor(OperatingSystem,
#                  levels=c("Android",
#                           "iOS")),
#         DevModfac = 
#           factor(DeviceModel,
#                  levels=c("Google Pixel 5",
#                           "OnePlus 9",
#                           "Samsung Galaxy S21",
#                           "Xiaomi Mi 11",
#                           "iPhone 12")),
#         Genderfac = 
#           factor(Gender,
#                  levels=c("Female","Male"))
#  )

#ID Variable
id_vars <- c("User ID" = "UserID")

#Numerical Variables
num_vars <- c("App Usage Time (min/day)" = "AppUsageTime",
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

#Device Model values
DevModvals <- c( #"Device Models",
  "1" = "Google Pixel 5",
  "2" = "iPhone 12",
  "3" = "OnePlus 9",
  "4" = "Samsung Galaxy S21",
  "5" = "Xiaomi Mi 11")

#Operating System values
OpSysvals <- c( #"Operating Systems",
  "1" = "Android",
  "2" = "iOS")

#Gender values
Gendervals <- c(#"Gender',
  "1" = "Female",
  "2" = "Male")

#User Behavior Class values
UBCvals <- c(#"User Behavior Class",
  "1" = "Between 0 and 300 MB/day", 
  "2" = "Between 300 and 600 MB/day",
  "3" = "Between 600 and 1,000 MB/day",
  "4" = "Between 1,000 and 1,500 MB/day",
  "5" = "More than 1,500 MB/day")
