# =====================================
# Script: edit UN population.xlsx
# https://population.un.org/wpp/downloads?folder=Archive&group=Standard%20Projections
# Author: Ke Pang
# Date: 2025-08-13
# Purpose: extract pupulation and annual death from UN population
# Data: "WPP2022" 等
# =====================================

library(dplyr)
library(rio)
library(tidyr)

#1. Death----
meta <- import("Data/population data from UN/WPP2022-death-2021-metadata.xlsx")
data <- import("Data/population data from UN/WPP2022-death-2021-number.xlsx")

row.names(data) <- meta$`Location code`
data$LocationId <- row.names(data)
data  <- data %>%
  select(LocationId,everything())

# 把宽表转成长表
death_long <- data %>%
  pivot_longer(
    cols = -LocationId,
    names_to = "Age",
    values_to = "Deaths"
  )

# 把年龄转换成数字（去掉 100+）
death_long <- death_long %>%
  mutate(Age = ifelse(Age == "100+", 100, as.numeric(Age)))

# 定义 AgeStart 和 AgeEnd
death_long <- death_long %>%
  mutate(
    AgeGroup = ifelse(Age >= 85, "85-999",
                      paste0(floor(Age / 5) * 5, "-",
                             floor(Age / 5) * 5 + 4)),
    AgeStart = as.numeric(sub("-.*", "", AgeGroup)),
    AgeEnd = ifelse(AgeGroup == "85-999", 999,
                    as.numeric(sub(".*-", "", AgeGroup)))
  )

# 按组汇总死亡人数
death <- death_long %>%
  group_by(LocationId, AgeStart, AgeEnd) %>%
  summarise(Value = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(LocationId, AgeStart)

death <- death %>%
  mutate(LocationId = as.numeric(LocationId)) %>%  # 转成数值
  mutate(Value = Value*1000) %>%
  arrange(LocationId, AgeStart)

write.csv(death,"Data/population data from UN/undeath_2022_2021.csv",row.names = FALSE)

#2. Population ----
meta <- import("Data/population data from UN/WPP2022-population-2021-metadata.xlsx")
data <- import("Data/population data from UN/WPP2022-population-2021-number.xlsx")

row.names(data) <- meta$`Location code`
data$LocationId <- row.names(data)
data  <- data %>%
  select(LocationId,everything())

# 把宽表转成长表
population_long <- data %>%
  pivot_longer(
    cols = -LocationId,
    names_to = "Age",
    values_to = "Deaths"
  )

# 把年龄转换成数字（去掉 100+）
population_long <- population_long %>%
  mutate(Age = ifelse(Age == "100+", 100, as.numeric(Age)))

# 定义 AgeStart 和 AgeEnd
population_long <- population_long %>%
  mutate(
    AgeGroup = ifelse(Age >= 85, "85-999",
                      paste0(floor(Age / 5) * 5, "-",
                             floor(Age / 5) * 5 + 4)),
    AgeStart = as.numeric(sub("-.*", "", AgeGroup)),
    AgeEnd = ifelse(AgeGroup == "85-999", 999,
                    as.numeric(sub(".*-", "", AgeGroup)))
  )

# 按组汇总死亡人数
population <- population_long %>%
  group_by(LocationId, AgeStart, AgeEnd) %>%
  summarise(Value = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(LocationId, AgeStart)

population <- population %>%
  mutate(LocationId = as.numeric(LocationId)) %>%  # 转成数值
  mutate(Value = Value*1000) %>%
  arrange(LocationId, AgeStart)

write.csv(population,"Data/population data from UN/unpopulation_2022_2021.csv",row.names = FALSE)
