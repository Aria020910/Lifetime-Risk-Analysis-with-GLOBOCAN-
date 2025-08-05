# =====================================
# Script: AMP-based Lifetime risk
# Author: Ke Pang
# Date: 2025-07-15
# Purpose: AMP方法计算终生风险
# Data: "Data/longdf_colorectal.csv" 等
# =====================================


library(dplyr)

#1. AMP计算终生风险的函数----

calculate_lifetime_risk <- function(df) {
  # 添加宽度列并处理85+组
  df <- df %>%
    mutate(
      width = ifelse(AgeEnd == 999, NA, AgeEnd - AgeStart + 1),
      is_last_group = (AgeEnd == 999)  # 标记是否为最后一个年龄组(85+)
    )
  
  # 按地区和年龄排序
  df <- df %>% arrange(Region, RegionType,AgeStart)
  
  # 计算每个地区的终生风险
  results <- df %>%
    group_by(Region,RegionType) %>%
    group_modify(~ {
      data <- .x
      n <- nrow(data)
      
      # 计算健康损耗率 = (R + M - D)/N
      data$hazard_rate <- (data$Ri + data$Mi - data$Di)/data$Ni
      
      # 计算累积风险（到达当前年龄组前的累积）
      cum_hazard <- cumsum(c(0, head(data$hazard_rate, -1)))
      data$cum_hazard_before <- cum_hazard[1:n]
      
      # 计算生存概率 S0*(a_i)
      data$S0_star <- exp(-data$cum_hazard_before)
      
      # 计算分母 (R + M - D)
      data$denom <- data$Ri + data$Mi - data$Di
      
      # 计算每个年龄组的风险贡献
      data$term <- ifelse(
        data$is_last_group,
        # 85+组使用简化公式
        (data$Ri / data$denom) * data$S0_star,
        # 其他年龄组使用完整公式
        (data$Ri / data$denom) * data$S0_star * 
          (1 - exp(-(data$width / data$Ni) * data$denom))
      )
      
      # 处理分母为零的情况
      data$term[is.na(data$term) | is.infinite(data$term)] <- 0
      
      # 返回结果
      data
    }) %>%
    summarise(lifetime_risk = sum(term, na.rm = TRUE)) %>%
    ungroup()
  
  return(results)
}

#2. 读取数据----

data_colorectum <- read.csv("Data/longdf_colorectal.csv", stringsAsFactors = FALSE)
data_rectum <- read.csv("Data/longdf_rectum.csv",stringsAsFactors = FALSE)

#3. 计算终生风险----
results_colorectum <- calculate_lifetime_risk(data_colorectum)
results_rectum <- calculate_lifetime_risk(data_rectum)

print(results_rectum)

#4. 保存----
write.csv(results_colorectum, "Data/lifetime_risk_colorectum.csv", row.names = FALSE)
write.csv(results_rectum, "Data/lifetime_risk_rectum.csv", row.names = FALSE)

