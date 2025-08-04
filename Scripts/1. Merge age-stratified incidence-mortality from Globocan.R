# =====================================
# Script: Merge age-stratified incidence or mortality (number)
# Author: Ke Pang
# Date: 2025-07-04
# Purpose: 从GLOBOCAN数据库下载各年龄段肿瘤发病人数和死亡人数表格，合并表格数据;并拆分0-14年龄段为0-4，5-9，10-14
# Data: "dataset-inc-both-sexes-age-0-14-in-2022-colon.csv" 等
# =====================================
library(dplyr)

#相对路径
data_dir <- "Data/colorectum"

#1. 合并csv表格数据（以colorectum incidence为例）----

# 手动读取所有csv并存入一个list，同时记录年龄上限
age_ends <- c(14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 999)  # 999 for 85+
files <- list(
  "dataset-inc-both-sexes-age-0-14-in-2022-colorectum.csv", #GLOBOCAN第一个年龄段是0-14，这个后面会拆分成0-4，5-9，10-14
  "dataset-inc-both-sexes-age-0-19-in-2022-colorectum.csv", #实操，如果要按5岁一个年龄段，只能这样做减法，GLOBOCAN没法直接选
  "dataset-inc-both-sexes-age-0-24-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-29-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-34-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-39-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-44-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-49-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-54-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-59-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-64-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-69-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-74-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-79-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-age-0-84-in-2022-colorectum.csv",
  "dataset-inc-both-sexes-in-2022-colorectum.csv"  # 85+
)

file_path <- file.path(data_dir, files[1])
df_tmp <- read.csv(file_path)
colnames(df_tmp)

# 读取所有数据，统一处理，只保留Population Code(ISO通用地区代码，这样后面不管是国家还是UNsubregion都可以用)和Number
df_list <- lapply(files, function(fname) {
  file_path <- file.path(data_dir, fname)  
  df <- read.csv(file_path)
  df <- df[, c("Population.code..ISO.UN.", "Number")]
  df <- df %>%
    rename(
      ISOcode = Population.code..ISO.UN.
    )
  return(df)
})

colnames(df_list[[1]])
# 初始化结果列表
results <- list()

# 循环处理每一个年龄段（相邻两个累计数据的差）
for (i in 1:(length(df_list))) {
  if (i == 1) {
    # 第一组，0–14 岁，直接取第一个df
    age_start <- 0
    age_end <- age_ends[i]
    Ri <- df_list[[i]]
  } else {
    # 之后每组，用当前累计减去前一个
    age_start <- age_ends[i - 1] + 1
    age_end <- age_ends[i]
    Ri <- merge(df_list[[i]], df_list[[i - 1]], by = "ISOcode", suffixes = c("_current", "_prev"))
    Ri$Number <- Ri$Number_current - Ri$Number_prev
  }
  
  # 生成每组结果
  result_df <- data.frame(
    ISOcode = Ri$ISOcode,
    AgeStart = rep(age_start, nrow(Ri)),
    AgeEnd = rep(age_end, nrow(Ri)),
    Ri = Ri$Number
  )
  
  # 加入总列表
  results[[i]] <- result_df
}

# 合并所有结果
final_df <- do.call(rbind, results)

# 查看前几行结果
head(final_df)


#2. 拆分0-14岁年龄段为0-4，5-9，10-14，此处忽略0-4，5-9两个年龄段可能发生结直肠肿瘤的风险，我觉得这样比合并0-14年龄段的人口和全因死亡更合理

final_df_split <- final_df %>%
  # 只处理 AgeStart=0 & AgeEnd=14 这行
  filter(AgeStart == 0 & AgeEnd == 14) %>%
  # 为每个原行创建三行新记录
  group_by(ISOcode) %>%
  do({
    data.frame(
      ISOcode = .$ISOcode,
      AgeStart = c(0, 5, 10),
      AgeEnd = c(4, 9, 14),
      Ri = c(0, 0, .$Ri)  # 前两段0，最后一段保留原Ri
    )
  }) %>%
  ungroup()

# 把原来的 0–14 替换成新三段
final_df_new <- final_df %>%
  filter(!(AgeStart == 0 & AgeEnd == 14)) %>%  # 删除原0–14
  bind_rows(final_df_split) %>%                # 加入拆分后的
  arrange(ISOcode, AgeStart)                   # 排序



# 保存为CSV
write.csv(final_df_new, "Data/colorectum/age_specific_incidence.csv", row.names = FALSE)




