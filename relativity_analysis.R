library(dplyr)

# 读取股票数据
stock_data <- read.csv("stock_600600.csv", stringsAsFactors = FALSE)
# 读取情绪数据
sentiment_data <- read.csv("daily_sentiment.csv", stringsAsFactors = FALSE)

# 预处理股票数据
stock_processed <- stock_data %>%
  select(date, close = names(.)[3], volume = names(.)[6]) %>%
  mutate(
    date = as.Date(date),
    close = as.numeric(close),
    volume = as.numeric(volume)
  )

# 预处理情绪数据
sentiment_processed <- sentiment_data %>%
  rename(mood = names(.)[2]) %>%
  mutate(
    date = as.Date(date),
    mood = as.numeric(mood)
  ) %>%
  group_by(date) %>%
  summarise(mood_mean = mean(mood, na.rm = TRUE))

# 合并两个数据集并按日期排序
merged_data <- inner_join(stock_processed, sentiment_processed, by = "date") %>%
  arrange(date)  # 确保按日期排序

# 创建滞后变量
merged_data <- merged_data %>%
  mutate(
    lag1_mood = lag(mood_mean, 1),  # 前一日情绪值
    lag2_avg_mood = (lag(mood_mean, 1) + lag(mood_mean, 2)) / 2  # 前两日情绪平均值
  )

# 进行相关性分析
cor_close <- cor.test(merged_data$mood_mean, merged_data$close, method = "pearson", use = "complete.obs")
cor_volume <- cor.test(merged_data$mood_mean, merged_data$volume, method = "pearson", use = "complete.obs")

# 新增滞后变量相关性分析
cor_lag1_close <- cor.test(merged_data$lag1_mood, merged_data$close, method = "pearson", use = "complete.obs")
cor_lag2_avg_close <- cor.test(merged_data$lag2_avg_mood, merged_data$close, method = "pearson", use = "complete.obs")

# 打印结果
cat("当日情绪值与收盘价的相关性分析结果:\n")
print(cor_close)
cat("\n前一日情绪值与当日收盘价的相关性分析结果:\n")
print(cor_lag1_close)
cat("\n前两日情绪平均值与当日收盘价的相关性分析结果:\n")
print(cor_lag2_avg_close)
cat("\n情绪值与成交量的相关性分析结果:\n")
print(cor_volume)

# 保存情绪值 vs 收盘价图形
png("sentiment_vs_close.png", width = 1000, height = 1000, res = 100)
plot(merged_data$mood_mean, merged_data$close,
     main = "当日情绪值 vs 收盘价",
     xlab = "情绪值", ylab = "收盘价",
     pch = 19, col = "blue")
abline(lm(close ~ mood_mean, data = merged_data), col = "red")
# 添加相关系数和p值标注
cor_text <- sprintf("r = %.3f\np = %.4f", cor_close$estimate, cor_close$p.value)
text(x = min(merged_data$mood_mean, na.rm = TRUE), 
     y = max(merged_data$close, na.rm = TRUE), 
     labels = cor_text,
     pos = 4, col = "darkred", cex = 1.2)
dev.off()

# 保存前一日情绪值 vs 收盘价图形
png("lag1_sentiment_vs_close.png", width = 1000, height = 1000, res = 100)
plot(merged_data$lag1_mood, merged_data$close,
     main = "前一日情绪值 vs 当日收盘价",
     xlab = "前一日情绪值", ylab = "收盘价",
     pch = 19, col = "purple")
abline(lm(close ~ lag1_mood, data = merged_data), col = "red")
# 添加相关系数和p值标注
cor_text <- sprintf("r = %.3f\np = %.4f", cor_lag1_close$estimate, cor_lag1_close$p.value)
text(x = min(merged_data$lag1_mood, na.rm = TRUE), 
     y = max(merged_data$close, na.rm = TRUE), 
     labels = cor_text,
     pos = 4, col = "darkred", cex = 1.2)
dev.off()

# 保存前两日情绪均值 vs 收盘价图形
png("lag2_avg_sentiment_vs_close.png", width = 1000, height = 1000, res = 100)
plot(merged_data$lag2_avg_mood, merged_data$close,
     main = "前两日情绪均值 vs 当日收盘价",
     xlab = "前两日情绪均值", ylab = "收盘价",
     pch = 19, col = "orange")
abline(lm(close ~ lag2_avg_mood, data = merged_data), col = "red")
# 添加相关系数和p值标注
cor_text <- sprintf("r = %.3f\np = %.4f", cor_lag2_avg_close$estimate, cor_lag2_avg_close$p.value)
text(x = min(merged_data$lag2_avg_mood, na.rm = TRUE), 
     y = max(merged_data$close, na.rm = TRUE), 
     labels = cor_text,
     pos = 4, col = "darkred", cex = 1.2)
dev.off()

# 保存情绪值 vs 成交量图形
png("sentiment_vs_volume.png", width = 1000, height = 1000, res = 100)
plot(merged_data$mood_mean, merged_data$volume,
     main = "情绪值 vs 成交量",
     xlab = "情绪值", ylab = "成交量",
     pch = 19, col = "darkgreen")
abline(lm(volume ~ mood_mean, data = merged_data), col = "red")
# 添加相关系数和p值标注
cor_text <- sprintf("r = %.3f\np = %.4f", cor_volume$estimate, cor_volume$p.value)
text(x = min(merged_data$mood_mean, na.rm = TRUE), 
     y = max(merged_data$volume, na.rm = TRUE), 
     labels = cor_text,
     pos = 4, col = "darkred", cex = 1.2)
dev.off()

# 提示信息
cat("\n图形已保存到工作目录：\n")
cat("当日情绪值 vs 收盘价: sentiment_vs_close.png\n")
cat("前一日情绪值 vs 收盘价: lag1_sentiment_vs_close.png\n")
cat("前两日情绪均值 vs 收盘价: lag2_avg_sentiment_vs_close.png\n")
cat("情绪值 vs 成交量: sentiment_vs_volume.png\n")