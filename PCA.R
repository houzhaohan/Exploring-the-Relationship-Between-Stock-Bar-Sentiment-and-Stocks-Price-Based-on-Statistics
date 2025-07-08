# 读取股票数据
stock_data <- read.csv("stock_600600.csv")
stock_data$date <- as.Date(stock_data$date)

# 读取情绪数据
sentiment_data <- read.csv("daily_sentiment.csv")
sentiment_data$date <- as.Date(sentiment_data$date, format = "%Y/%m/%d")

# 合并数据集
merged_data <- merge(stock_data, sentiment_data, by = "date")

# 数据预处理：计算每日价格波动幅度和价格变化
merged_data$price_range <- merged_data$high - merged_data$low
merged_data$price_change <- merged_data$close - merged_data$open

# 创建PCA分析数据集（包含技术指标和情绪指标）
pca_data <- merged_data[, c("volume", "price_range", "price_change", "mood")]

# 数据标准化
scaled_data <- scale(pca_data)

# 执行主成分分析
pca_result <- prcomp(scaled_data, center = FALSE, scale. = FALSE)  # 已手动标准化

# 分析结果摘要
summary(pca_result)

# 输出主成分载荷（旋转矩阵）
print("主成分载荷矩阵：")
print(pca_result$rotation)

# 输出前四个样本的主成分得分
print("前四个样本的主成分得分：")
print(head(pca_result$x, 4))

# 计算各主成分解释的方差比例
variance_proportion <- pca_result$sdev^2 / sum(pca_result$sdev^2)
print("各主成分解释的方差比例：")
print(variance_proportion)

# 保存方差解释比例图
png("variance_explained.png", width = 800, height = 600, res = 100)  # 设置分辨率
plot(variance_proportion, 
     type = "b", 
     main = "主成分方差解释比例",
     xlab = "主成分", 
     ylab = "解释方差比例",
     col = "blue", 
     pch = 19)
dev.off()