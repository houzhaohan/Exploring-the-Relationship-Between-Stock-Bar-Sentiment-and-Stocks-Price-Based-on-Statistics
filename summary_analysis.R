# 设置工作目录
setwd("d:/8_python_project/R_QIMO")

# 读取 daily_sentiment.csv
daily_sentiment <- read.csv("daily_sentiment.csv", stringsAsFactors = FALSE)

# 输出 daily_sentiment 的统计摘要
cat("==== daily_sentiment.csv 统计摘要 ====\n")
print(summary(daily_sentiment))

# 读取 stock_600600.csv
stock_data <- read.csv("stock_600600.csv", stringsAsFactors = FALSE)

# 输出 stock_600600.csv 的统计摘要
cat("\n==== stock_600600.csv 统计摘要 ====\n")
print(summary(stock_data))