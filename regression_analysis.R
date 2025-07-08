# 加载必要的库
library(dplyr)
library(ggplot2)
library(lmtest)

# 读取数据
stock_data <- read.csv("stock_600600.csv", header = TRUE, stringsAsFactors = FALSE)
sentiment_data <- read.csv("daily_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)

# 统一日期格式
sentiment_data$date <- as.Date(sentiment_data$date, format = "%Y/%m/%d")
stock_data$date <- as.Date(stock_data$date)

# 合并数据集
merged_data <- merge(stock_data, sentiment_data, by = "date") %>%
  mutate(mood_group = ifelse(mood >= 0, "Positive", "Negative"))

# 1. close ~ mood
model_simple <- lm(close ~ mood, data = merged_data)
summary(model_simple)

p1 <- ggplot(merged_data, aes(x = mood, y = close)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "模型1: close ~ mood", 
       subtitle = "简单线性回归",
       x = "市场情绪", y = "收盘价") +
  theme_minimal()
ggsave("model1.png", p1, width = 8, height = 6, dpi = 300)

# 2. close ~ mood + volume
model_multiple <- lm(close ~ mood + volume, data = merged_data)
summary(model_multiple)

p2 <- ggplot(merged_data, aes(x = mood, y = close, size = volume)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "模型2: close ~ mood + volume", 
       subtitle = "多元线性回归",
       x = "市场情绪", y = "收盘价",
       size = "交易量") +
  theme_minimal() +
  scale_size_continuous(range = c(1, 5))
ggsave("model2.png", p2, width = 8, height = 6, dpi = 300)

# 3. close ~ mood * volume
model_interaction <- lm(close ~ mood * volume, data = merged_data)
summary(model_interaction)

# 按交易量中位数分组
merged_data$volume_group <- ifelse(merged_data$volume > median(merged_data$volume), 
                                   "高交易量", "低交易量")
p3 <- ggplot(merged_data, aes(x = mood, y = close, color = volume_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "模型3: close ~ mood * volume", 
       subtitle = "交互效应模型",
       x = "市场情绪", y = "收盘价",
       color = "交易量分组") +
  theme_minimal() +
  scale_color_manual(values = c("高交易量" = "darkred", "低交易量" = "darkblue"))
ggsave("model3.png", p3, width = 8, height = 6, dpi = 300)

# 4. close ~ mood + I(mood^2)
model_poly <- lm(close ~ mood + I(mood^2), data = merged_data)
summary(model_poly)

p4 <- ggplot(merged_data, aes(x = mood, y = close)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, color = "orange") +
  labs(title = "模型4: close ~ mood + mood²", 
       subtitle = "二次多项式回归",
       x = "市场情绪", y = "收盘价") +
  theme_minimal()
ggsave("model4.png", p4, width = 8, height = 6, dpi = 300)

# 5. close ~ mood + I(mood^2) + volume
model_poly_volume <- lm(close ~ mood + I(mood^2) + volume, data = merged_data)
summary(model_poly_volume)

# 6. close ~ mood + I(mood^2) + volume + I(volume^2)
mood2_volume2 <- lm(close ~ mood + I(mood^2) + volume + I(volume^2), data = merged_data)
summary(mood2_volume2)

# 7. close ~ mood + I(mood^2) + volume + I(volume^2) + mood * volume
mood2_volume2_interaction <- lm(close ~ mood + I(mood^2) + volume + I(volume^2) + mood * volume, data = merged_data)
summary(mood2_volume2_interaction)

# 8. 方差分析
anova_model <- aov(close ~ mood_group, data = merged_data)
summary(anova_model)
p8 <- ggplot(merged_data, aes(x = mood_group, y = close, fill = mood_group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "方差分析: 情绪分组对收盘价的影响",
       x = "情绪分组", y = "收盘价") +
  scale_fill_manual(values = c("Positive" = "gold", "Negative" = "gray70")) +
  theme_minimal()
ggsave("plot8_anova.png", p8, width = 8, height = 6, dpi = 300)

# 9. 模型诊断和检验
# 比较线性模型系列
cat("\n线性模型比较\n")
anova_linear <- anova(model_simple, model_multiple, model_interaction)
print(anova_linear)

# 比较多项式模型系列
cat("\n多项式模型比较\n")
anova_poly <- anova(model_poly, model_poly_volume, mood2_volume2, mood2_volume2_interaction)
print(anova_poly)

# 模型7的详细方差分析
cat("\n完整模型方差分析 (Type I SS)\n")
print(anova(mood2_volume2_interaction))