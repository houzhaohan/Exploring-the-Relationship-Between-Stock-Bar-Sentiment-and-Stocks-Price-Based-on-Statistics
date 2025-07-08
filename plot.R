# 加载必要的包
library(ggplot2)
library(dplyr)
library(scales)

# 读取数据
stock_data <- read.csv("stock_600600.csv") %>%
  select(date = 1, close = 3, volume = 6) %>%
  mutate(date = as.Date(date))

sentiment_data <- read.csv("daily_sentiment.csv") %>%
  group_by(date = as.Date(date)) %>%
  summarise(mood = mean(mood))

combined_data <- inner_join(stock_data, sentiment_data, by = "date")

# 使用标准化缩放情绪值 (Z-score)
scale_mood <- function(x) {
  (x - mean(x)) / sd(x)
}
combined_data <- combined_data %>%
  mutate(
    scaled_mood_close = scale_mood(mood) * sd(close) + mean(close),
    scaled_mood_volume = scale_mood(mood) * sd(volume) + mean(volume)
  )

# 计算合适的纵坐标范围
calc_axis_range <- function(primary, secondary) {
  combined <- c(primary, secondary)
  buffer <- (max(combined) - min(combined)) * 0.1  # 10%缓冲
  c(min(combined) - buffer, max(combined) + buffer)
}

# 设置PNG输出参数
png("sentiment_price.png", width=10, height=6, units="in", res=300)

# 绘制情绪值与收盘价
y_range_close <- calc_axis_range(combined_data$close, combined_data$scaled_mood_close)

print(
  ggplot(combined_data, aes(x = date)) +
    geom_line(aes(y = close, color = "收盘价"), size = 0.8) +
    geom_line(aes(y = scaled_mood_close, color = "情绪值"), size = 0.8, alpha = 0.8) +
    scale_y_continuous(
      name = "收盘价",
      limits = y_range_close,
      sec.axis = sec_axis(
        ~ (. - mean(combined_data$close)) / sd(combined_data$close) * sd(combined_data$mood) + mean(combined_data$mood),
        name = "情绪值"
      )
    ) +
    scale_color_manual(values = c("收盘价" = "#1f77b4", "情绪值" = "#ff7f0e")) +
    labs(title = "情绪值与收盘价走势", x = "日期", color = "指标") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y.right = element_text(color = "#ff7f0e"),
      axis.text.y.right = element_text(color = "#ff7f0e"),
      axis.title.y.left = element_text(color = "#1f77b4"),
      axis.text.y.left = element_text(color = "#1f77b4"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "grey50")
    ) +
    guides(color = guide_legend(override.aes = list(size = 2)))
)

dev.off()

# 第二个PNG输出
png("sentiment_volume.png", width=10, height=6, units="in", res=300)

# 绘制情绪值与成交量
y_range_volume <- calc_axis_range(combined_data$volume, combined_data$scaled_mood_volume)

print(
  ggplot(combined_data, aes(x = date)) +
    geom_line(aes(y = volume, color = "成交量"), size = 0.8) +
    geom_line(aes(y = scaled_mood_volume, color = "情绪值"), size = 0.8, alpha = 0.8) +
    scale_y_continuous(
      name = "成交量",
      labels = comma,
      limits = y_range_volume,
      sec.axis = sec_axis(
        ~ (. - mean(combined_data$volume)) / sd(combined_data$volume) * sd(combined_data$mood) + mean(combined_data$mood),
        name = "情绪值"
      )
    ) +
    scale_color_manual(values = c("成交量" = "#2ca02c", "情绪值" = "#ff7f0e")) +
    labs(title = "情绪值与成交量走势", x = "日期", color = "指标") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.y.right = element_text(color = "#ff7f0e"),
      axis.text.y.right = element_text(color = "#ff7f0e"),
      axis.title.y.left = element_text(color = "#2ca02c"),
      axis.text.y.left = element_text(color = "#2ca02c"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "grey50")
    ) +
    guides(color = guide_legend(override.aes = list(size = 2)))
)

dev.off()