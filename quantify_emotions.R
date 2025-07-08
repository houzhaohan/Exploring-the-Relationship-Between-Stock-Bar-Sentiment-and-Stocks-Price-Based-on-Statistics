# 读取正面词汇
positive_words <- readLines('positive.txt', encoding = 'UTF-8')
positive_words <- trimws(positive_words)

# 读取负面词汇
negative_words <- readLines('negative.txt', encoding = 'UTF-8')
negative_words <- trimws(negative_words)

# 读取评论数据
comments <- read.csv('comment.csv', header = FALSE, stringsAsFactors = FALSE, 
                     fileEncoding = 'UTF-8', fill = TRUE)

# 按日期存储评论数据
daily_comments <- list()

# 处理每一行评论
for (i in 1:nrow(comments)) {
  if (ncol(comments) < 5) next  # 确保有5列
  
  text <- trimws(comments[i, 3])
  date_str <- strsplit(trimws(comments[i, 5]), " ")[[1]][1]  # 只取日期部分
  
  if (nchar(text) > 0 && !is.na(date_str) && nchar(date_str) > 0) {
    if (is.null(daily_comments[[date_str]])) {
      daily_comments[[date_str]] <- c()
    }
    daily_comments[[date_str]] <- c(daily_comments[[date_str]], text)
  }
}

# 计算每日情绪值
daily_sentiments <- data.frame(date = character(), mood = numeric(), stringsAsFactors = FALSE)

for (date in names(daily_comments)) {
  texts <- daily_comments[[date]]
  total_score <- 0
  total_chars <- 0
  comment_count <- length(texts)
  
  for (text in texts) {
    score <- 0
    # 计算正面词汇得分
    for (word in positive_words) {
      if (nchar(word) > 0 && grepl(word, text, fixed = TRUE)) {
        count <- lengths(regmatches(text, gregexpr(word, text, fixed = TRUE)))
        score <- score + count
      }
    }
    # 计算负面词汇得分
    for (word in negative_words) {
      if (nchar(word) > 0 && grepl(word, text, fixed = TRUE)) {
        count <- lengths(regmatches(text, gregexpr(word, text, fixed = TRUE)))
        score <- score - count
      }
    }
    total_score <- total_score + score
    total_chars <- total_chars + nchar(text)
  }
  
  # 计算当日情绪值（防止除以零）
  if (total_chars > 0 && comment_count > 0) {
    sentiment <- total_score / (total_chars * comment_count)
  } else {
    sentiment <- 0.0
  }
  
  daily_sentiments <- rbind(daily_sentiments, 
                            data.frame(date = date, mood = sentiment, stringsAsFactors = FALSE))
}

# 按日期排序（转换为日期对象排序）
daily_sentiments$date_obj <- as.Date(paste0("2023-", daily_sentiments$date), format = "%Y-%m-%d")
daily_sentiments <- daily_sentiments[order(daily_sentiments$date_obj), ]
daily_sentiments$date_obj <- NULL  # 移除临时日期列

# 写入结果文件
write.csv(daily_sentiments, file = 'daily_sentiment.csv', row.names = FALSE, 
          fileEncoding = 'UTF-8', quote = FALSE)

cat("每日情绪值计算完成，结果已保存到 daily_sentiment.csv\n")