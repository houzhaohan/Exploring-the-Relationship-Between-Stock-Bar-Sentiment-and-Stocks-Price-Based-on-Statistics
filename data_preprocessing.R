extract_and_convert <- function(input_file, output_file) {
  # 读取文件所有行
  lines <- readLines(input_file, encoding = "UTF-8")
  
  # 初始化变量
  records <- list()
  current_group <- character()
  start_processing <- FALSE
  
  # 处理每一行
  for (line in lines) {
    stripped_line <- trimws(line)  # 去除首尾空白
    
    if (stripped_line == "最后更新") {
      start_processing <- TRUE
      next
    }
    
    if (stripped_line == "emytylist") {
      start_processing <- FALSE
      break
    }
    
    if (start_processing) {
      current_group <- c(current_group, stripped_line)
      
      # 每5行组成一个记录
      if (length(current_group) == 5) {
        records <- c(records, list(current_group))
        current_group <- character()
      }
    }
  }
  
  # 将列表转换为数据框
  if (length(records) > 0) {
    # 计算最大列数以处理不完整记录
    max_cols <- max(sapply(records, length))
    
    # 创建数据框
    df <- as.data.frame(
      do.call(rbind, lapply(records, function(x) {
        c(x, rep(NA, max_cols - length(x)))
      })),
      stringsAsFactors = FALSE
    )
    
    # 写入CSV文件
    write.table(df, output_file, sep = ",", 
                row.names = FALSE, col.names = FALSE, 
                na = "", fileEncoding = "UTF-8")
  } else {
    # 如果没有有效记录，创建空文件
    file.create(output_file)
  }
}

# 执行
extract_and_convert("website_content.txt", "structured_data.csv")