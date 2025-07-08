library(httr)
library(rvest)
library(xml2)

crawl_website <- function(url) {
  tryCatch({
    # 设置请求头
    headers <- c(
      'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
    )
    
    # 发送HTTP请求
    response <- GET(url, add_headers(.headers = headers), timeout(10))
    stop_for_status(response)  # 检查HTTP错误
    
    # 解析HTML内容
    html_content <- read_html(response)
    
    # 移除不需要的标签
    tags_to_remove <- c("script", "style", "meta", "link", "nav", "footer", "header", "form")
    xpath_query <- paste0(".//", tags_to_remove, collapse = " | ")
    xml_remove(xml_find_all(html_content, xpath_query))
    
    # 提取并处理文本
    text_nodes <- xml_find_all(html_content, "//text()[not(ancestor::script | ancestor::style | ancestor::meta | ancestor::link | ancestor::nav | ancestor::footer | ancestor::header | ancestor::form)]")
    extracted_text <- sapply(text_nodes, function(x) {
      trimws(xml_text(x), which = "both")
    })
    
    # 组合文本并添加换行符
    combined_text <- paste(extracted_text, collapse = "\n")
    
    # 清理多余空行
    cleaned_text <- gsub("\\n\\s*\\n", "\n\n", combined_text)
    cleaned_text <- gsub("[\r\n]{3,}", "\n\n", cleaned_text)  # 额外处理连续换行
    
    return(trimws(cleaned_text))
    
  }, error = function(e) {
    message("Error: ", conditionMessage(e))
    return(NULL)
  })
}

# 使用示例
url <- "https://guba.eastmoney.com/list,600600_9.html"
result <- crawl_website(url)

if (!is.null(result) && nchar(result) > 0) {
  # 显示部分内容
  cat(substr(result, 1, 200), "\n\n... (content truncated) ...\n")
  
  # 保存完整内容
  writeLines(result, "website_content.txt", useBytes = TRUE)
} else {
  cat("Failed to retrieve content\n")
}