library(dplyr)
library(lubridate)
data <- read_excel("Хедвей Карфилзомиб-2.xlsx")

# Параметры для расчета
premium_distributors <- 0.10  # Премии дистрибьюторам 10%
discount_rate <- 0.20  # Ставка дисконта 20%

# Функции для расчета денежного потока и DCF
calculate_cash_flow_and_dcf <- function(data, company) {
  company_data <- data %>%
    filter(`Производитель (контракт)` == company) %>%
    mutate(`Денежный поток` = `Сумма за ед. по контракту (руб)` * (1 - premium_distributors)) %>%
    mutate(DCF = `Денежный поток` / (1 + discount_rate))
  
  return(company_data)
}

unique_companies <- unique(na.omit(data$`Производитель (контракт)`))

# Расчет для каждой компании
results <- lapply(unique_companies, function(company) {
  calculate_cash_flow_and_dcf(data, company)
})
results_summary <- lapply(results, function(df) {
  list(
    `Сумма за ед. по контракту (руб)` = sum(df$`Сумма за ед. по контракту (руб)`),
    `Денежный поток` = sum(df$`Денежный поток`),
    `DCF` = sum(df$DCF)
  )
})

print(results_summary)
data1 <- results[[1]][, c("Заказ №", "Дата проведения тендера", "Производитель (контракт)", "Денежный поток", "DCF")]
data2 <- results[[2]][, c("Заказ №", "Дата проведения тендера", "Производитель (контракт)", "Денежный поток", "DCF")]

data3 <- results[[3]][, c("Заказ №", "Дата проведения тендера", "Производитель (контракт)", "Денежный поток", "DCF")]

library(ggplot2)
write.table(data1, "БАКСТЕР.txt", sep = "\t", row.names = FALSE)
write.table(data2, "ПАТЕОН.txt", sep = "\t", row.names = FALSE)
write.table(data2, "АМДЖЕН.txt", sep = "\t", row.names = FALSE)

# Преобразование results_summary в датафрейм
summary_df <- data.frame(
  Компания = unique_companies,
  Сумма_по_контракту = sapply(results_summary, function(x) x$`Сумма за ед. по контракту (руб)`),
  Денежный_поток = sapply(results_summary, function(x) x$`Денежный поток`),
  DCF = sapply(results_summary, function(x) x$DCF)
)

print(summary_df)


ggplot(summary_df, aes(x = reorder(Компания, -DCF), y = DCF / 1e9)) +
  geom_bar(stat = "identity", fill = c("АМДЖЕН" = "#16b02c", 
                                       "БАКСТЕР" = "#5086ff", 
                                       "ПАТЕОН" = "#f25e5a")) +
  labs(title = "Дисконтированный денежный поток (DCF) 2018-2022", 
       x = "Компания", 
       y = "млрд. руб") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(summary_df, aes(x = Компания, y = Денежный_поток / 1e9, fill = Компания)) +
  geom_bar(stat = "identity", fill = c("ПАТЕОН" = "#16b02c", 
                                         "БАКСТЕР" = "#5086ff", 
                                         "АМДЖЕН" = "#f25e5a")) +
  labs(title = "Денежный поток 2018-2022: сравнение общей эффективности основных производителей карфилзомиба", 
       x = "Компания", 
       y = "млрд. руб") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


