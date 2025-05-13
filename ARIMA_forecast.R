library(dplyr)
library(readxl)
library(forecast)
library(ggplot2)
library(lubridate)

data <- read_excel("Хедвей Карфилзомиб-2.xlsx", sheet = "Headway Company")

# сумма контрактов по месяцам
monthly_sales <- data %>%
  mutate(Date = as.Date(`Дата проведения тендера`),
         YearMonth = format(Date, "%Y-%m"),
         # Используем сумму заказа по контракту (столбец 'Сумма за ед. по контракту (руб)' * 'Количество упаковок (контракт)')
         ContractSum = `Сумма за ед. по контракту (руб)`) %>%
  group_by(YearMonth) %>%
  summarise(TotalSum = sum(ContractSum, na.rm = TRUE)) %>%
  mutate(YearMonth = as.Date(paste0(YearMonth, "-01"))) %>%
  arrange(YearMonth)

# временной ряд
ts_data <- ts(monthly_sales$TotalSum,  # Переводим в миллионы рублей для удобства
              frequency = 12, 
              start = c(year(min(monthly_sales$YearMonth)), 
                        month(min(monthly_sales$YearMonth))))

autoplot(ts_data) + 
  ggtitle("Динамика продаж Карфилзомиба по сумме контрактов") +
  ylab("Сумма контрактов, млн руб") +
  xlab("Время")

# модель ARIMA
fit <- auto.arima(ts_data)

# Проверка остатков модели
checkresiduals(fit)

# Прогноз на 12 месяцев 
forecast_values <- forecast(fit, h = 12)
autoplot(forecast_values) +
  ggtitle("Прогноз продаж Карфилзомиба на 12 месяцев") +
  ylab("Объемы закупок, млн руб") +
  xlab("Время")
print(forecast_values)

# Детализация прогноза с доверительными интервалами
forecast_df <- as.data.frame(forecast_values) %>%
  mutate(Month = seq.Date(from = max(monthly_sales$YearMonth) + months(1),
                          by = "month",
                          length.out = 12))
