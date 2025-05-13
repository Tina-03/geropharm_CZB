df <- read_excel("Хедвей Карфилзомиб-2.xlsx")
# Удаляем строки, где нет производителя или сумма по контракту не указана
df_clean <- df %>%
  filter(
    !is.na(`Производитель (контракт)`),
    !is.na(`Сумма за ед. по контракту (руб)`),
    `Сумма за ед. по контракту (руб)` > 0,
    `Производитель (контракт)` %in% c("ПАТЕОН")
  )

# 2. Преобразование даты в кварталы
df_clean$Квартал <- quarter(df_clean$`Дата проведения тендера`, with_year = TRUE)

# 3. Группировка по кварталам и компаниям
grouped <- df_clean %>%
  group_by(Квартал, `Производитель (контракт)`) %>%
  summarise(Сумма = sum(`Сумма за ед. по контракту (руб)`), .groups = "drop") %>%
  pivot_wider(
    names_from = `Производитель (контракт)`, 
    values_from = Сумма, 
    values_fill = 0 
  )

  # Преобразуем в матрицу (для удобства)
ts_data <- as.matrix(grouped[, -1])
rownames(ts_data) <- grouped$Квартал

# создание лагов
create_lags <- function(series, lags = 4) {
  df <- data.frame(y = series)
  for (lag in 1:lags) {
    df[[paste0("lag", lag)]] <- dplyr::lag(df$y, lag)
  }
  na.omit(df)
}

# Функция для прогнозирования
forecast_rf <- function(series, lags = 4, h = 4) {
  df <- create_lags(series, lags)
  model <- randomForest(y ~ ., data = df, ntree = 500)  # 500 деревьев
  
  last_known <- tail(df, 1)
  predictions <- numeric(h)
    for (i in 1:h) {
    new_data <- last_known
    new_data$y <- NA  
    pred <- predict(model, new_data)
    predictions[i] <- pred
    last_known <- data.frame(
      y = NA,
      lag1 = pred,
      lag2 = last_known$lag1,
      lag3 = last_known$lag2,
      lag4 = last_known$lag3
    )
  }
  return(predictions)
}

forecast_results <- lapply(1:ncol(ts_data), function(i) {
  forecast_rf(ts_data[, i], h = 4)
})
names(forecast_results) <- colnames(ts_data)

plot_data <- data.frame(
  Квартал = rep(as.numeric(rownames(ts_data)), ncol(ts_data)),
  Сумма = as.vector(ts_data),
  Компания = rep(colnames(ts_data), each = nrow(ts_data)),
  Тип = "Факт"
)

forecast_dates <- max(as.numeric(rownames(ts_data))) + (1:4) * 0.25  # Следующие 4 квартала
forecast_values <- do.call(rbind, forecast_results)

forecast_plot_data <- data.frame(
  Квартал = rep(forecast_dates, ncol(ts_data)),
  Сумма = as.vector(forecast_values),
  Компания = rep(colnames(ts_data), each = 4),
  Тип = "Прогноз"
)

full_plot_data <- rbind(plot_data, forecast_plot_data)

# Визуализация
ggplot(full_plot_data, aes(x = Квартал, y = Сумма / 1e6, color = Компания, linetype = Тип)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("ПАТЕОН" = "#5086ff")) +
  geom_point(data = filter(full_plot_data, Тип == "Прогноз"), size = 3) +
  labs(
    title = "Прогноз денежных потоков (Random Forest) для компании ПАТЕОН",
    x = "Квартал (Год.Квартал)",
    y = "млн.руб",
    color = "Компания",
    linetypeм = "Тип данных"
  ) +
  scale_linetype_manual(values = c("Факт" = "solid", "Прогноз" = "dashed")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2018, 2023, by = 0.5), 
                     labels = function(x) paste0(floor(x), ".Q", (x %% 1) * 4 + 1))