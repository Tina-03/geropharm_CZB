import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf

data = pd.read_excel('Хедвей Карфилзомиб-2.xlsx', sheet_name='Headway Company')

# Преобразование даты в кварталы
data['Дата проведения тендера'] = pd.to_datetime(data['Дата проведения тендера'])
data['Квартал'] = data['Дата проведения тендера'].dt.to_period('Q')

data = data[['Дата проведения тендера', 'Квартал', 'Производитель (контракт)', 'Сумма за ед. по контракту (руб)']]
data = data.dropna()

# Группировка по кварталам и компаниям
grouped = data.groupby(['Квартал', 'Производитель (контракт)'])['Сумма за ед. по контракту (руб)'].sum().unstack()

forecast_results = {}

company = 'ПАТЕОН'
# Временной ряд 
ts = grouped[company]
# data_diff = ts.diff().dropna()
# result = adfuller(data_diff)
# print(result[1])

# fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(10, 8))
# plot_acf(data_diff, lags=8, ax=ax1)
# plot_pacf(data_diff, lags=8, ax=ax2, method='ols')
# plt.show()

# from pmdarima import auto_arima

# model = auto_arima(
#     ts,
#     seasonal=False,  # Для несезонных данных
#     stepwise=True,    # Ускоренный поиск
#     information_criterion='aic',
#     trace=True        # Вывод процесса подбора
# )

# Модель ARIMA
model = ARIMA(ts, order=(0, 1, 1))
model_fit = model.fit()
print(model_fit.summary())
forecast = model_fit.forecast(steps=4) 
forecast_results[company] = forecast

# Визуализация
fig, ax = plt.subplots(figsize=(12, 6))

true_x_labels = [str(x) for x in grouped.index.to_list()]
true_x = np.arange(len(true_x_labels))
predict_x_labels = [str(x) for x in forecast_results[company].index.to_list()]
predict_x = np.arange(len(predict_x_labels)) + len(true_x)

ax.plot(true_x, grouped[company].to_numpy(), label=f'{company} (Факт)')
ax.plot(predict_x, forecast_results[company].to_numpy(), label=f'{company} (Прогноз)')
all_x = true_x_labels + predict_x_labels
ax.set_xticks(np.arange(len(all_x)), all_x, rotation='vertical')

ax.set_title('Прогноз денежных потоков (ARIMA) для компании ПАТЕОН')
ax.set_xlabel('Квартал')
ax.set_ylabel('Денежный поток (руб)')
ax.legend()
ax.grid()
plt.show()