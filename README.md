# Previsao_de_Demanda_por_Aluguel_de_Bikes

Informações dos dataset.

Both hour.csv and day.csv have the following fields, except hr which is not available in day.csv

- instant: record index
- dteday : date
- season : season (1:winter, 2:spring, 3:summer, 4:fall)
- yr : year (0: 2011, 1:2012)
- mnth : month ( 1 to 12)
- hr : hour (0 to 23)
- holiday : weather day is holiday or not (extracted from [Web Link])
- weekday : day of the week
- workingday : if day is neither weekend nor holiday is 1, otherwise is 0.
+ weathersit :
- 1: Clear, Few clouds, Partly cloudy, Partly cloudy
- 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
- 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
- 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
- temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
- atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
- hum: Normalized humidity. The values are divided to 100 (max)
- windspeed: Normalized wind speed. The values are divided to 67 (max)
- casual: count of casual users
- registered: count of registered users
- cnt: count of total rental bikes including both casual and registered



Sumário
 
Este experimento tem como objetivo criar um modelo preditivo para estimar a demanda pelo aluguel de bicicletas.


Descrição

Este experimento visa demonstrar o processo de construção de um modelo de regressão para prever a demanda por 
aluguel de bicicletas.Usaremos um conjunto de dados para construir e treinar nosso modelo.

Dados

Dataset contém 17.379 observações e 17 variáveis, representando o número de bicicletas alugadas dentro de horas
específicas do dia, nos anos de 2011 e 2012. Condições climáticas (como temperatura, humidade e velocidade do vento)
foram incluídas no dataset e as datas foram categorizadas como feriados e dias da semana.

Objetivo 

O objetivo será prever o valor da variável cnt(count)que representa a quantidade de bicicletas alugadas dentro de uma 
hora específica e cujo range é de 1 a 977.




Dataset pode ser encontrado em : https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset


 
