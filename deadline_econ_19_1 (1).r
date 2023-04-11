## Загрузка пакетов ------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(sandwich)
library(lmtest)
library(car)
library(broom)
library(xtable)
library(ggpubr)
library(stargazer)
library(modelsummary)

# Загрузка данных ==============================================================

sub_fertility <- drop_na(sub_fertility) # Удаляем пропущенные переменные 

sub_fertility %>% glimpse() # Просто функция, чтобы посмотреть что там лежит



## modelsummary ----------------------------------------------------------------


datasummary((`Коэффициент рождаемости` = fertility_rate) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max), 
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'fertility.docx'
)

datasummary((`Удовлетворённость финансовым положением` = share_of_prosp_hh) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'share_of_prosp_hh.docx'
)

datasummary((`Возраст матерей` = f15_19 + f18_19 + f20_24 + f25_29 + f30_34 + f35_39 + f40_44 + f45_49) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'fertility_age.docx'
)

datasummary((`Меры социальной поддержки` = cons_budj) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'cons_budj.docx'
)


datasummary((`Среднедушевой доход` = av_inc) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'av_inc.docx'
)

datasummary((`Размер базового ежемесячного пособия` = base) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Descriptive statistics", 
            output = 'base.docx'
)

datasummary((`Субсидии на оплату жилого помещения ` = subsidies) + (`Меры социальной поддержки` = cons_budj) + (`Размер базового ежемесячного пособия` = base) + (`Возраст матерей (15-19) ` = f15_19) + (`Возраст матерей (18-19)` = f18_19) + (`Возраст матерей (20-24)` = f20_24) + (`Возраст матерей (25-29)` = f25_29) + (`Возраст матерей (30-34)` = f30_34) + (`Возраст матерей (35-39)` = f35_39) + (`Возраст матерей (40-44)` = f40_44) + (`Возраст матерей (45-49)` = f45_49) + (`Удовлетворённость финансовым положением` = share_of_prosp_hh) + (`Среднедушевой доход` = av_inc) ~ (`Среднее` = Mean) + (`Медиана` = Median) + (`Станд. откл.`= SD) + (`Минимум` = Min) + (`Максимум` = Max),
            data = sub_fertility, 
            title = "Описательная статистика", 
            output = 'all_variables.docx'
)


# Графики ======================================================================

# Построим ящик с усами, чтобы найти выбросы

ggplot(data = sub_fertility, aes(y = base)) + xlab("Субъеты РФ") + ylab("Размер субсидии") +
  ggtitle("Размер базового ежемесячного пособия") +
  geom_boxplot()

# Найдены выбросы, которые объяняются тем, что в крупных регионах размер субсидии 
# может быть достаточно высоким, в то время как в регионах с низкими ценами на продукты и тд размер субсидии может быть низким
# Я считаю, что мы не имеем права избавляться от этих выбросов

# Гистограмма
ggplot(data = sub_fertility, aes(x = av_inc)) +
  geom_histogram(binwidth = 100, 
                 fill = "yellowgreen", 
                 color = "black") + ylab("Частота") + xlab("Размер субсидии") + ggtitle("Коэффициент рождаемости")

# График плотности распределения
ggplot(data = sub_fertility, aes(x = av_inc)) + ylab("Плотность") + xlab("Среднедушевой доход") +
  ggtitle("Среднедушевой доход") +
  geom_density(alpha = 0.5)




# Субсидии на оплату жилья и среднедушевые доходы

ggplot(data = sub_fertility, aes(x = base, y = av_inc)) + geom_point(shape = 1, size = 4) +
  geom_smooth(method=lm) + ylab("Размер среднедушевого дохода") + xlab("Размер базового ежемесячного пособия")

# Субсидии и коэффициент рождаемости корреляция

ggplot(data = sub_fertility, aes(x = cons_budj, y = fertility_rate)) + geom_point(shape = 1, size = 4) +
  geom_smooth(method=lm) + ylab("Коэффициент рождаемости") + xlab("Меры социальной поддержки семьям с детьми") +
  ggtitle("График корреляции коэффициента рождаемости и\nмер социальной поддержки семьям с детьми")

ggplot(data = sub_fertility, aes(x = base, y = fertility_rate)) + geom_point(shape = 1, size = 4) +
  geom_smooth(method=lm) + ylab("Коэффициент рождаемости") + xlab("Размер базового ежемесячного пособия") +
  ggtitle("График корреляции коэффициента рождаемости и\nразмера базового ежемесячного пособия")

ggplot(data = sub_fertility, aes(x = subsidies, y = fertility_rate)) + geom_point(shape = 1, size = 4) +
  geom_smooth(method=lm) + ylab("Коэффициент рождаемости") + xlab("Субсидии на оплату жилого помещения") +
  ggtitle("График корреляции коэффициента рождаемости и\nсубсидий на оплату жилого помещения")
