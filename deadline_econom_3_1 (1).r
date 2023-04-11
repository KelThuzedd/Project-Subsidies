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
install.packages("nlme")
library(nlme)
sub_fertility <- drop_na(sub_fertility)

# Регрессионный анализ =========================================================

## Спецификации ----------------------------------------------------------------

spec0 <- fertility_rate ~ 1 + subsidies + cons_budj + base + av_inc + share_of_prosp_hh + f15_19 + f18_19 + f20_24 + f25_29 + f30_34 + f35_39 + f40_44 + f45_49
spec1 <- fertility_rate ~ 1 + subsidies + cons_budj + base + av_inc + share_of_prosp_hh + f15_19 + f18_19 + f20_24 + f25_29 + f30_34 + f35_39 + f40_44 + f45_49 + I(subsidies*av_inc) + I(cons_budj*av_inc) + I(base*av_inc)
spec2 <- fertility_rate ~ 1 + subsidies + cons_budj + base + av_inc + share_of_prosp_hh + f20_24 + f35_39 + I(subsidies*av_inc) + I(cons_budj*av_inc) + I(base*av_inc)

reg0 <- lm(spec0, data = sub_fertility)
cov0 <- vcovHC(reg0, type = "HC0")
se0 <- sqrt(diag(cov0))

reg1 <- lm(spec1, data = sub_fertility)
cov1 <- vcovHC(reg1, type = "HC0")
se1 <- sqrt(diag(cov1))

reg2 <- lm(spec2, data = sub_fertility)
cov2 <- vcovHC(reg2, type = "HC0")
se2 <- sqrt(diag(cov2))

modelsummary(models = list("Модель №1" = reg0, "Модель №2" = reg1),                       # список оцененных моделей
             vcov = list(cov0, cov1),                         # список ковариционных матриц для расчета стандартных ошибок
             #output = '2models.docx',
             coef_map = c("(Intercept)" = "Константа", 
                          "subsidies" = "Субсидии на жильё", 
                          "cons_budj" = "Меры социальной поддержки",
                          "base" = "Базовое пособие", 
                          "av_inc" = "Среднедушевой доход", 
                          "share_of_prosp_hh" = "Удовлетворённость финансовым положением",
                          "f15_19" = "Возраст матерей (15-19)", 
                          "f18_19" = "Возраст матерей (18-19)", 
                          "f20_24" = "Возраст матерей (20-24)",
                          "f25_29" = "Возраст матерей (25-29)", 
                          "f30_34" = "Возраст матерей (30-34)", 
                          "f35_39" = "Возраст матерей (35-39)",
                          "f40_44" = "Возраст матерей (40-44)", 
                          "f45_49" = "Возраст матерей (45-49)", 
                          "I(subsidies * av_inc)" = "Субсидии на жильё X Среднедушевой доход",
                          "I(cons_budj * av_inc)" = "Меры социальной поддержки X Среднедушевой доход", 
                          "I(base * av_inc)" = "Базовое пособие X Среднедушевой доход"),
             statistic = "std.error",                                     # выводить стандартные ошибки
             stars = TRUE,                                                # звездочки для уровня значимости
             gof_omit = ".*",                                             # не выводить никаких показателей качества моделей
             notes = list("В скобках даны робастные стандартные ошибки",
                          "Все регрессии содержат контрольные переменные"), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценивания")

modelsummary(models = list("Модель №1" = reg0, "Модель №2" = reg1),                       # список оцененных моделей
             vcov = list(cov0, cov1),                         # список ковариционных матриц для расчета стандартных ошибок
             #output = '2models_short.docx',
             #coef_omit = "av_inc|share_of_prosp_hh|f15_19|f18_19|f20_24|f25_29|f30_34|f35_39|f40_44|f45_49",
             coef_map = c("(Intercept)" = "Константа", 
                          "subsidies" = "Субсидии на жильё", 
                          "cons_budj" = "Меры социальной поддержки",
                          "base" = "Базовое пособие", 
                          "I(subsidies * av_inc)" = "Субсидии на жильё X Среднедушевой доход",
                          "I(cons_budj * av_inc)" = "Меры социальной поддержки X Среднедушевой доход", 
                          "I(base * av_inc)" = "Базовое пособие X Среднедушевой доход"),
             statistic = "std.error",                                     # выводить стандартные ошибки
             stars = TRUE,                                                # звездочки для уровня значимости
             gof_omit = ".*",                                             # не выводить никаких показателей качества моделей
             notes = list("В скобках даны робастные стандартные ошибки",
                          "Все регрессии содержат контрольные переменные"), # комментарий по поводу расчета стандартных ошибок
             title = "Результаты оценивания")

# Tests ========================================================================
# Для первой модели
linearHypothesis(reg0, 
                 c("subsidies = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") #гипотеза не отвергается


linearHypothesis(reg0, 
                 c("av_inc = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") #гипотеза отвергается 10%

linearHypothesis(reg0, 
                 c("share_of_prosp_hh = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") #Гипотеза не отвергается 

linearHypothesis(reg0, 
                 c("cons_budj = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") # гипотеза не отвергается

linearHypothesis(reg0, 
                 c("base = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") # гипотеза отвергается 10%


# Для второй модели
linearHypothesis(reg1, 
                 c("subsidies = 0"), 
                 test = "Chisq") # Гипотеза отвергается 0,1%

linearHypothesis(reg1, 
                 c("av_inc = 0"), 
                 test = "Chisq") # Гипотеза отвергается 0,1%

linearHypothesis(reg1, 
                 c("share_of_prosp_hh = 0"), 
                 test = "Chisq") # Гипотеза не отвергается 

linearHypothesis(reg1, 
                 c("cons_budj = 0"), 
                 test = "Chisq") # Гипотеза не отвергается

linearHypothesis(reg1, 
                 c("base = 0"), 
                 test = "Chisq") # Гипотеза не отвергается


# Для третьей модели
linearHypothesis(reg2, 
                 c("subsidies = 0"), 
                 test = "Chisq") # Гипотеза отвергается 10%

linearHypothesis(reg2, 
                 c("av_inc = 0"), 
                 test = "Chisq") # Гипотеза отвергается 0,1%

linearHypothesis(reg2, 
                 c("share_of_prosp_hh = 0"), 
                 test = "Chisq") # Гипотеза не отвергается

linearHypothesis(reg2, 
                 c("cons_budj = 0"), 
                 test = "Chisq") # Гипотеза не отвергается

linearHypothesis(reg2, 
                 c("base = 0"), 
                 test = "Chisq") # Гипотеза не отвергается







linearHypothesis(reg0, 
                 c("subsidies - base = 0"), 
                 test = "Chisq", 
                 white.adjust = "hc0") #гипотеза отвергается на 10% уровне

linearHypothesis(reg0, 
                 c("subsidies - base = 0"), 
                 test = "Chisq") #гипотеза отвергается на 10% уровне

#linearHypothesis(reg1, 
                # c("base:av_inc"), 
                # test = "Chisq") #гипотеза отвергается на 10% уровне
