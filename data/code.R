setwd("/home/agricolamz/work/LAB/annual_reports/2026_labs_annual_report/merged_quarto/data")

library(tidyverse)
read_csv("all_people.csv") |> 
  filter(position != "на безвозмездной основе") |> 
  mutate(name = str_extract(name, ".") |> str_c(... = _, "."),
         f_name = str_extract(f_name, ".") |> str_c(... = _, "."),
         stepen = case_when(stepen == "Отсутствует" ~ "",
                            stepen == "Доктор психологических наук" ~ "д-р психол. наук",
                            stepen == "Доктор филологических наук" ~ "д-р филол. наук",
                            stepen == "Доктор философских наук" ~ "д-р филос. наук",
                            stepen == "Кандидат физико-математических наук" ~ "канд. физ.-мат. наук",
                            stepen == "Кандидат филологических наук" ~ "канд. филол. наук",
                            stepen == "Кандидат философских наук" ~ "канд. филос. наук",
                            is.na(stepen) ~ "PhD",
                            TRUE ~ stepen),
         position = case_when(position == "стажер-исследователь" ~ "Стажер-исследователь",
                              position == "старший научный сотрудник" ~ "Cт. науч. сотр.",
                              position == "научный сотрудник" ~ "Науч. сотр.",
                              position == "ведущий научный сотрудник" ~ "Вед. науч. сотр.",
                              position == "заведующий лабораторией" ~ "Зав. лаб.",
                              position == "младший научный сотрудник" ~ "Мл. науч. сотр.",
                              position == "менеджер" ~ "менеджер",
                              position == "Стажер-исследователь" ~ "Стажер-исследователь",
                              position == "Ведущий научный сотрудник" ~ "Вед. науч. сотр.",
                              position == "Научный сотрудник" ~ "Науч. сотр.",
                              position == "Заместитель заведующего лабораторией" ~ "Зам. зав. лаб.",
                              position == "Главный научный сотрудник, заведующий лабораторией" ~ "Зав. лаб., гл. науч. сотр.",
                              position == "доцент" ~ "Доц.",
                              position == "Заведующий лабораторией" ~ "Зав. лаб.",
                              position == "Доцент" ~ "Доц.",
                              position == "профессор" ~ "Проф.",
                              position == "стажёр-исследователь" ~ "Стажер-исследователь"),
         position = factor(position, levels = c("Проф.",
                                                "Зав. лаб.",
                                                "Зав. лаб., гл. науч. сотр.",
                                                "Зам. зав. лаб.",
                                                "Доц.",
                                                "Вед. науч. сотр.",
                                                "Cт. науч. сотр.",
                                                "Науч. сотр.",
                                                "Мл. науч. сотр.",
                                                "Стажер-исследователь"))) |> 
  arrange(position, last_name) |> 
  mutate(position = if_else(stepen != "", str_c(position, ","), position),
         result_name = str_c(name, f_name, last_name)) |> 
  select(position, stepen, result_name) |> 
  pivot_longer(names_to = "type", values_to = "value", position:stepen) |> 
  select(-type) |> 
  mutate(s = rep(c("____________", "подпись, дата"), 155),
         result_name = if_else(s == "подпись, дата", "(раздел )", result_name)) |> 
  select(value, s, result_name) |> 
  write_csv("people.csv")
