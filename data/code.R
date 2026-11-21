library(tidyverse)
read_csv("/home/agricolamz/work/LAB/annual_reports/2026_labs_annual_report/merged_quarto/data/all_people.csv") |> 
  mutate(across(everything(), function(x) str_remove_all(x, "\n")),
         n = str_replace(n, "\\(", " \\("),
         n = str_squish(n)) |> 
  separate(n, sep = " \\(", into = c("name", "part")) |>
  separate(p, sep = ", ", into = c("position", "regalue")) |>
  mutate(part = str_c("(", part),
         regalue = if_else(is.na(regalue), "", regalue),
         last_name = str_extract(name, "\\s.*?$"),
         last_name = str_squish(last_name),
         position = factor(position, levels = c("Проф.",
                                                "Зав. лабораторией",
                                                "Зам. зав. лабораторией",
                                                "Доц.",
                                                "Главн. науч. сотр.,Зав. лабораторией",
                                                "Вед. науч. сотр.",
                                                "Ст. преподаватель",
                                                "Ст. науч. сотр.",
                                                "Науч. сотр.",
                                                "Мл. науч. сотр.",
                                                "Стажер-исследователь",
                                                "Нормоконтроль"))) |> 
  arrange(position, last_name) |> 
  mutate(id = 1:n()) ->
  result

result |> 
  select(name, part) |> 
  pivot_longer(names_to = "name", values_to = "n", everything()) |> 
  select(-name) ->
  name

result |> 
  select(position, regalue) |> 
  pivot_longer(names_to = "name", values_to = "p", everything()) |> 
  select(-name) |> 
  bind_cols(name) |> 
  mutate(s = rep(c("____________", "подпись, дата"), 108)) |> 
  relocate("n", .after = "s") |> 
  write_csv("people.csv")
