Final_Dataset_2_STA <- readxl::read_excel("C:/Users/HP/Downloads/Final Dataset 2_ STA.xlsx")[,c(1:16)]

Final_Dataset_2_STA <- Final_Dataset_2_STA %>%
  mutate_at(c("General","Special","Extended"),funs(recode(.,"1"="core",
                                                          "2" = "optional",
                                                          "0" = "not related"))) %>%
  pivot_longer(c(14:16), names_to = "degree type",
               values_to = "core/optional") %>%
  mutate(`degree type`= paste0(`degree type`," Degree"))