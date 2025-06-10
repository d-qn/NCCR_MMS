source("Scripts/_helpers.R")

c2t_A1 <- read_csv(here("Data", "code2txt", "NCCR MMS Codes A1.csv"))
c2t_B1 <- read_csv(here("Data", "code2txt", "NCCR MMS Codes B1.csv"))
c2t_D1 <- read_csv(here("Data", "code2txt", "NCCR MMS Codes D1.csv"))
c2t_E38 <- read_csv(here("Data", "code2txt", "NCCR MMS Codes E38.csv"))


data_all <- mms_df  %>% 
  ### subset !!!
  select(
    A1, A6, B1, B9, D1, E38, year, weight, everything()
  ) %>% 
  left_join(c2t_A1) %>% 
  left_join(c2t_B1) %>% 
  left_join(c2t_D1) %>% 
  left_join(c2t_E38)


data_all %>% filter(is.na(pays)) %>% .$B1 %>% unique() %>% sort()

data_all %>% 
  group_by(year, groupe_edu) %>% 
  summarise(
    tot = sum(weight, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(
    toty = sum(tot)
  ) %>% 
  ungroup %>% 
  mutate(
    share = tot/toty
  )

# https://projects.susielu.com/viz-palette?colors=%5B%22#66b8d4%22,%22%23f7d777%22,%22%23d1ab75%22,%22%23d97053%22,%22%238fd176%22,%22%23198f58%22,%22%236a7aaa%22,%22%230050b5%22,%22%236d5288%22,%22%23d17393%22,%22%23eca53a%22,%22%2354aa9f%22,%22%23b1b1b1%22%5D&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
pays2col <- tibble(
  pays = c("France", "Allemagne", "Espagne", "Autriche", "Italie", "Portugal",
           "Autre UE/AELE",
           "Autre Europe",
           "Autre OCDE",
           "Asie",
           "Amerique latine",
           "Afrique",
           "Autres pays"),
  
  color = c("#66b8d4","#f7d777","#d1ab75","#d97053",
            "#8fd176","#198f58","#6a7aaa","#0050b5",
            "#6d5288","#d17393","#eca53a","#855541",
            "#b1b1b1")
)



### MAIN ###

data_all %>% colnames()
