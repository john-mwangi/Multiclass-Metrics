knitr::opts_chunk$set(echo = FALSE)

library(tidyverse)

cm_actual <- dir(path = "./confusion_matrices/actual/", 
    pattern = "*.csv", 
    all.files = FALSE, 
    full.names = TRUE, 
    recursive = FALSE, 
    ignore.case = TRUE, 
    include.dirs = FALSE) %>% 
  map_dfr(read_csv)

dim(cm_actual)
head(cm_actual)

## # Convert to Confusion Matrix
## # Columns required: Actual, Predicted, Count
## cm_actual_wide <- cm_actual %>%
##   select(`Actual Class`,`Predicted Class`,`Confusion Matrix Count`) %>%
##   pivot_wider(names_from = `Actual Class`,
##               values_from = `Confusion Matrix Count`)
## 
## cm_actual_wide

cm_actual %>% 
  summarise(sum(`Confusion Matrix Count`))

parasite_data <- readxl::read_excel(path = "./research/parasite_data.xlsx")

parasite_longer <- parasite_data %>% 
  pivot_longer(cols = Ascaris_lumbricoides_fertile:Negative) %>% 
  rename(actual = PARASITE,
         predicted = name,
         count = value)

parasite_longer

parasite_count <- parasite_data %>% 
  mutate(total = apply(X = .[,c(2:length(.))], MARGIN = 1, FUN = sum)) %>% 
  select(PARASITE,total)

parasite_count

parasite_longer %>% 
  filter(actual %in% c("Hymenolepsis_nana","Trichuris_trichiura","Hookworm")) %>% 
  filter(actual==predicted) %>% 
  left_join(parasite_count, by = c("actual" = "PARASITE")) %>% 
  mutate(tpr = count/total)

parasite_longer %>% 
  filter(predicted %in% c("Hymenolepsis_nana","Trichuris_trichiura","Hookworm")) %>% 
  filter(predicted != actual) %>% 
  left_join(parasite_count, by = c("actual"="PARASITE")) %>% 
  group_by(predicted) %>% 
  summarise(fp = sum(count),
            total = sum(total)) %>% 
  mutate(fpr = fp/total)

product_counts <- cm_actual %>% 
 select(`Actual Class`,`Predicted Class`,`Confusion Matrix Count`) %>% 
  group_by(`Actual Class`) %>% 
  summarise(count = sum(`Confusion Matrix Count`))

product_counts

dr_tpr_prec <- cm_actual %>% 
  select(`Actual Class`,`Recall (Actual Class)`,`Precision (Actual Class)`) %>% 
  distinct(`Actual Class`, .keep_all = TRUE) %>% 
  rename(tpr = `Recall (Actual Class)`,
         precision = `Precision (Actual Class)`)

dr_tpr_prec

dr_fpr <- cm_actual %>% 
  select(`Actual Class`,`Predicted Class`,`Confusion Matrix Count`) %>% 
  filter(`Actual Class` != `Predicted Class`) %>% 
  left_join(product_counts, by = c("Actual Class"="Actual Class")) %>% 
  group_by(`Predicted Class`) %>% 
  summarise(fp = sum(`Confusion Matrix Count`),
            count = sum(count)) %>% 
  mutate(fpr = fp/count)

dr_fpr

dr_summary <- dr_tpr_prec %>% 
  left_join(dr_fpr, by = c("Actual Class"="Predicted Class")) %>% 
  select(`Actual Class`,tpr,fpr,precision) %>% 
  summarise_if(.predicate = is.numeric, .funs = mean) %>% 
  mutate(model = "propensity",
         n = 1)

dr_summary

tibble(precision = c(0.07470031,0.04899754,0.03903401),
       fpr = c(0.04871455,0.10024740,0.14913219),
       tpr = c(0.5943513,0.7537632,0.8840006),
       model = "recommender",
       n = c(1,2,3)) %>% 
  bind_rows(dr_summary) %>% 
  writexl::write_xlsx(path = "./comparison.xlsx")
