rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console when working in RStudio

# load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R")
# load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library("magrittr") #Pipes
library("ggplot2")  # graphs
library("dplyr")
requireNamespace("dplyr")

# ---- declare-globals ---------------------------------------------------------
path_input <- "data-unshared/raw/Validation+of+SUN+Mental+Health+App+Instrument_November+20,+2020_04.37.sav"

# ---- load-data ---------------------------------------------------------------
# ds0 <- haven::read_sav(path_input, )
ds0 <- haven::read_sav(path_input)

# ---- inspect-data -------------------------------------------------------------
ds0 %>% distinct(ResponseId) %>% count()

# ds0 %>% names_labels() %>% readr::write_csv("./data-unshared/derived/meta-2000.csv") # to get started
metadata <- readr::read_csv("./data-public/metadata/meta-2020.csv")


# ---- define-content --------------------------
# ds0 %>% distinct(Q40)
# -----tweak-data ---------------------------------------------------------------

new_names <- dplyr::left_join(
  data.frame("old_names" = names(ds0)) %>% tibble::as_tibble()
  , metadata %>% distinct(item_name_old, item_name)
  ,by = c("old_names" = "item_name_old")
) %>%
  pull(item_name)
ds1 <- ds0
names(ds1) <- new_names
ds1 %>% glimpse()

metadata %>% distinct(section)

items_chu <- metadata %>% filter(section == "CHU") %>% pull(item_name)
items_wtd <- metadata %>% filter(section == "WTD") %>% pull(item_name)
items_w2w <- metadata %>% filter(section == "W2W") %>% pull(item_name)
items_sun <- metadata %>% filter(section == "SUN", likert==T) %>% pull(item_name)
items_sun_why <- metadata %>% filter(section == "SUN", is.na(likert)) %>% pull(item_name)

items_scales <- c(items_chu, items_wtd, items_w2w, items_sun)

ds1 %>% group_by(Status, Finished) %>% count()
ds2 <- ds1 %>%
  filter(Finished==1) %>%
  mutate_all(dplyr::na_if,-99)

dscale <- ds2 %>%
  select(c("ResponseId", items_scales))


ds2 %>% select(items_chu) %>%   missing_summary()
ds2 %>% select(items_wtd) %>%   missing_summary()
ds2 %>% select(items_w2w) %>%   missing_summary()
ds2 %>% select(items_sun) %>%   missing_summary()

ds2 %>% select(c(items_chu, items_sun)) %>% missing_summary()


ds2 %>% select(c(items_chu, items_sun)) %>% filter(complete.cases(.))

ds2 %>% select(items_wtd)  %>% missing_summary()
ds2 %>% select(items_wtd)  %>% show_missing_points()


cat("\nOverall % of missing data: ",scales::percent(ms$miss_df_prop))
cat("\nVariables that contain missing data: ",scales::percent(ms$miss_var_prop))
cat("\nCases that contain missing data: ",scales::percent(ms$miss_case_prop))


missing_summary()


naniar::miss_summary(ds2 %>% select(items_wtd))

# ---- convert-to-factors -----------------------

convert_to_factors <- function(d,varname){
  # d <- ds1 %>% dplyr::select(id, Q4_1)
  # varname <- "Q4_1"
  (factor_labels <- labelled::val_labels(d[,varname]))
  (variable_label <- labelled::var_label(d[,varname])[[1]])

  d <- d %>% dplyr::rename(temp = varname )
  d <- d %>% dplyr::mutate(
    temp = factor(temp, levels = factor_labels[[1]], labels = names(factor_labels[[1]])),
    temp = forcats::fct_explicit_na(temp)
  )
  d <- d %>% plyr::rename( c("temp" = varname))
  labelled::var_label(d[,varname]) <- variable_label
  return(d)
}


ds2 <- ds1
for(i in c("Status","Finished", items_scales, items_sun_why,"sex", "ethnicity","race","class_standing")){
  ds2 <- ds2 %>% convert_to_factors(i)
}

ds2 %>% glimpse()


# ---- tweak-data-2 ---------------
ds2 %>% group_by(Status, Finished) %>% count()
ds3 <- ds2 %>%
  filter(Finished=="True") %>%
  mutate_all(na_if,"(Missing)")

ds3 %>%
  select(c("ResponseId", items_scales)) %>%
  filter(complete.cases(.))


# ---- create-meta --------------------
ds_labels <- ds0 %>% names_labels()

ds_labels <- ds_labels %>%
  dplyr::mutate(
    label_raw = label,
    label = gsub("^Please indicate how strongly you agree or disagree with the following statements. - "
                 ,"",label),

    label = gsub("^I did the following things for myself today \\(please choose all that apply\\): "
                 ,"Today I ... ",label),
    label = gsub("^The following activities helped me to meet my goal \\(please choose all that apply\\): - Selected Choice "
                 ,"Helped goal ... ",label),
    label = gsub("^The following activities contributed to not meeting my goal \\(please choose all that apply\\): - Selected Choice "
                 ,"Hindered goal ... ",label),
    label = gsub("^To what extent do you agree with the following statement: \\\n"
                 ,"",label),
    label = gsub("^The following activities helped me to meet my goal \\(please choose all that apply\\): - Other: - Text"
                 ,"Helped goal ... Free text",label),
    label = gsub("^The following activities contributed to not meeting my goal \\(please choose all that apply\\): - Other - Text"
                 ,"Hindered goal ... Free text",label),
    label = gsub('^Do you have any comments about the question above \\("I did the following things for myself today"\\); was there anything unclear or confusing, or any choices missing that should be included\\?'
                 ,"Comments to Q7",label),
    label = gsub('^Do you have any comments about the prior question \\("The following activities helped me to meet my goal"\\); is anything unclear or confusing, or are any answer choices missing that should be included\\?'
                 ,"Comments to Q12",label),
    label = gsub('^Do you have any comments about the prior question \\("The following activities contributed to not meeting my goal"\\); is anything unclear or confusing, or are any answer choices missing that should be included\\?'
                 ,"Comments to Q13",label)

  )

# quick inspection
# ds_labels[81, 2]
ds_labels %>% readr::write_csv("./data-unshared/derived/labels.csv")

# ---- load-meta -------------------
# after adding some manual changes to the metadata outside of this script:
# ds_meta <- readxl::read_xlsx("./data-unshared/raw/meta.xlsx")
ds_meta <- readr::read_csv("./data-unshared/raw/meta.csv")


# ----- reverse-coding --------------
# the following items are contributing to the negative/adverse state we are measuring

# ds0 %>% select(ResponseId, Q4_4) %>% head()
# ds0 %>% select(ResponseId, Q4_4) %>% glimpse()
# ds0 %>% distinct(Q4_4)
#
# ds1 <- ds0 %>%
#   dplyr::mutate(
#     Q4_4r = ifelse(Q4_4 == 1, 5,
#                    ifelse(Q4_4 == 2, 4,
#                           ifelse(Q4_4 == 4, 2,
#                                  ifelse(Q4_4 == 5, 1, Q4_4))))
#   )
# ds1 %>% select(ResponseId, Q4_4r) %>% head()
# ds1 %>% select(ResponseId, Q4_4r) %>% glimpse()
# ds1 %>% distinct(Q4_4r)
#
#
# ds0 %>% select(ResponseId, Q4_4) %>% head()
# ds0 %>% select(ResponseId, Q4_4) %>% glimpse()
# ds0 %>% distinct(Q4_4)

# Q4_4
# Q4_7
# Q4_11
# Q4_9
# Q4_15
#
# Q20
# Q21
# Q22
# Q23
# Q24
# Q25
# Q28
# Q29
# Q30




# --- rename-variables ------------------------------

# ds1 <- ds0
# names_old <- names(ds0) %>% tibble::enframe(value = "name_old")
# var_rename <- ds_meta %>% dplyr::select(item_name, item_label, section) %>%
#   dplyr::left_join(
#     names_old, by = c("item_name" = "name_old")
#   )

#
# for(i in unique(colnames(ds1))){
#   d_rules <- metaData %>%
#     dplyr::filter(name_new %in% names(ds1)) %>%
#     dplyr::select(name_new, label_short )
#   attr(ds_small[,i], "label") <-  as.character(d_rules[d_rules$name_new==i,"label_short"])
# }



# # basic-table --------------------------------------------------------------
#
# ds0 %>% dplyr::distinct(Q14___Parent_Topics)
#
# d1 <- ds0 %>%
#   dplyr::filter(Q14___Topics == "Unknown") %>%
#   dplyr::select(Q14___Topics, Q14)
# # basic-graph --------------------------------------------------------------
#
# ds0 %>%
#   dplyr::select(Q4_2, Q39) %>%
#   explore::describe_all()

# save-to-disk -------------------------------------------------------------
names(ds0)
dto <- list(
   "raw0"     = ds0
  ,"microData" = ds2
  ,"metaData" = ds_meta
)
dto %>% saveRDS("./data-unshared/derived/dto.rds")
