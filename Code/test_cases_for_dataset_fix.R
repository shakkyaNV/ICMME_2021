# ***********
getPos(test_lm, meat_index, list())

# ***********
test_lm <- df_to_lm %>% 
  filter(Code == "BLR" | Code == "AUS", Year > 2000, Year < 2015) %>% 
  select(1:7)

# when testing with many Codes fix the below indexing
test_lm[1, 3] = 5
test_lm[14, 6] = 80
test_lm[13, 6] = NA # real value is 89
test_lm[10, 5] = NA # real value is 3.06
test_lm[6, 6] = NA # real value is 68.80
test_lm[6, 3] = NA
res <- imputeMineTest1(test_lm, meat_index)

# ***********
res <- df_to_lm %>% sample_n(1000) 
res2 <- res %>% 
  imputeMineTest1(meat_index)

# ***********
df_to_lm %>% filter(Code == "BWA") %>% View()

# ***********
df1 <- df_to_lm %>% filter(Code == "KAZ")

df <- bind_cols( df1  %>% 
             select_if(~!any(is.na(.))),
           df1 %>% 
             select(meat_index))

# ***********
less <- get_gp_tbl(df, meat_index, "KAZ")
final_df <- imputeMineTest1(df1, meat_index)

# ***********
repalce_means <- function(df, var) {
  var = enquo(var)
  
  df %>% mutate(
    !!var := replace_na(df[[as_label(var)]], 
                        mean(df[[as_label(var)]], na.rm = T))
  )
}
# ***********