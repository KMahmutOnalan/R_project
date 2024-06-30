


block4 <- read_excel("/Users/kubramahmutonalan/Downloads/block4.xlsx")


block4_faces<-  block4 %>% dplyr:: select(images_part4, key_resp_8.corr, key_resp_8.rt)

block4_words<-  block4 %>% dplyr:: select(words_part4, key_resp_10.corr, key_resp_10.rt)


block4_faces <- block4_faces %>%
  dplyr:: select(-matches(""^Visuals/uncovered_glasses_([1-9]|[1-2][0-9]|30)\\.png$""))


block4_df <- block4_faces %>% dplyr:: filter(!grepl("^Visuals/uncovered_glasses_([1-9]|[1-2][0-9]|30)\\.png$|^Visuals/uncovered_([1-9]|[1-2][0-9]|30)\\.png$",
images_part4))


block4_d  <- block4 %>% dplyr:: filter(!grepl("^Visuals/uncovered_glasses_([1-9]|[1-2][0-9]|30)\\.png$|^Visuals/uncovered_([1-9]|[1-2][0-9]|30)\\.png$|^Visuals/covered_([1-9]|[1-2][0-9]|30)\\.png$",
                                                images_part4))

correct_data <- filter(block4_d, key_resp_8.corr == 1)

