# # tar_load(calc_drought)
# # 
# # library(data.table)
# 
# # Assuming 'your_data' is the data.table containing adm0, adm1, adm2, and standardised_intensity
# # Filter for the specific adm1 or adm2 values
# filtered_data <- calc_drought[
#   adm1 %in% c("Mokhotlong", "Leribe", "Butha-Buthe") |
#     adm2 %in% c("Anaocha", "Akko", "Biu", "Chikun", "Funakaye", "Guyuk", 
#                 "Igabi", "Igbo-Eze North", "Kaduna North", "Kanam", "Kauru", 
#                 "Kubau", "Kwami", "Lafia", "Mangu", "Mubi North", "Numan", 
#                 "Paikoro", "Qua'an Pan", "Shani", "Soba", "Suleja", "Yumbe", 
#                 "Arua", "Gulu", "Kisoro", "Buhweju", "Rukungiri", "Isingiro", 
#                 "Kabarole", "Bushenyi", "Adjumani", "Kabale", "Ntungamo", 
#                 "Mbarara", "Kamwenge", "Kanungu", "Kole", "Kyegegwa", 
#                 "Maracha", "Kyenjojo", "Nebbi", "Ntoroko", "Nwoya", "Kiruhura", 
#                 "Zombo", "Oyam", "Mitooma", "Sheema", "Florencia", "Leticia", 
#                 "Mitú", "Puerto Nariño", "San Andres de Tumaco", "San Felipe (ANM)", 
#                 "Solano", "Timbiquí", "Uribe", "Eengodi", "Eenhana", "Epembe", 
#                 "Guinas", "Kapako", "Karasburg", "Naminus (Luderitz)", "Okahao", 
#                 "Okaku", "Okankolo", "Okatana", "Olukonda", "Omulonga", 
#                 "Omuntele", "Onayena", "Ondangwa", "Ongwediva", "Oniipa", 
#                 "Onyaanya", "Oshikango", "Oshikuku", "Outapi", "Tsandi"),
#   .(adm0, adm1, adm2, standardised_intensity)
# ]
# 
# # Remove duplicate adm2 values and keep the first occurrence
# filtered_adm2 <- unique(filtered_data, by = "adm2")
# 
# # Add the adm1 regions where adm2 is not available
# adm1_data <- calc_drought[adm1 %in% c("Mokhotlong", "Leribe", "Butha-Buthe"),
#                        .(adm0, adm1, adm2 = NA, standardised_intensity)]
# 
# filtered_adm1 <- unique(adm1_data, by = "adm1")
# 
# # Combine the unique adm2 data with the unique adm1 regions without adm2
# final_data <- rbind(filtered_adm2, filtered_adm1, fill = TRUE)
# 
# # Order by adm0
# final_data <- final_data[order(adm0)]
# 
# ft <- flextable::flextable(final_data)
# 
# flextable::save_as_docx(ft, path = "figures_and_tables/dry_standard.docx")
