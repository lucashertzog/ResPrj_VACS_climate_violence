sens_do_mrg <- function(
    dat_vacs,
    spei
){
  d <- spei[, c(
    "adm0", "adm1", "adm2",
    "zero_to_moderate",
    "very_dry_drought_extreme",
    "constant_and_recent_long"
    ,
    "recent_long_period"
  )]
  
  for (col in c("zero_to_moderate", 
                "very_dry_drought_extreme",
                "constant_and_recent_long"
                ,
                "recent_long_period"
                
  )) {
    d[[col]] <- as.factor(d[[col]])
  }
  
  v <- dat_vacs[, c(
    "adm0", "adm1", "adm2",
    "strata", "cluster", "weight",
    "age", "marital", "edu_enrol", "pvt",
    "viol_sex", "ever_viol_sex"
  )]
  
  setDT(d)
  setDT(v)
  # Assign a random unique ID to each participant in 'v'
  v[, unique_id := sample.int(n = 1e9, size = .N, replace = FALSE)]
  
  v[, adm_id := paste(adm0, adm1, adm2, sep = "_")]
  d[, adm_id := paste(adm0, adm1, adm2, sep = "_")]
  
  ### Check ###
  
  # lets fix these.
  # > unique(v_not_in_mrg$adm_id)
  # [1] "Cambodia_Banteay Meanchey_Krong Paoy Paet"   
  # [2] "Cambodia_Battambang_Krong Battambang"        
  # [3] "Cambodia_Battambang_Rukhak Kiri"             
  # [4] "Cambodia_Kampong Cham_Dambae"                
  # [5] "Cambodia_Kampong Cham_Memot"                 
  # [6] "Cambodia_Kampong Cham_Ponhea Kraek"          
  # [7] "Cambodia_Kampong Speu_Krong Chbar Mon"       
  # [8] "Cambodia_Kandal_Popnhea Lueu"                
  # [9] "Cambodia_Kandal_Krong Ta Khmau"              
  # [10] "Cambodia_Phnom Penh_Ruessei Kaev"            
  # [11] "Cambodia_Phnom Penh_Pou Saenchey"            
  # [12] "Cambodia_Siemreap_Krong Siem Reab"           
  # [13] "Cambodia_Svay Rieng_Krong Svay Rieng"        
  # [14] "Cambodia_Oddar Meanchey_Krong Samraong"  
  
  v[adm_id == "Cambodia_Kampong Cham_Dambae", adm_id := "Cambodia_Tboung Khmum_Dambae"]
  v[adm_id == "Cambodia_Kampong Cham_Memot", adm_id := "Cambodia_Tboung Khmum_Memot"]
  v[adm_id == "Cambodia_Kampong Cham_Ponhea Kraek", adm_id := "Cambodia_Tboung Khmum_Ponhea Kraek"]
  v[adm_id == "Cambodia_Banteay Meanchey_Krong Paoy Paet", adm_id := "Cambodia_Banteay Meanchey_Paoy Paet"]
  v[adm_id == "Cambodia_Battambang_Krong Battambang", adm_id := "Cambodia_Battambang_Battambang"]
  v[adm_id == "Cambodia_Battambang_Rukhak Kiri", adm_id := "Cambodia_Battambang_Rukh Kiri"]
  v[adm_id == "Cambodia_Kampong Speu_Krong Chbar Mon", adm_id := "Cambodia_Kampong Speu_Chbar Mon"]
  v[adm_id == "Cambodia_Kandal_Popnhea Lueu", adm_id := "Cambodia_Kandal_Ponhea Lueu"]
  v[adm_id == "Cambodia_Kandal_Krong Ta Khmau", adm_id := "Cambodia_Kandal_Ta Khmau"]
  v[adm_id == "Cambodia_Phnom Penh_Ruessei Kaev", adm_id := "Cambodia_Phnom Penh_Russey Keo"]
  v[adm_id == "Cambodia_Phnom Penh_Pou Saenchey", adm_id := "Cambodia_Phnom Penh_Pur SenChey"]
  v[adm_id == "Cambodia_Siemreap_Krong Siem Reab", adm_id := "Cambodia_Siemreap_Siem Reap"]
  v[adm_id == "Cambodia_Svay Rieng_Krong Svay Rieng", adm_id := "Cambodia_Svay Rieng_Svay Rieng"]
  v[adm_id == "Cambodia_Oddar Meanchey_Krong Samraong", adm_id := "Cambodia_Oddar Meanchey_Samraong"]
  
  # [15] "Colombia_Bolívar_Cartagena"                  
  # [16] "Colombia_Nariño_San Andres de Tumaco"        
  # [17] "Colombia_Norte de Santander_Cúcuta"          
  # [18] "Colombia_San Adres y Providencia_San Andrés" 
  # [19] "Colombia_San Adres y Providencia_Providencia"
  # [20] "Colombia_Amazonas_Tarapacá (ANM)"            
  # [21] "Colombia_Guainía_Mapiripana (ANM)"           
  # [22] "Colombia_Guainía_San Felipe (ANM)"           
  # [23] "Colombia_Vaupés_Pacoa (ANM)"     
  
  
  v[adm_id == "Colombia_Amazonas_Tarapacá (ANM)", adm_id := "Colombia_Amazonas_Tarapacá"]
  v[adm_id == "Colombia_Bolívar_Cartagena", adm_id := "Colombia_Bolívar_Cartagena de Indias"]
  v[adm_id == "Colombia_Guainía_Mapiripana (ANM)", adm_id := "Colombia_Guainía_Mapiripana"]
  v[adm_id == "Colombia_Guainía_San Felipe (ANM)", adm_id := "Colombia_Guainía_San Felipe"]
  v[adm_id == "Colombia_Nariño_San Andres de Tumaco", adm_id := "Colombia_Nariño_San Andrés de Tumaco"]
  v[adm_id == "Colombia_Norte de Santander_Cúcuta", adm_id := "Colombia_Norte de Santander_Villa del Rosario"]
  v[adm_id == "Colombia_Vaupés_Pacoa (ANM)", adm_id := "Colombia_Vaupés_Pacoa"]
  v[adm_id == "Colombia_San Adres y Providencia_Providencia", adm_id := "Colombia_Archipiélago de San Andrés, Providencia y Santa Catalina_Providencia"]
  v[adm_id == "Colombia_San Adres y Providencia_San Andrés", adm_id := "Colombia_Archipiélago de San Andrés, Providencia y Santa Catalina_San Andrés"]
  
  # [24] "Côte d'Ivoire_N'zi_NA"  
  v[adm_id == "Côte d'Ivoire_N'zi_NA", adm_id := "Côte d'Ivoire_N'Zi_NA"]
  
  # [25] "El Salvador_La Libertad_Santa Tecla"         
  # [26] "El Salvador_La Libertad_San Juan Opico"      
  # [27] "El Salvador_San Salvador_Ciudad Delgado"     
  # [28] "El Salvador_Usulután_Berlin"                 
  # [29] "El Salvador_Usulután_Usulutan"               
  # [30] "El Salvador_San Miguel_San Rafael Oriente"   
  # [31] "El Salvador_La Unión_Yucuaiquin" 
  
  
  v[adm_id == "El Salvador_La Libertad_Santa Tecla", adm_id := "El Salvador_San Salvador_San Salvador"]
  v[adm_id == "El Salvador_La Libertad_San Juan Opico", adm_id := "El Salvador_La Libertad_Opico"]
  v[adm_id == "El Salvador_San Salvador_Ciudad Delgado", adm_id := "El Salvador_San Salvador_Delgado"]
  v[adm_id == "El Salvador_Usulután_Berlin", adm_id := "El Salvador_Usulután_Berlín"]
  v[adm_id == "El Salvador_Usulután_Usulutan", adm_id := "El Salvador_Usulután_Usulután"]
  v[adm_id == "El Salvador_San Miguel_San Rafael Oriente", adm_id := "El Salvador_San Miguel_San Rafael"]
  v[adm_id == "El Salvador_La Unión_Yucuaiquin", adm_id := "El Salvador_La Unión_Yucuaiquín"]
  
  
  # [32] "Kenya_Coast_Taita Taveta"                    
  # [33] "Kenya_North Eastern_Marsabit"                
  # [34] "Kenya_Eastern_Meru"                          
  # [35] "Kenya_Central_Laikipia"                      
  # [36] "Kenya_Nyanza_Kisii"                          
  # [37] "Kenya_Nyanza_Nyamira" 
  
  v[adm_id == "Kenya_Coast_Taita Taveta", adm_id := "Kenya_Coast_Taita"]
  v[adm_id == "Kenya_North Eastern_Marsabit", adm_id := "Kenya_Eastern_Marsabit"]
  v[adm_id == "Kenya_Eastern_Meru", adm_id := "Kenya_Eastern_Meru Central"]
  v[adm_id == "Kenya_Central_Laikipia", adm_id := "Kenya_Rift Valley_Laikipia"]
  v[adm_id == "Kenya_Nyanza_Kisii", adm_id := "Kenya_Nyanza_Kisii Central"]
  v[adm_id == "Kenya_Nyanza_Nyamira", adm_id := "Kenya_Nyanza_Kisii North (Nyamira)"]
  
  # [38] "Namibia_Oshikoto_Eenhana"
  v[adm_id == "Namibia_Oshikoto_Eenhana", adm_id := "Namibia_Ohangwena_Eenhana"]
  
  # [39] "Nigeria_Ekiti_Ado"  
  # [40] "Nigeria_Federal Capital Territory_Anaocha" 
  v[adm_id == "Nigeria_Ekiti_Ado", adm_id := "Nigeria_Ekiti_Ado Ekiti"]
  v[adm_id == "Nigeria_Federal Capital Territory_Anaocha", adm_id := "Nigeria_Federal Capital Territory_Abuja Municipal"]
  
  # [41] "Uganda_Eastern_Kakumiro" - checked crosswalks 208 district CDC
  v[adm_id == "Uganda_Eastern_Kakumiro", adm_id := "Uganda_Eastern_Kumi"] 
  
  # [42] "Zambia_Southern_Chikankata" 
  v[adm_id == "Zambia_Southern_Chikankata", adm_id := "Zambia_Southern_Chikankanta"] 
  
  # merge
  mrg <- v[d, on = "adm_id", nomatch = 0]
  
  return(mrg)
  
}

# v_not_in_mrg <- v[!mrg, on = "unique_id"]
# 
# col_d <- d[adm0 == "Colombia"]
# col_d <- col_d[order(adm_id)]
# unique(col_d$adm_id)
# head(unique(col_d$adm_id), 100)
# print(unique(col_d$adm_id)[101:150])


