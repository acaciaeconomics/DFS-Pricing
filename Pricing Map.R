#Mattew Russell
#30/04/2026

rm(list = ls())
#-------------------------------------------------------------------------------
#Libraries
#-------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(rnaturalearth)
library(stringr)
library(priceR)

options(scipen = 999)
#-------------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------

#IPA data
dfs_pricing = read.csv("DFS_Transations_Pricing_Data_IPA.csv")

dfs_pricing_clean = dfs_pricing %>% 
  group_by(country,provider,fsp_type,transaction_type)%>%
  mutate(most_recent_date = max(date_collection)) %>% 
  filter(most_recent_date == date_collection) %>% 
  ungroup()

dat_all = dfs_pricing_clean %>%
  mutate(country = str_to_title(country)) %>% 
  mutate(across(c(country, fsp_type, transaction_type), ~ str_to_title(.x)),
         value_max = as.numeric(value_max)) %>%
  filter(
    !(country %in% c("Myanmar","Colombia","India","Mali","Sierra Leone","Paraguay")), #Paraguay data seems to contain errors
         !if_any(c(value_max,value_min,fee), ~ .x<0)
         ) %>%  #Change p2p on-net trafs etc to on us off us etc
  mutate(country = ifelse(country == "Cote D'ivoire", "Ivory Coast", country),
         ipa_data = 1,
         pct_excess = 0,
         transaction_type = str_to_title(transaction_type),
         transaction_type = ifelse(transaction_type=="P2p On-Network Transfer", "P2P On-Us Transfer", transaction_type),
         transaction_type = ifelse(transaction_type=="P2p Off-Network Transfer", "P2P Off-Us Transfer", transaction_type),
         fee_pct = fee_pct/100) #We work with decimal rather than % for the remainder of this workbook


#-------------------------------------------------------------------------------
#Countries to add
#Appending countries to this data
#-------------------------------------------------------------------------------

dat_all = dat_all %>% 
  #-------------------------------------------------------------------------------------------------
  #Brazil
  add_row(country = "Brazil", fsp_type = "Mobile Banking",transaction_type = "P2P On-Us Transfer",
          value_min = 1, provider = "PicPay",value_max = Inf, fee = 0, exchange_rate = 4.99367) %>%  #exchange rate as at 30/04/2026:https://www.oanda.com/currency-converter/en/?from=USD&to=BRL&amount=1
  add_row(country = "Brazil", fsp_type = "Mobile Banking",transaction_type = "P2P Off-Us Transfer",
          value_min = 1, provider = "PicPay",value_max = Inf, fee = 0, exchange_rate = 4.99367) %>% 
  add_row(country = "Brazil", fsp_type = "Mobile Banking",transaction_type = "P2P On-Us Transfer",
          value_min = 1, provider = "Nubank",value_max = Inf, fee = 0, exchange_rate = 4.99367) %>% 
  add_row(country = "Brazil", fsp_type = "Mobile Banking",transaction_type = "P2P Off-Us Transfer",
          value_min = 1, provider = "Nubank",value_max = Inf, fee = 0, exchange_rate = 4.99367) %>% 
  #-------------------------------------------------------------------------------------------------
  #Peru
  # Yape
  add_row(country = "Peru", fsp_type = "Mobile Banking",transaction_type = "P2P On-Us Transfer",
          value_min = 1, provider = "Yape",value_max = Inf, fee = 0, exchange_rate = 3.51242) %>%  #exchange rate as at 30/04/2026:https://www.oanda.com/currency-converter/en/?from=USD&to=PEN&amount=1
 
  add_row(country = "Peru", fsp_type = "Mobile Banking",transaction_type = "P2P Off-Us Transfer",
          value_min = 1, provider = "Yape",value_max = Inf, fee = 0, exchange_rate = 3.51242) %>%
  
  add_row(country = "Peru", fsp_type = "Mobile Banking",transaction_type = "P2P On-Us Transfer",
          value_min = 1, provider = "Plin",value_max = Inf, fee = 0, exchange_rate = 3.51242) %>%
  add_row(country = "Peru", fsp_type = "Mobile Banking",transaction_type = "P2P Off-Us Transfer",
          value_min = 1, provider = "Plin",value_max = Inf, fee = 0, exchange_rate = 3.51242) %>%
  
   #-------------------------------------------------------------------------------------------------
  #Rwanda
  #P2P On-Us Transfer (On-net) #https://www.mtn.co.rw/momo-tarrif/
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 1, provider = "MTN", value_max = 1000, fee = 20, exchange_rate = 1461) %>% #exchange rate as at 30/04/2026:https://www.oanda.com/currency-converter/en/?from=USD&to=RWF&amount=1
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 1001, provider = "MTN", value_max = 10000, fee = 100, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 10001, provider = "MTN", value_max = 150000, fee = 250, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 150001, provider = "MTN", value_max = 2000000, fee = 1500, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 2000001, provider = "MTN", value_max = 5000000, fee = 3000, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 5000001, provider = "MTN", value_max = 10000000, fee = 5000, exchange_rate = 1461) %>%
  
  #P2P Off-Us Transfer (Off-net)
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 1, provider = "MTN", value_max = 1000, fee = 40, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 1001, provider = "MTN", value_max = 10000, fee = 120, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 10001, provider = "MTN", value_max = 150000, fee = 270, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 150001, provider = "MTN", value_max = 2000000, fee = 1520, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 2000001, provider = "MTN", value_max = 5000000, fee = 3020, exchange_rate = 1461) %>%
  add_row(country = "Rwanda", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 5000001, provider = "MTN", value_max = 10000000, fee = NA, exchange_rate = 1461) %>% 
  
  #https://bk.rw/about/bk-tariffs
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Bank of Kigali", value_max = Inf, fee = 500, exchange_rate = 1461) %>% #exchange rate as at 30/04/2026:https://www.oanda.com/currency-converter/en/?from=USD&to=RWF&amount=1
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Bank of Kigali", value_max = Inf, fee = 0, exchange_rate = 1461) %>%
  
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 0, provider = "Bank of Kigali", value_max = 1000, fee = 20, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 1001, provider = "Bank of Kigali", value_max = 10000, fee = 100, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 10001, provider = "Bank of Kigali", value_max = 150000, fee = 200, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 150001, provider = "Bank of Kigali", value_max = 2000000, fee = 1350, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 2000001, provider = "Bank of Kigali", value_max = 5000000, fee = 3000, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 5000001, provider = "Bank of Kigali", value_max = Inf, fee = 5000, exchange_rate = 1461) %>% 
  
  
  #https://www.imbankgroup.com/rw/wp-content/uploads/sites/4/2025/08/TARIFF_IM_EN_25.4X33.5CM_190825_DW.pdf
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
        value_min = 0, provider = "I&M", value_max = Inf, fee = 200, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "I&M", value_max = Inf, fee = 0, exchange_rate = 1461) %>% 
  
  #MTN fees
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 0, provider = "Bank of Kigali", value_max = 5000, fee = 100, exchange_rate = 1461) %>%   #very unique scheme, push and pull fees
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 5001, provider = "Bank of Kigali", value_max = 40000, fee = 600, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 40001, provider = "Bank of Kigali", value_max = 300000, fee = 1500, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 300001, provider = "Bank of Kigali", value_max = 2000000, fee = 5000, exchange_rate = 1461) %>% 

  #Airtel fees
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 1001, provider = "Airtel", value_max = 3000, fee = 200, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 3001, provider = "Airtel", value_max = 5000, fee = 300, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 5001, provider = "Airtel", value_max = 10000, fee = 400, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 10001, provider = "Airtel", value_max = 20000, fee = 500, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 20001, provider = "Airtel", value_max = 40000, fee = 700, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 40001, provider = "Airtel", value_max = 75000, fee = 1100, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 75001, provider = "Airtel", value_max = 150000, fee = 2000, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 150001, provider = "Airtel", value_max = 300000, fee = 3000, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 300001, provider = "Airtel", value_max = 500000, fee = 4900, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 500001, provider = "Airtel", value_max = 1000000, fee = 8800, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 1000001, provider = "Airtel", value_max = 1500000, fee = 12000, exchange_rate = 1461) %>% 
  add_row(country = "Rwanda", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 1500001, provider = "Airtel", value_max = 2000000, fee = 15000, exchange_rate = 1461) %>%
  
  #-------------------------------------------------------------------------------------------------
  #Paraguay
  #https://ayuda.tigo.com.py/hc/centro-de-ayuda/articles/2611174965838611-que-diferencia-hay-entre-un-giro-y-un-envio-tigo-money
  add_row(country = "Paraguay", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Tigo Money", value_max = Inf, fee = 0, exchange_rate = 6159.41) %>% #exchange rate as at 30/04/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=PYG&amount=1
 
   #-------------------------------------------------------------------------------------------------
  #Cote d'ivoire
  #https://web.archive.org/web/20250720101358/https://societegenerale.ci/fileadmin/user_upload/cote_ivoire/PDF/CONDITION_BANCAIRE_PARTICULIER_60x80.pdf
  add_row(country = "Ivory Coast", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "SGCI", value_max = Inf, fee = 0, exchange_rate = 559.472) %>% #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=UGX&amount=1

  add_row(country = "Ivory Coast", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "SGCI", value_max = 1718750, fee = 0, fee_pct = 0.02, exchange_rate = 559.472, notes = "max of 34375") %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=UGX&amount=1
  
  add_row(country = "Ivory Coast", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 1718750, provider = "SGCI", value_max = Inf, fee = 34375, exchange_rate = 559.472, notes = "max of 34375") %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=UGX&amount=1
  
  #https://www.banqueatlantique.net/wp-content/uploads/2026/01/MISE-A-JOUR-PARTICULIER-CONDITIONS-DEBITRICES-BACI-Semestre-1-2026.pdf
  add_row(country = "Ivory Coast", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Banque Atlantique", value_max = Inf, fee = 0, exchange_rate = 559.472, notes = "max of 34375") %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=UGX&amount=1

  #-------------------------------------------------------------------------------------------------
  #Nigeria
  #https://www.momo.ng/frequently-asked-questions/
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
        value_min = 0, provider = "MoMoPSB", value_max = Inf, fee = 0,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mo", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "MoMoPSB", value_max = Inf, fee = 10,exchange_rate = 1375.48) %>% 

  #https://www.opaycheckout.com/pricing.html
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Opay", value_max = Inf, fee = 5, notes = "Max of N2000",exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Opay", value_max = 5000, fee = 10,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 5001, provider = "Opay", value_max = 50000, fee = 25,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 50001, provider = "Opay", value_max = Inf, fee = 50,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
 
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "Wallet To Bank",
          value_min = 0, provider = "Opay", value_max = 5000, fee = 10,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "Wallet To Bank",
          value_min = 5001, provider = "Opay", value_max = 50000, fee = 25,exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "Wallet To Bank",
          value_min = 50001, provider = "Opay", value_max = Inf, fee = 50,exchange_rate = 1375.48) %>%
  
  #https://www.palmpay.com/nigeria/
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Palmpay", value_max = Inf, fee = 0, exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Money", transaction_type = "Wallet To Bank",
          value_min = 0, provider = "Palmpay", value_max = Inf, fee = 0, exchange_rate = 1375.48) %>% 
  
  #Access Bank
  #https://www.accessbankplc.com/Rates-Guide/index.html#mobilebanking
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "Access Bank", value_max = Inf, fee = 0, notes = "Intra-Bank Transfer (Access To Access)", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Access Bank", value_max = 5000, fee = 10.00, notes = "N10 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 5001, provider = "Access Bank", value_max = 50000, fee = 25.00, notes = "N25 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 50001, provider = "Access Bank", value_max = Inf, fee = 50, notes = "N50 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
 
   add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 0, provider = "Access Bank", value_max = 5000, fee = 10.00, notes = "N10 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 5001, provider = "Access Bank", value_max = 50000, fee = 25.00, notes = "N25 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 50001, provider = "Access Bank", value_max = Inf, fee = 50, notes = "N50 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  
  #https://www.ubagroup.com/nigeria/wp-content/uploads/sites/2/2025/11/UBA-Service-Charges.pdf
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "UBA", value_max = Inf, fee = 0, notes = "Send Money UBA to UBA", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "UBA", value_max = 5000, fee = 10.00, notes = "N10 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 5001, provider = "UBA", value_max = 50000, fee = 25, notes = "N25 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 50001, provider = "UBA", value_max = Inf, fee = 50, notes = "N50 + 7.5% VAT", exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
   
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 0, provider = "UBA", value_max = 5000, fee = 20, exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Nigeria", fsp_type = "Mobile Banking", transaction_type = "Bank To Wallet",
          value_min = 5001, provider = "UBA", value_max = Inf, fee = 50, exchange_rate = 1375.48) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  
  #-------------------------------------------------------------------------------------------------
  #Paraguay
  #https://www.bnf.gov.py/bnf/web/#/tarifario
  add_row(country = "Paraguay", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",
          value_min = 0, provider = "BNF", value_max = Inf, fee = 33, exchange_rate = 6134.21) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  add_row(country = "Paraguay", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "BNF", value_max = Inf, fee = 33, exchange_rate = 6134.21) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1
  #https://www.itau.com.py/Content/archivos/Dinamicos/Tasas_y_tarifas_vigentes-8584245702488609972.pdf
  add_row(country = "Paraguay", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Itau", value_max = Inf, fee = 33, exchange_rate = 6134.21) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=NGN&amount=1

  #-------------------------------------------------------------------------------------------------
  #Ghana
    # https://www.stanbicbank.com.gh/static_file/ghana/Downloadable%20Files/Pricing%20Guides/Private%20Banking%20Pricing.pdf
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Stanbic Bank", value_max = Inf, fee = 0, exchange_rate = 11.20) %>% # Inter account transfers 
  
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
              value_min = 0, provider = "Stanbic Bank", value_max = Inf, notes = "ACH transfer",fee = 5, exchange_rate = 11.20) %>%  #exchange rate as at 04/05/2026 :https://www.oanda.com/currency-converter/en/?from=USD&to=GHS&amount=1

      #Removed for now
      # add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
      #         value_min = 0, provider = "Stanbic Bank", value_max = Inf, fee = 30, notes = "RTGS transfer",exchange_rate = 11.20) %>% 
  
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
              value_min = 0, provider = "Stanbic Bank", value_max = Inf, fee = 10, exchange_rate = 11.20) %>% #Done
      # GIP Transfers (1% capped at GHS 10)
  
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
              value_min = 0, provider = "Stanbic Bank", value_max = 1000, fee = 0.01, notes = "GIP", exchange_rate = 11.20) %>% 
  
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
              value_min = 1000, provider = "Stanbic Bank", value_max = Inf, notes = "GIP",fee = 10, exchange_rate = 11.20) %>% 
     
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "Bank-to-Wallet Transfer", 
              value_min = 0, provider = "Stanbic Bank", value_max = 1000, fee = 0.01, exchange_rate = 11.20) %>% 
    add_row(country = "Ghana", fsp_type = "Mobile Banking", transaction_type = "Bank-to-Wallet Transfer", 
              value_min = 1000, provider = "Stanbic Bank", value_max = Inf, fee = 10, exchange_rate = 11.20) %>% 
  
  #-------------------------------------------------------------------------------------------------
  #Malaysia 
  #https://www.touchngo.com.my/consumer/payments/ewallet-transfer/
    add_row(country = "Malaysia", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
              value_min = 0, provider = "TNG", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    add_row(country = "Malaysia", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", 
              value_min = 0, provider = "TNG", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>% #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
     
   #https://www.grab.com/my/terms-policies/grabpay-wallet-product-disclosure-sheet-risalah-pendedahan-produk-dompet-grabpay/
    add_row(country = "Malaysia", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
              value_min = 0, provider = "Grabpay", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    add_row(country = "Malaysia", fsp_type = "Fintech", transaction_type = "P2P Off-Us Transfer", 
                value_min = 0, provider = "Grabpay", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
      
    #https://www.maybank2u.com.my/maybank2u/malaysia/en/personal/services/digital_banking/duitnow.page
    #https://paynet.my/faq/business-duitnow.html
    #https://www.pbebank.com/en/rates-charges/online-banking-and-mobile-banking/
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Public Bank", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "Public Bank", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%   #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
   
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "CIMB", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "CIMB", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%    #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Maybank", value_max = Inf, fee = 0, exchange_rate = 3.9675) %>%  #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
    add_row(country = "Malaysia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "Maybank", value_max = Inf, fee = 0, exchange_rate = 3.9675)  %>%   #exchange rate as at 04/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=MYR&amount=1
  
  #-------------------------------------------------------------------------------------------------
  #Thailand
  #https://www.truemoney.com/en/rates/
    add_row(country = "Thailand", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "True Money", value_max = Inf, fee = 0, exchange_rate = 32.591) %>%  #exchange rate as at 05/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
    add_row(country = "Thailand", fsp_type = "Fintech", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "True Money", value_max = Inf, fee = 20, exchange_rate = 32.591) %>% 

    #https://help2.line.me/linepay_th/ios/pc?country=TH&lang=en&contentId=50010551
    add_row(country = "Thailand", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Line pay", value_max = Inf, fee = 0, exchange_rate = 32.591) %>% 
  
    #https://krungthai.com/en/rates/viewdetail/41
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Krungthai", value_max = Inf, fee = 0, exchange_rate = 32.591) %>%  #exchange rate as at 05/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "Krungthai", value_max = Inf, fee = 0, exchange_rate = 32.591) %>%  
    
    #https://www.krungsri.com/en/personal/digital-banking/krungsri-app/knowledge/fees-limit-amounts
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Bank of Ayudhya", value_max = Inf, fee = 0, exchange_rate = 32.591) %>%  #exchange rate as at 05/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "Bank of Ayudhya", value_max = Inf, fee = 0, exchange_rate = 32.591)  %>% 
    
    #https://www.kasikornbank.com/en/personal/services/payment/pages/transferkplus.aspx
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "Kasikorn Bank", value_max = Inf, fee = 0, exchange_rate = 32.591) %>%  #exchange rate as at 05/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
    add_row(country = "Thailand", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
            value_min = 0, provider = "Kasikorn Bank", value_max = Inf, fee = 0, exchange_rate = 32.591) %>% 
  
  
  #-------------------------------------------------------------------------------------------------
  #Philippines
  #https://help.grab.com/passenger/en-ph/360029791152-How-do-I-transfer-my-GrabPay-balance-to-a-bank-account-or-another-e-wallet#:~:text=Facility%3A%20Users%20can%20transfer%20a,Tap%20on%20'Send'
  add_row(country = "Philippines", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "Grab Pay", value_max = Inf, fee = 0, exchange_rate = 61.5134) %>%  #exchange rate as at 06/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
  add_row(country = "Philippines", fsp_type = "Fintech", transaction_type = "P2P Off-Us Transfer", 
        value_min = 0, provider = "Grab pay", value_max = Inf, fee = 15, exchange_rate = 61.5134)  %>% 
  
  #-------------------------------------------------------------------------------------------------
  #Singapore
  #https://www.grab.com/sg/pay/funds-transfer/
  add_row(country = "Singapore", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "Grab Pay", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>%  #exchange rate as at 06/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
  add_row(country = "Singapore", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "Grab pay", value_max = Inf, fee = 0, exchange_rate = 1.27613)  %>%
  
  #https://www.dbs.com.sg/personal/landing/paylah/faq.html
  add_row(country = "Singapore", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "DBS Paylah", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>%  #exchange rate as at 06/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
  add_row(country = "Singapore", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "DBS Paylah", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>% 
  
  #https://www.dbs.com.sg/personal/support/bank-local-funds-transfer-transfer-to-other-bank-accounts.html
  add_row(country = "Singapore", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "DBS", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>%  #exchange rate as at 06/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
  add_row(country = "Singapore", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "DBS", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>% 
  
  #https://www.ocbc.com/iwov-resources/sg/ocbc/personal/pdf/help-and-support/general/personal-banking-pricing-guide-wef-1-march-2k25.pdf
  add_row(country = "Singapore", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "OCBC", value_max = Inf, fee = 0, exchange_rate = 1.27613) %>%  #exchange rate as at 06/05/2026 : https://www.oanda.com/currency-converter/en/?from=USD&to=THB&amount=1
  add_row(country = "Singapore", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "OCBC", value_max = Inf, fee = 0.5, exchange_rate = 1.27613) %>% 
  #-------------------------------------------------------------------------------------------------
  #India
  add_row(country = "India", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>% 
  add_row(country = "India", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>% 
  add_row(country = "India", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>% 
  add_row(country = "India", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>%
  add_row(country = "India", fsp_type = "Fintech", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>% 
  add_row(country = "India", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "", value_max = Inf, fee = 0, exchange_rate = 95.1368) %>%
  
  #-------------------------------------------------------------------------------------------------
  #Bangladesh
  add_row(country = "Bangladesh", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "Nagad", value_max = Inf, fee = 5, exchange_rate =121.602 ) %>%   #exchange rate as at 06/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=BDT&amount=1
  
  #-------------------------------------------------------------------------------------------------
  #Indonesia 
  #https://gopay.co.id/transfer
  add_row(country = "Indonesia", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "gopay", value_max = Inf, fee = 5, exchange_rate =17407.6 ) %>%   #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
  
  #https://www.dana.id/blog/apakah-ada-biaya-bulanan-di-dana/
  add_row(country = "Indonesia", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "Dana", value_max = Inf, fee = 0, exchange_rate =17407.6 ) %>% 
  add_row(country = "Indonesia", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "Dana", value_max = Inf, fee = 2500, notes = "If limit of 100 transfer per month exceeded.",exchange_rate =17407.6 ) %>%  #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
 
  #https://www.bca.co.id/en/Individu/produk/simpanan/Tahapan
   add_row(country = "Indonesia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "BCA", value_max = Inf, fee = 2500,exchange_rate =17407.6 ) %>% 
  add_row(country = "Indonesia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
          value_min = 0, provider = "BCA", value_max = Inf, fee = 0,exchange_rate =17407.6 ) %>% 

  #https://bri.co.id/en/fees-and-rates
  add_row(country = "Indonesia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", 
          value_min = 0, provider = "BRI", value_max = Inf, fee = 2500,exchange_rate =17407.6 ) %>% 
  add_row(country = "Indonesia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", 
            value_min = 0, provider = "BRI", value_max = Inf, fee = 0,exchange_rate =17407.6) %>% 
  
  #-------------------------------------------------------------------------------------------------
  #Costa Rica 
  #https://app.powerbi.com/view?r=eyJrIjoiZmVkOGM0M2MtODc1Mi00ZjZkLWE0MGYtYjZmMmJlMGY5NjA2IiwidCI6IjYxOGQwYTQ1LTI1YTYtNDYxOC05ZjgwLThmNzBhNDM1ZWU1MiJ9
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco Nacional de Costa Rica", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 200000, provider = "Banco Nacional de Costa Rica", value_max =Inf , fee = 2 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>%  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco Nacional de Costa Rica", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    
          value_min = 200000, provider = "Banco Nacional de Costa Rica", value_max =Inf , fee = 2 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>%  
  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco de Costa Rica", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 200000, provider = "Banco de Costa Rica", value_max =Inf , fee = 3 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>%  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco de Costa Rica", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    
          value_min = 200000, provider = "Banco de Costa Rica", value_max =Inf , fee = 3 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>%  
  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco Popular y de Desarrollo Comunal", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 200000, provider = "Banco Popular y de Desarrollo Comunal", value_max =Inf , fee = 3 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>%  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",     #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Banco Popular y de Desarrollo Comunal", value_max = 200000, fee = 0,exchange_rate =452.371 ) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    
          value_min = 200000, provider = "Banco Popular y de Desarrollo Comunal", value_max =Inf , fee = 3 ,exchange_rate =1, notes = "Charge listed in dollars." ) %>% 
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Teledolar", value_max = 200000, fee = 0,exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    
          value_min = 200000, provider = "Teledolar", value_max =Inf , pct_excess = 0.01, exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Teledolar", value_max = 200000, fee = 0,exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    
          value_min = 200000, provider = "Teledolar", value_max =Inf , pct_excess = 0.01, exchange_rate=452.371) %>% 
  
  
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Airpak", value_max = 200000, fee = 0,exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    
          value_min = 200000, provider = "Airpak", value_max =Inf , fee = 1000, exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=IDR&amount=1
          value_min = 0, provider = "Airpak", value_max = 200000, fee = 0,exchange_rate=452.371) %>% 
  add_row(country = "Costa Rica", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    
          value_min = 200000, provider = "Airpak", value_max =Inf , fee = 1000, exchange_rate=452.371) %>% 
  #-------------------------------------------------------------------------------------------------
  #Philipines  
  #https://www.bsp.gov.ph/PaymentAndSettlement/Fees.pdf
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "BDO", value_max = Inf, fee = 10,exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "BDO", value_max =Inf , fee = 10, exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "Union Bank of the Philippines", value_max = Inf, fee = 10,exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "Union Bank of the Philippines", value_max =Inf , fee = 10, exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "Union Bank of the Philippines", value_max = Inf, fee = 10,exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "Union Bank of the Philippines", value_max =Inf , fee = 10, exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "Metropolitan Bank and Trust Company", value_max = Inf, fee = 16.5, notes = "Midpoint of 8-25 taken.", exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "Metropolitan Bank and Trust Company", value_max =Inf , fee = 16.5, notes = "Midpoint of 8-25 taken.", exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "Metropolitan Bank and Trust Company", value_max = Inf, fee = 16.5, notes = "Midpoint of 8-25 taken.", exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "Metropolitan Bank and Trust Company", value_max =Inf , fee = 16.5, notes = "Midpoint of 8-25 taken.", exchange_rate=60.9896) %>% 
  #Mobile money - taken instapay fees
  #G-exchange
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "G-exchange", value_max = Inf, fee = 15, exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "G-exchange", value_max =Inf , fee = 15, exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "G-exchange", value_max = Inf, fee = 15, exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "G-exchange", value_max =Inf , fee = 15, exchange_rate=60.9896) %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "Maya Bank", value_max = Inf, fee = 15, exchange_rate=60.9896) %>% 
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",    
          value_min = 0, provider = "Maya Bank", value_max =Inf , fee = 15, exchange_rate=60.9896)  %>% 
  
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=PHP&amount=1
          value_min = 0, provider = "GoTyme Bank", value_max = Inf, fee = 4.5, notes = "Midpoint of 0-9 taken.", exchange_rate=60.9896) %>%
  add_row(country = "Philippines", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "GoTyme Bank", value_max =Inf , fee = 4.5, notes = "Midpoint of 0-9 taken.", exchange_rate=60.9896) %>% 
  
  #-------------------------------------------------------------------------------------------------
  #Australia
  #https://www.nab.com.au/important-information/personal/internet-banking-terms-conditions
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=AUD&amount=1
          value_min = 0, provider = "NAB", value_max = Inf, fee = 0, exchange_rate=1.38319) %>%
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "NAB", value_max =Inf , fee = 0, exchange_rate=1.38319) %>% 
  
  #https://www.nab.com.au/important-information/personal/internet-banking-terms-conditions
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=AUD&amount=1
          value_min = 0, provider = "CommonWealth Bank", value_max = Inf, fee = 0, exchange_rate=1.38319) %>%
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "CommonWealth Bank", value_max =Inf , fee = 0, exchange_rate=1.38319) %>% 

  #https://www.westpac.com.au/faq/payid-osko-fees/
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=AUD&amount=1
          value_min = 0, provider = "Westpac", value_max = Inf, fee = 0, exchange_rate=1.38319) %>%
  add_row(country = "Australia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Westpac", value_max =Inf , fee = 0, exchange_rate=1.38319) %>% 
  
  #-------------------------------------------------------------------------------------------------
  #UK
  #https://www.lloydsbank.com/business/commercial-banking/rates-and-charges.html
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=GBP&amount=1
          value_min = 0, provider = "lloyds", value_max = Inf, fee = 0, exchange_rate=0.73816) %>%
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "lloyds", value_max =Inf , fee = 0, exchange_rate=0.73816) %>% 
  
  #https://www.hsbc.co.uk/current-accounts/what-is-faster-payments/
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=GBP&amount=1
          value_min = 0, provider = "HSBC", value_max = Inf, fee = 0, exchange_rate=0.73816) %>%
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "HSBC", value_max =Inf , fee = 0, exchange_rate=0.73816) %>% 
  
  #https://www.barclays.co.uk/content/dam/documents/personal/International/International_Banking_Tariff_guide_PP045_UK.pdf
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=GBP&amount=1
          value_min = 0, provider = "HSBC", value_max = Inf, fee = 0, exchange_rate=0.73816) %>%
  add_row(country = "United Kingdom", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "HSBC", value_max =Inf , fee = 0, exchange_rate=0.73816) %>% 
  
  #-------------------------------------------------------------------------------------------------
  #Argentina
  #https://www.mercadopago.com.mx/knowledge-hub/costo-transferencias-fondos_16229
  add_row(country = "Argentina", fsp_type = "Fintech", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=ARS&amount=1
          value_min = 0, provider = "Mercado Pagos", value_max = Inf, fee = 0, exchange_rate=1390.02) %>%
  add_row(country = "Argentina", fsp_type = "Fintech", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Fintech", value_max =Inf , fee = 0, exchange_rate=1390.02) %>% 
  
  #https://www.modo.com.ar/blog/que-es-modo-y-como-funciona-guia-completa
  add_row(country = "Argentina", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=ARS&amount=1
          value_min = 0, provider = "Modo", value_max = Inf, fee = 0, exchange_rate=1390.02) %>%
  add_row(country = "Argentina", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Modo", value_max =Inf , fee = 0, exchange_rate=1390.02) %>% 
  
  #https://www.uala.com.ar/transferencias
  add_row(country = "Argentina", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer",    #exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=ARS&amount=1
          value_min = 0, provider = "Uala", value_max = Inf, fee = 0, exchange_rate=1390.02) %>%
  add_row(country = "Argentina", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer",
          value_min = 0, provider = "Uala", value_max =Inf , fee = 0, exchange_rate=1390.02) %>% 
  
  
  #-------------------------------------------------------------------------------------------------
#Tanzania
#https://cdn-webportal.airtelstream.net/website/airtel-money/tanzania/assets/pdf/Airtel-Money-Tarrif-English-2026.pdf
#exchange rate as at 12/05/2025: https://www.oanda.com/currency-converter/en/?from=USD&to=TZS&amount=1

# Airtel P2P On-Us Transfer (Airtel to Airtel)
add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 100, value_max = 999, fee = 10) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 1000, value_max = 1999, fee = 25) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 2000, value_max = 2999, fee = 25) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 3000, value_max = 3999, fee = 40) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 4000, value_max = 4999, fee = 50) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 5000, value_max = 6999, fee = 120) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 7000, value_max = 9999, fee = 140) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 10000, value_max = 14999, fee = 325) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 15000, value_max = 19999, fee = 350) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 20000, value_max = 29999, fee = 360) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 30000, value_max = 39999, fee = 375) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 40000, value_max = 49999, fee = 380) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 50000, value_max = 99999, fee = 675) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 100000, value_max = 199999, fee = 940) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 200000, value_max = 299999, fee = 1200) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 300000, value_max = 399999, fee = 1450) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 400000, value_max = 499999, fee = 1450) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 500000, value_max = 599999, fee = 2100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 600000, value_max = 699999, fee = 3100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 700000, value_max = 799999, fee = 3100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 800000, value_max = 899999, fee = 3250) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 900000, value_max = 1000000, fee = 3300) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 1000001, value_max = 3000000, fee = 4000) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 3000001, value_max = 5000000, fee = 4000) %>%
  
  # Airtel P2P Off-Us Transfer (To Other Networks)
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 100, value_max = 999, fee = 10) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 1000, value_max = 1999, fee = 45) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 2000, value_max = 2999, fee = 45) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 3000, value_max = 3999, fee = 90) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 4000, value_max = 4999, fee = 90) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 5000, value_max = 6999, fee = 180) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 7000, value_max = 9999, fee = 180) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 10000, value_max = 14999, fee = 495) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 15000, value_max = 19999, fee = 495) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 20000, value_max = 29999, fee = 540) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 30000, value_max = 39999, fee = 612) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 40000, value_max = 49999, fee = 675) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 50000, value_max = 99999, fee = 1125) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 100000, value_max = 199999, fee = 1440) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 200000, value_max = 299999, fee = 1710) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 300000, value_max = 399999, fee = 2070) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 400000, value_max = 499999, fee = 2250) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 500000, value_max = 599999, fee = 2880) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 600000, value_max = 699999, fee = 3870) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 700000, value_max = 799999, fee = 3870) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 800000, value_max = 899999, fee = 3870) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 900000, value_max = 1000000, fee = 5400) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 1000001, value_max = 3000000, fee = 5400) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Airtel", exchange_rate = 2694.99, value_min = 3000001, value_max = 5000000, fee = 5400) %>%
 
  #CRDB Bank
  #https://crdbbank.co.tz/en/personal/digital-banking/simbanking
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=TZS&amount=1
  
  # CRDB P2P On-Us Transfer
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CRDB", exchange_rate = 2605.15, value_min = 1, value_max = Inf, fee = 0) %>%
    
  # CRDB P2P Off-Us Transfer
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "CRDB", exchange_rate = 2605.15, value_min = 1, value_max = 10000000, fee = 2360) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "CRDB", exchange_rate = 2605.15, value_min = 10000001, value_max = 50000000, fee = 5900) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "CRDB", exchange_rate = 2605.15, value_min = 50000001, value_max = Inf, fee = 11800) %>%
    
  # CRDB Bank to Wallet
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 0, value_max = 4999, fee = 950) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 5000, value_max = 9999, fee = 1800) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 10000, value_max = 19999, fee = 2100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 20000, value_max = 29999, fee = 2700) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 30000, value_max = 49999, fee = 4000) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 50000, value_max = 99999, fee = 5300) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 100000, value_max = 199999, fee = 7100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 200000, value_max = 299999, fee = 7700) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 300000, value_max = 399999, fee = 8300) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 400000, value_max = 499999, fee = 9100) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 500000, value_max = 999999, fee = 10700) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "CRDB", exchange_rate = 2605.15, value_min = 1000000, value_max = Inf, fee = 12000) %>%
    
  # CRDB Cash-Out
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 5000, value_max = 19999, fee = 1200) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 20000, value_max = 49999, fee = 1300) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 50000, value_max = 99000, fee = 1500) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 100000, value_max = 199999, fee = 1600) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 200000, value_max = 399999, fee = 1700) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 400000, value_max = 499999, fee = 2200) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 500000, value_max = 599999, fee = 2500) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 600000, value_max = 799999, fee = 3000) %>%
  add_row(country = "Tanzania", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "CRDB", exchange_rate = 2605.15, value_min = 800000, value_max = 1000000, fee = 4000) %>%
  #-------------------------------------------------------------------------------------------------
  #Ethiopia
  #https://combanketh.et/uploads/Terms_and_Tariffs_8151c11230.pdf
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=ETB&amount=1
  
  # CBE P2P On-Us Transfer (On-network)
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 1, value_max = 1000, fee= 0.5) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 1001, value_max = 5000, fee = 1) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 5001, value_max = 10000, fee = 2) %>%    
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 10001, value_max = 50000, fee = 3) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 50001, value_max = 100000, fee = 5) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 100001, value_max = 200000, fee = 10) %>%    
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 200001, value_max = 300000, fee = 15) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 300001, value_max = Inf, fee = 20) %>%
    
  # CBE P2P Off-Us Transfer (Off-network)
    add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "CBE", exchange_rate = 157.375, value_min = 1, value_max = Inf, fee = 50) %>%
      
    
  #Awash Bank
  #https://awashbank.com/
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=ETB&amount=1
  
  # Awash Bank P2P On-Us Transfer (On-network)
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 1, value_max = 1000, fee = 1) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 1001, value_max = 5000, fee = 2) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 5001, value_max = 10000, fee = 3) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 10001, value_max = 50000, fee = 4) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 50001, value_max = 100000, fee = 6) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 100001, value_max = 300000, fee = 12) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 300001, value_max = 500000, fee = 20) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 500001, value_max = Inf, fee = 25) %>%
  
  # Awash Bank P2P Off-Us Transfer (Off-network - 0.2% fee)
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Awash Bank", exchange_rate = 157.375, value_min = 0, value_max = Inf, fee = 0, fee_pct = 0.002) %>%
  
  # Awash Bank Bank to Wallet
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 0, value_max = 5000, fee = 5) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 5001, value_max = 10000, fee = 10) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 10001, value_max = 50000, fee = 15) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 50001, value_max = 100000, fee = 20) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 100001, value_max = 300000, fee = 25) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 300001, value_max = 500000, fee = 35) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Awash Bank", exchange_rate = 157.375, value_min = 500001, value_max = Inf, fee = 50) %>%
  
  # Awash Bank Cash-Out (Note: Percentage row 0.35% + fixed fee tiers)
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 0, value_max = Inf, fee = 0, fee_pct = 0.0035) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 25, value_max = 100, fee = 4) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 101, value_max = 500, fee = 5) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 501, value_max = 1000, fee = 6) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 1001, value_max = 3000, fee = 8) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 3001, value_max = 6000, fee = 10) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 6001, value_max = 8000, fee = 13) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 10001, value_max = 15000, fee = 15) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 15001, value_max = 20000, fee = 17) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Awash Bank", exchange_rate = 157.375, value_min = 20001, value_max = 50000, fee = 20) %>%
  
  # Awash Bank Merchant Payment (0.2% fee via EthSwitch)
  add_row(country = "Ethiopia", fsp_type = "Mobile Banking", transaction_type = "Merchant Payment", provider = "Awash Bank", exchange_rate = 157.375, value_min = 0, value_max = Inf, fee = 0, fee_pct = 0.002) %>%
  
  #Safaricom (M-Pesa)
  #https://m-pesa.safaricom.et/
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=ETB&amount=1
  
  # Safaricom P2P On-Us Transfer (On-network)
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Safaricom", exchange_rate = 157.375, value_min = 1, value_max = 1000, fee = 0) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Safaricom", exchange_rate = 157.375, value_min = 1001, value_max = 3000, fee = 0) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Safaricom", exchange_rate = 157.375, value_min = 3001, value_max = 30000, fee = 3) %>%
    
    # Safaricom Cash-Out (Note: 1% fee percentage)
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "Cash-Out", provider = "Safaricom", exchange_rate = 157.375, value_min = 0, value_max = 75000, fee = 0, fee_pct = 0.01) %>%
    
  # Safaricom Wallet to Bank (Transfer to Bank)
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Safaricom", exchange_rate = 157.375, value_min = 0, value_max = 5000, fee = 0) %>%
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Safaricom", exchange_rate = 157.375, value_min = 5001, value_max = Inf, fee = 0) %>%
    
  # Safaricom Merchant Payment
  add_row(country = "Ethiopia", fsp_type = "Mobile Money", transaction_type = "Merchant Payment", provider = "Safaricom", exchange_rate = 157.375, value_min = 0, value_max = 75000, fee = 0) %>%

  #-------------------------------------------------------------------------------------------------
  #Kenya - KCB Bank
  #https://ke.kcbgroup.com/our-tariffs
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=KES&amount=1
  
  # KCB PesaLink (P2P Off-Us Transfer)
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 1, value_max = 1000, fee = 0) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 1001, value_max = 5000, fee = 34.50) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 5001, value_max = 10000, fee = 46.00) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 10001, value_max = 50000, fee = 60.60) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 50001, value_max = 100000, fee = 92.00) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 100001, value_max = 200000, fee = 115.00) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 200001, value_max = 999999, fee = 230.00) %>%
  
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "KCB", exchange_rate = 129.30, value_min = 0, value_max = Inf, fee = 34.50) %>%
  
  #Equity Bank
  #https://equitygroupholdings.com/ke/media/images/docs/tariff-guide.pdf
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=KES&amount=1
  
  # Equity Bank P2P On-Us Transfer
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Equity Bank", exchange_rate = 129.30, value_min = 0, value_max = Inf, fee = 0) %>%
  
  # Equity Bank P2P Off-Us Transfer
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Equity Bank", exchange_rate = 129.30, value_min = 0, value_max = 1000, fee = 0) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Equity Bank", exchange_rate = 129.30, value_min = 1001, value_max = 100000, fee = 50) %>%
  add_row(country = "Kenya", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Equity Bank", exchange_rate = 129.30, value_min = 100001, value_max = 999999, fee = 100) %>%
  #-------------------------------------------------------------------------------------------------
  #Pakistan - Habib Bank (HBL)
  #https://www.hbl.com/assets/documents/SOBC_2025_Jul-Dec_-_English.pdf
  #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=PKR&amount=1
  
  # HBL P2P On-Us Transfer
  add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P On-Us Transfer", provider = "Habib Bank", exchange_rate = 278.60, value_min = 0, value_max = Inf, fee = 0) %>%
    
    # HBL Bank to Wallet
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Habib Bank", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Bank to Wallet", provider = "Habib Bank", exchange_rate = 278.60, value_min = 25001, value_max = Inf, fee = 0, fee_pct = 0.001) %>%
    
    # HBL P2P Off-Us Transfer
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0, fee_pct = 0.001) %>%
    
    # HBL P2P Off-Us Transfer (Konnect by HBL)
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 1, value_max = 26000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 26001, value_max = 27000, fee = 1) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 27001, value_max = 28000, fee = 2) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 28001, value_max = 29000, fee = 3) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 29001, value_max = 30000, fee = 4) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 30001, value_max = 31000, fee = 5) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 31001, value_max = 32000, fee = 6) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 32001, value_max = 33000, fee = 7) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 33001, value_max = 34000, fee = 8) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 34001, value_max = 35000, fee = 9) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 35001, value_max = 36000, fee = 10) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 36001, value_max = 37000, fee = 11) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 37001, value_max = 38000, fee = 12) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 38001, value_max = 39000, fee = 13) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 39001, value_max = 40000, fee = 14) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 40001, value_max = 41000, fee = 15) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 41001, value_max = 42000, fee = 16) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 42001, value_max = 43000, fee = 17) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 43001, value_max = 44000, fee = 18) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 44001, value_max = 45000, fee = 19) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 45001, value_max = 46000, fee = 20) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 46001, value_max = 47000, fee = 21) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 47001, value_max = 48000, fee = 22) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 48001, value_max = 49000, fee = 23) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "P2P Off-Us Transfer", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 49001, value_max = 50000, fee = 24) %>%
    
    # HBL Cash-Out (Konnect by HBL - Agent/ATM)
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 1, value_max = 200, fee = 15) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 201, value_max = 500, fee = 15) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 501, value_max = 1000, fee = 20) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 1001, value_max = 2500, fee = 45) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 2501, value_max = 4000, fee = 80) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 4001, value_max = 6000, fee = 100) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 6001, value_max = 8000, fee = 125) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 8001, value_max = 10000, fee = 180) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 10001, value_max = 13000, fee = 230) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 13001, value_max = 16000, fee = 280) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 16001, value_max = 20000, fee = 330) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 20001, value_max = 25000, fee = 380) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 25001, value_max = 30000, fee = 470) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 30001, value_max = 40000, fee = 560) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Banking", transaction_type = "Cash-Out", provider = "Habib Bank", notes = "Konnect by HBL", exchange_rate = 278.60, value_min = 40001, value_max = 50000, fee = 690) %>%
    
    # Easypaisa
    #https://easypaisa.com.pk/public-information/Schedule-of-Bank-Charges/Schedule-of-Bank-Charges-Branchless-Banking-English.pdf
    #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=PKR&amount=1
    
    # Easypaisa P2P On-Us Transfer (On-network)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Easypaisa", exchange_rate = 278.60, value_min = 1, value_max = Inf, fee = 0) %>%
      
    # Easypaisa Wallet to Bank
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Easypaisa", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Easypaisa", exchange_rate = 278.60, value_min = 25001, value_max = Inf, fee = 0) %>%
      
    # Easypaisa P2P Off-Us Transfer (Off-network)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Easypaisa", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Easypaisa", exchange_rate = 278.60, value_min = 25001, value_max = Inf, fee = 0) %>%
    
    #Jazz (JazzCash)
    #https://www.jazzcash.com.pk/mobile-account/insurance/soc/
    #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=PKR&amount=1
    
    # Jazz P2P On-Us Transfer (On-network)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Jazz", exchange_rate = 278.60, value_min = 1, value_max = Inf, fee = 0) %>%
      
      # Jazz Merchant Payment (Payment at Merchant)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Merchant Payment", provider = "Jazz", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 1) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Merchant Payment", provider = "Jazz", notes = "flat rate", exchange_rate = 278.60, value_min = 25001, value_max = 200000, fee = 0, fee_pct = 0.0002) %>%
      
    # Jazz Wallet to Bank (Transfer to Bank)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Jazz", notes = "sometimes", exchange_rate = 278.60, value_min = 1, value_max = 50000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Jazz", notes = "sometimes", exchange_rate = 278.60, value_min = 50001, value_max = Inf, fee = 0, fee_pct = 0.001) %>%
    
    #Upaisa
    #https://www.upaisa.com/assets/pdf/UMBL_BB_SOC_4th_Quarter_2025.pdf
    #exchange rate as at 14/05/2026: https://www.oanda.com/currency-converter/en/?from=USD&to=PKR&amount=1
    
    # Upaisa P2P On-Us Transfer (On-network)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P On-Us Transfer", provider = "Upaisa", exchange_rate = 278.60, value_min = 1, value_max = 50000, fee = 0) %>%
      
    # Upaisa P2P Off-Us Transfer (Off-network)
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Upaisa", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "P2P Off-Us Transfer", provider = "Upaisa", notes = "monthly cumulative", exchange_rate = 278.60, value_min = 25001, value_max = Inf, fee = 0, fee_pct = 0.001) %>%
      
    # Upaisa Wallet to Bank
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Upaisa", exchange_rate = 278.60, value_min = 1, value_max = 25000, fee = 0) %>%
    add_row(country = "Pakistan", fsp_type = "Mobile Money", transaction_type = "Wallet to Bank", provider = "Upaisa", notes = "monthly cumulative", exchange_rate = 278.60, value_min = 25001, value_max = Inf, fee = 0, fee_pct = 0.001)%>%
  
  
    mutate(ipa_data = ifelse(is.na(ipa_data),0, ipa_data))
  #-------------------------------------------------------------------------------------------------
  
#-------------------------------------------------------------------------------
#Exchange rate conversions
#Exchange rate missing for some values 
#-------------------------------------------------------------------------------
library(quantmod)
library(countrycode)

dat_all = dat_all %>% 
  mutate(country_code = countrycode(country, origin = 'country.name', destination = 'iso4217c' ))


#Exchange rate function
get_exch = function(from, to){
  pair  = paste0(from,to,'=X')
  data  = getSymbols(pair, src = "yahoo", from = Sys.Date(), auto.assign = FALSE)
  as.numeric(last(Cl(data)))
  
  
}

#Testing
get_exch(from = "USD", to = "AUD")

# Commented out for now
countries = data.frame( country_code = unique(dat_all$country_code))

countries = countries %>%
  rowwise() %>%
  mutate(exchange_rate = get_exch("USD", country_code),
         date_collection_exch = Sys.Date())
save(countries, file = "countries")

load('countries')


dat_all = dat_all %>% 
  select(-exchange_rate) %>% 
  full_join(countries, by = "country_code")


#Convert to usd amounts
dat_all = dat_all %>% 
  mutate(value_min_usd = value_min/exchange_rate,
         value_max_usd = value_max/exchange_rate,
         fee_usd       = fee/exchange_rate,
         fee_pct       = ifelse(is.na(fee_pct), 0, fee_pct) #Fee pct is 0 or greater than zero, to make computation easier
         )

unique(dat_all$country)
#-------------------------------------------------------------------------------
#function to work out cost per country
#-------------------------------------------------------------------------------
cost_func = function(usd, dat, transaction, group){
  
  #filter out appropriate bands
  dat = dat %>% 
    filter(transaction_type == transaction,
           fsp_type == group,
           value_min_usd <= usd,
           value_max_usd >= usd) %>% 
  mutate(fee_usd = ifelse(fee_pct>0,usd*fee_pct,fee_usd)) %>% 
  group_by(country) %>% 
  summarise(avg_fee_usd = mean(fee_usd)) %>% 
    ungroup() %>% 
    mutate(fee_is = paste0("USD:",usd))

  return(dat)
  
}


plot_map = function(usd, dat , 
                    transaction,
                    group){

cost_country = cost_func(usd = usd, dat = dat, 
          transaction = transaction,
          group = group)

no_country = length(unique(cost_country$country))

#World map 
world_map = ne_countries(scale = "medium", returnclass = "sf")%>% 
  filter(continent != "Antarctica") 

world = world_map %>% 
  mutate(sovereignt = ifelse(sovereignt == "United Republic of Tanzania","Tanzania", sovereignt),
         sovereignt = ifelse(sovereignt == "Ivory Coast","Côte d'Ivoire", sovereignt)) %>% 
  full_join(cost_country, by = c("sovereignt"= "country"))



legend_title = unique(cost_country$fee_is)

unique(world$avg_fee_usd)

#plot
ggplot(data = world) +
  # color = "gray90" gives the countries a very faint border
  geom_sf(aes(fill = avg_fee_usd), color = "gray90", size = 0.1) + 
  
  # na.value = "white" makes countries with no data invisible against the background
  scale_fill_viridis_c(name = legend_title, na.value = "white") + 
  
  # Removes the coordinate grid
  coord_sf(datum = NA) + 
  
  # theme_void removes all axes/labels; theme() forces the paper to be white
  theme_void() + 
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )+labs(title = paste0(transaction,"; ", group,"; ", "Countries: ", no_country))

}

#Plots to analyse correctness of data
plot_map(usd = 10, dat = dat_all, transaction = "P2P Off-Us Transfer",
         group = "Mobile Money")
plot_map(usd = 10, dat = dat_all, transaction = "P2P On-Us Transfer",
         group = "Mobile Money")
plot_map(usd = 10, dat = dat_all, transaction = "P2P Off-Us Transfer",
         group = "Mobile Banking")
plot_map(usd = 10, dat = dat_all, transaction = "P2P On-Us Transfer",
         group = "Mobile Banking")
plot_map(usd = 10, dat = dat_all, transaction = "P2P Off-Us Transfer",
         group = "Fintech")
plot_map(usd = 10, dat = dat_all, transaction = "P2P On-Us Transfer",
         group = "Fintech")

#uganda, Nigeria, ivory coast, paraguay, Rwanda, 
usd=10
final_dat = dat_all %>% 
  group_by(country, transaction_type, fsp_type) %>% 
  filter(value_min_usd <= usd,
         value_max_usd >= usd,
         transaction_type %in% c("P2P On-Us Transfer", "P2P Off-Us Transfer")) %>% 
  summarise(average_price_usd = mean(fee_usd)) 
  


library(writexl)
write_xlsx(final_dat, "global_price_dataset.xlsx")


