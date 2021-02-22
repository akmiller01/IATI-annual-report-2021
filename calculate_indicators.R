list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-annual-report-2021/output")

# Account for odd currency codes
currency_remap = c(
  "BEF" = "EUR",
  "GIP" = "GBP",
  "AON" = "AOA",
  "USS" = "USD",
  "FKP" = "GBP",
  "ZMK" = "ZMW",
  "USN" = "USD",
  "FIM" = "EUR",
  "EURO" = "EUR",
  "GHC" = "GHS"
)

# Read re-calculated currency codes
ex_rates = fread("../ex_rates.csv")
ccs = unique(ex_rates$cc)
for(cc in ccs){
  currency_remap[[cc]] = cc
}

# Find XML files
xml_files = list.files(path="~/git/IATI-Registry-Refresher/data",full.names=T,recursive=T)

# Setup indicator holding vars
unique_reporting_orgs = c()
unique_iati_identifiers = c()
r_org_by_year = list()
r_org_by_type = list()
activity_count = 0
humanitarian_activity_count = 0
unique_emergencies = c()
unique_appeals = c()
activity_using_sdg_count = 0
publishers_using_sdgs = c()
unique_sdg_goals = c()
unique_sdg_targets = c()
a_count_by_year = list()
transaction_count = list()
t_count_by_year = list()
spend_total = 0
s_total_by_year = list()
budget_total = 0
b_total_by_year = list()
sector_spend_2020 = list()
recipient_spend_2020 = list()
recipient_budget_2021 = list()
org_type_spend_2020 = list()

# Loop
pb = txtProgressBar(max=length(xml_files),style=3)
for(idx in 1:length(xml_files)){
  setTxtProgressBar(pb,idx)
  xml_file = xml_files[idx]
  xml_dat <- tryCatch(
    xmlParse(xml_file),
    error=function(e) e
  )

  # Try reading the XML, otherwise skip it
  if(inherits(xml_dat, "error")){next}
  activities = getNodeSet(xml_dat, "//iati-activity")
  for(activity in activities){
    activity_count = activity_count + 1
    # Activity sectors
    activity_sectors = getNodeSet(activity,"./sector")
    activity_sector_list = list()
    activity_sector_idx = 1
    if(length(activity_sectors)>0){
      for(activity_sector in activity_sectors){
        act_sec_code = xmlGetAttr(activity_sector,"code")
        if(length(act_sec_code)==0){
          act_sec_code = NA
        }
        act_sec_vocab = xmlGetAttr(activity_sector,"vocabulary")
        if(length(act_sec_vocab)==0){
          act_sec_vocab = NA
        }
        act_sec_percentage = xmlGetAttr(activity_sector,"percentage")
        if(length(act_sec_percentage)==0){
          act_sec_percentage = "100"
        }
        tmp_act_sec_df = list(vocab=act_sec_vocab,code=act_sec_code,percentage=act_sec_percentage)
        activity_sector_list[[activity_sector_idx]] = tmp_act_sec_df
        activity_sector_idx = activity_sector_idx + 1
      }
    }
    activity_sector_df = rbindlist(activity_sector_list)
    if("1" %in% activity_sector_df$vocab){
      activity_sector_df = activity_sector_df[which(activity_sector_df$vocab=="1"),]
      activity_sector_df$code = substr(activity_sector_df$code,1,3)
    }else if("2" %in% activity_sector_df$vocab){
      activity_sector_df = activity_sector_df[which(activity_sector_df$vocab=="2"),]
    }
    if(nrow(activity_sector_df)>0){
      activity_sector_df = activity_sector_df[which(!is.na(activity_sector_df$code) & activity_sector_df$code!=""),]
      activity_sector_df$percentage = as.numeric(activity_sector_df$percentage)
    }
    # Activity recipients
    activity_recipients = getNodeSet(activity,"./recipient-country")
    activity_recipient_list = list()
    activity_recipient_idx = 1
    if(length(activity_recipients)>0){
      for(activity_recipient in activity_recipients){
        act_recip_code = xmlGetAttr(activity_recipient,"code")
        if(length(act_recip_code)==0){
          act_recip_code = NA
        }
        act_recip_percentage = xmlGetAttr(activity_recipient,"percentage")
        if(length(act_recip_percentage)==0){
          act_recip_percentage = "100"
        }
        tmp_act_recip_df = list(code=act_recip_code,percentage=act_recip_percentage)
        activity_recipient_list[[activity_recipient_idx]] = tmp_act_recip_df
        activity_recipient_idx = activity_recipient_idx + 1
      }
    }
    activity_recipient_df = rbindlist(activity_recipient_list)
    if(nrow(activity_recipient_df)>0){
      activity_recipient_df = activity_recipient_df[which(!is.na(activity_recipient_df$code) & activity_recipient_df$code!=""),]
      activity_recipient_df$code = toupper(activity_recipient_df$code)
    }
    # Humanitarian flag
    humanitarian_flag = xmlGetAttr(activity,"humanitarian")
    if(length(humanitarian_flag)>0){
      if(humanitarian_flag %in% c("1","true")){
        humanitarian_activity_count = humanitarian_activity_count + 1
      }
    }
    # Humanitarian scope
    humanitarian_scopes = getNodeSet(activity,"./humanitarian-scope")
    if(length(humanitarian_scopes)>0){
      for(h_scope in humanitarian_scopes){
        h_scope_vocab = xmlGetAttr(h_scope,"vocabulary")
        h_scope_code = xmlGetAttr(h_scope,"code")
        if(length(h_scope_vocab)>0 & length(h_scope_code)>0){
          if(!is.na(h_scope_code) & h_scope_code!=""){
            if(h_scope_vocab=="1-2"){
              unique_emergencies = unique(c(unique_emergencies, h_scope_code))
            }
            if(h_scope_vocab=="2-1"){
              unique_appeals = unique(c(unique_appeals, h_scope_code))
            }
          }
        }
      }
    }
    # IATI identifier
    iati_identifier = sapply(getNodeSet(activity,"./iati-identifier"),xmlValue)
    if(length(iati_identifier)==0){
      iati_identifier = NA
    }
    if(!is.na(iati_identifier) & iati_identifier!=""){
      unique_iati_identifiers = unique(c(unique_iati_identifiers, iati_identifier))
    }
    # Activity date
    activity_date_elems = getNodeSet(activity,"./activity-date/@iso-date")
    if(length(activity_date_elems)==0){
      activity_date = NA
      activity_year = NA
    }else{
      activity_dates = anydate(sapply(activity_date_elems,`[[`,"iso-date"))
      activity_date = mean(activity_dates)
      activity_year = as.character(year(activity_date))
    }
    # Activity count by year
    if(!is.na(activity_year)){
      if(activity_year %in% names(a_count_by_year)){
        a_count_by_year[[activity_year]] = a_count_by_year[[activity_year]] + 1
      }else{
        a_count_by_year[[activity_year]] = 1
      }
    }
    # Default currency
    default_currency = xmlGetAttr(activity,"default-currency")
    if(length(default_currency)==0){
      default_currency = NA
    }
    # Reporting org ref
    reporting_org_ref = NA
    reporting_org_type = NA
    reporting_org_elem = getNodeSet(activity,"./reporting-org")
    if(length(reporting_org_elem)>0){
      reporting_org_attrs = xmlAttrs(reporting_org_elem[[1]])
      if("ref" %in% names(reporting_org_attrs)){
        reporting_org_ref = reporting_org_attrs[["ref"]]
      }
      if("type" %in% names(reporting_org_attrs)){
        reporting_org_type = reporting_org_attrs[["type"]]
      }
    }
    # Reporting org ref list and reporting org ref by year
    if(!is.na(reporting_org_ref) & reporting_org_ref!=""){
      unique_reporting_orgs = unique(c(unique_reporting_orgs,reporting_org_ref))
      if(!is.na(reporting_org_type)){
        if(reporting_org_type %in% names(r_org_by_type)){
          r_org_by_type[[reporting_org_type]] = unique(c(r_org_by_type[[reporting_org_type]], reporting_org_ref))
        }else{
          r_org_by_type[[reporting_org_type]] = c(reporting_org_ref)
        }
      }
      if(!is.na(activity_year)){
        if(activity_year %in% names(r_org_by_year)){
          r_org_by_year[[activity_year]] = unique(c(r_org_by_year[[activity_year]], reporting_org_ref))
        }else{
          r_org_by_year[[activity_year]] = c(reporting_org_ref)
        }
      }
    }
    #SDG tags
    tags = getNodeSet(activity,"./tag")
    using_sdg_tag = F
    for(tag in tags){
      tag_vocabulary = xmlGetAttr(tag, "vocabulary")
      if(tag_vocabulary %in% c("2","3")){
        using_sdg_tag = T
      }
      #Goals
      if(tag_vocabulary=="2"){
        tag_code = xmlGetAttr(tag, "code")
        if(length(tag_code)==0){
          tag_code = NA
        }
        if(!is.na(tag_code) & tag_code!=""){
          unique_sdg_goals = unique(c(unique_sdg_goals, tag_code))
        }
      }
      # Targets
      if(tag_vocabulary=="3"){
        tag_code = xmlGetAttr(tag, "code")
        if(length(tag_code)==0){
          tag_code = NA
        }
        if(!is.na(tag_code) & tag_code!=""){
          unique_sdg_targets = unique(c(unique_sdg_targets, tag_code))
        }
      }
    }
    if(using_sdg_tag){
      activity_using_sdg_count = activity_using_sdg_count + 1
      if(!is.na(reporting_org_ref) & reporting_org_ref!=""){
        publishers_using_sdgs = unique(c(publishers_using_sdgs, reporting_org_ref))
      }
    }
    # Transactions
    transactions = getNodeSet(activity,"./transaction")
    if(length(transactions)>0){
      for(transaction in transactions){
        # Transaction sectors
        transaction_sectors = getNodeSet(transaction,"./sector")
        transaction_sector_list = list()
        transaction_sector_idx = 1
        if(length(transaction_sectors)>0){
          for(transaction_sector in transaction_sectors){
            trans_sec_code = xmlGetAttr(transaction_sector,"code")
            if(length(trans_sec_code)==0){
              trans_sec_code = NA
            }
            trans_sec_vocab = xmlGetAttr(transaction_sector,"vocabulary")
            if(length(trans_sec_vocab)==0){
              trans_sec_vocab = NA
            }
            trans_sec_percentage = xmlGetAttr(transaction_sector,"percentage")
            if(length(trans_sec_percentage)==0){
              trans_sec_percentage = 100
            }
            tmp_trans_sec_df = list(vocab=trans_sec_vocab,code=trans_sec_code,percentage=trans_sec_percentage)
            transaction_sector_list[[transaction_sector_idx]] = tmp_trans_sec_df
            transaction_sector_idx = transaction_sector_idx + 1
          }
        }
        transaction_sector_df = rbindlist(transaction_sector_list)
        if("1" %in% transaction_sector_df$vocab){
          transaction_sector_df = transaction_sector_df[which(transaction_sector_df$vocab=="1"),]
          transaction_sector_df$code = substr(transaction_sector_df$code,1,3)
        }else if("2" %in% transaction_sector_df$vocab){
          transaction_sector_df = transaction_sector_df[which(transaction_sector_df$vocab=="2"),]
        }
        if(nrow(transaction_sector_df)>0){
          transaction_sector_df = subset(transaction_sector_df,!is.na(code) & code!="")
          transaction_sector_df$percentage = as.numeric(transaction_sector_df$percentage)
        }
        if(nrow(transaction_sector_df)==0){
          transaction_sector_df = activity_sector_df
        }
        
        # Transaction recipients
        transaction_recipients = getNodeSet(transaction,"./recipient-country")
        transaction_recipient_list = list()
        transaction_recipient_idx = 1
        if(length(transaction_recipients)>0){
          for(transaction_recipient in transaction_recipients){
            trans_recip_code = xmlGetAttr(transaction_recipient,"code")
            if(length(trans_recip_code)==0){
              trans_recip_code = NA
            }
            trans_recip_percentage = "100"
            tmp_trans_recip_df = list(code=trans_recip_code,percentage=trans_recip_percentage)
            transaction_recipient_list[[transaction_recipient_idx]] = tmp_trans_recip_df
            transaction_recipient_idx = transaction_recipient_idx + 1
          }
        }
        transaction_recipient_df = rbindlist(transaction_recipient_list)
        if(nrow(transaction_recipient_df)>0){
          transaction_recipient_df = transaction_recipient_df[which(!is.na(transaction_recipient_df$code) & transaction_recipient_df$code!=""),]
          transaction_recipient_df$code = toupper(transaction_recipient_df$code)
        }
        if(nrow(transaction_recipient_df)==0){
          transaction_recipient_df = activity_recipient_df
        }
        
        # Transaction type
        t_type_code_elems = getNodeSet(transaction,"./transaction-type/@code")
        if(length(t_type_code_elems)==0){
          t_type = NA
        }else{
          t_type = t_type_code_elems[[1]][["code"]]
        }
        # Transaction count by transaction type
        if(!is.na(t_type) & t_type!=""){
          if(t_type %in% names(transaction_count)){
            transaction_count[[t_type]] = transaction_count[[t_type]] + 1
          }else{
            transaction_count[[t_type]] = 1
          }
        }
        # Transaction date
        t_date_code_elems = getNodeSet(transaction,"./transaction-date/@iso-date")
        if(length(t_date_code_elems)==0){
          t_date = NULL
          t_year = NA
        }else{
          t_date = t_date_code_elems[[1]][["iso-date"]]
          t_year = as.numeric(substr(t_date,1,4))
        }
        # Transaction count by type and year
        if(!is.na(t_type) & !is.na(t_year) & t_type!=""){
          t_year = as.character(t_year)
          if(t_year %in% names(t_count_by_year)){
            if(t_type %in% names(t_count_by_year[[t_year]])){
              t_count_by_year[[t_year]][[t_type]] = t_count_by_year[[t_year]][[t_type]] + 1
            }else{
              t_count_by_year[[t_year]][[t_type]] = 1
            }
          }else{
            t_count_by_year[[t_year]] = list()
            t_count_by_year[[t_year]][[t_type]] = 1
          }
        }
        # Transaction value, value date, currency
        t_value_elem = getNodeSet(transaction,"./value")
        if(length(t_value_elem)>0){
          t_value = as.numeric(gsub(",","",sapply(t_value_elem,xmlValue)))
          t_currency = sapply(t_value_elem,xmlGetAttr,"currency")[[1]]
          if(length(t_currency)==0){
            t_currency = default_currency
          }
          t_value_date = sapply(t_value_elem,xmlGetAttr,"value-date")[[1]]
          if(length(t_date)==0){
            t_date = t_value_date
          }
          if(length(t_date)==0){
            t_date = NA
          }
          # Conversion and total spend count
          if(!is.na(t_date) & !is.na(t_value) & !is.na(t_currency) &
             t_date!="" & t_value!="" & t_currency!="" &
             t_type %in% c("3","4","D","E","Disbursement","Expenditure")
             ){
            t_currency = gsub(" ","",toupper(t_currency))
            if(t_currency %in% names(currency_remap)){
              t_currency = currency_remap[[t_currency]]
            }
            t_year = as.numeric(substr(t_date,1,4))
            ex_rate = subset(ex_rates,cc==t_currency & year==t_year)$ex.rate
            if(length(ex_rate)>0){
              t_value_usd = t_value / ex_rate
              spend_total = spend_total + t_value_usd
              if(t_year %in% names(s_total_by_year)){
                s_total_by_year[[as.character(t_year)]] = s_total_by_year[[as.character(t_year)]] + t_value_usd
              }else{
                s_total_by_year[[as.character(t_year)]] = t_value_usd
              }
              # org type transactions 2020
              if(t_year==2020 & !is.na(reporting_org_type)){
                if(reporting_org_type %in% names(org_type_spend_2020)){
                  org_type_spend_2020[[reporting_org_type]] = org_type_spend_2020[[reporting_org_type]] + t_value_usd
                }else{
                  org_type_spend_2020[[reporting_org_type]] = t_value_usd
                }
              }
              # sector transactions 2020
              if(t_year==2020 & nrow(transaction_sector_df)>0){
                for(sector_idx in nrow(transaction_sector_df)){
                  sec_percentage = transaction_sector_df[sector_idx,][["percentage"]]
                  sec_code = transaction_sector_df[sector_idx,][["code"]]
                  t_value_usd_split_sector = t_value_usd * (as.numeric(sec_percentage)/100)
                  if(sec_code %in% names(sector_spend_2020)){
                    sector_spend_2020[[sec_code]] = sector_spend_2020[[sec_code]] + t_value_usd_split_sector
                  }else{
                    sector_spend_2020[[sec_code]] = t_value_usd_split_sector
                  }
                }
              }
              # recipient transactions 2020
              if(t_year==2020 & nrow(transaction_recipient_df)>0){
                for(recip_idx in nrow(transaction_recipient_df)){
                  recip_percentage = transaction_recipient_df[recip_idx,][["percentage"]]
                  recip_code = transaction_recipient_df[recip_idx,][["code"]]
                  t_value_usd_split_recip = t_value_usd * (as.numeric(recip_percentage)/100)
                  if(recip_code %in% names(recipient_spend_2020)){
                    recipient_spend_2020[[recip_code]] = recipient_spend_2020[[recip_code]] + t_value_usd_split_recip
                  }else{
                    recipient_spend_2020[[recip_code]] = t_value_usd_split_recip
                  }
                }
              }
            }
          }
        }
        rm(t_type,t_date,t_value_elem,t_value,t_currency)
      }
    }
    # Budgets
    budgets = getNodeSet(activity,"./budget")
    if(length(budgets)>0){
      for(budget in budgets){
        budget_period_start_iso_date = sapply(getNodeSet(budget, "./period-start"),xmlValue)
        if(length(budget_period_start_iso_date)==0){
          budget_period_start_iso_date = getNodeSet(budget, "./period-start/@iso-date")[[1]][["iso-date"]]
          if(length(budget_period_start_iso_date)==0){
            budget_period_start_iso_date = NA
          }
        }else if(budget_period_start_iso_date==""){
          budget_period_start_iso_date = getNodeSet(budget, "./period-start/@iso-date")[[1]][["iso-date"]]
          if(length(budget_period_start_iso_date)==0){
            budget_period_start_iso_date = NA
          }
        }
        b_value_elem = getNodeSet(budget,"./value")
        if(length(b_value_elem)>0){
          b_currency = sapply(b_value_elem,xmlGetAttr,"currency")[[1]]
          if(length(b_currency)==0){
            b_currency = default_currency
          }
          
          b_value = as.numeric(gsub(",","",sapply(b_value_elem,xmlValue)))[1]
          if(!is.na(budget_period_start_iso_date) & !is.na(b_value) & !is.na(b_currency) &
             budget_period_start_iso_date!="" & b_value!="" & b_currency!=""
          ){
            b_currency = gsub(" ","",toupper(b_currency))
            if(b_currency %in% names(currency_remap)){
              b_currency = currency_remap[[b_currency]]
            }
            b_year = as.numeric(substr(budget_period_start_iso_date,1,4))
            ex_rate = subset(ex_rates,cc==b_currency & year==b_year)$ex.rate
            if(length(ex_rate)>0){
              b_value_usd = b_value / ex_rate
              budget_total = budget_total + b_value_usd
              if(b_year %in% names(b_total_by_year)){
                b_total_by_year[[as.character(b_year)]] = b_total_by_year[[as.character(b_year)]] + b_value_usd
              }else{
                b_total_by_year[[as.character(b_year)]] = b_value_usd
              }
              # recipient budgets 2021
              if(b_year==2021 & nrow(activity_recipient_df)>0){
                for(recip_idx in nrow(activity_recipient_df)){
                  recip_percentage = activity_recipient_df[recip_idx,][["percentage"]]
                  recip_code = activity_recipient_df[recip_idx,][["code"]]
                  b_value_usd_split_recip = b_value_usd * (as.numeric(recip_percentage)/100)
                  if(recip_code %in% names(recipient_budget_2021)){
                    recipient_budget_2021[[recip_code]] = recipient_budget_2021[[recip_code]] + b_value_usd_split_recip
                  }else{
                    recipient_budget_2021[[recip_code]] = b_value_usd_split_recip
                  }
                }
              }
            }
          }
        }
        rm(budget_period_start_iso_date,b_value_elem,b_value,b_currency)
      }
    }
    rm(iati_identifier,default_currency,reporting_org_elem,reporting_org_attrs,reporting_org_ref,transactions)
  }
  rm(xml_dat)
}
close(pb)

length(unique_reporting_orgs)
length(unique_iati_identifiers)
length(r_org_by_year[["2019"]])
length(r_org_by_year[["2020"]])
length(r_org_by_type[["10"]])
activity_count
humanitarian_activity_count
length(unique_emergencies)
length(unique_appeals)
activity_using_sdg_count
length(publishers_using_sdgs)
unique_sdg_goals
unique_sdg_targets
a_count_by_year[["2019"]]
a_count_by_year[["2020"]]
spend_total
s_total_by_year[["2019"]]
s_total_by_year[["2020"]]
budget_total
b_total_by_year[["2019"]]
b_total_by_year[["2020"]]
transaction_count[["3"]] + transaction_count[["4"]]
t_count_by_year[["2019"]][["3"]] + t_count_by_year[["2019"]][["4"]]
t_count_by_year[["2020"]][["3"]] + t_count_by_year[["2020"]][["4"]]
sector_spend_2020[["151"]]
recipient_spend_2020[["BD"]]
recipient_budget_2021[["BD"]]
org_type_spend_2020[["10"]]
save(
  unique_reporting_orgs,
  unique_iati_identifiers,
  r_org_by_year,
  r_org_by_type,
  activity_count,
  humanitarian_activity_count,
  unique_emergencies,
  unique_appeals,
  activity_using_sdg_count,
  publishers_using_sdgs,
  unique_sdg_goals,
  unique_sdg_targets,
  a_count_by_year,
  spend_total,
  s_total_by_year,
  budget_total,
  b_total_by_year,
  transaction_count,
  t_count_by_year,
  sector_spend_2020,
  recipient_spend_2020,
  recipient_budget_2021,
  org_type_spend_2020,
  file="indicators_feb_22_2021.RData"
)
