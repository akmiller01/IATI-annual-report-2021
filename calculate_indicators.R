list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-annual-report-2021/output")


xml_files = list.files(path="~/git/IATI-Better-Refresher/data",full.names=T)
dat_list = list()
dat_index = 1
pb = txtProgressBar(max=length(xml_files),style=3)
for(idx in 1:length(xml_files)){
  setTxtProgressBar(pb,idx)
  xml_file = xml_files[idx]
  xml_dat <- tryCatch(
    xmlParse(xml_file),
    error=function(e) e
  )

  if(inherits(xml_dat, "error")){next}
  activities = getNodeSet(xml_dat, "//iati-activity")
  for(activity in activities){
    iati_identifier = sapply(getNodeSet(activity,"./iati-identifier"),xmlValue)
    if(length(iati_identifier)==0){
      iati_identifier = NA
    }
    default_currency = xmlGetAttr(activity,"default-currency")
    if(length(default_currency)==0){
      default_currency = NA
    }
    reporting_org_ref = NA
    reporting_org_elem = getNodeSet(activity,"./reporting-org")
    if(length(reporting_org_elem)>0){
      reporting_org_attrs = xmlAttrs(reporting_org_elem[[1]])
      if("ref" %in% names(reporting_org_attrs)){
        reporting_org_ref = reporting_org_attrs[["ref"]]
      }
    }
    transactions = getNodeSet(activity,"./transaction")
    if(length(transactions)>0){
      for(transaction in transactions){
        if(length(getNodeSet(transaction,"./transaction-type/@code"))==0){
          t_type = NA
        }else{
          t_type = getNodeSet(transaction,"./transaction-type/@code")[[1]][["code"]]
        }

        if(length(getNodeSet(transaction,"./transaction-date/@iso-date"))==0){
          t_date = NULL
        }else{
          t_date = getNodeSet(transaction,"./transaction-date/@iso-date")[[1]][["iso-date"]]
        }
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
          if(!is.na(t_date) & !is.na(t_value) & !is.na(t_currency) & !is.na(reporting_org_ref) &
             t_date!="" & t_value!="" & t_currency!="" & reporting_org_ref!=""){
            dat_list[[dat_index]] = data.frame(iati_identifier,reporting_org_ref,type=t_type,date=t_date,value=t_value,currency=t_currency)
            dat_index = dat_index + 1
          }
          rm(t_type,t_date,t_value_elem,t_value,t_currency)
        }
      }
      rm(iati_identifier,default_currency,reporting_org_elem,reporting_org_attrs,reporting_org_ref,transactions)
    }
  }
  rm(xml_dat)
}
close(pb)

dat = rbindlist(dat_list)
save(dat,file="report_data.RData")
# load("report_data.RData")

length(unique(dat$reporting_org_ref))

# dat$currency = gsub(" ","",toupper(dat$currency))
# dat$currency[which(dat$currency=="BEF")] = "EUR"
# dat$currency[which(dat$currency=="GIP")] = "GBP"
# dat$currency[which(dat$currency=="AON")] = "AOA"
# dat$currency[which(dat$currency=="USS")] = "USD"
# dat$currency[which(dat$currency=="FKP")] = "GBP"
# dat$currency[which(dat$currency=="ZMK")] = "ZMW"
# dat$currency[which(dat$currency=="USN")] = "USD"
# dat$currency[which(dat$currency=="FIM")] = "EUR"
# dat$currency[which(dat$currency=="EURO")] = "EUR"
# dat$currency[which(dat$currency=="GHC")] = "GHS"
# 
# ex_rates = fread("../ex_rates.csv")
# setnames(ex_rates,"cc","currency")
# setdiff(unique(dat$currency),unique(ex_rates$currency))
# dat = merge(dat,ex_rates,by=c("year","currency"))
# dat = subset(dat,ex.rate>0 & type %in% c(
#   "3",
#   "4",
#   "D",
#   "E",
#   "Disbursement",
#   "Expenditure"
# ))
# dat$value_usd = dat$value / dat$ex.rate
