list.of.packages <- c("data.table","openxlsx","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-annual-report-2021/output")
load("indicators_feb_22_2020_2.RData")

rm(org_type_spend_2020,recipient_budget_2021,recipient_spend_2020,sector_spend_2020)
for(env_var in ls()){
  new_name = paste0(env_var,"_2019")
  assign(new_name, get(env_var))
  rm(list=env_var)
}

load("indicators_feb_22_2021_2.RData")

billion = 1000000000
trillion = billion * 1000

# How many publishers?
length(unique_reporting_orgs)
# Growth in publishers
length(unique_reporting_orgs) - length(unique_reporting_orgs_2019)
# New publishers
new_refs = setdiff(unique_reporting_orgs, unique_reporting_orgs_2019)
length(new_refs)
length(setdiff(unique_reporting_orgs_2019, unique_reporting_orgs))
fwrite(data.frame(new_refs),"new_publishers.csv")

# Total activities
activity_count
activity_count - activity_count_2019
# Past 5 years
a_count = stack(a_count_by_year)
setnames(a_count,"ind","year")
setnames(a_count,"values","activities")
a_count$year = as.numeric(as.character(a_count$year))
a_count = subset(a_count,year>2014 & year<2021)
a_count = a_count[order(a_count$year),]
a_count$cumulative_activities = cumsum(a_count$activities)
fwrite(a_count,"activities_count.csv")
a_count_long = melt(a_count,id.vars="year")
ggplot(a_count_long,aes(x=year,y=value,group=variable,color=variable)) + geom_line() + theme_bw()

# Total budgets
budget_total / trillion
# Total spend
spend_total / trillion

# Transactions disaggregated by type
t_types = fread("../TransactionType.csv")[,c("code","name")]
names(t_types) = c("code","transaction_type")
t_types$code = as.character(t_types$code)
t_count = stack(t_count_by_year[["2020"]])
setnames(t_count,"ind","code")
setnames(t_count,"values","transactions")
t_count$code = as.character(t_count$code)
t_count$code[which(t_count$code=="C")] = "2"
t_count$code[which(t_count$code=="D")] = "3"
t_count$code[which(t_count$code=="E")] = "4"
t_count = data.table(t_count)[,.(transactions=sum(transactions)),by=.(code)]
t_count = merge(t_count,t_types,by="code")
t_count$code = NULL
t_count = t_count[order(t_count$transactions),]
fwrite(t_count,"transaction_count.csv")
t_count$transaction_type = factor(t_count$transaction_type,levels=t_count$transaction_type)
ggplot(t_count,aes(x=transaction_type,y=transactions)) + geom_bar(stat="identity") + coord_flip() + theme_bw()

# Activity growth
activity_count - activity_count_2019
# New activities
new_activities = setdiff(unique_iati_identifiers, unique_iati_identifiers_2019)
length(new_activities)
length(setdiff(unique_iati_identifiers_2019,unique_iati_identifiers))

# Activity count with SDG Tags
activity_using_sdg_count
# Growth
activity_using_sdg_count - activity_using_sdg_count_2019
# Publishers using SDG Tags
length(publishers_using_sdgs)
# Growth
length(publishers_using_sdgs) - length(publishers_using_sdgs_2019)
# Goals
unique_sdg_goals[order(unique_sdg_goals)]
unique_sdg_goals_2019[order(unique_sdg_goals_2019)]
length(unique_sdg_goals) - length(unique_sdg_goals_2019)
# Targets
unique_sdg_targets[order(unique_sdg_targets)]
unique_sdg_targets_2019[order(unique_sdg_targets_2019)]
length(unique_sdg_targets) - length(unique_sdg_targets_2019)

# SDG vocabs
activity_using_sdg_vocab_count
activity_using_sdg_vocab_count - activity_using_sdg_vocab_count_2019
length(publishers_using_sdg_vocabs)
length(setdiff(publishers_using_sdg_vocabs, publishers_using_sdg_vocabs_2019))

# Humanitarian scope types
humanitarian_scope_emergency_count
humanitarian_scope_emergency_count - humanitarian_scope_emergency_count_2019
humanitarian_scope_appeal_count
humanitarian_scope_appeal_count - humanitarian_scope_appeal_count_2019

# Humanitarian
humanitarian_activity_count
humanitarian_activity_count - humanitarian_activity_count_2019
# Emergencies
length(unique_emergencies)
sum(grepl("2020",unique_emergencies))
emergency_2020 = unique_emergencies[which(grepl("2020",unique_emergencies))]
fwrite(data.frame(glide=emergency_2020),"emergencies_2020.csv")
# Appeals
length(unique_appeals)
fwrite(data.frame(appeal=unique_appeals),"appeals_2020.csv")

# 2020 spend by sector
sector_cats = fread("../SectorCategory.csv")[,c("code","name")]
names(sector_cats) = c("sector_code","sector_name")
sector_cats$sector_code = as.character(sector_cats$sector_code)
sector_stack = lapply(sector_spend_2020, stack)
for(sector_name in names(sector_stack)){
  sector_stack[[sector_name]]$sector_code = sector_name
}
sector_spend_disagg = rbindlist(sector_stack)
names(sector_spend_disagg) = c("spend","reporting_org_ref","sector_code")
sector_spend_disagg_clean = sector_spend_disagg[which(sector_spend_disagg$sector_code %in% sector_cats$sector_code),]
sector_spend_disagg_cleanable = sector_spend_disagg[which(nchar(sector_spend_disagg$sector_code)==5),]
sector_spend_disagg_cleanable$sector_code = substr(sector_spend_disagg_cleanable$sector_code,1,3)
sector_spend_disagg_cleanable = sector_spend_disagg_cleanable[which(sector_spend_disagg_cleanable$sector_code %in% sector_cats$sector_code),]
sector_spend_disagg_clean = rbind(sector_spend_disagg_clean, sector_spend_disagg_cleanable)
sector_spend_disagg_clean = data.table(sector_spend_disagg_clean)[,.(spend=sum(spend)),by=.(sector_code, reporting_org_ref)]
sector_spend_disagg_clean = merge(sector_spend_disagg_clean,sector_cats,by="sector_code")
sector_spend_disagg_clean$sector_code = NULL
sector_spend_disagg_clean = sector_spend_disagg_clean[order(-sector_spend_disagg_clean$spend),]
sector_spend_disagg_clean = sector_spend_disagg_clean[,c("reporting_org_ref","sector_name","spend")]
fwrite(sector_spend_disagg_clean,"sector_spend_disagg_2020.csv")
sector_spend_clean = sector_spend_disagg_clean[,.(spend=sum(spend,na.rm=T)),by=.(sector_name)]
sector_spend_clean = sector_spend_clean[order(-sector_spend_clean$spend),]
sector_spend_clean = sector_spend_clean[,c("sector_name","spend")]
fwrite(sector_spend_clean,"Sector_spend_2020.csv")

# 2020 spend by recipient
recipients = fread("../Country.csv")[,c("code","name")]
names(recipients) = c("recipient_code","recipient_name")
recipients$recipient_code = as.character(recipients$recipient_code)
recipient_stack = lapply(recipient_spend_2020, stack)
for(recipient_name in names(recipient_stack)){
  recipient_stack[[recipient_name]]$recipient_code = recipient_name
}
recipient_spend_disagg = rbindlist(recipient_stack)
names(recipient_spend_disagg) = c("spend","reporting_org_ref","recipient_code")
recipient_spend_disagg = merge(recipient_spend_disagg,recipients,by="recipient_code")
recipient_spend_disagg$recipient_code = NULL
recipient_spend_disagg = recipient_spend_disagg[order(-recipient_spend_disagg$spend),]
recipient_spend_disagg = recipient_spend_disagg[,c("reporting_org_ref","recipient_name","spend")]
fwrite(recipient_spend_disagg,"recipient_spend_disagg_2020.csv")
recipient_spend = recipient_spend_disagg[,.(spend=sum(spend,na.rm=T)),by=.(recipient_name)]
recipient_spend = recipient_spend[order(-recipient_spend$spend),]
recipient_spend = recipient_spend[,c("recipient_name","spend")]
fwrite(recipient_spend,"recipient_spend_2020.csv")

# 2021 budget by recipient
recipient_budget_stack = lapply(recipient_budget_2021, stack)
for(recipient_name in names(recipient_budget_stack)){
  recipient_budget_stack[[recipient_name]]$recipient_code = recipient_name
}
recipient_budget_disagg = rbindlist(recipient_budget_stack)
names(recipient_budget_disagg) = c("budget","reporting_org_ref","recipient_code")
recipient_budget_disagg = merge(recipient_budget_disagg,recipients,by="recipient_code")
recipient_budget_disagg$recipient_code = NULL
recipient_budget_disagg = recipient_budget_disagg[order(-recipient_budget_disagg$budget),]
recipient_budget_disagg = recipient_budget_disagg[,c("reporting_org_ref","recipient_name","budget")]
fwrite(recipient_budget_disagg,"recipient_budget_disagg_2021.csv")
recipient_budget = recipient_budget_disagg[,.(budget=sum(budget,na.rm=T)),by=.(recipient_name)]
recipient_budget = recipient_budget[order(-recipient_budget$budget),]
recipient_budget = recipient_budget[,c("recipient_name","budget")]
fwrite(recipient_budget,"recipient_budget_2021.csv")

# 2020 spend by org_type
org_types = fread("../OrganisationType.csv")[,c("code","name")]
names(org_types) = c("org_type_code","org_type_name")
org_types$org_type_code = as.character(org_types$org_type_code)
org_type_stack = lapply(org_type_spend_2020, stack)
for(org_type_name in names(org_type_stack)){
  org_type_stack[[org_type_name]]$org_type_code = org_type_name
}
org_type_spend_disagg = rbindlist(org_type_stack)
names(org_type_spend_disagg) = c("spend","reporting_org_ref","org_type_code")
org_type_spend_disagg = merge(org_type_spend_disagg,org_types,by="org_type_code")
org_type_spend_disagg$org_type_code = NULL
org_type_spend_disagg = org_type_spend_disagg[order(-org_type_spend_disagg$spend),]
org_type_spend_disagg = org_type_spend_disagg[,c("reporting_org_ref","org_type_name","spend")]
fwrite(org_type_spend_disagg,"org_type_spend_disagg_2020.csv")
org_type_spend = org_type_spend_disagg[,.(spend=sum(spend,na.rm=T)),by=.(org_type_name)]
org_type_spend = org_type_spend[order(-org_type_spend$spend),]
org_type_spend = org_type_spend[,c("org_type_name","spend")]
fwrite(org_type_spend,"org_type_spend_2020.csv")

# Reporting org by type
r_org = stack(r_org_by_type)
names(r_org) = c("reporting_org_ref", "org_type_code")
r_org = merge(r_org,org_types,by="org_type_code")
r_org$org_type_code = NULL
r_org = r_org[order(r_org$reporting_org_ref),c("reporting_org_ref","org_type_name")]
fwrite(r_org,"reporting_org_types.csv")

# Extra
s_total = stack(s_total_by_year)
setnames(s_total,"ind","year")
setnames(s_total,"values","total_spend")
s_total$year = as.numeric(as.character(s_total$year))
s_total$total_spend = s_total$total_spend/trillion
s_total = subset(s_total,year>2000 & year<2021)
s_total = s_total[order(s_total$year),]
ggplot(s_total,aes(x=year,y=total_spend)) + geom_line() + theme_bw() + labs(y="Spend (trillions)")

b_total_by_year = lapply(b_total_by_year,`[[`,1)
b_total = stack(b_total_by_year)
setnames(b_total,"ind","year")
setnames(b_total,"values","total_budget")
b_total$year = as.numeric(as.character(b_total$year))
b_total$total_budget = b_total$total_budget/trillion
b_total = subset(b_total,year>2000 & year<2021)
b_total = b_total[order(b_total$year),]
ggplot(b_total,aes(x=year,y=total_budget)) + geom_line() + theme_bw() + labs(y="Budget (trillions)")
