list.of.packages <- c("data.table","openxlsx","ggplot2","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-annual-report-2021/output")
load("indicators_feb_22_2020.RData")

rm(org_type_spend_2020,recipient_budget_2021,recipient_spend_2020,sector_spend_2020)
for(env_var in ls()){
  new_name = paste0(env_var,"_2019")
  assign(new_name, get(env_var))
  rm(list=env_var)
}

load("indicators_feb_22_2021.RData")

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

# 2020 spend by sector
sector_cats = fread("../SectorCategory.csv")[,c("code","name")]
names(sector_cats) = c("sector_code","sector_name")
sector_cats$sector_code = as.character(sector_cats$sector_code)
sector_spend = stack(sector_spend_2020)
setnames(sector_spend,"values","spend")
setnames(sector_spend,"ind","sector_code")
sector_spend$sector_code = as.character(sector_spend$sector_code)
sector_spend_clean = sector_spend[which(sector_spend$sector_code %in% sector_cats$sector_code),]
sector_spend_cleanable = sector_spend[which(nchar(sector_spend$sector_code)==5),]
sector_spend_cleanable$sector_code = substr(sector_spend_cleanable$sector_code,1,3)
sector_spend_cleanable = sector_spend_cleanable[which(sector_spend_cleanable$sector_code %in% sector_cats$sector_code),]
sector_spend_clean = rbind(sector_spend_clean, sector_spend_cleanable)
sector_spend_clean = data.table(sector_spend_clean)[,.(spend=sum(spend)),by=.(sector_code)]
sector_spend_clean = merge(sector_spend_clean,sector_cats,by="sector_code")
sector_spend_clean$sector_code = NULL
sector_spend_clean = sector_spend_clean[order(sector_spend_clean$spend),]
fwrite(sector_spend_clean,"Sector_spend_2020.csv")
sector_spend_clean$sector_name = factor(sector_spend_clean$sector_name,levels=sector_spend_clean$sector_name)
ggplot(sector_spend_clean,aes(x=sector_name,y=spend)) + geom_bar(stat="identity") + coord_flip() + theme_bw()


# 2020 spend by recipient
recipients = fread("../Country.csv")[,c("code","name")]
names(recipients) = c("recipient_code","recipient_name")
recipients$recipient_code = as.character(recipients$recipient_code)
recipient_spend = stack(recipient_spend_2020)
setnames(recipient_spend,"values","spend")
setnames(recipient_spend,"ind","recipient_code")
recipient_spend$recipient_code = as.character(recipient_spend$recipient_code)
recipient_spend = merge(recipient_spend,recipients,by="recipient_code")
recipient_spend$recipient_code = NULL
recipient_spend$spend_billions = recipient_spend$spend/billion
recipient_spend = recipient_spend[order(-recipient_spend$spend),]
fwrite(recipient_spend,"Recipient_spend_2020.csv")
recipient_spend = recipient_spend[1:10,]
recipient_spend$recipient_name = factor(recipient_spend$recipient_name,levels=rev(recipient_spend$recipient_name))
ggplot(recipient_spend,aes(x=recipient_name,y=spend_billions)) + geom_bar(stat="identity") + scale_y_continuous(labels=dollar) + coord_flip() + theme_bw()

# 2021 budget by recipient
recipient_budget = stack(recipient_budget_2021)
setnames(recipient_budget,"values","budget")
setnames(recipient_budget,"ind","recipient_code")
recipient_budget$recipient_code = as.character(recipient_budget$recipient_code)
recipient_budget = merge(recipient_budget,recipients,by="recipient_code")
recipient_budget$recipient_code = NULL
recipient_budget$budget_billions = recipient_budget$budget/billion
recipient_budget = recipient_budget[order(-recipient_budget$budget),]
fwrite(recipient_budget,"Recipient_budget_2021.csv")
recipient_budget = recipient_budget[1:10,]
recipient_budget$recipient_name = factor(recipient_budget$recipient_name,levels=rev(recipient_budget$recipient_name))
ggplot(recipient_budget,aes(x=recipient_name,y=budget_billions)) + geom_bar(stat="identity") + scale_y_continuous(labels=dollar) + coord_flip() + theme_bw()

# 2020 spend by org_type
org_types = fread("../OrganisationType.csv")[,c("code","name")]
names(org_types) = c("org_type_code","org_type_name")
org_types$org_type_code = as.character(org_types$org_type_code)
org_type_spend = stack(org_type_spend_2020)
setnames(org_type_spend,"values","spend")
setnames(org_type_spend,"ind","org_type_code")
org_type_spend$org_type_code = as.character(org_type_spend$org_type_code)
org_type_spend = merge(org_type_spend,org_types,by="org_type_code")
org_type_spend$org_type_code = NULL
fwrite(org_type_spend,"Org_type_spend_2020.csv")
org_type_spend$spend_trillions = org_type_spend$spend / trillion
org_type_spend = org_type_spend[order(org_type_spend$spend),]
org_type_spend$org_type_name = factor(org_type_spend$org_type_name,levels=org_type_spend$org_type_name)
ggplot(org_type_spend,aes(x=org_type_name,y=spend_trillions)) + geom_bar(stat="identity") + coord_flip() + theme_bw()

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
