import datetime
from dateutil import parser
import gc
import json
from lxml import etree
from lxml.etree import XMLParser
import os
import progressbar


def safe_float(x):
    try:
        return float(x)
    except ValueError:
        return 0


def safe_int(x):
    try:
        return int(x)
    except ValueError:
        return None

# Used for ambiguously structed arrays resulting from XML queries. If an array has any entries, take the first one.
def default_first(array):
    # If an array isn't empty, give us the first element
    return array[0] if array is not None and len(array) > 0 else None


# Used for ambiguous result default replacement. If value doesn't exist, replace it with the default.
def replace_default_if_none(value, default):
    if value is None:
        return default
    elif str.strip(value) == "":
        return default
    else:
        return value


# Used for currency conversion. Works like recode_if_not_none but for our 2-dimension exchange rate dictionary
def convert_usd(value, year, currency, ratedf):
    if value == 0:
        return 0
    elif value is None or year is None or currency is None:
        return None
    try:
        conversion_factor = ratedf[currency][str(year)]
        if conversion_factor > 0:
            return value*conversion_factor
        else:
            return None
    except KeyError:
        return None

if __name__ == '__main__':
    with open('ex_rates.json') as f:
        ratedf = json.load(f)
    ratedf["BEF"] = ratedf["EUR"]
    ratedf["GIP"] = ratedf["GBP"]
    ratedf["AON"] = ratedf["AOA"]
    ratedf["USS"] = ratedf["USD"]
    ratedf["FKP"] = ratedf["GBP"]
    ratedf["ZMK"] = ratedf["ZMW"]
    ratedf["USN"] = ratedf["USD"]
    ratedf["FIM"] = ratedf["EUR"]
    ratedf["EURO"] = ratedf["EUR"]
    ratedf["GHC"] = ratedf["GHS"]

    large_parser = XMLParser(huge_tree=True)
    file_count = 0

    unique_reporting_orgs = list()
    unique_iati_identifiers = list()
    r_org_by_year = dict()
    r_org_by_type = dict()
    activity_count = 0
    humanitarian_activity_count = 0
    humanitarian_scope_emergency_count = 0
    humanitarian_scope_appeal_count = 0
    unique_emergencies = list()
    unique_appeals = list()
    activity_using_sdg_count = 0
    publishers_using_sdgs = list()
    activity_using_sdg_vocab_count = 0
    publishers_using_sdg_vocabs = list()
    unique_sdg_goals = list()
    unique_sdg_targets = list()
    a_count_by_year = dict()
    transaction_count = dict()
    t_count_by_year = dict()
    spend_total = 0
    s_total_by_year = dict()
    budget_total = 0
    b_total_by_year = dict()
    a_count_with_transaction_by_year = dict()
    sector_spend_2020 = dict()
    recipient_spend_2020 = dict()
    recipient_budget_2021 = dict()
    org_type_spend_2020 = dict()

    rootdir = '/home/alex/git/IATI-Registry-Refresher/data'

    mem_keep_vars = dir()
    mem_keep_vars.append("mem_keep_vars")
    mem_keep_vars.append("subdir")
    mem_keep_vars.append("dirs")
    mem_keep_vars.append("files")
    mem_keep_vars.append("filename")

    for subdir, dirs, files in os.walk(rootdir):
        for filename in files:
            file_count = file_count + 1
            print(file_count, " - ", filename)
            filepath = os.path.join(subdir, filename)
            try:
                root = etree.parse(filepath, parser=large_parser).getroot()
            except etree.XMLSyntaxError:
                continue

            activity_len = len(root.findall("iati-activity"))
            bar = progressbar.ProgressBar()
            for i in bar(range(0, activity_len)):
                activity = root.xpath('iati-activity[%s]' % (i + 1))[0]
                activity_count = activity_count + 1

                # Activity sectors
                activity_sector_codes = []
                activity_sector_percentages = []
                non_oecd_sectors = False
                using_sdg_vocab = False
                activity_sectors = activity.findall("sector")
                for activity_sector in activity_sectors:
                    attribs = activity_sector.attrib
                    attrib_keys = list(attribs.keys())
                    percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                    if percentage is not None:
                        percentage = percentage.replace("%", "")
                        percentage = safe_float(percentage)
                    vocab = attribs['vocabulary'] if 'vocabulary' in attrib_keys else ""
                    code = attribs['code'] if 'code' in attrib_keys else None
                    if code is not None:
                        if vocab == "2":
                            activity_sector_codes.append(code)
                            activity_sector_percentages.append(percentage)
                        elif vocab == "1":
                            activity_sector_codes.append(code[:3])
                            activity_sector_percentages.append(percentage)
                        else:
                            if vocab in ["7", "8", "9"]:
                                using_sdg_vocab = True
                            non_oecd_sectors = True
                if len(activity_sector_codes) == 0:
                    if non_oecd_sectors:
                        activity_sector_codes = ["999"]
                    else:
                        activity_sector_codes = ["000"]
                    activity_sector_percentages = [100.0]

                # Activity recipients
                activity_recipient_country_codes = []
                activity_recipient_country_percentages = []
                activity_recipient_countries = activity.findall("recipient-country")
                for activity_recipient_country in activity_recipient_countries:
                    attribs = activity_recipient_country.attrib
                    attrib_keys = list(attribs.keys())
                    percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                    if percentage is not None:
                        percentage = percentage.replace("%", "")
                        percentage = safe_float(percentage)
                    code = attribs['code'].upper() if 'code' in attrib_keys else None
                    if code is not None:
                        activity_recipient_country_codes.append(code)
                        activity_recipient_country_percentages.append(percentage)

                # Humanitarian flag
                humanitarian_flag = default_first(activity.xpath("@humanitarian"))
                if humanitarian_flag in ["1", "true", True]:
                    humanitarian_activity_count = humanitarian_activity_count + 1

                # Humanitarian scope
                using_hs_etype = False
                using_hs_atype = False
                humanitarian_scopes = activity.findall("humanitarian-scope")
                for h_scope in humanitarian_scopes:
                    attribs = h_scope.attrib
                    attrib_keys = list(attribs.keys())
                    vocab = attribs['vocabulary'] if 'vocabulary' in attrib_keys else None
                    code = attribs['code'] if 'code' in attrib_keys else None
                    h_type = attribs['type'] if 'type' in attrib_keys else None
                    if h_type == "1":
                        using_hs_etype = True
                    elif h_type == "2":
                        using_hs_atype = True
                    if code is not None:
                        if vocab == "1-2":
                            if code not in unique_emergencies:
                                unique_emergencies.append(code)
                        elif vocab == "2-1":
                            if code not in unique_appeals:
                                unique_appeals.append(code)
                if using_hs_etype:
                    humanitarian_scope_emergency_count = humanitarian_scope_emergency_count + 1
                if using_hs_atype:
                    humanitarian_scope_appeal_count = humanitarian_scope_appeal_count + 1

                # IATI identifier
                iati_identifier = default_first(activity.xpath("iati-identifier/text()"))
                if iati_identifier is not None:
                    if iati_identifier not in unique_iati_identifiers:
                        unique_iati_identifiers.append(iati_identifier)

                # Activity date
                activity_dates = activity.findall("activity-date")
                start_date = None
                start_year = None
                for activity_date in activity_dates:
                    attribs = activity_date.attrib
                    attrib_keys = list(attribs.keys())
                    d_type = attribs['type'] if 'type' in attrib_keys else None
                    iso_date = attribs['iso-date'] if 'iso-date' in attrib_keys else None
                    if iso_date is not None:
                        if d_type == "1" and start_date is None:
                            start_date = iso_date
                        elif d_type == "2":
                            start_date = iso_date
                if start_date is not None:
                    start_year = safe_int(start_date[0:4])

                # Activity count by year
                if start_year is not None:
                    if start_year in a_count_by_year.keys():
                        a_count_by_year[start_year] = a_count_by_year[start_year] + 1
                    else:
                        a_count_by_year[start_year] = 1

                # Default currency
                defaults = {}
                default_tags = ["default-currency"]
                for tag in default_tags:
                    if tag in activity.attrib.keys():
                        defaults[tag] = activity.attrib[tag]
                    else:
                        defaults[tag] = None

                # Reporting org ref
                reporting_org_ref = default_first(activity.xpath("reporting-org/@ref"))
                reporting_org_type = default_first(activity.xpath("reporting-org/@type"))
                if reporting_org_ref is not None and reporting_org_ref not in unique_reporting_orgs:
                    unique_reporting_orgs.append(reporting_org_ref)
                if reporting_org_ref is not None and reporting_org_type is not None:
                    if reporting_org_type in r_org_by_type.keys():
                        if reporting_org_ref not in r_org_by_type[reporting_org_type]:
                            r_org_by_type[reporting_org_type].append(reporting_org_ref) 
                    else:
                        r_org_by_type[reporting_org_type] = list()
                        r_org_by_type[reporting_org_type].append(reporting_org_ref)
                if reporting_org_ref is not None and start_year is not None:
                    if start_year in r_org_by_year.keys():
                        if reporting_org_ref not in r_org_by_year[start_year]:
                            r_org_by_year[start_year].append(reporting_org_ref) 
                    else:
                        r_org_by_year[start_year] = list()
                        r_org_by_year[start_year].append(reporting_org_ref)

                # SDG tags
                tags = activity.findall("tag")
                using_sdg_tag = False
                for tag in tags:
                    attribs = tag.attrib
                    attrib_keys = list(attribs.keys())
                    vocab = attribs['vocabulary'] if 'vocabulary' in attrib_keys else None
                    code = attribs['code'] if 'code' in attrib_keys else None
                    if vocab in ["2", "3"]:
                        using_sdg_tag = True
                        if code is not None:
                            if vocab == "2":
                                if code not in unique_sdg_goals:
                                    unique_sdg_goals.append(code)
                            elif vocab == "3":
                                if code not in unique_sdg_targets:
                                    unique_sdg_targets.append(code)
                if using_sdg_tag:
                    activity_using_sdg_count = activity_using_sdg_count + 1
                    if reporting_org_ref is not None and reporting_org_ref not in publishers_using_sdgs:
                        publishers_using_sdgs.append(reporting_org_ref)

                # Transactions
                transactions = activity.findall("transaction")
                transaction_a_count_years = list()
                for transaction in transactions:
                    # Transaction sectors
                    transaction_sector_codes = []
                    transaction_sector_percentages = []
                    non_oecd_sectors = False
                    transaction_sectors = transaction.findall("sector")
                    for transaction_sector in transaction_sectors:
                        attribs = transaction_sector.attrib
                        attrib_keys = list(attribs.keys())
                        percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                        if percentage is not None:
                            percentage = percentage.replace("%", "")
                            percentage = safe_float(percentage)
                        vocab = attribs['vocabulary'] if 'vocabulary' in attrib_keys else ""
                        code = attribs['code'] if 'code' in attrib_keys else None
                        if code is not None:
                            if vocab == "2":
                                transaction_sector_codes.append(code)
                                transaction_sector_percentages.append(percentage)
                            elif vocab == "1":
                                transaction_sector_codes.append(code[:3])
                                transaction_sector_percentages.append(percentage)
                            else:
                                if vocab in ["7", "8", "9"]:
                                    using_sdg_vocab = True
                                non_oecd_sectors = True
                    if len(transaction_sector_codes) == 0:
                        if non_oecd_sectors:
                            transaction_sector_codes = ["999"]
                            transaction_sector_percentages = [100.0]
                        else:
                            transaction_sector_codes = activity_sector_codes
                            transaction_sector_percentages = activity_sector_percentages
                    # Transaction recipients
                    transaction_recipient_country_codes = []
                    transaction_recipient_country_percentages = []
                    transaction_recipient_countries = transaction.findall("recipient-country")
                    for transaction_recipient_country in transaction_recipient_countries:
                        attribs = transaction_recipient_country.attrib
                        attrib_keys = list(attribs.keys())
                        percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                        if percentage is not None:
                            percentage = percentage.replace("%", "")
                            percentage = safe_float(percentage)
                        code = attribs['code'].upper() if 'code' in attrib_keys else None
                        if code is not None:
                            transaction_recipient_country_codes.append(code)
                            transaction_recipient_country_percentages.append(percentage)
                    if len(transaction_recipient_country_codes) == 0:
                        activity_recipient_regions = activity.findall("recipient-region")
                        transaction_recipient_regions = transaction.findall("recipient-region")
                        if len(activity_recipient_country_codes) == 0:
                            if len(activity_recipient_regions) > 0 or len(transaction_recipient_regions) > 0:
                                transaction_recipient_country_codes = ["REG"]
                            else:
                                transaction_recipient_country_codes = ["MIS"]
                            transaction_recipient_country_percentages = [100.0]
                        else:
                            transaction_recipient_country_codes = activity_recipient_country_codes
                            transaction_recipient_country_percentages = activity_recipient_country_percentages
                    # Transaction type
                    transaction_type_code = default_first(transaction.xpath("transaction-type/@code"))
                    # Transaction count by transaction type
                    if transaction_type_code is not None:
                        if transaction_type_code in transaction_count.keys():
                            transaction_count[transaction_type_code] = transaction_count[transaction_type_code] + 1
                        else:
                            transaction_count[transaction_type_code] = 1
                    # Transaction date
                    t_year = None
                    transaction_date = default_first(transaction.xpath("transaction-date/@iso-date"))
                    if transaction_date is not None:
                        try:
                            t_year = int(transaction_date[:4]) if transaction_date is not None else None
                        except ValueError:
                            pass
                    if t_year is not None:
                        if t_year in t_count_by_year.keys():
                            if transaction_type_code in t_count_by_year[t_year].keys():
                                t_count_by_year[t_year][transaction_type_code] = t_count_by_year[t_year][transaction_type_code] + 1
                            else:
                                t_count_by_year[t_year][transaction_type_code] = 1
                        else:
                            t_count_by_year[t_year] = dict()
                            t_count_by_year[t_year][transaction_type_code] = 1
                        if t_year not in transaction_a_count_years:
                            transaction_a_count_years.append(t_year)
                    # Transaction value, value date, currency
                    t_currency = default_first(transaction.xpath("value/@currency"))
                    t_currency = replace_default_if_none(t_currency, defaults["default-currency"])
                    if t_currency == "":
                        t_currency = None
                    if t_currency is not None:
                        t_currency = t_currency.replace(" ", "")
                        t_currency = t_currency.upper()

                    t_value = default_first(transaction.xpath("value/text()"))
                    t_value = safe_float(t_value.replace(" ", "").replace(",","")) if t_value is not None else None
                    t_value_date = default_first(transaction.xpath("value/@value-date"))
                    if t_year is None:
                        transaction_date = t_value_date
                        try:
                            t_year = int(transaction_date[:4]) if transaction_date is not None else None
                        except ValueError:
                            pass
                    if t_year is not None and t_value is not None and t_currency is not None and transaction_type_code in ["3", "4", "D", "E", "Disbursement", "Expenditure"]:
                        t_value_usd = convert_usd(t_value, t_year, t_currency, ratedf)
                        if t_value_usd is not None:
                            spend_total = spend_total + t_value_usd
                            if t_year in s_total_by_year.keys():
                                s_total_by_year[t_year] = s_total_by_year[t_year] + t_value_usd
                            else:
                                s_total_by_year[t_year] = t_value_usd
                            # org type transactions 2020
                            if t_year == 2020 and reporting_org_type is not None and reporting_org_ref is not None:
                                if reporting_org_type in org_type_spend_2020.keys():
                                    if reporting_org_ref in org_type_spend_2020[reporting_org_type].keys():
                                        org_type_spend_2020[reporting_org_type][reporting_org_ref] = org_type_spend_2020[reporting_org_type][reporting_org_ref] + t_value_usd
                                    else:
                                        org_type_spend_2020[reporting_org_type][reporting_org_ref] = t_value_usd
                                else:
                                    org_type_spend_2020[reporting_org_type] = dict()
                                    org_type_spend_2020[reporting_org_type][reporting_org_ref] = t_value_usd
                            # sector transactions 2020
                            if t_year == 2020 and len(transaction_sector_codes) > 0 and reporting_org_ref is not None:
                                for sector_idx in range(0, len(transaction_sector_codes)):
                                    sec_percentage = transaction_sector_percentages[sector_idx]
                                    sec_code = transaction_sector_codes[sector_idx]
                                    t_value_usd_split_sector = t_value_usd * (sec_percentage/100.0)
                                    if t_value_usd_split_sector is not None:
                                        if sec_code in sector_spend_2020.keys():
                                            if reporting_org_ref in sector_spend_2020[sec_code].keys():
                                                sector_spend_2020[sec_code][reporting_org_ref] = sector_spend_2020[sec_code][reporting_org_ref] + t_value_usd_split_sector
                                            else:
                                                sector_spend_2020[sec_code][reporting_org_ref] = t_value_usd_split_sector
                                        else:
                                            sector_spend_2020[sec_code] = dict()
                                            sector_spend_2020[sec_code][reporting_org_ref] = t_value_usd_split_sector
                            # recipient transactions 2020
                            if t_year == 2020 and len(transaction_recipient_country_codes) > 0 and reporting_org_ref is not None:
                                for recip_idx in range(0, len(transaction_recipient_country_codes)):
                                    recip_percentage = transaction_recipient_country_percentages[recip_idx]
                                    recip_code = transaction_recipient_country_codes[recip_idx]
                                    t_value_usd_split_recip = t_value_usd * (recip_percentage/100.0)
                                    if t_value_usd_split_recip is not None:
                                        if recip_code in recipient_spend_2020.keys():
                                            if reporting_org_ref in recipient_spend_2020[recip_code].keys():
                                                recipient_spend_2020[recip_code][reporting_org_ref] = recipient_spend_2020[recip_code][reporting_org_ref] + t_value_usd_split_recip
                                            else:
                                                recipient_spend_2020[recip_code][reporting_org_ref] = t_value_usd_split_recip
                                        else:
                                            recipient_spend_2020[recip_code] = dict()
                                            recipient_spend_2020[recip_code][reporting_org_ref] = t_value_usd_split_recip
                if using_sdg_vocab:
                    activity_using_sdg_vocab_count = activity_using_sdg_vocab_count + 1
                    if reporting_org_ref is not None and reporting_org_ref not in publishers_using_sdg_vocabs:
                        publishers_using_sdg_vocabs.append(reporting_org_ref)
                for transaction_a_count_year in transaction_a_count_years:
                    if transaction_a_count_year in a_count_with_transaction_by_year.keys():
                        a_count_with_transaction_by_year[transaction_a_count_year] = a_count_with_transaction_by_year[transaction_a_count_year] + 1
                    else:
                        a_count_with_transaction_by_year[transaction_a_count_year] = 1

                # Budgets
                original_budgets = []
                revised_budgets = []
                budgets = activity.findall("budget")

                for budget in budgets:
                    if "type" in budget.attrib.keys():
                        budget_type = budget.attrib["type"]
                    else:
                        budget_type = None
                    b_start_date = default_first(budget.xpath("period-start/@iso-date"))
                    b_end_date = default_first(budget.xpath("period-end/@iso-date"))
                    time_range = {}
                    try:
                        time_range["start"] = parser.parse(b_start_date)
                        time_range["end"] = parser.parse(b_end_date)
                    except (TypeError, ValueError) as error:
                        time_range["start"] = None
                        time_range["end"] = None
                    if time_range["start"] is not None:
                        time_range["length"] = time_range["end"]-time_range["start"]
                        b_year = time_range["start"].year

                        b_value = default_first(budget.xpath("value/text()"))
                        try:
                            b_value = float(b_value.replace(" ", "").replace(",","")) if b_value is not None else None
                        except ValueError:
                            b_value = None
                        b_currency = default_first(budget.xpath("value/@currency"))
                        b_currency = replace_default_if_none(b_currency, defaults["default-currency"])
                        if b_currency is not None:
                            b_currency = b_currency.replace(" ", "").upper()

                        if b_value is not None and b_currency is not None and b_year is not None:
                            b_value_usd = convert_usd(b_value, b_year, b_currency, ratedf)
                            tmp_meta = {"time_range": time_range, "b_value_usd": b_value_usd}
                            if budget_type == "1":
                                original_budgets.append(tmp_meta)
                            elif budget_type == "2":
                                revised_budgets.append(tmp_meta)
                fixed_budgets = []
                if len(original_budgets) == 0:
                    fixed_budgets = revised_budgets
                elif len(revised_budgets) == 0:
                    fixed_budgets = original_budgets
                else:
                    fixed_budgets = revised_budgets
                    for o_budget in original_budgets:
                        o_range = o_budget["time_range"]
                        for r_budget in revised_budgets:
                            r_range = r_budget["time_range"]
                            inner_overlap = (o_range["start"] >= r_range["start"]) and (o_range["end"] <= r_range["end"])
                            outer_overlap = (r_range["start"] >= o_range["start"]) and (r_range["end"] <= o_range["end"])
                            start_overlap = (o_range["start"] >= r_range["start"]) and  (o_range["start"] <= r_range["end"])
                            end_overlap = (o_range["end"] >= r_range["start"]) and (o_range["end"] <= r_range["end"])
                            any_overlap = any([inner_overlap, outer_overlap, start_overlap, end_overlap, False])
                            if not any_overlap:
                                fixed_budgets.append(o_budget)
                for fixed_budget in fixed_budgets:
                    b_year = fixed_budget["time_range"]["start"].year
                    b_value_usd = fixed_budget["b_value_usd"]
                    if b_value_usd is not None:
                        budget_total = budget_total + b_value_usd
                        if b_year in b_total_by_year.keys():
                            b_total_by_year[b_year] = b_total_by_year[b_year] + b_value_usd
                        else:
                            b_total_by_year[b_year] = b_value_usd
                        if len(activity_recipient_country_codes) == 0:
                            budget_recipient_country_codes = ["MIS"]
                            budget_recipient_country_percentages = [100.0]
                        else:
                            budget_recipient_country_codes = activity_recipient_country_codes
                            budget_recipient_country_percentages = activity_recipient_country_percentages
                        if b_year==2021 and len(budget_recipient_country_codes) > 0 and reporting_org_ref is not None:
                            for recip_idx in range(0, len(budget_recipient_country_codes)):
                                recip_percentage = budget_recipient_country_percentages[recip_idx]
                                recip_code = budget_recipient_country_codes[recip_idx]
                                b_value_usd_split_recip = b_value_usd * (recip_percentage/100.0)
                                if b_value_usd_split_recip is not None:
                                    if recip_code in recipient_budget_2021.keys():
                                        if reporting_org_ref in recipient_budget_2021[recip_code].keys():
                                            recipient_budget_2021[recip_code][reporting_org_ref] = recipient_budget_2021[recip_code][reporting_org_ref] + b_value_usd_split_recip
                                        else:
                                            recipient_budget_2021[recip_code][reporting_org_ref] = b_value_usd_split_recip
                                    else:
                                        recipient_budget_2021[recip_code] = dict()
                                        recipient_budget_2021[recip_code][reporting_org_ref] = b_value_usd_split_recip

            for varname in dir():
                if varname not in mem_keep_vars:
                    del globals()[varname]

            gc.collect()
    # Write
    write_obj = {
        "unique_reporting_orgs": unique_reporting_orgs,
        "unique_iati_identifiers": unique_iati_identifiers,
        "r_org_by_year": r_org_by_year,
        "r_org_by_type": r_org_by_type,
        "activity_count": activity_count,
        "humanitarian_activity_count": humanitarian_activity_count,
        "humanitarian_scope_emergency_count": humanitarian_scope_emergency_count,
        "humanitarian_scope_appeal_count": humanitarian_scope_appeal_count,
        "unique_emergencies": unique_emergencies,
        "unique_appeals": unique_appeals,
        "activity_using_sdg_count": activity_using_sdg_count,
        "publishers_using_sdgs": publishers_using_sdgs,
        "activity_using_sdg_vocab_count": activity_using_sdg_vocab_count,
        "publishers_using_sdg_vocabs": publishers_using_sdg_vocabs,
        "unique_sdg_goals": unique_sdg_goals,
        "unique_sdg_targets": unique_sdg_targets,
        "a_count_by_year": a_count_by_year,
        "transaction_count": transaction_count,
        "t_count_by_year": t_count_by_year,
        "spend_total": spend_total,
        "s_total_by_year": s_total_by_year,
        "budget_total": budget_total,
        "b_total_by_year": b_total_by_year,
        "a_count_with_transaction_by_year": a_count_with_transaction_by_year,
        "sector_spend_2020": sector_spend_2020,
        "recipient_spend_2020": recipient_spend_2020,
        "recipient_budget_2021": recipient_budget_2021,
        "org_type_spend_2020": org_type_spend_2020
    }
    with open("output/indicators_feb_22_2021.json", "w") as outfile:
        json.dump(write_obj, outfile)
