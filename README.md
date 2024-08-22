# UKGE24_wpc_census_summaries

This repo contains constituency-level Census summaries for all of the 632 new 2024 Westminster Parliamentary Constituencies in Great Britain, as well as the data and replication code used to produce them.

The following file in the root directory is the key output you're probably looking for: 2024-UK-General-Election-Census-Constituency-Summaries-File-v1.1.csv

Everything else (data and scripts) are the working files so you can adapt for your own purposes in future should you wish to, eg by adding new Census measures.

Measures are derived from available Census tables based on those used previously in the 2019 BES constituency results file (with some hopefully helpful additions for Scotland and Wales): https://www.doi.org/10.48420/20278599

For England and Wales, these are drawn from the Nomis API where possible, and where not, from the ONS custom dataset tool: https://www.ons.gov.uk/datasets/create

For Scotland (for which there are currently limited measures), these are taken from the Scotland's Census website's custom dataset tool: https://www.scotlandscensus.gov.uk/search-the-census#/location/topics

The file also integrates:

1. the 2024 UK General Election results file produced by the House of Commons Library: https://commonslibrary.parliament.uk/research-briefings/cbp-10009/

2. the notional 2019 General Election results for the new boundaries produced by Colin Rallings and Michael Thrasher: https://downloads.bbc.co.uk/news/nol/shared/spl/xls_spreadsheets/results_spreadsheet.ods

3. the notional 2016 EU referendum results for the new boundaries produced by Chris Hanretty: https://docs.google.com/spreadsheets/d/1mtph-ml7CYVoeEUIc1g_IbOvbiZMa_ezRGQlHQoCpF4/edit?gid=341382343#gid=341382343

4. the notional 2014 Scottish independence referendum results for the new boundaries produced by Marta Miori: https://doi.org/10.48420/26340568

## Version history

**Version 1.1**

- Added 2024 results from HoC library file.
- Added Miori ScotRef notionals.
- Updated ONS codes for some Scottish constituencies, so that these are now based on those in HoC Library file (thanks to David Jeffery for the heads up on these).

**Version 1.0**

- Created file, incorporating 2019 notionals, Census summaries and Hanretty EU ref vote estimates at 2024 boundaries.
