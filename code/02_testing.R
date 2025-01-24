source("code/00_dependencies.R")
source("code/01_functions.R")



# analysis setup ----------------------------------------------------------

as_of_date <- Sys.Date() - days(1)

# lookup files

asset_file <- "lookup_data/asset_types.xlsx"
groups_lu_file <- "lookup_data/groups.xlsx"
restricted_lu_file <- "lookup_data/restricted_securities.xlsx"
translate_file <- "lookup_data/translate_tickers.xlsx"


# Bloomberg and CAM files

apx_file <- glue("data/{as_of_date}_mutualfundtest.csv")
bbg_file <- glue("data/{as_of_date}_MF-Testing.csv")


# 2. prepare tibbles for analysis -----------------------------------------

# lookup tables

asset_tbl <- read_xlsx(asset_file)
groups_tbl <- read_xlsx(groups_lu_file)
restricted_tbl <- read_xlsx(restricted_lu_file) 
translate_tbl <- read_xlsx(translate_file)


# initial Bloomberg and CAM tables

fund_tbl <- parse_apx_file(apx_file)
raw_bbg_tbl <- parse_raw_bbg(bbg_file)
bbg_tbl <- prep_bbg_tbl(raw_bbg_tbl, asset_tbl)
index_tbl <- prep_index_tbl(raw_bbg_tbl)


# clean up Bloomberg and CAM tables

bbg_tbl <- translate_bad_tickers(bbg_tbl, translate_tbl)

analysis_tbl <- prep_analysis_tbl(fund_tbl, bbg_tbl)
# analysis_tbl <- prep_analysis_tbl(
#   fund_tbl |> filter(!(ticker %in% c("USFD", "CRTO"))),
#   bbg_tbl
# )

holdings_tbl <- prep_holdings_tbl(analysis_tbl)


# 3. run tests and prepare printable results ------------------------------

test_summary <- analysis_tbl |> 
  mutate(
    il_test_01     = map(fund_data, test_01),
    il_test_01_di  = map(fund_data, test_01_di),
    il_test_01_dii = map(fund_data, test_01_dii),
    il_test_02_a   = map(fund_data, test_02_a),
    il_test_02_b   = map(fund_data, test_02_b),
    il_test_02_c   = map(fund_data, test_02_c),
    il_test_03     = map(fund_data, test_03),
    il_test_04     = map(fund_data, test_04),
    il_test_05     = map(fund_data, test_05),
    il_test_06_a   = map(fund_data, test_06_a),
    il_test_06_b   = map(fund_data, test_06_b),
    il_test_06_c   = map(fund_data, test_06_c),
    il_test_07     = map(fund_data, ~test_07(.x, groups_tbl, index_tbl)),
    nf_test_01     = map(fund_data, nf_test_01),
    nf_test_02     = map(fund_data, ~nf_test_02(.x, groups_tbl)),
    nf_test_03     = map(fund_data, nf_test_03),
    nf_test_04     = map(fund_data, nf_test_04),
    ra_test_01     = map(fund_data, ra_test_01), # doesn't really test anything
    ra_test_02     = map(fund_data, ~ra_test_02(.x, holdings_tbl, restricted_tbl))
  )


results_summary <- test_summary |> 
  mutate(
    il_test_01     = map(il_test_01,       il_test_01_report_details),
    il_test_01_di  = map(il_test_01_di,    ~report_details(.x, c("parent", "Wt (%)", "test"))),
    il_test_01_dii = map(il_test_01_dii,   ~report_details(.x, c("parent", "Shares Out Held (%)", "test"))),
    il_test_02_a   = map(il_test_02_a,     ~report_details(.x, c("security", "Shares Out Held (%)", "test"))),
    il_test_02_b   = map(il_test_02_b,     ~report_details(.x, c("security", "Wt (%)", "test"))),
    il_test_02_c   = map(il_test_02_c,     ~report_details(.x, c("security", "Wt (%)", "test"))),
    il_test_03     = map(il_test_03,       ~report_details(.x, c("other", "Wt (%)", "test"))),
    il_test_04     = map(il_test_04,       ~report_details(.x, c("parent", "Wt (%)", "test"))),
    il_test_05     = map(il_test_05,       ~report_details(.x, c("security", "Shares Out Held (%)", "test"))),
    il_test_06_a   = map(il_test_06_a,     ~report_details(.x, c("security", "Shares Out Held (%)", "test"))),
    il_test_06_b   = map(il_test_06_b,     ~report_details(.x, c("security", "Shares Out Held (%)", "test"))),
    il_test_06_c   = map(il_test_06_c,     ~report_details(.x, c("parent", "Wt (%)", "test"))),
    il_test_07     = map(il_test_07,       il_test_07_report_details),
    nf_test_01     = map(nf_test_01,       ~report_details(.x, c("parent", "Shares Out Held (%)", "test"))),
    nf_test_02     = map(nf_test_02,       ~report_details(.x, c("security", "Wt (%)", "test"))),
    nf_test_03     = map(nf_test_03,       ~report_details(.x, c("security", "Wt (%)", "test"))),
    nf_test_04     = map(nf_test_04,       ~report_details(.x, c("parent", "Wt (%)", "test"))),
    ra_test_01     = map(ra_test_01,       ra_test_01_report_details),
    ra_test_02     = map(ra_test_02,       ra_test_02_report_details)
  )



# 4. archive results to rds file ------------------------------------------

# TO-DO: make file names reflect testing date

input_list <- c(
  apx_file,
  asset_file,
  bbg_file,
  groups_lu_file,
  restricted_lu_file,
  translate_file
)

calc_list <- list(
  analysis_tbl = analysis_tbl,
  results_summary = results_summary,
  test_summary = test_summary
)

dir.create(glue("output/{as_of_date}_data-archive"))

saveRDS(
  object = calc_list,
  file = glue("output/{as_of_date}_data-archive/{as_of_date}_output-files.rds")
)

utils::zip(
  glue("output/{as_of_date}_data-archive/{as_of_date}_input-files.zip"),
  input_list
)


# 5. render html report ---------------------------------------------------

render_mf_report(
  rds_file = glue("output/{as_of_date}_data-archive/{as_of_date}_output-files.rds"),
  n_file = "lookup_data/groups.xlsx",
  dt = as.character(as_of_date),
  template = "mf_testing.qmd"
)

file.rename(
  glue("{as_of_date}_fund_testing.html"),
  glue("output/{as_of_date}_fund_testing.html")
)