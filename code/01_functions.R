# parsing files -----------------------------------------------------------

# parse apx file by mutual fund

parse_apx_file <- function(file) {
  
  columns <- c(
    "fund_code", "quantity", "security", "apx_sec_type", "sec_cusip", "sec_symbol",
    "unit_cost", "tot_cost", "price", "mkt_value", "pct_assets"
  )
  
  tbl <- read_csv(
    file,
    col_names = columns,
    col_types = "cnccccnnnnn"
  )
  
  stop_for_problems(tbl)
  
  tbl <- tbl |> 
    mutate(
      sec_symbol = str_replace(sec_symbol, '[.]', '/')
    )
  
  tbl <- tbl |> 
    mutate(
      type = case_when(
        apx_sec_type %in% c("csus", "adus") ~ "Common Stock",
        apx_sec_type %in% c("caus") ~ "Cash & Equivalents",
        .default = "Unknown"
      )
    ) |> 
    mutate(
      quantity = if_else(apx_sec_type == "caus", 0, quantity),
      unit_cost = if_else(apx_sec_type == "caus", 1, unit_cost),
      price = if_else(apx_sec_type == "caus", 1, price),
      sec_symbol = if_else(apx_sec_type == "caus", NA, sec_symbol)
    ) |> 
    mutate(
      ticker = toupper(sec_symbol),
      .after = security
    ) |> 
    select(-sec_symbol)
  
  return(tbl)
  
}

parse_raw_bbg <- function(file){
  
  bbg_tbl <- suppressWarnings(
    read_csv(
      file,
      skip = 8,
      col_names = c("name", "cusip", "bbg_ticker", "parent", "sec_type", "shares_out",
                    "mkt_cap", "sector", "industry"),
      col_types = "cccccnncc",
      show_col_types = FALSE
    )
  )
  
  bbg_tbl <- bbg_tbl |>
    # change tickers so they match sec_symbol from APX file
    mutate(
      ticker = str_remove(bbg_ticker, ' .*$')
    ) |>
    mutate(
      bbg_ticker = str_replace(bbg_ticker, " +", " ")
    ) |>
    relocate(ticker, .after = bbg_ticker)
  
  return(bbg_tbl)
  
}

prep_bbg_tbl <- function(b_tbl, a_tbl) {
  
  bbg_tbl <- b_tbl |> 
    filter(!is.na(bbg_ticker)) |> 
    distinct()
  
  bbg_tbl <- left_join(
    bbg_tbl, a_tbl,
    by = join_by(sec_type)
  )
  
  bbg_tbl <- bbg_tbl |> 
    replace_na(list(asset_type = "Unknown"))
  
}

# parse Bloomberg data for index information
prep_index_tbl <- function(b_tbl){
  
  index_tbl <- b_tbl |> 
    mutate(
      group = if_else(is.na(bbg_ticker), name, NA)
    ) |> 
    fill(group) |> 
    relocate(group) |> 
    filter(!is.na(bbg_ticker)) |> 
    filter(!str_detect(group, "MF")) |> 
    group_by(group) |> 
    summarize(
      min_mkt_cap = min(mkt_cap, na.rm = TRUE),
      max_mkt_cap = max(mkt_cap, na.rm = TRUE)
    )
  
  return(index_tbl)
}

prep_fund_tbl <- function(tbl){
  tbl |> 
    group_by(fund_num) |> 
    group_split() |> 
    map(get_dat) |> 
    bind_rows() |> 
    group_by(ticker) |> 
    mutate(shares_across_ports = sum(quantity)) |> 
    ungroup()
}



# find holding type and remove non-holding rows, convert numbers from char to dbl

get_dat <- function(tbl){
  
  # capture fund code
  fund_code <- tbl |> slice(1) |> pull(quantity)
  
  cash_symbols <- c("cash", "divacc")
  
  xx <- which(tbl$quantity == "Cash & Equivalents") 
  yy <- which(tbl$quantity == "TOTAL PORTFOLIO") - 5
  zz <- which(tbl$quantity == "Common Stock")
  
  type <- if_else(1:nrow(tbl) < zz, "Cash & Equivalents", "Common Stock", "Unknown")
  tbl <- add_column(tbl, type)
  
  tbl <- tbl[xx:yy,]
  tbl <- tbl[!is.na(tbl$security), ]
  
  tbl <- tbl |> 
    mutate(
      quantity = as.numeric(quantity),
      across(
        .cols = unit_cost:yield,
        .fns = \(x) suppressWarnings(as.numeric(x))
        
      )
    )
  
  # convert cash items to conform with common stock
  tbl <- tbl |> 
    mutate(
      quantity = if_else(sec_symbol %in% cash_symbols, 0, quantity),
      unit_cost = if_else(sec_symbol %in% cash_symbols, 1, unit_cost),
      price = if_else(sec_symbol %in% cash_symbols, 1, price),
      sec_symbol = if_else(sec_symbol %in% cash_symbols, NA, sec_symbol)
    )
  
  tbl <- tbl |> 
    mutate(fund_code = fund_code) |> 
    select(-fund_num) |> 
    relocate(fund_code)
  
  tbl <- tbl |> 
    mutate(sec_symbol = toupper(sec_symbol)) |> 
    rename(ticker = sec_symbol)
  
  return(tbl)
}

translate_bad_tickers <- function(b_tbl, t_tbl) {
  
  b_tbl |> 
    mutate(
      ticker = if_else(
        bbg_ticker %in% t_tbl$bbg_ticker,
        t_tbl |> filter(bbg_ticker == bbg_ticker) |> pull(cam_ticker),
        ticker
      )
    )
  
}

safely_combine_tbls <- function(cam_tbl, bloomberg_tbl) {
  
  unmatched_tbl <- cam_tbl |> 
    filter(!is.na(ticker)) |> 
    anti_join(bloomberg_tbl, by = join_by(ticker))
  
  tryCatch(
    {
      stopifnot(
        nrow(unmatched_tbl) == 0
      )
    },
    error = function(e) {
      u_tickers <- paste0(unmatched_tbl$ticker |> unique(), collapse = ", ")
      cat(paste("Unmatched tickers:", u_tickers, "\n\n"))
      
      stop(
        paste(
          "The unmatched tickers need to be added to the lookup_ticker table",
          "before proceeding."
        ),
        call. = FALSE
      )
      
    },
    finally = {}
  )
  
  tbl <- cam_tbl |> 
    left_join(bloomberg_tbl, by = join_by(ticker))
  
  return(tbl)
  
}

get_parent_os <- function(tbl){
  
  tbl <- tbl |> 
    group_by(parent) |> 
    mutate(
      parent_pct_os = signif(sum(quantity) / sum(shares_out)*100, digits = 3)
    ) |> 
    ungroup()
  
}

prep_analysis_tbl <- function(f_tbl, b_tbl){
  
  analysis_tbl <- safely_combine_tbls(f_tbl, b_tbl)
  
  analysis_tbl <- analysis_tbl |> 
    mutate(
      pct_shares_out = signif((quantity / shares_out) * 100, digits = 3),
      pct_shares_out = if_else(is.na(pct_shares_out), 0, pct_shares_out),
      asset_type = if_else(type == "Cash & Equivalents", "Cash", asset_type)
    ) |> 
    group_by(fund_code) |> 
    mutate(
      port_value = sum(mkt_value),
      pct_port = mkt_value / port_value * 100
    ) |> 
    ungroup() |> 
    group_nest(fund_code, .key = "fund_data", keep = TRUE) |> 
    mutate()
  
  analysis_tbl <- analysis_tbl |> 
    mutate(fund_data = map(fund_data, get_parent_os))
}

prep_holdings_tbl <- function(tbl){
  tbl |> 
    select(-fund_code) |> 
    unnest(fund_data) |> 
    filter(!is.na(ticker)) |> 
    group_by(ticker) |> 
    summarize(
      held = paste(fund_code, collapse = ", ")
    )
}


# tests -------------------------------------------------------------------

# Compile result type and details for each test

compile_test_result <- function(tbl){
  result <- tbl |> 
    filter(test == FALSE)
  
  if(nrow(result) == 0){
    alert_type = "OK"
  } 
  else{
    alert_type = "Alert"
  }
  
  return(
    list(
      type = alert_type,
      tbl = tbl
    )
  )
  
}

#Test 1

test_01 <- function(tbl){
  
  over_10_voting <- tbl |> 
    filter(!is.na(parent)) |> 
    select(parent, parent_pct_os, pct_port) |>
    filter(parent_pct_os > 10) |> 
    summarize(over_10_voting = sum(pct_port))
  
  over_5_port <- tbl |> 
    filter(!is.na(parent)) |> 
    select(parent, pct_port) |>
    group_by(parent) |> 
    mutate(parent_pct_port = sum(pct_port)) |> 
    select(parent, parent_pct_port) |> 
    distinct() |> 
    ungroup() |> 
    filter(parent_pct_port > 5) |> 
    summarize(over_5_port = sum(parent_pct_port))
  
  tbl <- tbl |> 
    select(asset_type, pct_port) |> 
    group_by(asset_type) |> 
    summarize(
      pct_asset_type = sum(pct_port)
    ) |> 
    add_row(
      asset_type = "Violation: Issuer where Fund holds >5% of Fund assets",
      pct_asset_type = pull(over_5_port) * -1
    ) |> 
    add_row(
      asset_type = "Violation: Issuer where Fund holds >10% voting sec.", 
      pct_asset_type = pull(over_10_voting) * -1
    ) |> 
    adorn_totals("row")
  
  total <- tbl |>
    filter(asset_type == "Total") |> 
    pull(pct_asset_type)
  
  type <- if_else(total >= 75, "OK", "Alert")
  
  def <- "Fund must normally have at least 75% of its total
  assets invested in Cash & Equivalents, Govt Issues, Funds, or Other Securities
  where the Fund holds less than 10% of the issuers' voting shares or invests
  less than 5% of total assets."
  
  test_results <- lst(type, tbl, def)
  
  return(test_results)
  
}

# Test 1.d.i

test_01_di <- function(tbl) {
  tbl <- tbl |>
    select(parent, ticker, pct_port) |>
    group_by(parent) |>
    filter(!is.na(parent)) |> 
    mutate(
      parent_pct_port = sum(pct_port),
      test = if_else(parent_pct_port <= 5, TRUE, FALSE)
    ) |>
    select(parent, parent_pct_port, test) |> 
    distinct() |> 
    arrange(desc(parent_pct_port)) |>
    ungroup() |> 
    head(10)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- paste0(
    "Other securities limited in respect of any one issuer to an amount greater 
    than 5% of its total assets."
  )
  
  test_results$vio_count <- tbl |> 
    filter(test == FALSE) |> 
    count() |> 
    as.double()
  
  test_results$vio_pct <- tbl |> 
    filter(test == FALSE) |> 
    summarize(vio_count = sum(parent_pct_port))
  
  return(test_results)
  
}

# Test 1.d.ii

test_01_dii <- function(f_tbl){
  
  tbl <- f_tbl |>
    select(parent, parent_pct_os) |>
    distinct() |>
    mutate(
      test = if_else(parent_pct_os <= 10, TRUE, FALSE)
    ) |> 
    arrange(desc(parent_pct_os)) |> 
    head(3)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "Other securities limited in respect of any one issuer to 
  no greater than 10% of the oustanding voting securities of such issuer. "
  
  test_results$vio_count <- tbl |> 
    filter(test == FALSE) |> 
    count() |> 
    as.double()
  
  test_results$vio_pct <- f_tbl |> 
    filter(!is.na(parent)) |> 
    select(parent, parent_pct_os, pct_port) |> 
    filter(parent_pct_os > 10) |> 
    summarize(vio_pct = sum(pct_port))
  
  return(test_results)
}

# Test 2.a

test_02_a <- function(tbl){
  tbl <- tbl |> 
    filter(sec_type == "Fund") |> 
    select(security, pct_shares_out) |> 
    mutate(
      test = if_else(pct_shares_out < 3, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_shares_out)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No funds held",
      pct_shares_out = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund cannot own more than 3% of the total outstanding
  voting stock of another investment company."
  
  return(test_results)
}

# Test 2.b

test_02_b <- function(tbl){
  tbl <- tbl |> 
    filter(sec_type == "Fund") |> 
    select(security, pct_port) |> 
    mutate(
      test = if_else(pct_port <= 5, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_port)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No funds held",
      pct_port = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund cannot invest more than 5% of its assets in 
  securities on another investment company."
  
  return(test_results)
}

# Test 2.c

test_02_c <- function(tbl){
  tbl <- tbl |> 
    filter(sec_type == "Fund") |> 
    select(security, pct_port) |> 
    mutate(
      test = if_else(sum(pct_port) <= 10, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_port))
  
  if(nrow(tbl) != 0){ 
    tbl <- tbl |> 
      adorn_totals("row") |> 
      mutate(tbl$Total[[test]] <- if_else(pct_port <= 10, TRUE, FALSE))
    
    tbl_head <- head(tbl, 3)
    tbl_tail <- tail(tbl, 1)
    
    tbl <- rbind(tbl_head, tbl_tail) 
  }
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No securities of other investment companies held",
      pct_port = NA,
      test = NA
    )
    
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund cannot invest more than 10% of its assets in 
  securities of other investment companies in aggregate."
  
  return(test_results)
}

# Test 3

test_03 <- function(tbl){
  tbl <- tbl |> 
    select(pct_port, industry) |> 
    group_by(industry) |> 
    mutate(
      ind_pct = sum(pct_port),
      test = if_else(ind_pct < 25, TRUE, FALSE)
    ) |>
    select(industry, ind_pct, test) |> 
    distinct() |> 
    ungroup() |> 
    arrange(desc(ind_pct)) |> 
    head(3)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may not invest more than 25% of net assets in 
  any one industry (other than government securities)."
  
  return(test_results)
}

# Test 4

test_04 <- function(tbl){
  tbl <- tbl |> 
    filter(sec_type == "Closed-End fund") |> 
    select(security, pct_shares_out) |> 
    mutate(
      test = if_else(pct_shares_out < 10, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_shares_out)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No funds held",
      pct_shares_out = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "Fund family combined holdings of a closed end fund may 
  not exceed 10% of the total outstanding voting stock of the closed end 
  investment fund."
  
  return(test_results)
}


# Test 5

test_05 <- function(tbl){
  tbl <- tbl |> 
    filter(industry == 'Insurance') |> 
    select(security, pct_shares_out) |> 
    mutate(
      test = if_else(pct_shares_out < 10, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_shares_out)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No insurance companies held",
      pct_shares_out = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "Fund cannot own more than 10% of the outstanding voting 
  stock of an insurance company."
  
  return(test_results)
}

# Test 6a

test_06_a <- function(tbl){
  tbl <- tbl |> 
    filter(sector == 'Financials') |> 
    select(parent, parent_pct_os) |> 
    distinct() |> 
    mutate(
      test = if_else(parent_pct_os < 5, TRUE, FALSE)
    ) |> 
    arrange(desc(parent_pct_os)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      parent = "No finance companies held",
      parent_pct_os = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may acquire any security issued by a company 
  that derives 15+% of gross revenues from securities related activities, 
  provided that: the acquiring company owns not more than 5% of the outstanding 
  securities of that class of the issuer's equity securities."
  
  return(test_results)
}

# Test 6b

test_06_b <- function(tbl){
  tbl <- tbl |> 
    filter(sector == 'Financials') |> 
    filter(sec_type == "Bond") |> 
    select(security, pct_shares_out) |> 
    mutate(
      test = if_else(pct_shares_out < 10, TRUE, FALSE)
    ) |> 
    arrange(desc(pct_shares_out)) |> 
    head(3)
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No Finance Co Bonds Held",
      pct_shares_out = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may acquire any security issued by a company 
  that derives 15+% of gross revenues from securities related activities,
  provided that the acquiring company owns not more than 10% of the outstanding
  principal amount of the issuer's debt securities."
  
  return(test_results)
}

# Test 6c

test_06_c <- function(tbl) {
  tbl <- tbl |>
    filter(sector == 'Financials') |> 
    select(parent, ticker, pct_port) |>
    group_by(parent) |>
    mutate(
      parent_pct_port = sum(pct_port),
      test = if_else(parent_pct_port <= 5, TRUE, FALSE)
    ) |>
    select(parent, parent_pct_port, test) |> 
    distinct() |> 
    arrange(desc(parent_pct_port)) |>
    ungroup() |> 
    head(3)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may acquire any security issued by a company 
  that derives 15+% of gross revenues from securities related activities,
  provided that the acquiring company has invested not more than 5% of the value
  of its total assets in the securities of the issuer."
  
  return(test_results)
}

# Test 7

test_07 <- function(f_tbl, g_tbl, i_tbl) {
  
  tbl_fund_code <- f_tbl |> 
    select(fund_code) |> 
    distinct() |> 
    pull(fund_code)
  
  fund_group <- g_tbl |> 
    slice(which(fund_code == tbl_fund_code)) |> 
    pull(index)
  
  bench <- g_tbl |> 
    slice(which(fund_code == tbl_fund_code)) |> 
    pull(index_name)
  
  bench_low_end <- i_tbl |> 
    filter(group == fund_group) |> 
    pull(min_mkt_cap)
  
  bench_high_end <- i_tbl |> 
    filter(group == fund_group) |> 
    pull(max_mkt_cap)
  
  bench_range <- tibble(
    name = bench,
    low_end = bench_low_end / 1e6,
    high_end = bench_high_end / 1e6,
    pct_in_bench = NA_real_, 
    test = NA_character_)
  
  tbl <- f_tbl |>
    select(fund_code, pct_port, mkt_cap) |>
    summarize(
      name = "Portfolio",
      low_end = min(mkt_cap, na.rm = TRUE) / 1e6,
      high_end = max(mkt_cap, na.rm = TRUE) / 1e6,
      pct_in_bench = sum(
        (mkt_cap >= bench_low_end & mkt_cap <= bench_high_end) * pct_port, na.rm = TRUE),
      test = if_else(pct_in_bench >= 80, "OK", "Alert")
    ) |> 
    rbind(bench_range) |> 
    column_to_rownames(var = "name")
  
  pct_in_bench <- tbl |>
    filter(!is.na(pct_in_bench)) |>
    pull(pct_in_bench)
  
  type <- tbl |> 
    filter(!is.na(test)) |> 
    pull(test)
  
  def <- "80% of the Fund's assets must be invested within the Market Cap Range 
  of the Relevant Benchmark."
  
  test_results <- lst(type, tbl, def, bench, pct_in_bench)
  
  return(test_results)
}


# Non Fundamental Limitations Test 1

nf_test_01 <- function(tbl){
  tbl <- tbl |> 
    select(parent, parent_pct_os) |> 
    distinct() |> 
    mutate(
      test = if_else(parent_pct_os <= 4.9, TRUE, FALSE)
    ) |> 
    arrange(desc(parent_pct_os)) |> 
    head(7)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "List of issues where the fund owns >4.9% of outstanding 
  shares, holdings ranked by percent shares held."
  
  return(test_results)
}

# Non Fundamental Limitation Test 2

nf_test_02 <- function(f_tbl, g_tbl){
  
  tbl_fund_code <- f_tbl |> 
    select(fund_code) |> 
    distinct() |> 
    pull(fund_code)
  
  fund_lim <- g_tbl |> 
    slice(which(fund_code == tbl_fund_code)) |> 
    pull(adr_gdr_pct_lim)
  
  f_tbl <- f_tbl |> 
    select(security, sec_type, pct_port) |> 
    filter(sec_type == "ADR" | sec_type == "GDR") |> 
    mutate(
      type_sum = sum(pct_port),
      test = if_else(type_sum <= fund_lim, TRUE, FALSE)
    ) |> 
    select(security, type_sum) |> 
    arrange(desc(type_sum)) |> 
    head(3)
  
  if(nrow(f_tbl) == 0) {
    f_tbl <- tibble(
      security = "No assets in ADRs or GDRs",
      type_sum = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(f_tbl)
  
  test_results$def <- "The % of total assets that the Fund may invest in ADRs 
  and GDRs is limited by its strategy. Midcap and large cap strategies have a 
  limitation of 15%. Small cap growth strategies are limited to 20%, and small
  cap value strategies are limited to 10%."
  
  test_results$fund_lim <- fund_lim
  
  
  return(test_results)
}

#Non Fundamental Limitation Test 3

nf_test_03 <- function(tbl){
  tbl <- tbl |> 
    select(security, sec_type, pct_port) |> 
    filter(sec_type == "ETP") |> 
    mutate(
      test = if_else(pct_port <= 5, TRUE, FALSE)
    ) |> 
    select(security, pct_port, test) |> 
    arrange(desc(pct_port))
  
  if(nrow(tbl) != 0){ 
    tbl <- tbl |> 
      adorn_totals("row") |> 
      mutate(tbl$Total[[test]] <- if_else(pct_port <= 10, TRUE, FALSE))
    
    tbl_head = head(tbl, 3)
    tbl_tail = tail(tbl, 1)
    
    tbl <- rbind(tbl_head, tbl_tail) 
  }
  
  if(nrow(tbl) == 0) {
    tbl <- tibble(
      security = "No ETFs Held",
      pct_port = NA,
      test = NA
    )
  }
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may invest 10% of its total assets in ETFs with 
  no single ETF greater than 5% of its total assets."
  
  return(test_results)
}

# Non Fundamental Limitations Test 4

nf_test_04 <- function(tbl){
  tbl <- tbl |> 
    select(parent, ticker, pct_port) |>
    group_by(parent) |>
    mutate(
      parent_pct_port = sum(pct_port),
      test = if_else(parent_pct_port <= 25, TRUE, FALSE)
    ) |>
    select(parent, parent_pct_port, test) |> 
    distinct() |> 
    arrange(desc(parent_pct_port)) |> 
    ungroup() |> 
    head(3)
  
  test_results <- compile_test_result(tbl)
  
  test_results$def <- "The Fund may invest no more than 25% of its total assets 
  in securities of any one issuer."
  
  return(test_results)
}

# Restricted Assets Test 1

ra_test_01 <- function(tbl){
  
  type <- "OK"
  
  tbl <- NULL
  
  def <- "The Fund may not purchase or sell real estate, purchase or 
  sell physical commodities, or make loans of money. WARNING: This test 
  currently does not rely on data. Please sign off on the test to verify that 
  it is OK."
  
  test_results <- lst(type, tbl, def)
  
  return(test_results)
  
}

# Restricted Assets Test 2

ra_test_02 <- function(f_tbl, h_tbl, r_tbl){
  
  def <- "The Fund may not hold any of these stocks."
  
  tbl_fund_code <- f_tbl |> 
    select(fund_code) |> 
    distinct() |> 
    pull(fund_code)
  
  r_tbl <- r_tbl |> 
    filter(fund_code == tbl_fund_code)
  
  if(nrow(r_tbl) == 0){
    type <- NA
    tbl <- NA
  } else {
    f_tbl <- f_tbl |> 
      select(security, ticker)
    
    tbl <- r_tbl |> 
      mutate(
        held = if_else(r_tbl$restricted_sec %in% f_tbl$ticker == TRUE, TRUE, FALSE),
        held_cam = if_else(r_tbl$restricted_sec %in% h_tbl$ticker == TRUE, TRUE, FALSE)
      ) |> 
      select(!fund_code)
    
    result <- tbl |> 
      filter(held == TRUE)
    
    type <- if_else(nrow(result) == 0, "OK", "Alert")
  }
  test_results <- lst(type, tbl, def)
  return(test_results)
}


# results -----------------------------------------------------------------

report_details <- function(lst, names) {
  
  title <- paste0("Test Result: ",lst$type)
  
  lst$tbl |> 
    set_names(names) |> 
    gt(
      rowname_col = names[[1]],
      
    ) |> 
    tab_header(
      title = title,
      if(length(lst) == 3){
        subtitle = lst$def
      }
      else if(length(lst) == 4){
        subtitle = md(paste0(lst$def, " This Fund may invest up to ", 
                             lst$fund_lim, "% of total assets in ADRs and GDRs."))
      }
      else if(length(lst) == 5){
        subtitle = paste0(lst$def, " ", lst$vio_count, " securities are in 
                          violation of the limit. Offending securities make up ",
                          round(lst$vio_pct, digits = 2), "% of the fund.")
      }
    ) |> 
    cols_label(
      test = "Test"
    ) |>
    tab_style(
      style = cell_fill(color = "#F9FF78"),
      locations = cells_body(
        columns = test,
        rows = test == FALSE
      )
    ) |> 
    fmt_number(
      decimals = if_else(names[2] == "Shares Out Held (%)", 4, 2)
    )
  
}

il_test_07_report_details <- function(lst){
  title <- paste0("Test Result: ", lst$type)
  
  lst$tbl |> 
    gt(
      rownames_to_stub = TRUE
    ) |> 
    cols_hide(
      columns = c(pct_in_bench, test)
    ) |>
    tab_header(
      title = title,
      subtitle = paste0(lst$def, " ", lst$bench, " is the relevant benchmark
                        for this fund. ", round(lst$pct_in_bench, digits = 2), 
                        "% of the fund's holdings are within benchmark range.")
    ) |> 
    fmt_number(
      decimal = 1
    ) |> 
    cols_label(
      low_end = "Low end (MM)",
      high_end = "High end (MM)"
    )
}

il_test_01_report_details <- function(lst){
  
  title <- paste0("Test Result: ",lst$type)
  
  lst$tbl |> 
    gt(rowname_col = "") |> 
    tab_header(
      title = title,
      subtitle = lst$def
    ) |>
    cols_label(
      asset_type = "",
      pct_asset_type = "Portfolio Wt (%)"
    ) |>
    tab_style(
      style = cell_fill(color = "#F9FF78"),
      locations = cells_body(
        columns = pct_asset_type,
        row = asset_type == "Total" & pct_asset_type < 75
      )
    ) |> 
    fmt_number(decimals = 2)
}

ra_test_01_report_details <- function(lst) {
  lst$def
}

ra_test_02_report_details <- function(lst){
  
  if(is.na(lst$type)){
    return(NA)
  } else {
    
    title <- paste0("Test Result: ",lst$type)
    
    lst$tbl |>
      gt() |>
      tab_header(
        title = title,
        subtitle = lst$def
      ) |>
      cols_label(
        issuer = "Issuer",
        restricted_sec = "Restricted Security",
        held = "Held by Fund",
        held_cam = "Held Elsewhere by CAM"
      ) |>
      tab_style(
        style = cell_fill(color = "#F9FF78"),
        locations = cells_body(
          columns = held,
          rows = held == TRUE
        )
      ) |> 
      tab_style(
        style = cell_fill(color = "#F9FF78"),
        locations = cells_body(
          columns = held_cam,
          rows = held_cam == TRUE
        )
      )
  }
}

# render results in quarto docs from results_summary


render_mf_report <- function(
    rds_file, n_file, dt, template
) {
  
  quarto::quarto_render(
    input = template,
    execute_params = list (
      output_rds = rds_file,
      names_file = n_file,
      dt = dt
    ),
    output_format = "html",
    output_file = glue("{dt}_fund_testing.html"))
  
}