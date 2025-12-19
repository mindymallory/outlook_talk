library(tidyverse)
library(kableExtra)
library(lubridate)
options(digits = 8)

# Set colorscheme
purduegold <- "#CEB888"
colorscheme <- purduegold

print("Starting Outlook Script...")
print("Mode: Manual Data Loading Only")

# -------------------------------------------------------------------------
# Get Historical WASDE's
# -------------------------------------------------------------------------

# Previous Years
print("Loading Historical WASDE Data (2010-2020)...")

# 2010-2015
if (file.exists("oce-wasde-report-data-2010-04-to-2015-12.csv")) {
    print("Found local 2010-2015 data.")
    wasde_old <- read.csv("oce-wasde-report-data-2010-04-to-2015-12.csv")
} else {
    print("Warning: 'oce-wasde-report-data-2010-04-to-2015-12.csv' not found.")
}

# 2016-2020
if (file.exists("oce-wasde-report-data-2016-01-to-2020-12.csv")) {
    print("Found local 2016-2020 data.")
    wasde_recent <- read.csv("oce-wasde-report-data-2016-01-to-2020-12.csv")
} else {
    print("Warning: 'oce-wasde-report-data-2016-01-to-2020-12.csv' not found.")
}

# Combine old data
wasde <- data.frame()
if (exists("wasde_old")) wasde <- rbind(wasde, wasde_old)
if (exists("wasde_recent")) wasde <- rbind(wasde, wasde_recent)

if (nrow(wasde) == 0) {
    print("Warning: No historical WASDE data loaded (2010-2020). Charts may be incomplete.")
}


# -------------------------------------------------------------------------
# Current Year Loop (Manual Local Files 2021-2025+)
# -------------------------------------------------------------------------
print("Processing Monthly WASDE Data (2021-Present)...")
print("Expected format: oce-wasde-report-data-YYYY-MM.csv")

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2025-12-01")
current_date <- start_date

files_loaded <- 0

while (current_date <= end_date) {
    dataset_str <- format(current_date, "%Y-%m")
    local_filename <- paste0("oce-wasde-report-data-", dataset_str, ".csv")

    if (file.exists(local_filename)) {
        info <- file.info(local_filename)
        if (!is.na(info$size) && info$size > 1000) {
            tryCatch(
                {
                    subset_data <- read.csv(local_filename)
                    wasde <- rbind(wasde, subset_data)
                    files_loaded <- files_loaded + 1
                },
                error = function(e) {
                    print(paste("Error reading local file", local_filename, ":", e$message))
                }
            )
        } else {
            print(paste("Skipping", local_filename, ": File too small or empty."))
        }
    }
    current_date <- current_date %m+% months(1)
}

print(paste("Total Monthly Files Loaded (2021+):", files_loaded))
print(paste("Total WASDE records in memory:", nrow(wasde)))

if (nrow(wasde) > 0) {
    print(paste("Latest WASDE Number:", tail(wasde$WasdeNumber, 1)))
} else {
    stop("Critical Error: No data loaded at all. Ensure CSV files are in the directory.")
}

# -------------------------------------------------------------------------
# Charts
# -------------------------------------------------------------------------
print("Generating Charts...")

latest_wasde_num <- tail(wasde$WasdeNumber, 1)

# 1. Corn Exports
print("Generating corn_exports.png")
corn_export_val <- wasde %>%
    filter(ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
    filter(Commodity == "Corn") %>%
    filter(WasdeNumber == latest_wasde_num) %>%
    filter(Attribute == "Exports") %>%
    filter(AnnualQuarterFlag == "Annual") %>%
    tail(1)

if (nrow(corn_export_val) > 0) {
    new_corn_exports <- data_frame(
        MarketYear = corn_export_val$MarketYear,
        Value = corn_export_val$Value
    )
    rep18_corn <- data_frame(MarketYear = c("2018/19"), Value = c(2066))


    p1 <- wasde %>%
        filter(ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
        filter(Commodity == "Corn") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Attribute == "Exports") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        ggplot(aes(x = MarketYear, y = Value)) +
        geom_col(width = .4) +
        geom_text(aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_corn, aes(x = MarketYear, y = Value), width = .4) +
        geom_text(data = rep18_corn, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_corn_exports, aes(x = MarketYear, y = Value), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_corn_exports, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "Millions of Bushels", title = "US Corn Exports")

    ggsave("corn_exports.png", plot = p1, width = 8, height = 6)
} else {
    print("Skipped corn_exports.png (No data found for latest WASDE)")
}


# 2. Corn Ethanol
print("Generating corn_ethanol.png")
ethanol_val <- wasde %>%
    filter(ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
    filter(Commodity == "Corn") %>%
    filter(WasdeNumber == latest_wasde_num) %>%
    filter(Attribute == "Ethanol for Fuel" | Attribute == "Ethanol & by-products") %>%
    filter(AnnualQuarterFlag == "Annual") %>%
    tail(1)

if (nrow(ethanol_val) > 0) {
    new_ethanol <- data_frame(
        MarketYear = ethanol_val$MarketYear,
        Value = ethanol_val$Value
    )
    rep18_eth <- data_frame(MarketYear = c("2018/19"), Value = c(5378))

    p2 <- wasde %>%
        filter(ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
        filter(Commodity == "Corn") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Attribute == "Ethanol for Fuel" | Attribute == "Ethanol & by-products") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        ggplot(aes(x = MarketYear, y = Value)) +
        geom_col(width = .4) +
        geom_text(aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_eth, aes(x = MarketYear, y = Value), width = .4) +
        geom_text(data = rep18_eth, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_ethanol, aes(x = MarketYear, y = Value), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_ethanol, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "Millions of Bushels", title = "Corn Used for Ethanol")

    ggsave("corn_ethanol.png", plot = p2, width = 8, height = 6)
}

# 3. Corn Stocks/Use
print("Generating corn_stocksuse.png")
stocks_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Corn", Attribute == "Ending Stocks", ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
    tail(1)
use_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Corn", Attribute == "Use, Total", ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
    tail(1)

if (nrow(stocks_row) > 0 && nrow(use_row) > 0) {
    stocks_val <- stocks_row$Value
    use_val <- use_row$Value
    stocks_use_calc <- 100 * stocks_val / use_val
    latest_market_year <- stocks_row$MarketYear

    new_stocksuse <- data_frame(
        MarketYear = latest_market_year,
        Stocks_Use = stocks_use_calc
    )
    rep18_su <- data_frame(MarketYear = c("2018/19"), Stocks_Use = c(15.5))

    p3 <- wasde %>%
        filter(ReportTitle == "U.S. Feed Grain and Corn Supply and Use") %>%
        filter(Commodity == "Corn") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Attribute == "Ending Stocks" | Attribute == "Use, Total") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        pivot_wider(names_from = "Attribute", values_from = "Value") %>%
        mutate(Stocks_Use = 100 * `Ending Stocks` / `Use, Total`) %>%
        ggplot(aes(x = MarketYear, y = Stocks_Use)) +
        geom_col(width = .4) +
        geom_text(aes(label = format(round(Stocks_Use, 2), n_small = 3), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_su, aes(x = MarketYear, y = Stocks_Use), width = .4) +
        geom_text(data = rep18_su, aes(label = format(round(Stocks_Use, 2), n_small = 2), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_stocksuse, aes(x = MarketYear, y = Stocks_Use), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_stocksuse, aes(label = format(round(Stocks_Use, 5), n_small = 5), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "% of Usage", title = "U.S. Corn Ending Stock as % of Usage")

    ggsave("corn_stocksuse.png", plot = p3, width = 8, height = 6)
}

# 4. World Corn Stocks/Use
print("Generating corn_worldstocksuse.png")
w_stocks_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Corn", Region == "World", Attribute == "Ending Stocks", ReportTitle == "World Corn Supply and Use") %>%
    tail(1)
w_dom_total_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Corn", Region == "World", Attribute == "Domestic Total", ReportTitle == "World Corn Supply and Use") %>%
    tail(1)

if (nrow(w_stocks_row) > 0 && nrow(w_dom_total_row) > 0) {
    w_stocks_val <- w_stocks_row$Value
    w_dom_total_val <- w_dom_total_row$Value
    w_stocks_use_calc <- 100 * w_stocks_val / w_dom_total_val
    latest_market_year <- w_stocks_row$MarketYear

    new_w_stocksuse <- data_frame(
        MarketYear = latest_market_year,
        Stocks_Use = w_stocks_use_calc
    )
    rep18_wsu <- data_frame(MarketYear = c("2018/19"), Stocks_Use = c(28.8))

    p4 <- wasde %>%
        filter(ReportTitle == "World Corn Supply and Use") %>%
        filter(Commodity == "Corn") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Region == "World") %>%
        filter(Attribute == "Ending Stocks" | Attribute == "Domestic Total") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        pivot_wider(names_from = "Attribute", values_from = "Value") %>%
        mutate(Stocks_Use = 100 * `Ending Stocks` / `Domestic Total`) %>%
        ggplot(aes(x = MarketYear, y = Stocks_Use)) +
        geom_col(width = .4) +
        geom_text(aes(label = format(round(Stocks_Use, 2), n_small = 3), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_wsu, aes(x = MarketYear, y = Stocks_Use), width = .4) +
        geom_text(data = rep18_wsu, aes(label = format(round(Stocks_Use, 2), n_small = 2), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_w_stocksuse, aes(x = MarketYear, y = Stocks_Use), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_w_stocksuse, aes(label = format(round(Stocks_Use, 5), n_small = 5), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "% of Usage", title = "World Corn Ending Stock as % of Usage")

    ggsave("corn_worldstocksuse.png", plot = p4, width = 8, height = 6)
}


# -------------------------------------------------------------------------
# Soybeans Charts
# -------------------------------------------------------------------------

# 5. Soy Exports
print("Generating soy_exports.png")
soy_export_val <- wasde %>%
    filter(ReportTitle == "U.S. Soybeans and Products Supply and Use (Domestic Measure)") %>%
    filter(Commodity == "Oilseed, Soybean") %>%
    filter(WasdeNumber == latest_wasde_num) %>%
    filter(Attribute == "Exports") %>%
    tail(1)

if (nrow(soy_export_val) > 0) {
    new_soy_exports <- data_frame(
        MarketYear = soy_export_val$MarketYear,
        Value = soy_export_val$Value
    )
    rep18_soy <- data_frame(MarketYear = c("2018/19"), Value = c(1752))

    p5 <- wasde %>%
        filter(ReportTitle == "U.S. Soybeans and Products Supply and Use (Domestic Measure)") %>%
        filter(Commodity == "Oilseed, Soybean") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Attribute == "Exports") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        ggplot(aes(x = MarketYear, y = Value)) +
        geom_col(width = .4) +
        geom_text(aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_soy, aes(x = MarketYear, y = Value), width = .4) +
        geom_text(data = rep18_soy, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_soy_exports, aes(x = MarketYear, y = Value), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_soy_exports, aes(label = Value, Value = Value), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "Millions of Bushels", title = "US Soybean Exports")

    ggsave("soy_exports.png", plot = p5, width = 8, height = 6)
}

# 6. Soy Stocks/Use
print("Generating soystocksuse.png")
s_stocks_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Oilseed, Soybean", Attribute == "Ending Stocks", ReportTitle == "U.S. Soybeans and Products Supply and Use (Domestic Measure)") %>%
    tail(1)
s_use_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Oilseed, Soybean", Attribute == "Use, Total", ReportTitle == "U.S. Soybeans and Products Supply and Use (Domestic Measure)") %>%
    tail(1)

if (nrow(s_stocks_row) > 0 && nrow(s_use_row) > 0) {
    s_stocks_val <- s_stocks_row$Value
    s_use_val <- s_use_row$Value
    s_stocks_use_calc <- 100 * s_stocks_val / s_use_val
    latest_market_year <- s_stocks_row$MarketYear

    new_s_stocksuse <- data_frame(
        MarketYear = latest_market_year,
        Stocks_Use = s_stocks_use_calc
    )
    rep18_ssu <- data_frame(MarketYear = c("2018/19"), Stocks_Use = c(22.90))

    p6 <- wasde %>%
        filter(ReportTitle == "U.S. Soybeans and Products Supply and Use (Domestic Measure)") %>%
        filter(Commodity == "Oilseed, Soybean") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Attribute == "Ending Stocks" | Attribute == "Use, Total") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        pivot_wider(names_from = "Attribute", values_from = "Value") %>%
        mutate(Stocks_Use = 100 * `Ending Stocks` / `Use, Total`) %>%
        ggplot(aes(x = MarketYear, y = Stocks_Use)) +
        geom_col(width = .4) +
        geom_text(aes(label = format(round(Stocks_Use, 2), n_small = 3), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_ssu, aes(x = MarketYear, y = Stocks_Use), width = .4) +
        geom_text(data = rep18_ssu, aes(label = format(round(Stocks_Use, 2), n_small = 2), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_s_stocksuse, aes(x = MarketYear, y = Stocks_Use), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_s_stocksuse, aes(label = format(round(Stocks_Use, 5), n_small = 5), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "% of Usage", title = "U.S. Soybean Ending Stock as % of Usage")

    ggsave("soystocksuse.png", plot = p6, width = 8, height = 6)
}

# 7. World Soy Stocks/Use
print("Generating soyworldstocksuse.png")
ws_stocks_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Oilseed, Soybean", Region == "World", Attribute == "Ending Stocks", ReportTitle == "World Soybean Supply and Use") %>%
    tail(1)
ws_dom_total_row <- wasde %>%
    filter(WasdeNumber == latest_wasde_num, Commodity == "Oilseed, Soybean", Region == "World", Attribute == "Domestic Total", ReportTitle == "World Soybean Supply and Use") %>%
    tail(1)

if (nrow(ws_stocks_row) > 0 && nrow(ws_dom_total_row) > 0) {
    ws_stocks_val <- ws_stocks_row$Value
    ws_dom_total_val <- ws_dom_total_row$Value
    ws_stocks_use_calc <- 100 * ws_stocks_val / ws_dom_total_val
    latest_market_year <- ws_stocks_row$MarketYear

    new_ws_stocksuse <- data_frame(
        MarketYear = latest_market_year,
        Stocks_Use = ws_stocks_use_calc
    )
    rep18_wssu <- data_frame(MarketYear = c("2018/19"), Stocks_Use = c(33.01))

    p7 <- wasde %>%
        filter(ReportTitle == "World Soybean Supply and Use") %>%
        filter(Commodity == "Oilseed, Soybean") %>%
        mutate(ReportDate_Month = str_sub(ReportDate, start = 1, end = -6)) %>%
        filter(ReportDate_Month == "January") %>%
        filter(Region == "World") %>%
        filter(Attribute == "Ending Stocks" | Attribute == "Domestic Total") %>%
        filter(AnnualQuarterFlag == "Annual") %>%
        filter(ProjEstFlag == "Proj.") %>%
        pivot_wider(names_from = "Attribute", values_from = "Value") %>%
        mutate(Stocks_Use = 100 * `Ending Stocks` / `Domestic Total`) %>%
        ggplot(aes(x = MarketYear, y = Stocks_Use)) +
        geom_col(width = .4) +
        geom_text(aes(label = format(round(Stocks_Use, 2), n_small = 3), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = rep18_wssu, aes(x = MarketYear, y = Stocks_Use), width = .4) +
        geom_text(data = rep18_wssu, aes(label = format(round(Stocks_Use, 2), n_small = 2), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        geom_col(data = new_ws_stocksuse, aes(x = MarketYear, y = Stocks_Use), color = colorscheme, width = .4, fill = colorscheme) +
        geom_text(data = new_ws_stocksuse, aes(label = format(round(Stocks_Use, 5), n_small = 5), Stocks_Use = Stocks_Use), position = position_dodge(.9), vjust = -1) +
        theme_bw() +
        labs(x = "Marketing Year", y = "% of Usage", title = "World Soybean Ending Stock as % of Usage")

    ggsave("soyworldstocksuse.png", plot = p7, width = 8, height = 6)
}


# -------------------------------------------------------------------------
# Intra Year Exports (Historical 2010-Present)
# -------------------------------------------------------------------------
print("Generating intra-year export charts...")

if (file.exists("ExportSalesDataByCommodity.xlsx")) {
    # Function to process historical data
    process_export_history <- function(commodity_name) {
        tryCatch(
            {
                sheets <- readxl::excel_sheets("ExportSalesDataByCommodity.xlsx")
                all_data <- data.frame()

                for (sh in sheets) {
                    raw_df <- readxl::read_excel("ExportSalesDataByCommodity.xlsx", sheet = sh, skip = 6, col_names = FALSE, col_types = "text")

                    if (nrow(raw_df) > 0) {
                        # Dynamically find which column contains the commodity name
                        # Search all rows just to be sure
                        col_sums <- sapply(raw_df, function(col) any(trimws(col) == commodity_name, na.rm = TRUE))
                        comm_col <- which(col_sums)[1]

                        if (!is.na(comm_col)) {
                            # Indices are relative to comm_col:
                            # If comm_col is 2 (Sheet 1): Date=3, Country=5, Commitments=11
                            # If comm_col is 1 (Sheet 2): Date=2, Country=4, Commitments=10
                            date_col <- comm_col + 1
                            country_col <- comm_col + 3
                            comm_val_col <- comm_col + 9

                            if (ncol(raw_df) >= comm_val_col) {
                                df_sh <- raw_df[, c(comm_col, date_col, country_col, comm_val_col)]
                                colnames(df_sh) <- c("Commodity", "Date", "Country", "Commitments")

                                df_sh <- df_sh %>%
                                    filter(!is.na(Commodity)) %>%
                                    mutate(Commodity = trimws(Commodity), Country = trimws(Country)) %>%
                                    filter(Commodity == commodity_name)

                                all_data <- rbind(all_data, df_sh)
                            }
                        }
                    }
                }

                if (nrow(all_data) == 0) {
                    print(paste("No matching rows for", commodity_name, "found in any sheet."))
                    return(NULL)
                }

                df_clean <- all_data %>%
                    mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
                    filter(!is.na(Date)) %>%
                    mutate(MY = if_else(month(Date) >= 9, year(Date), year(Date) - 1)) %>%
                    filter(MY >= 2010) %>%
                    mutate(Commitments = as.numeric(gsub(",", "", Commitments)))

                # Diagnostic: Print row counts and range
                print(paste("Data loaded for", commodity_name, "- Range:", min(df_clean$MY), "to", max(df_clean$MY)))
                print(table(df_clean$MY))

                # Aggregate: Get the MAX commitment value for each Country per Marketing Year
                df_agg <- df_clean %>%
                    group_by(MY, Country) %>%
                    summarize(Value = max(Commitments, na.rm = TRUE), .groups = "drop")

                # Split into Region: China vs Rest of World
                df_regional <- df_agg %>%
                    mutate(Region = if_else(Country == "CHINA, PEOPLES REPUBLIC OF", "China", "Rest of World")) %>%
                    group_by(MY, Region) %>%
                    summarize(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
                    mutate(Value = Value / 1000) # Convert to 1,000 MT

                return(df_regional)
            },
            error = function(e) {
                message(paste("Error processing", commodity_name, ":", e))
                NULL
            }
        )
    }

    # 8. Corn Export Charts (Historical)
    print("Processing Corn Export History...")
    corn_data <- process_export_history("Corn")

    if (!is.null(corn_data)) {
        p8 <- corn_data %>%
            ggplot(aes(x = as.factor(MY), y = Value, fill = Region)) +
            geom_col(position = "stack", width = 0.6) +
            theme_bw() +
            theme(legend.position = "bottom") +
            scale_fill_manual(values = c(colorscheme, "black")) +
            labs(
                x = "Marketing Year", y = "1,000 MT",
                title = "U.S. Corn Export Commitments (2010-Present)",
                subtitle = "China vs. Rest of World (stacked)"
            )

        ggsave("corn_exportscurrentyear.png", plot = p8, width = 10, height = 6)
        print("Generated corn_exportscurrentyear.png (Historical)")
    } else {
        print("Skipped corn_exportscurrentyear.png (No data)")
    }

    # 9. Soy Export Charts (Historical)
    print("Processing Soy Export History...")
    soy_data <- process_export_history("Soybeans")

    if (!is.null(soy_data)) {
        p9 <- soy_data %>%
            ggplot(aes(x = as.factor(MY), y = Value, fill = Region)) +
            geom_col(position = "stack", width = 0.6) +
            theme_bw() +
            theme(legend.position = "bottom") +
            scale_fill_manual(values = c(colorscheme, "black")) +
            labs(
                x = "Marketing Year", y = "1,000 MT",
                title = "U.S. Soybean Export Commitments (2010-Present)",
                subtitle = "China vs. Rest of World (stacked)"
            )

        ggsave("soy_exportscurrentyear.png", plot = p9, width = 10, height = 6)
        print("Generated soy_exportscurrentyear.png (Historical)")
    } else {
        print("Skipped soy_exportscurrentyear.png (No data)")
    }
} else {
    print("Warning: ExportSalesDataByCommodity.xlsx not found.")
}


# -------------------------------------------------------------------------
# Ethanol Margins
# -------------------------------------------------------------------------
print("Generating corn_ethanolmargins.png")

if (file.exists("hist_eth_gm.csv")) {
    tryCatch(
        {
            p10 <- read.csv("hist_eth_gm.csv", skip = 2) %>%
                mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
                select(c(1, 5)) %>%
                rename(`Return Over Opeating Cost per gallon` = "Return.Over.Operating.Costs....gallon.") %>%
                filter(Date > as.Date("2019-01-01")) %>%
                ggplot(aes(x = Date, y = `Return Over Opeating Cost per gallon`)) +
                geom_line() +
                theme_bw() +
                labs(x = "", y = "Return Over Opeating Cost $/gal", title = "Estimated Daily Ethanol Plant Margins")

            ggsave("corn_ethanolmargins.png", plot = p10, width = 8, height = 6)
            print("Generated corn_ethanolmargins.png")
        },
        error = function(e) {
            print(paste("Error processing hist_eth_gm.csv:", e$message))
        }
    )
} else {
    print("Warning: hist_eth_gm.csv not found.")
}

print("Script Complete.")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

url_milho <- "https://www.gov.br/conab/pt-br/atuacao/informacoes-agropecuarias/safras/series-historicas/graos/milho/milhototalseriehist.xls/@@download/file"
# https://www.gov.br/conab/pt-br/atuacao/informacoes-agropecuarias/safras/series-historicas/graos/soja
tmp <- tempfile(fileext = ".xls")
download.file(url_milho, tmp, mode = "wb")

milho_prod <- read_excel(tmp, sheet = "Produção", col_names = FALSE)

# header row (contains the safra labels like 1976/77 ... 2025/26 Previsão)
hdr <- as.character(unlist(milho_prod[5, ])) # row 5 in Excel = 1-indexed
milho_prod2 <- milho_prod
names(milho_prod2) <- hdr

milho_brasil <- milho_prod2 %>%
    filter(`REGIÃO/UF` == "BRASIL") %>%
    pivot_longer(-`REGIÃO/UF`, names_to = "Safra", values_to = "Production_kt") %>%
    mutate(
        YearStart = as.integer(str_extract(Safra, "^\\d{4}")),
        Production_mmt = as.numeric(Production_kt) / 1000
    ) %>%
    filter(!is.na(YearStart)) %>%
    select(YearStart, Safra, Production_mmt) %>%
    ggplot(aes(x = YearStart + 1, y = Production_mmt)) +
    geom_line(color = purduegold, size = .75) +
    theme_bw()


read.csv("export_data_all_years_concat.csv")


library(tidyverse)
library(lubridate)

purduegold <- "#CEB888"

weekly_my <- read.csv("export_data_all_years_concat.csv") %>%
    select(Grain, Thursday, Pounds, MKT.YR) %>%
    filter(Grain == "CORN") %>%
    mutate(
        Thursday = ymd(Thursday),
        MY_start_year = 2000 + as.integer(substr(as.character(MKT.YR), 1, 2)),
        MY_start_date = ymd(paste0(MY_start_year, "-09-01")),
        Week = as.integer(floor(as.numeric(Thursday - MY_start_date) / 7) + 1),
        MY_label = paste0(MY_start_year, "/", substr(MY_start_year + 1, 3, 4))
    ) %>%
    filter(Week >= 1, Week <= 53) %>%
    group_by(MY_label, Week) %>%
    summarise(
        Total_mil_bu = sum(Pounds, na.rm = TRUE) / 56 / 1e6,
        .groups = "drop"
    )

cur_label <- "2025/26"

avg_line <- weekly_my %>%
    filter(MY_label != cur_label) %>%
    group_by(Week) %>%
    summarise(Total_mil_bu = mean(Total_mil_bu, na.rm = TRUE), .groups = "drop") %>%
    mutate(Series = "Avg (ex-2025/26)")

cur_line <- weekly_my %>%
    filter(MY_label == cur_label) %>%
    transmute(Week, Total_mil_bu, Series = "2025/26")

plot_df <- bind_rows(avg_line, cur_line)

ggplot(plot_df, aes(x = Week, y = Total_mil_bu, color = Series)) +
    geom_line(linewidth = 0.95) +
    scale_color_manual(values = c(
        "Avg (ex-2025/26)" = "black",
        "2025/26" = purduegold
    )) +
    theme_bw() +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    labs(
        x = "Week of marketing year (Sep 1 = Week 1)",
        y = "Weekly total (million bushels)",
        title = "Corn weekly export pace: 2025/26 vs average of prior years"
    )
ggsave("corn_weekly_exports_2025_26.png", width = 8, height = 6)


library(tidyverse)
library(lubridate)

purduegold <- "#CEB888"

weekly_my <- read.csv("export_data_all_years_concat.csv") %>%
    select(Grain, Thursday, Pounds, MKT.YR) %>%
    filter(Grain == "SOYBEANS") %>%
    mutate(
        Thursday = ymd(Thursday),
        MY_start_year = 2000 + as.integer(substr(as.character(MKT.YR), 1, 2)),
        MY_start_date = ymd(paste0(MY_start_year, "-09-01")),
        Week = as.integer(floor(as.numeric(Thursday - MY_start_date) / 7) + 1),
        MY_label = paste0(MY_start_year, "/", substr(MY_start_year + 1, 3, 4))
    ) %>%
    filter(Week >= 1, Week <= 53) %>%
    group_by(MY_label, Week) %>%
    summarise(
        Total_mil_bu = sum(Pounds, na.rm = TRUE) / 56 / 1e6,
        .groups = "drop"
    )

cur_label <- "2025/26"

avg_line <- weekly_my %>%
    filter(MY_label != cur_label) %>%
    group_by(Week) %>%
    summarise(Total_mil_bu = mean(Total_mil_bu, na.rm = TRUE), .groups = "drop") %>%
    mutate(Series = "Avg (ex-2025/26)")

cur_line <- weekly_my %>%
    filter(MY_label == cur_label) %>%
    transmute(Week, Total_mil_bu, Series = "2025/26")

plot_df <- bind_rows(avg_line, cur_line)

ggplot(plot_df, aes(x = Week, y = Total_mil_bu, color = Series)) +
    geom_line(linewidth = 0.95) +
    scale_color_manual(values = c(
        "Avg (ex-2025/26)" = "black",
        "2025/26" = purduegold
    )) +
    theme_bw() +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    labs(
        x = "Week of marketing year (Sep 1 = Week 1)",
        y = "Weekly total (million bushels)",
        title = "Soybeans weekly export pace: 2025/26 vs average of prior years"
    )

ggsave("soybeans_weekly_exports_2025_26.png", width = 8, height = 6)
