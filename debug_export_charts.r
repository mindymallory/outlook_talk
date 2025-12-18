library(tidyverse)
library(readxl)
library(lubridate)

print("--- Debugging Corn Data ---")
yyear <- 2024 # Hardcode for debug

# 1. Read Raw
raw_df <- readxl::read_excel("ExportSalesDataByCommodity.xlsx", skip = 6, col_names = FALSE)
print("Raw Data Head:")
print(head(raw_df))

# 2. Process expData (Total Corn)
print("Processing expData (Total Corn)...")
expData <- tryCatch(
    {
        df <- raw_df %>%
            select(Commodity = 1, Date = 3, Country = 5, "Out. Sales CMY" = 8) %>%
            filter(!is.na(Commodity)) %>%
            filter(Commodity == "Corn")

        print("Filtered Corn Data (Head):")
        print(head(df))

        df_long <- df %>%
            pivot_longer(cols = "Out. Sales CMY", names_to = "Year", values_to = "Value")

        print("Pivoted Data (Head):")
        print(head(df_long))

        final_exp <- df_long %>%
            group_by(Year) %>%
            summarize(Value = sum(as.numeric(Value), na.rm = TRUE)) %>%
            mutate(Year1 = yyear) %>%
            mutate(Value = Value / 1000)

        print("Final expData:")
        print(final_exp)
        final_exp
    },
    error = function(e) {
        message(e)
        NULL
    }
)

# 3. Process CexpData (China Corn)
print("Processing CexpData (China Corn)...")
CexpData <- tryCatch(
    {
        df <- raw_df %>%
            select(Commodity = 1, Date = 3, Country = 5, "Out. Sales CMY" = 8) %>%
            filter(!is.na(Commodity)) %>%
            filter(Commodity == "Corn") %>%
            filter(Country == "CHINA, PEOPLES REPUBLIC OF")

        print("Filtered China Data (Head):")
        print(head(df))

        final_cexp <- df %>%
            pivot_longer(cols = "Out. Sales CMY", names_to = "Year", values_to = "Value") %>%
            # NOTE: Logic differs here, summing vs individual rows?
            # Original script grouped by Year? No, China data might be multiple rows?
            # Let's check if we need to summarize China data too.
            summarize(Value = sum(as.numeric(Value), na.rm = TRUE)) %>% # Added sum just in case
            mutate(Year1 = yyear) %>%
            mutate(Value = Value / 1000)

        print("Final CexpData:")
        print(final_cexp)
        final_cexp
    },
    error = function(e) {
        message(e)
        NULL
    }
)

# 4. Join
print("--- Joining ---")
if (!is.null(expData) && !is.null(CexpData)) {
    print(paste("expData Year1:", expData$Year1))
    print(paste("CexpData Year1:", CexpData$Year1))

    expd <- inner_join(expData, CexpData, by = "Year1")
    print("Joined Data:")
    print(expd)
}
