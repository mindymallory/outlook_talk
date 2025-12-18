library(tidyverse)
library(readxl)
library(lubridate)

print("--- Debugging Corn Data V2 ---")
yyear <- 2024

# 1. Read Raw
raw_df <- readxl::read_excel("ExportSalesDataByCommodity.xlsx", skip = 6, col_names = FALSE)

# 2. Inspect Date
dates <- raw_df[[3]] # Column 3
print("Sample Dates:")
print(head(dates))

# Determine if Date is numeric (Excel serial) or character
numeric_date <- as.numeric(dates[1])
if (!is.na(numeric_date) && numeric_date > 40000) {
    print("Dates detected as Excel Serial Numbers.")
    year_china <- year(as.Date(as.numeric(dates), origin = "1899-12-30"))
} else {
    print("Dates detected as Strings.")
    year_china <- year(as.Date(dates))
}
print("Sample Parsed Years for China Data:")
print(head(year_china))


# 3. Process expData (Total Corn)
print("Processing expData...")
expData <- tryCatch(
    {
        df <- raw_df %>%
            select(Commodity = 1, "Out. Sales CMY" = 8) %>%
            filter(!is.na(Commodity), Commodity == "Corn") %>%
            mutate(Value = as.numeric(gsub(",", "", `Out. Sales CMY`)))

        final_exp <- df %>%
            summarize(Value = sum(Value, na.rm = TRUE)) %>%
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

# 4. Process CexpData (China Corn)
print("Processing CexpData...")
CexpData <- tryCatch(
    {
        df <- raw_df %>%
            select(Commodity = 1, Date = 3, Country = 5, "Out. Sales CMY" = 8) %>%
            filter(!is.na(Commodity), Commodity == "Corn") %>%
            filter(Country == "CHINA, PEOPLES REPUBLIC OF") %>%
            mutate(Value = as.numeric(gsub(",", "", `Out. Sales CMY`)))

        print("Filtered China Data Rows:")
        print(head(df))

        # Replicate Patched Logic
        final_cexp <- df %>%
            mutate(
                Year1 = case_when(
                    TRUE ~ year(as.Date(as.numeric(Date), origin = "1899-12-30")) # Assuming Serial based on check above
                )
            ) %>%
            summarize(Value = sum(Value, na.rm = TRUE), Year1 = max(Year1, na.rm = TRUE)) %>% # Hack to get single row
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

# 5. Join Check
print("--- Joining ---")
if (!is.null(expData) && !is.null(CexpData)) {
    print(paste("expData Year1:", expData$Year1))
    print(paste("CexpData Year1:", CexpData$Year1))

    if (expData$Year1 == CexpData$Year1) {
        print("Join SUCCESS: Years Match")
    } else {
        print("Join FAILURE: Years Do Not Match")
    }
}
