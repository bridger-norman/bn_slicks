#'is_holiday
#'
#' Takes in a date and return true if it is a holiday specified by SGH
#'
#' @param date Any date (data type must be date)
#' @export
#' @returns a TRUE or FALSE if the date provided was a holiday

is_holiday <- function(dates) {
  # Extract month, day, and weekday information
  month_day <- format(dates, "%m-%d")
  month <- format(dates, "%m")
  day <- as.numeric(format(dates, "%d"))
  weekday <- weekdays(dates)
  day_of_week <- as.numeric(format(dates, "%u"))
  year <- format(dates, '%Y')
  end_may <- as.Date(paste0(year,'-05-31'))

  # Define holiday conditions
  new_years <- month_day == "01-01"
  mlk_day <- month == "01" & weekday == "Monday" & day > 14 & day < 22
  presidents_day <- month == "02" & weekday == "Monday" & day > 14 & day < 22
  memorial_day <- dates ==  end_may - lubridate::wday(end_may) + 2
  easter <- dates %in% c("2020-04-12", "2021-04-04", "2022-04-17", "2023-04-09", "2024-03-31",
                         "2025-04-20", "2026-04-05", "2027-03-28", "2028-04-16", "2029-04-01",
                         "2030-04-21", "2031-04-13", "2032-03-28", "2033-04-17", "2034-04-09",
                         "2035-03-25", "2036-04-13", "2037-04-05", "2038-04-25", "2039-04-10",
                         "2040-04-01", "2041-04-21", "2042-04-06", "2043-03-29", "2044-04-17",
                         "2045-04-09", "2046-03-25", "2047-04-14", "2048-04-05", "2049-04-18",
                         "2050-04-10")
  juneteenth <- month_day == "06-19"
  independence_day <- month_day == "07-04"
  labor_day <- month == "09" & weekday == "Monday" & day <= 7
  thanksgiving <- month == "11" & weekday == "Thursday" & day > 21 & day < 29
  christmas <- month_day == "12-25"

  # Combine all holiday conditions into a single vector
  result <- new_years | mlk_day | presidents_day | memorial_day | easter | juneteenth |
    independence_day | labor_day | thanksgiving | christmas

  return(result)
}
