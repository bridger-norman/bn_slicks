

create_primary_schedule <- function(names_data,
                                    year,
                                    special_sundays = list()){

  dates <- calc_primary_sundays(year,
                                special_sundays)
}

#' returns a dataframe with all the sundays for a certain year
#' @param year the year of dates you want as a string
#' @param special_sundays other special dates that might need filtered like
#' Christmas as a list see examples below
#' @examples
#'
#' calc_primary_sundays('2025')
#'
#' # filtering out special dates like conference
#' calc_primary_sundays('2025', list('2025-01-05', '2025-01-12'))
#'
calc_primary_sundays <- function(year,
                                 special_sundays = list()){

  #establish date range
  start_date <- as.Date(paste0(year, "-01-01"))
  end_date <- as.Date(paste0(year, "-12-31"))

  # Generate sequence of all dates in the year
  all_dates <- seq.Date(start_date, end_date, by = "day")

  # Filter to keep only Sundays
  sundays <- all_dates[lubridate::wday(all_dates) == 1]

  # Create a tibble
  sundays <- tibble::tibble(sundays = sundays)

  #remove general conference
  primary_sundays <- sundays |>
    dplyr::mutate(month = lubridate::month(sundays)) |>
    dplyr::group_by(month) |>
    dplyr::mutate(rank = rank(sundays)) |>
    dplyr::filter(!((month == 4 & rank == 1) | (month == 10 & rank == 1))) |>
    dplyr::filter(rank != 3)

  #filter based off of user input
  if (!missing(special_sundays)){
    primary_sundays <- primary_sundays |> dplyr::filter(!(sundays %in% special_sundays))
  }

  return(primary_sundays)
}


#' create the schedule as a data frame
#'
#' @param names_data a list of kids names and their active status
#' @param dates dates from `calc_primary_sundays`
#'
#' @examples
#' primary <- read.csv('/Users/bridgernorman/Downloads/primary.csv')
#' assign_days(primary, dates)
#'
assign_days <- function(names_data,
                        dates){

  active <- names_data |>
    dplyr::filter(active == 'Y') |>
    dplyr::select(name)

  names <- active$name
  dates <- dates$sundays



  n <- length(dates)

  set.seed(123) # Set seed for reproducibility
  # Shuffle names for talk, prayer, and scripture
  talk <- sample(names, n)
  prayer <- sample(names, n)
  scripture <- sample(names, n)

  mapply(function(talk, prayer, scripture){
    if (talk == prayer | talk == scripture | prayer == scripture){
      prayer <- sample(setdiff(names, prayer), 1)
      scripture <- sample(setdiff(names, scripture), 1)
    }else{
      prayer <- prayer
      scripture <- scripture
    }

  })

  # # Ensure no duplicates across talk, prayer, and scripture on the same date
  # while (any(talk == prayer | talk == scripture | prayer == scripture)) {
  #   prayer <- sample(setdiff(names, prayer), 1) #, replace = TRUE
  #   scripture <- sample(setdiff(names, scripture), 1) #, replace = TRUE
  # }

  # Create the schedule
  schedule <- data.frame(
    date = dates,
    talk = talk,
    prayer = prayer,
    scripture = scripture
  )

  return(schedule)

  #finding extras
  # extratalk <- setdiff(names, temp$talk)
  # extraprayer <- setdiff(names, temp$prayer)
  # extrascript <- setdiff(names, temp$scripture)
  # data.frame(talk = extratalk, prayer = extraprayer, script = extrascript)

}

