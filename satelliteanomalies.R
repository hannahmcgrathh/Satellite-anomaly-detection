#### initial set up etc ####
pacman::p_load(tidyverse, skimr, xgboost, yardstick, forecast, dplyr, lubridate, plotly)

#read in data
satellites <- c("CryoSat-2", "Fengyun-2D", "Fengyun-2E", "Fengyun-2F", "Fengyun-2H", "Fengyun-4A", "Haiyang-2A", "Sentinel-3A", "Jason-1", "Jason-2", "Jason-3", "SARAL", "Sentinel-3B", "Sentinel-6A", "TOPEX")

read_satellite_data <- function(satellite) {
  propagated_file <- paste0("propagated_elements_", satellite, ".csv") #unpropagated and propagated data stored in working directory
  unpropagated_file <- paste0("unpropagated_elements_", satellite, ".csv")
  
  list(
    propagated = read_csv(propagated_file),
    unpropagated = read_csv(unpropagated_file)
  )
}

satellite_data <- lapply(satellites, read_satellite_data)

names(satellite_data) <- satellites



#fix `...1` (name of the date column)
rename_date_column <- function(df) {
  if ("...1" %in% colnames(df)) {
    df <- df %>%
      rename(Date = `...1`)
  }
  return(df)
}

satellite_data <- lapply(satellite_data, function(satellite_df) {
  
  if ("unpropagated" %in% names(satellite_df)) {
    satellite_df$unpropagated <- rename_date_column(satellite_df$unpropagated)
  }
  return(satellite_df)
})


#### adding maneuvers ####


#read maneuver data from a text file
read_maneuver_data <- function(file_path) {
  read.table(file_path, fill = TRUE, stringsAsFactors = FALSE)
}

#  dates to datetime
convert_maneuver_dates <- function(maneuver_data) {
  maneuver_data <- maneuver_data %>%
    mutate(
      start_date = as.Date(V3 - 1, origin = paste0(V2, "-01-01")),
      end_date = as.Date(V7 - 1, origin = paste0(V6, "-01-01")),
      start_datetime = as.POSIXct(paste(start_date, sprintf("%02d:%02d:00", V4, V5)), format = "%Y-%m-%d %H:%M:%S"),
      end_datetime = as.POSIXct(paste(end_date, sprintf("%02d:%02d:00", V8, V9)), format = "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(-start_date, -end_date)
  return(maneuver_data)
  
}

convert_maneuver_dates(read_maneuver_data("uni/satellite/satellite_data/manoeuvres/cs2man.txt"))

# Mark / identify maneuvers in data
mark_maneuvers <- function(unpropagated_data, maneuver_data) {
  #`Date` column is in POSIXct format
  unpropagated_data$Date <- as.POSIXct(unpropagated_data$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  
  unpropagated_data$maneuver <- "NO"
  
  # Loop through maneuver events
  for (i in seq_len(nrow(maneuver_data))) {
    end_time <- maneuver_data$end_datetime[i]
    
    # Find the index of the next observation after the maneuver end time
    next_obs_index <- which(unpropagated_data$Date > end_time)[1]
    
    # Update maneuver column to "YES" for the next observation after maneuver end time
    if (!is.na(next_obs_index)) {
      unpropagated_data$maneuver[next_obs_index] <- "YES"
    } else {
      # If no next observation is found (end of data), mark the last observation as "YES"
      unpropagated_data$maneuver[nrow(unpropagated_data)] <- "YES"
    }
  }
  
  return(unpropagated_data)
}




#process a satellite's maneuver data and update the unpropagated data
process_satellite_maneuvers <- function(satellite_name, maneuver_file_path) {
  maneuver_data <- read_maneuver_data(maneuver_file_path)
  maneuver_data <- convert_maneuver_dates(maneuver_data)
  unpropagated_data <- satellite_data[[satellite_name]]$unpropagated
  unpropagated_data <- mark_maneuvers(unpropagated_data, maneuver_data)
  satellite_data[[satellite_name]]$unpropagated <<- unpropagated_data
}


#apply function to all satellites with this format:
format1_satellites <- c("CryoSat-2", "Haiyang-2A", "Sentinel-3A", "Jason-1", "Jason-2", "Jason-3", "Sentinel-3B",  "TOPEX")
format1_maneuvres <- c("cs2man.txt", "h2aman.txt", "s3aman.txt", "ja1man.txt", "ja2man.txt", "ja3man.txt", "s3bman.txt", "topman.txt")

#Folder
maneuver_folder <- "uni/satellite/satellite_data/manoeuvres"

#process
for (i in seq_along(format1_satellites)) {
  satellite_name <- format1_satellites[i]
  maneuver_file_path <- file.path(maneuver_folder, format1_maneuvres[i])
  
  
  process_satellite_maneuvers(satellite_name, maneuver_file_path)
}





#fengyen satellites have a different format:

format2_satellites <- c("Fengyun-2D", "Fengyun-2E","Fengyun-2F","Fengyun-2H", "Fengyun-4A")
format2_maneuvres <- c("manFY2D.txt.fy","manFY2E.txt.fy","manFY2F.txt.fy","manFY2H.txt.fy","manFY4A.txt.fy" )

# Function to read maneuver data from a text file for Fengyun satellites
read_maneuver_data_fengyun <- function(file_path) {
  maneuver_data <- read.table(file_path, fill = TRUE, stringsAsFactors = FALSE)
  
  # Remove the column named "V2" if it exists
  if ("V2" %in% colnames(maneuver_data)) {
    maneuver_data <- maneuver_data[, !colnames(maneuver_data) %in% "V2"]
  }
  
  colnames(maneuver_data) <- c("type", "start_datetime", "end_datetime")
  
  return(maneuver_data)
}

# Function to convert maneuver dates to datetime for Fengyun satellites
convert_maneuver_dates_fengyun <- function(maneuver_data) {
  maneuver_data <- maneuver_data %>%
    mutate(
      start_datetime = as.POSIXct(start_datetime, format = "%Y-%m-%dT%H:%M:%S"),
      end_datetime = as.POSIXct(end_datetime, format = "%Y-%m-%dT%H:%M:%S")
    )
  
  return(maneuver_data)
}

convert_maneuver_dates_fengyun(read_maneuver_data_fengyun("uni/satellite/satellite_data/manoeuvres/manFY2D.txt.fy"))

# Function to mark maneuvers in unpropagated data for Fengyun satellites
mark_maneuvers_fengyun <- function(unpropagated_data, maneuver_data) {
  # Ensure `Date` column is in POSIXct format
  unpropagated_data$Date <- as.POSIXct(unpropagated_data$Date, format = "%Y-%m-%dT%H:%M:%S %Z")

  unpropagated_data$maneuver <- "NO"
  

  for (i in 1:nrow(maneuver_data)) {
    start_time <- maneuver_data$start_datetime[i]
    end_time <- maneuver_data$end_datetime[i]
    
    # Find the index of the next observation after the maneuver
    next_obs_index <- which(unpropagated_data$Date > end_time)[1]
    

    if (!is.na(next_obs_index)) {
      unpropagated_data$maneuver[next_obs_index] <- "YES"
    } else {

      unpropagated_data$maneuver[nrow(unpropagated_data)] <- "YES"
    }
  }
  
  return(unpropagated_data)
}

# Function to process a satellite's maneuver data and update the unpropagated data for Fengyun satellites
process_satellite_maneuvers_fengyun <- function(satellite_name, maneuver_file_path) {
  maneuver_data <- read_maneuver_data_fengyun(maneuver_file_path)
  maneuver_data <- convert_maneuver_dates_fengyun(maneuver_data)
  unpropagated_data <- satellite_data[[satellite_name]]$unpropagated
  unpropagated_data <- mark_maneuvers_fengyun(unpropagated_data, maneuver_data)
  satellite_data[[satellite_name]]$unpropagated <<- unpropagated_data

}


# Loop through each fengyun satellite and process the maneuvers
for (i in seq_along(format2_satellites)) {
  satellite_name <- format2_satellites[i]
  maneuver_file_path <- file.path(maneuver_folder, format2_maneuvres[i])
  
  # Apply the function to each satellite
  process_satellite_maneuvers_fengyun(satellite_name, maneuver_file_path)
}






#### preliminary cleaning ####

#looks like date is weird for propagated elements cryosat 2:
#(date contained last section of the unpropagated data date for this satellite - replace with this date)
# Extract the propagated and unpropagated data for CryoSat-2

propagated_elements_CryoSat_2 <- satellite_data[["CryoSat-2"]]$propagated
unpropagated_elements_CryoSat_2 <- satellite_data[["CryoSat-2"]]$unpropagated

# Update the date column in propagated data to match the unpropagated data (excluding the first row)
propagated_elements_CryoSat_2$'Date' <- unpropagated_elements_CryoSat_2$'Date'[-1]
satellite_data[["CryoSat-2"]]$propagated <- propagated_elements_CryoSat_2






#### EDA ####


#function for visualising elements (all elements):
plot_satellite_data <- function(satellite_name) {
  
  # Extract the unpropagated data for the given satellite and filter for 2011
  satellite_data2 <- satellite_data[[satellite_name]]$unpropagated #%>%
   # filter(lubridate::year(`Date`) == 2017)
  
  # Filter the unpropagated data to get the maneuver dates within 2011
  maneuver_dates <- satellite_data2 %>% 
    filter(maneuver == "YES") %>% 
    dplyr::select(`Date`)
  
  # Eccentricity Over Time
  p1 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = eccentricity), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Eccentricity",
         x = "Date", y = "Eccentricity") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Eccentricity")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p1 <- ggplotly(p1)
  
  # Argument of Perigee Over Time
  p2 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = `argument of perigee`), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Argument of Perigee",
         x = "Date", y = "Argument of Perigee (degrees)") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Argument of Perigee")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p2 <- ggplotly(p2)
  
  # Inclination Over Time
  p3 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = inclination), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Inclination",
         x = "Date", y = "Inclination (degrees)") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Inclination")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p3 <- ggplotly(p3)
  
  # Mean Anomaly Over Time
  p4 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = `mean anomaly`), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Mean Anomaly",
         x = "Date", y = "Mean Anomaly (degrees)") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Mean Anomaly")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p4 <- ggplotly(p4)
  
  # Brouwer Mean Motion Over Time
  p5 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = `Brouwer mean motion`), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Brouwer Mean Motion",
         x = "Date", y = "Brouwer Mean Motion (revolutions per day)") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Brouwer Mean Motion")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p5 <- ggplotly(p5)
  
  # Right Ascension Over Time
  p6 <- ggplot() +
    geom_line(data = satellite_data2, aes(x = `Date`, y = `right ascension`), color = "red") +
    geom_vline(data = maneuver_dates, aes(xintercept = as.numeric(`Date`)), linetype = "dashed") +
    labs(title = "Right Ascension",
         x = "Date", y = "Right Ascension (degrees)") +
    scale_color_manual(name = "Legend", 
                       values = c("red" = "red"), 
                       labels = c("Current Element" = "Right Ascension")) +
    scale_linetype_manual(name = "Legend", 
                          values = c("dashed" = "dashed"), 
                          labels = c("Maneuver" = "Maneuver")) +
    guides(color = guide_legend(title = NULL), 
           linetype = guide_legend(title = NULL))
  
  p6 <- ggplotly(p6)
  
  # Show all of the charts
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  print(p6)
}
#plot_satellite_data("Jason-3")
#plot_satellite_data("Sentinel-3B")
#plot_satellite_data("Fengyun-2D")

#visualise specific elements no maneuvers
plot_satellite_data_no_man <- function(satellite_name, element) {
  
  # Extract the unpropagated data for the given satellite
  satellite_data2 <- satellite_data[[satellite_name]]$unpropagated
  
  # Validate the element
  valid_elements <- c("eccentricity", "argument of perigee", "inclination", "mean anomaly", "Brouwer mean motion", "right ascension")
  if (!element %in% valid_elements) {
    stop("Invalid element specified. Valid options are: ", paste(valid_elements, collapse = ", "))
  }
  
  # Plot the specified element
  plot_element <- function(element, title, y_label) {
    ggplot(satellite_data2, aes(x = Date, y = !!sym(element))) +
      geom_line(color = "red") +
      labs(title = title,
           x = "Date", y = y_label,
           subtitle = "Red: unpropagated data")
  }
  
  p <- switch(
    element,
    "eccentricity" = plot_element("eccentricity", "Eccentricity ", "Eccentricity"),
    "argument of perigee" = plot_element("argument of perigee", "Argument of Perigee ", "Argument of Perigee (degrees)"),
    "inclination" = plot_element("inclination", "Inclination ", "Inclination (degrees)"),
    "mean anomaly" = plot_element("mean anomaly", "Mean Anomaly ", "Mean Anomaly (degrees)"),
    "Brouwer mean motion" = plot_element("Brouwer mean motion", "Brouwer Mean Motion ", "Brouwer Mean Motion (revolutions per day)"),
    "right ascension" = plot_element("right ascension", "Right Ascension ", "Right Ascension (degrees)")
  )
  
  # Convert to interactive plot
  p <- ggplotly(p)
  
  # Display the plot
  print(p)
}
#plot_satellite_data_no_man("Jason-3", "Brouwer mean motion")

#see if there is any seasonality?
plot_seasonal_stack <- function(satellite_name, orbital_element) {
  

  satellite_data2 <- satellite_data[[satellite_name]]$unpropagated

  maneuver_dates <- satellite_data2 %>% 
    filter(maneuver == "YES") %>% 
    select(Date)
  

  first_date <- min(satellite_data2$Date)
  last_date <- max(satellite_data2$Date)
  
  start_dates <- seq(from = as.Date(first_date), by = "year", length.out = 4)[1:3]
  

  start_dates <- start_dates + months(12)
  end_dates <- start_dates + years(1) - days(1)
  
  # Ensure the last period ends at the maximum date if less than a year
  end_dates[3] <- min(end_dates[3], last_date)
  
  # Function to plot a given year
  plot_year <- function(start_date, end_date, element, maneuver_dates) {
    filtered_data <- satellite_data2 %>%
      filter(Date >= start_date & Date <= end_date)
    
    ggplot(filtered_data, aes(x = Date, y = !!sym(element))) +
      geom_line(color = "blue") +
      geom_vline(data = maneuver_dates %>% filter(Date >= start_date & Date <= end_date), 
                 aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +
      labs(title = paste(element, "from", format(start_date, "%Y")),
           x = "Date", y = element)
  }
  
  # Create plots for each year
  p1 <- plot_year(start_dates[1], end_dates[1], orbital_element, maneuver_dates)
  p2 <- plot_year(start_dates[2], end_dates[2], orbital_element, maneuver_dates)
  p3 <- plot_year(start_dates[3], end_dates[3], orbital_element, maneuver_dates)
  
  
  combined_plot <- p1 / p2 / p3 +
    plot_layout(ncol = 1) +
    plot_annotation(title = paste("Yearly Plot of", orbital_element, "for", satellite_name))
  
  
  print(combined_plot)
}
#plot_seasonal_stack("CryoSat-2", "Brouwer mean motion")





#### more cleaning ####

#after looking at data, can identify periods of noise/ changing orbits...
truncate_satellite_data <- function(satellite_name, start_date, end_date) {
  
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)
  
  truncated_data <- satellite_data[[satellite_name]]$unpropagated %>%
    filter(Date >= start_date & Date <= end_date)
  
  # Update the global satellite_data
  satellite_data[[satellite_name]]$unpropagated <<- truncated_data
} #this keeps relevant section
delete_satellite_data_section <- function(satellite_name, start_date, end_date) {
  
  #dates to POSIXct
  start_date <- as.POSIXct(start_date)
  end_date <- as.POSIXct(end_date)
  
  #specified section
  updated_data <- satellite_data[[satellite_name]]$unpropagated %>%
    filter(!(Date >= start_date & Date <= end_date))
  
  #Update the global satellite_data
  satellite_data[[satellite_name]]$unpropagated <<- updated_data
} #this deletes section


# cutting out periods that are noisy/ have the satellite actively changing to a new orbit
truncate_satellite_data("Jason-1", "2002-01-14","2012-12-16")
delete_satellite_data_section("Jason-1", "2009-01-26","2009-02-23")
delete_satellite_data_section("Jason-1", "2011-09-04","2011-10-18")
delete_satellite_data_section("Jason-1", "2010-07-20","2010-08-09")

delete_satellite_data_section("Jason-3", "2022-04-07","2022-04-22")
delete_satellite_data_section("Jason-3", "2016-01-31","2016-02-13")

delete_satellite_data_section("Sentinel-3B", "2018-05-10","2018-06-13")
delete_satellite_data_section("Sentinel-3B", "2018-10-13","2018-11-28")

delete_satellite_data_section("CryoSat-2", "2010-04-25","2010-11-05")
delete_satellite_data_section("CryoSat-2", "2022-06-03","2022-06-28")

delete_satellite_data_section("Fengyun-2E", "2015-06-01","2015-06-30")
delete_satellite_data_section("Fengyun-2E", "2015-12-22","2015-12-27")

#correct for BIG chanegs in altitude

adjust_data_by_mean_difference <- function(satellite_name, start_date_1, end_date_1, start_date_2) {
  # Extract the unpropagated data for the given satellite
  satellite_data2 <- satellite_data[[satellite_name]]$unpropagated

  section_1 <- satellite_data2 %>%
    filter(Date >= as.Date(start_date_1) & Date <= as.Date(end_date_1))
  
  
  section_2 <- satellite_data2 %>%
    filter(Date >= as.Date(start_date_2))
  
  # Calculate the mean of Brouwer mean motion for both sections
  mean_1 <- mean(section_1$`Brouwer mean motion`, na.rm = TRUE)
  mean_2 <- mean(section_2$`Brouwer mean motion`, na.rm = TRUE)
  
  # mean difference
  mean_diff <- mean_1 - mean_2
  
  # Add the mean difference to the second section
  adjusted_section_2 <- section_2 %>%
    mutate(`Brouwer mean motion` = `Brouwer mean motion` + mean_diff)
  
  # Combine the sections back into the updated dataset
  updated_data <- bind_rows(section_1, adjusted_section_2)
  
  # Update the satellite_data list
  satellite_data[[satellite_name]]$unpropagated <<- updated_data
}
adjust_data_by_mean_difference_opposite <- function(satellite_name, start_date_1, end_date_1, start_date_2) {
  # Extract the unpropagated data for the given satellite
  satellite_data2 <- satellite_data[[satellite_name]]$unpropagated
  
  section_1 <- satellite_data2 %>%
    filter(Date >= as.Date(start_date_1) & Date <= as.Date(end_date_1))
  
  
  section_2 <- satellite_data2 %>%
    filter(Date >= as.Date(start_date_2))
  
  # Calculate the mean of Brouwer mean motion for both sections
  mean_1 <- mean(section_1$`Brouwer mean motion`, na.rm = TRUE)
  mean_2 <- mean(section_2$`Brouwer mean motion`, na.rm = TRUE)
  
  # mean difference
  mean_diff <- mean_2 - mean_1
  
  # subtract the mean difference to the second section
  adjusted_section_2 <- section_2 %>%
    mutate(`Brouwer mean motion` = `Brouwer mean motion` - mean_diff)
  
  # Combine the sections back into the updated dataset
  updated_data <- bind_rows(section_1, adjusted_section_2)
  
  # Update the satellite_data list
  satellite_data[[satellite_name]]$unpropagated <<- updated_data
}

adjust_data_by_mean_difference(
  satellite_name = "CryoSat-2",
  start_date_1 = "2010-06-19",
  end_date_1 = "2020-07-16",
  start_date_2 = "2020-07-31"
)

adjust_data_by_mean_difference_opposite(
  satellite_name = "Jason-1",
  start_date_1 = "2002-01-14",
  end_date_1 = "2012-04-25",
  start_date_2 = "2012-05-03"
)

#plot_satellite_data_no_man("CryoSat-2", "Brouwer mean motion")




#### MODELS ####

### HELPER FUNCTIONS:

compute_fourier_terms <- function(df, n_terms = 4) {
  #Date should be in date format
  df <- df %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  
  df <- df %>%
    mutate(DateNumeric = as.numeric(format(Date, "%j")))  #convert to day of year
  
  #Fourier terms
  for (i in 1:n_terms) {
    df <- df %>%
      mutate(
        !!paste0("sin_", i) := sin(2 * pi * i * DateNumeric / 365.25), #period = 365.25 days
        !!paste0("cos_", i) := cos(2 * pi * i * DateNumeric / 365.25)
      )
  }
  
  df
}

compute_precision_recall <- function(df_y_plotting, residuals, actual_maneuvers) {
  #Remove maneuvers within the last 2 observations in data by setting them to "NO" (these are impossible to predict so will not worry about these)
  if (length(actual_maneuvers) >= 2) {
    actual_maneuvers[(length(actual_maneuvers) - 1):length(actual_maneuvers)] <- "NO"
  }
  
  # all unique thresholds from residuals
  thresholds <- sort(unique(residuals))
  n <- length(thresholds)
  
  precision_recall_data <- data.frame(
    threshold = numeric(n),
    precision = numeric(n),
    recall = numeric(n),
    TP = integer(n),
    FP = integer(n),
    FN = integer(n)
  )
  
  # iterate through each threshold and compute precision, recall, TP, FP, FN (no TN here)
  for (i in seq_along(thresholds)) {
    thresh <- thresholds[i]
    
    df_y_plotting$predicted_maneuver <- ifelse(residuals >= thresh, "YES", "NO")
    
    #counters for TP, FP, FN
    tp <- 0
    fp <- 0
    fn <- 0
    
    # dont want to 'recount' the same maneuver as a TP and a FN, so need logical vector to track which maneuvers have been counted.
    counted_maneuvers <- rep(FALSE, nrow(df_y_plotting))
    
    # Loop through each observation to calculate TP and FP
    for (j in 1:nrow(df_y_plotting)) {
      predicted <- df_y_plotting$predicted_maneuver[j]
      
      if (predicted == "YES") {
        # Check for a true positive within the last 3 observations or the current one
        maneuver_matched <- FALSE
        for (k in max(1, j - 4):j) {
          if (actual_maneuvers[k] == "YES" && !counted_maneuvers[k]) {
            tp <- tp + 1
            counted_maneuvers[k] <- TRUE  #Mark this maneuver as counted
            maneuver_matched <- TRUE
            break
          }
        }
        # if no true maneuver was matched, count  as a false negative
        if (!maneuver_matched) {
          fn <- fn + 1
        }
      }
    }
    
    #remaining unmatched maneuvers should be FPs
    for (j in which(actual_maneuvers == "YES")) {
      if (!counted_maneuvers[j]) {
        fp <- fp + 1
      }
    }
    
    #Prec and recall
    precision <- (tp / (tp + fp))
    recall <- (tp / (tp + fn))
    
    precision_recall_data[i, ] <- c(thresh, precision, recall, tp, fp, fn)
  }
  
  return(precision_recall_data)
}


### Models:

#   xgboost
fit_and_evaluate_xgboost <- function(satellite_name, element="Brouwer mean motion", nrounds = 100, early_stopping_rounds = 10, threshold ) {
  # Prepare the data
  df_tles <- satellite_data[[satellite_name]]$unpropagated %>%
    rename(Element = all_of(element)) %>%
    arrange(Date) %>%
    compute_fourier_terms()
  
  # Select split date (set here at 75th percentile? change??)
  reference_date <- as.Date("1970-01-01")
  numeric_dates <- as.numeric(as.Date(df_tles$Date) - reference_date)
  split_date <- as.Date(quantile(numeric_dates, 0.75), origin = reference_date)
  
  # Normalise the selected element
  df_mm <- df_tles %>%
    select(Element) %>%
    mutate(Element = (Element - mean(Element)) * 1e7)
  
  # Prepare lagged features
  NUM_LAG_FEATURES <- 3
  df_x <- df_mm %>%
    mutate(lag_1 = lag(Element, 1))
  for (lag in 2:NUM_LAG_FEATURES) {
    df_x <- df_x %>%
      mutate(!!paste0("lag_", lag) := lag(Element, lag))
  }
  
  # Add Fourier terms to lagged features
  df_x <- df_x %>%
    bind_cols(select(df_tles, starts_with("sin_"), starts_with("cos_")))
  
  # Add additional time-based features
  df_x <- df_x %>%
    mutate(
      month = as.numeric(format(df_tles$Date, "%m")),
      week = as.numeric(format(df_tles$Date, "%U")),
      day = as.numeric(format(df_tles$Date, "%d"))
    )
  
  # Remove NA values due to lagging
  df_x <- df_x %>%
    filter(row_number() > NUM_LAG_FEATURES)
  df_y <- df_mm %>%
    filter(row_number() > NUM_LAG_FEATURES)
  
  # Add Date to df_x and df_y
  df_x <- df_x %>%
    mutate(Date = df_tles$Date[NUM_LAG_FEATURES + 1:n()]) %>%
    filter(!is.na(Date))
  df_y <- df_y %>%
    mutate(Date = df_tles$Date[NUM_LAG_FEATURES + 1:n()]) %>%
    filter(!is.na(Date))
  
  #train and test sets
  split_date <- as.Date(split_date)
  df_x_train <- df_x %>%
    filter(Date < split_date) %>%
    select(-Date)
  df_x_test <- df_x %>%
    filter(Date >= split_date) %>%
    select(-Date)
  df_y_train <- df_y %>%
    filter(Date < split_date) %>%
    select(-Date)
  df_y_test <- df_y %>%
    filter(Date >= split_date) %>%
    select(-Date)
  
  # Prepare data for xgboost
  dtrain <- xgb.DMatrix(data = as.matrix(select(df_x_train, -Element)), label = df_y_train$Element)
  dtest <- xgb.DMatrix(data = as.matrix(select(df_x_test, -Element)), label = df_y_test$Element)
  
  # parameter grid for tuning
  param_grid <- expand.grid(
    max_depth = c(1, 3, 5),
    eta = c(0.01, 0.1, 0.2),
    subsample = c(0.5, 0.7, 1.0),
    colsample_bytree = c(0.5, 0.7, 1.0),
    stringsAsFactors = FALSE
  )
  
  best_rmse <- Inf
  best_params <- list(max_depth = NA, eta = NA, subsample = NA, colsample_bytree = NA)
  
  #Grid search for the best parameters
  for (i in 1:nrow(param_grid)) {
    params <- param_grid[i, ]
    xgb_model <- xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        max_depth = params$max_depth,
        eta = params$eta,
        subsample = params$subsample,
        colsample_bytree = params$colsample_bytree
      ),
      data = dtrain,
      nrounds = nrounds,
      watchlist = list(train = dtrain, test = dtest),
      early_stopping_rounds = early_stopping_rounds,
      verbose = FALSE
    )
    
    # evaluate
    test_preds <- predict(xgb_model, newdata = dtest)
    test_rmse <- sqrt(mean((test_preds - df_y_test$Element)^2))
    
    if (test_rmse < best_rmse) {
      best_rmse <- test_rmse
      best_params <- params
    }
  }
  
  print(paste("Best max_depth:", best_params$max_depth))
  print(paste("Best eta:", best_params$eta))
  print(paste("Best subsample:", best_params$subsample))
  print(paste("Best colsample_bytree:", best_params$colsample_bytree))
  print(paste("Best RMSE from tuning:", best_rmse))
  
  #train final model with best parameters
  final_model <- xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = best_params$max_depth,
      eta = best_params$eta,
      subsample = best_params$subsample,
      colsample_bytree = best_params$colsample_bytree
    ),
    data = dtrain,
    nrounds = nrounds,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = early_stopping_rounds,
    verbose = TRUE
  )
  
  predictions <- predict(final_model, newdata = dtest)
  
  # residuals
  residuals <- predictions - df_y_test$Element
  residuals <- abs(residuals)
  
  # rmse
  final_rmse <- sqrt(mean((predictions - df_y_test$Element)^2))

  print(paste("Final model RMSE:", final_rmse))
  
  # Prepare data for plotting residuals
  df_y_plotting <- df_y_test %>%
    mutate(residuals = residuals) %>%
    mutate(Date = df_tles$Date[df_tles$Date >= split_date])
  
  # Label based on threshold set 
  df_y_plotting <- df_y_plotting %>%
    mutate(predicted_maneuver = ifelse(residuals > threshold, "YES", "NO")) %>%
    mutate(predicted_maneuver = factor(predicted_maneuver))
  
  
  # actual maneuver dates
  maneuver_dates <- satellite_data[[satellite_name]]$unpropagated %>%
    filter(maneuver == "YES") %>%
    filter(as.Date(`Date`) >= split_date) %>%
    pull(`Date`)
  
  # predicted maneuver dates
  predicted_maneuver_dates <- df_y_plotting %>%
    filter(predicted_maneuver == "YES") %>%
    pull(Date)
  
  maneuver_dates <- as.Date(maneuver_dates)
  
  # Plot residuals with maneuver markers
  g <- ggplot(df_y_plotting, aes(x = Date, y = residuals)) +
    geom_line(color = "blue") +
    geom_vline(data = data.frame(Date = maneuver_dates), aes(xintercept = as.numeric(Date)),
               color = "red", linetype = "dashed", size = 0.5, alpha = 0.8) +
    geom_vline(data = data.frame(Date = predicted_maneuver_dates), aes(xintercept = as.numeric(Date)),
               color = "green", linetype = "dotted", size = 0.5, alpha = 0.8) +
    labs(
      title = "XGBoost",
      x = "Date",
      y = "Residuals",
      subtitle = "Blue line: Residuals, Red dashed lines: Actual Maneuver Dates, Green dotted lines: Predicted Maneuver Dates"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(ggplotly(g))
  
  
  importance_matrix <- xgb.importance(model = xgb_model)  
  

  top6_importance <- importance_matrix[1:6,]
  

  #xgb.plot.importance(top6_importance, xlab = "Relative Importance")
  
  #precision-recall prep:
  df_y_plotting$actual_maneuver <- ifelse(df_y_plotting$Date %in% maneuver_dates, "YES", "NO")
  
  actual_maneuvers <- df_y_plotting$actual_maneuver
  
  # use compute_precision_recall to get precision-recall data
  precision_recall_data <- compute_precision_recall(df_y_plotting, residuals, actual_maneuvers)
  
  #new global df (to be used in PR calcs)
  cleaned_satellite_name <- gsub("[^[:alnum:]_]", "", satellite_name)
  df_name <- paste0("precision_recall_data_", cleaned_satellite_name)
  assign(df_name, precision_recall_data, envir = .GlobalEnv)

}

#   arima
fit_and_evaluate_arima <- function(satellite_name, element="Brouwer mean motion", threshold = 3e-5) {
  # Prepare the data
  df <- satellite_data[[satellite_name]]$unpropagated %>%
    rename(Element = all_of(element)) %>%
    arrange(Date)
  
  # Split into training and testing sets (using 75% train, 25% test)
  n <- nrow(df)
  train_size <- round(0.75 * n)
  
  # Create train and test sets
  df_train <- df[1:train_size, ]
  df_test <- df[(train_size + 1):n, ]
  
  # Apply Box-Cox transformation to the training set
  lambda <- BoxCox.lambda(df_train$Element)
  df_train$Element_transformed <- BoxCox(df_train$Element, lambda)
  
  # Fit ARIMA model using auto.arima on the transformed training set
  arima_model <- auto.arima(df_train$Element_transformed, seasonal = TRUE)
  
  # Forecast the test set period (on the transformed scale)
  forecast_transformed <- forecast(arima_model, h = n - train_size)
  
  # Inverse the Box-Cox transformation on the forecasted values
  forecast_values <- InvBoxCox(forecast_transformed$mean, lambda)
  
  # Convert forecast_values to a numeric vector (from ts object)
  forecast_values <- as.numeric(forecast_values)
  
  # Compute residuals
  residuals <- df_test$Element - forecast_values
  
  # After differencing the residuals, adjust all columns accordingly
  residuals <- diff(residuals)  # Detrend the residuals
  
  # Adjust df_test to match the length of the differenced residuals
  df_test <- df_test[-1, ]  # Remove the first row (since differencing removes 1 row)
  
  # Also adjust forecast_values to match the new df_test length
  forecast_values <- forecast_values[-1]
  
  # Add predicted maneuver dates based on the residual threshold
  df_test$predicted_maneuver <- ifelse(abs(residuals) > threshold, "YES", "NO")
  
  maneuver_dates <- df_test$Date[df_test$maneuver == "YES"]
  predicted_maneuver_dates <- df_test$Date[df_test$predicted_maneuver == "YES"]
  
  # Create the actual maneuver column
  df_test$actual_maneuver <- ifelse(df_test$Date %in% maneuver_dates, "YES", "NO")
  
  # Compute precision-recall
  precision_recall_data <- compute_precision_recall(df_test, abs(residuals), df_test$actual_maneuver)
  
  # Remove non-alphanumeric characters from the satellite name
  cleaned_satellite_name <- gsub("[^[:alnum:]_]", "", satellite_name)
  
  # Create a new variable name for the precision-recall data
  df_name <- paste0("precision_recall_data_arima_", cleaned_satellite_name)
  
  # Assign precision-recall data to a new data frame in the global environment
  assign(df_name, precision_recall_data, envir = .GlobalEnv)
  
  # Plot the residuals with vertical lines for maneuvers
  p <- ggplot(df_test, aes(x = Date, y = abs(residuals))) +
    geom_line(color = "blue") +
    
    # Add vertical lines for actual maneuvers (red, dashed)
    geom_vline(data = data.frame(Date = maneuver_dates),                aes(xintercept = as.numeric(Date)),                color = "red", linetype = "dashed", size = 0.5, alpha = 0.8) +
    
    # Add vertical lines for predicted maneuvers (green, dotted)
    geom_vline(data = data.frame(Date = predicted_maneuver_dates),                aes(xintercept = as.numeric(Date)), color = "green", linetype = "dotted", size = 0.5, alpha = 0.8) +
    
    # Titles and labels
    labs(
      title = "ARIMA",
      x = "Date",
      y = "Residuals",
      subtitle = "Blue line: Residuals, Red dashed lines: Actual Maneuver Dates, Green dotted lines: Predicted Maneuver Dates"
    ) +
    
    # Rotate x-axis text for better readability
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(ggplotly(p))  # Display the interactive plot
  
  # Second plot: Actual, Modeled, and Residuals
  d <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Element), color = "black", size = 0.5, linetype = "solid") +   # Actual element
    geom_line(aes(y = forecast_values), color = "blue", size = 0.5, linetype = "solid") +  # Modeled element
    #geom_line(aes(y = residuals), color = "red", size = 0.5, linetype = "solid") +  # Residuals
    labs(
      title = paste("Actual vs Modelled Brouwer Mean motion"),
      x = "Date",
      y = "Value",
      subtitle = "Black: Actual, Blue: Modeled, Red: Residuals"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #print(ggplotly(d))
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  cat("RMSE for", satellite_name, ":", rmse, "\n")
  
  # Return the forecast and model object
  return(list(model = arima_model, RMSE = rmse))
}

#   x reg arima (other elements as external regressors)
fit_and_evaluate_arimax <- function(satellite_name, element="Brouwer mean motion", threshold = 3e-5) {

  df <- satellite_data[[satellite_name]]$unpropagated %>%
    rename(Element = all_of(element)) %>%
    arrange(Date)
  
  # Select other elements as regressors
  regressors <- setdiff(names(df), c("Date", "Element", "maneuver"))
  
  # Convert all regressor columns to numeric and drop NA values
  df <- df %>%
    mutate(across(all_of(regressors), as.numeric)) %>%
    drop_na()
  
  # Split into training and testing sets (using 75% train, 25% test)
  n <- nrow(df)
  train_size <- round(0.75 * n)
  df_train <- df[1:train_size, ]
  df_test <- df[(train_size + 1):n, ]
  
  # Apply Box-Cox transformation to the target element in the training set
  lambda <- BoxCox.lambda(df_train$Element)
  df_train$Element_transformed <- BoxCox(df_train$Element, lambda)
  
  # Prepare the xreg matrices for training and testing
  xreg_train <- as.matrix(df_train[, regressors])
  xreg_test <- as.matrix(df_test[, regressors])
  
  # Fit ARIMAX model with external regressors
  arima_model <- tryCatch(
    auto.arima(df_train$Element_transformed, xreg = xreg_train, seasonal = TRUE),
    error = function(e) {
      warning("No suitable ARIMA model found for the target element.")
      return(NULL)
    }
  )
  
  # Check if the model fitting was successful
  if (is.null(arima_model)) {
    return(NULL)
  }
  
  # Forecast the test period using the fitted model and xreg for the test set
  forecast_transformed <- forecast(arima_model, xreg = xreg_test, h = n - train_size)
  
  # Inverse the Box-Cox transformation on forecasted values
  forecast_values <- InvBoxCox(forecast_transformed$mean, lambda)
  forecast_values <- as.numeric(forecast_values)
  
  # Calculate residuals
  residuals <- df_test$Element - forecast_values
  residuals <- diff(residuals)  # Differencing residuals
  
  # Adjust test data to match residual length after differencing
  df_test <- df_test[-1, ]
  forecast_values <- forecast_values[-1]
  
  # Predicted maneuver based on residual threshold
  df_test$predicted_maneuver <- ifelse(abs(residuals) > threshold, "YES", "NO")
  
  # Actual maneuvers for plotting
  maneuver_dates <- df_test$Date[df_test$maneuver == "YES"]
  predicted_maneuver_dates <- df_test$Date[df_test$predicted_maneuver == "YES"]
  
  # Create the actual maneuver column
  df_test$actual_maneuver <- ifelse(df_test$Date %in% maneuver_dates, "YES", "NO")
  
  # Compute precision-recall
  precision_recall_data <- compute_precision_recall(df_test, abs(residuals), df_test$actual_maneuver)
  
  # Remove non-alphanumeric characters from the satellite name
  cleaned_satellite_name <- gsub("[^[:alnum:]_]", "", satellite_name)
  
  # Create a new variable name for the precision-recall data
  df_name <- paste0("precision_recall_data_arimax_", cleaned_satellite_name)
  
  # Assign precision-recall data to a new data frame in the global environment
  assign(df_name, precision_recall_data, envir = .GlobalEnv)
  
  # Plotting residuals and maneuvers
  p <- ggplot(df_test, aes(x = Date, y = abs(residuals))) +
    geom_line(color = "blue") +
    geom_vline(data = data.frame(Date = maneuver_dates), aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed", size = 0.5, alpha = 0.8) +
    geom_vline(data = data.frame(Date = predicted_maneuver_dates), aes(xintercept = as.numeric(Date)), color = "green", linetype = "dotted", size = 0.5, alpha = 0.8) +
    labs(
      title = "ARIMAX",
      x = "Date",
      y = "Residuals",
      subtitle = "Blue line: Residuals, Red dashed lines: Actual Maneuver Dates, Green dotted lines: Predicted Maneuver Dates"
    ) +
  
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(ggplotly(p))
  
  # Second plot: Actual, Modeled, and Residuals
  d <- ggplot(df_test, aes(x = Date)) +
    geom_line(aes(y = Element, color = "Actual", linetype = "Actual"), size = 0.5) +
    geom_line(aes(y = forecast_values, color = "Modeled", linetype = "Modeled"), size = 0.5) +
    scale_color_manual(
      name = "Legend",
      values = c("Actual" = "black", "Modeled" = "blue")
    ) +
    scale_linetype_manual(
      name = "Legend",
      values = c("Actual" = "solid", "Modeled" = "solid")
    ) +
    labs(
      title = paste("Actual vs Modeled for", element),
      x = "Date",
      y = "Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    )
  
  #print(ggplotly(d))
  
  # Calculate RMSE
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  cat("RMSE for", satellite_name, ":", rmse, "\n")
  
  return(list(model = arima_model, RMSE = rmse))

}

  

#testing with individual cases

#fit_and_evaluate_xgboost("Sentinel-3B", threshold = 1.542469e+00)
#fit_and_evaluate_arima("Jason-1", threshold = 8.098494e-08)
#fit_and_evaluate_arimax("Sentinel-3B", threshold =2.673723e-08)

#could change this to experiment with another set from 'satellites'
satellites_here <- c("Sentinel-3A", "Sentinel-3B","CryoSat-2","Fengyun-2D","Fengyun-2E","Jason-1", "Jason-3")

# Loop over 'satellites here' i.e. run all functions on all satellites
for (i in 1:length(satellites_here)) {
  satellite_name <- satellites_here[i]

  # Run ARIMA model
  fit_and_evaluate_arima(satellite_name, threshold = 10) #threshold is arbitary at this stage
  
  # Run ARIMAX model
  fit_and_evaluate_arimax(satellite_name,  threshold = 10)
  
  # Run XGBoost model
  fit_and_evaluate_xgboost(satellite_name, threshold = 10)
  
  
}

#### Evaluation  ####

# see one PR curve (pass 'precision_recall_data[_arima(x)]_[satellit ename with no hyphen])
pr_plot <- function(precision_recall_data) {
  # Extract the name of the data frame passed to the function
  data_name <- deparse(substitute(precision_recall_data))
  
  # Calculate F1 score
  precision_recall_data$F1 <- 2 * (precision_recall_data$precision * precision_recall_data$recall) /
    (precision_recall_data$precision + precision_recall_data$recall)
  
  # Handle cases where precision + recall might be 0
  precision_recall_data$F1[is.na(precision_recall_data$F1)] <- 0
  
  # Find the index of the maximum F1 score
  best_index <- which.max(precision_recall_data$F1)
  
  # Get the corresponding threshold and F1 score
  best_threshold <- precision_recall_data$threshold[best_index]
  best_f1_score <- precision_recall_data$F1[best_index]
  
  # Create the plot
  p2 <- ggplot(precision_recall_data, aes(x = recall, y = precision)) +
    geom_line(color = "purple", size = 0.7) +
    geom_point(aes(text = paste("Threshold:", threshold)), size = 0.7, color="purple") +
    labs(
      title = paste("Precision-Recall Curve for", gsub("^precision_recall_data_", "", data_name)),  # Use the variable name without the prefix
      x = "Recall",
      y = "Precision"
    ) +
    theme_minimal()
  
  p2 <- ggplotly(p2, tooltip = c("text"))  # Specify tooltip to show the text
  
  print(p2)
  
  # Print the best threshold and F1 score
  print(paste("Best threshold:", best_threshold))
  print(paste("Best F1 score:", best_f1_score))
  
  # Return both threshold and F1 score
  return(list(threshold = best_threshold, F1_score = best_f1_score))
}

# see all PR curves for one satellie
plot_pr_curves <- function(satellite_name) {
  # Define the dataset names
  dataset_names <- c(
    paste0("precision_recall_data_", satellite_name),
    paste0("precision_recall_data_arimax_", satellite_name),
    paste0("precision_recall_data_arima_", satellite_name)
  )
  
  # Initialize an empty data frame to store all PR data for plotting
  combined_pr_data <- data.frame(
    recall = numeric(),
    precision = numeric(),
    model = character()
  )
  
  # Loop through each dataset and combine them
  for (dataset_name in dataset_names) {
    if (exists(dataset_name)) {
      pr_data <- get(dataset_name)
      pr_data$model <- gsub("precision_recall_data_", "", dataset_name)  # Extract the model name
      combined_pr_data <- rbind(combined_pr_data, pr_data)
    } else {
      print(paste("No data available for", dataset_name))
    }
  }
  
  # Ensure there's data to plot
  if (nrow(combined_pr_data) == 0) {
    stop("No Precision-Recall data available for the specified satellite.")
  }
  
  # Plot the combined Precision-Recall curves
  p <- ggplot(combined_pr_data, aes(x = recall, y = precision, color = model)) +
    geom_line(size = 1) +
    geom_point(aes(text = paste("Threshold:", threshold)), size = 0.5) +
    labs(
      title = paste("Precision-Recall Curves for", satellite_name),
      x = "Recall",
      y = "Precision",
      color = "Model"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Make the plot interactive with tooltips
  p <- ggplotly(p, tooltip = c("text"))
  
  # Print the plot
  print(p)
}
#plot_pr_curves("Sentinel3B")

# Define the models and their suffixes
models <- list(
  arima = "precision_recall_data_arima_",
  arimax = "precision_recall_data_arimax_",
  xgboost = "precision_recall_data_"
)

# Initialise data frame to show eval metrics
eval_metrics <- data.frame(
  Satellite = character(), 
  Method = character(), 
  Best_F1_Score = numeric(), 
  Best_Threshold = numeric(),
  Best_Precision = numeric(),
  Best_Recall = numeric(),
  Best_FNR = numeric()
)

for (satellite_name in satellites_here) {
  # Remove special characters to create valid data names
  clean_satellite_name <- gsub("[^[:alnum:]_]", "", satellite_name)
  
  # Loop through each model type
  for (model_name in names(models)) {
    data_name <- paste0(models[[model_name]], clean_satellite_name)
    
    # Check if data exists and process it
    if (exists(data_name)) {
      precision_recall_data <- get(data_name)
      
      # F1 score for each row
      precision_recall_data$F1 <- 2 * (precision_recall_data$precision * precision_recall_data$recall) /
        (precision_recall_data$precision + precision_recall_data$recall)
      precision_recall_data$F1[is.na(precision_recall_data$F1)] <- 0  # Handle division by zero
      
      # Best F1 score and corresponding threshold
      best_f1_index <- which.max(precision_recall_data$F1)
      best_f1 <- precision_recall_data$F1[best_f1_index]
      best_threshold <- precision_recall_data$threshold[best_f1_index]
      
      # Best precision with recall > 0.8
      best_precision_index <- which(precision_recall_data$recall > 0.8)
      best_precision <- if (length(best_precision_index) > 0) {
        max(precision_recall_data$precision[best_precision_index], na.rm = TRUE)
      } else {
        0
      }
      
      # Best recall with precision > 0.8
      best_recall_index <- which(precision_recall_data$precision > 0.8)
      best_recall <- if (length(best_recall_index) > 0) {
        max(precision_recall_data$recall[best_recall_index], na.rm = TRUE)
      } else {
        NA
      }
      
      # Compute FNR
      if (!is.null(precision_recall_data$FN) && !is.null(precision_recall_data$TP)) {
        best_fn <- precision_recall_data$FN[best_f1_index]
        best_tp <- precision_recall_data$TP[best_f1_index]
        best_fnr <- ifelse((best_fn + best_tp) > 0, best_fn / (best_fn + best_tp), NA)
      } else {
        best_fnr <- NA  # FNR cannot be calculated if FN or TP is missing
      }
      
      # append results to the eval metrics data frame
      eval_metrics <- rbind(eval_metrics, data.frame(
        Satellite = satellite_name,
        Method = toupper(model_name),
        Best_F1_Score = best_f1,
        Best_Threshold = best_threshold,
        Best_Precision = best_precision,
        Best_Recall = best_recall,
        Best_FNR = best_fnr
      ))
    } else {
      print(paste("No data available for", data_name))
    }
  }
}

# View the results
View(eval_metrics)

summary_metrics <- eval_metrics %>%
  group_by(Method) %>% 
  summarise(
    Avg_F1_Score = mean(Best_F1_Score, na.rm = TRUE),
    Avg_Threshold = mean(Best_Threshold, na.rm = TRUE),
    Avg_Precision = mean(Best_Precision, na.rm = TRUE),
    Avg_Recall = mean(Best_Recall, na.rm = TRUE),
    Avg_FNR = mean(Best_FNR, na.rm = TRUE)
  )

# Print the summary metrics dataframe
View(summary_metrics)



#### summary plots ####

eval_metrics <- eval_metrics %>%
  mutate(`1 - Best_FNR` = 1 - Best_FNR)


# Reshape data to long format
long_data <- eval_metrics %>%
  pivot_longer(cols = c(Best_F1_Score, Best_Precision, Best_Recall, `1 - Best_FNR`),
               names_to = "Metric",
               values_to = "Value")

# Plot grouped bar chart
ggplot(long_data, aes(x = Metric, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ Satellite, scales = "fixed", nrow = 2) +  # Fix scales to make them comparable
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Performance Metrics by Satellite and Method",
    x = "Metric",
    y = "Value",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  ylim(0, 1)

#custom colours
custom_colours <- c(
  "XGBOOST" = "#619CFF", # Blue
  "ARIMA" = "#F8756B",   # Orange Red?
  "ARIMAX" = "#00BA38"   # Green
)
satellite_mapping <- c(
  "CryoSat-2" = "C2",
  "Jason-1" = "J1",
  "Jason-3" = "J3",
  "Fengyun-2D" = "F2D",
  "Fengyun-2E" = "F2E",
  "Sentinel-3A" = "S3A",
  "Sentinel-3B" = "S3B"
)

long_data <- long_data %>%
  mutate(
    Nickname = satellite_mapping[Satellite],
    Metric = factor(Metric, levels = c("Best_F1_Score", "Best_Precision", "Best_Recall", "1 - Best_FNR"))
  )

# Create the box-and-whisker plot
ggplot(long_data, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colours) +
  labs(
    title = "Performance Metrics by Model",
    x = "Metric",
    y = "Value",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text( hjust = 1),
    legend.position = "top"
  )



long_data_filtered <- long_data %>%
  filter(Satellite != "Jason-1")



outliers <- long_data_filtered %>%
  group_by(Metric) %>%
  mutate(
    mean_value = mean(Value, na.rm = TRUE),
    sd_value = sd(Value, na.rm = TRUE),
    z_score = (Value - mean_value) / sd_value  # Calculate Z-score
  ) %>%
  filter(abs(z_score) > 2) %>%  # Consider outliers if the Z-score is greater than 3
  mutate(
    nudge_x = case_when(
      Method == "ARIMA" ~ -0.2,   # Nudge ARIMA points left
      Method == "XGBOOST" ~ 0.2,  # Nudge XGBoost points right
      TRUE ~ 0                    # No nudge for ARIMAX
    )
  )

# Create the box-and-whisker plot with Nicknames for outliers
ggplot(long_data_filtered, aes(x = Metric, y = Value, fill = Method)) +
  geom_boxplot() +
  # Add nicknames for outlier points with nudging based on the method
  geom_text(data = outliers, 
            aes(x = Metric, y = Value, label = Nickname, color = Method),
            vjust = -0.5,  # Adjust vertical positioning
            size = 4,      # Set font size
            nudge_x = outliers$nudge_x,  # Apply the nudging values directly
            show.legend = FALSE) +  # Prevent the legend from being created for text labels
  scale_fill_manual(values = custom_colours) +
  scale_color_manual(values = custom_colours) + 
  scale_y_continuous(
    limits = c(0, NA)  # Force the y-axis to start at 0, leaving the upper limit flexible
  ) +
  labs(
    title = "Performance Metrics by Model: Jason-1 excluded",
    x = "Metric",
    y = "Value",
    fill = "Model",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1),
    legend.position = "top"
  )


