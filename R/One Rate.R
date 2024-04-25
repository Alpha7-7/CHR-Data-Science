######## One rate Analysis 
find_shipment_size_and_rate <- function(length, width, height,actual_weight) {
  
  if (actual_weight > 50) {
    return(list(size = "Not Applicable", rate = NA))
  }
  # Helper function to check if item fits within any of the given sets of dimensions
  fits_within <- function(item_dims, box_dims_list) {
    for (box_dims in box_dims_list) {
      if (length(item_dims) == length(box_dims) && all(sort(item_dims) <= sort(box_dims))) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Define the dimensions (multiple for some types) and maximum volumes for each shipment size
  sizes <- list(
    envelope = list(dimensions = list(c(9.5, 12.5), c(9.5, 15.5), c(9.75, 11.5)), max_volume = 300),
    pak = list(dimensions = list(c(10.25, 12.75), c(12, 15.5), c(11.75, 14.75), c(10, 14.5)), max_volume = 650),
    small_box = list(dimensions = list(c(10.875, 1.5, 12.375), c(8.75, 2.625, 11.25)), max_volume = 420),
    medium_box = list(dimensions = list(c(11.5, 2.375, 13.25), c(8.75, 4.375, 11.25)), max_volume = 650),
    large_box = list(dimensions = list(c(12.375, 3, 17.5), c(8.75, 7.75, 11.25)), max_volume = 1100),
    extra_large_box = list(dimensions = list(c(11.875, 10.75, 11), c(15.75, 14.125, 6)), max_volume = 2200)
  )
  
  # Define the rates for each shipment size
  rates <- c(envelope = 8.45, pak = 10.95, small_box = 12.25, medium_box = 15.25, large_box = 20.75, extra_large_box = 27.75)
  
  # Initialize the shipment size and rate
  shipment_size <- "Not Applicable"
  rate <- NA
  
  # Calculate the volume and dimensions of the item
  item_volume <- length * width * height
  item_dims <- c(length, width, height)
  
  # Check each size to find the smallest possible one that fits the item
  for (size in names(sizes)) {
    size_info <- sizes[[size]]
    if (item_volume <= size_info$max_volume && fits_within(item_dims, size_info$dimensions)) {
      shipment_size <- size
      rate <- rates[size]
      break
    }
  }
  
  # Return the shipment size and rate
  list(size = shipment_size, rate = rate)
}

one_rate_shipments <- input_data[,c('unique_identifier','actual_weight','length','width','height')]

one_rate_results <- pmap(
  one_rate_shipments[, c('actual_weight', 'length', 'width', 'height')], 
  find_shipment_size_and_rate,
  .progress = TRUE
)

one_rate_results_df <- map_dfr(one_rate_results, ~as.data.frame.list(.))

# Combine the results with the original data frame
one_rate_shipments <- bind_cols(one_rate_shipments, one_rate_results_df)

wb <- createWorkbook()
sheet_name <- "one_rate"
addWorksheet(wb, sheetName = sheet_name)
writeData(wb, sheet = sheet_name, x = one_rate_shipments)
saveWorkbook(wb, paste0(scenarios$output_file_path[1],"One_rate_analysis.xlsx"),overwrite = TRUE)



not_elegible_one_rate <- one_rate_shipments[one_rate_shipments$size == "Not Applicable",]$unique_identifier