#-------------------------------------------------------------------------------
# Functions UPS Rating API
#
#-------------------------------------------------------------------------------

# Function for constructing XML request
construct_request <- function(api.credentials,
                              shipment.data,
                              request.type = "RATE"  # "RATE" or "SHOP"
                              ) {
  # Constructs XML request to get Negotiated rates from UPS.
  #
  # Args:
  #   api.credentials: a list, which contains 'UserID' (user id at UPS),
  #     'Password', 'AccessLicenseNumber', 'Account' (a shipper number at UPS).
  #   shipment.data: a data.frame or a tibble, which contains the following fields:
  #     "Shipper.Country", "Shipper.Postal.Code", "Recipient.Country", 
  #     "Recipient.State.Province", "Recipient.Postal.Code", "Recipient.Residential",
  #     "Weight", "Height", "Width", "Length", "UPS.Service.Type".
  #   request.type: one of "RATE", or "SHOP".
  #
  # Returns:
  #   a string with XML-formatted request.

  
  xml.request <- paste0('<?xml version="1.0"?>
<AccessRequest xml:lang="en-US">
  <AccessLicenseNumber>', api.credentials$AccessLicenseNumber, '</AccessLicenseNumber>
  <UserId>', api.credentials$UserID,'</UserId>
  <Password>', api.credentials$Password,'</Password>
</AccessRequest>
<?xml version="1.0"?>
<RatingServiceSelectionRequest xml:lang="en-US">
  <Request>
    <RequestAction>Rate</RequestAction>
    <RequestOption>', request.type, '</RequestOption>
  </Request>
  <Shipment>
    <RateInformation>
      <NegotiatedRatesIndicator/>
    </RateInformation>
    <Shipper>
      <ShipperNumber>', api.credentials$Account,'</ShipperNumber>
      <Address>
        <PostalCode>', shipment.data$Shipper.Postal.Code, '</PostalCode>
        <CountryCode>', shipment.data$Shipper.Country, '</CountryCode>
      </Address>
    </Shipper>
    <ShipTo>
      <Address>
        <PostalCode>', shipment.data$Recipient.Postal.Code, '</PostalCode>
        <StateProvinceCode>', shipment.data$Recipient.State.Province, '</StateProvinceCode>
        <CountryCode>', shipment.data$Recipient.Country, '</CountryCode>',
ifelse(shipment.data$Recipient.Residential == "R", '<ResidentialAddressIndicator/>', ''),
     '</Address>
    </ShipTo>
    <Service>
      <Code>', shipment.data$UPS.Service.Type, '</Code>
    </Service>
    <Package>
      <PackagingType>
        <Code>00</Code>
      </PackagingType>
      <PackageWeight>
        <Weight>', shipment.data$Weight, '</Weight>
      </PackageWeight>
    </Package>
 <NumOfPieces>', shipment.data$Pieces.in.Shipment,'</NumOfPieces>
  </Shipment>
</RatingServiceSelectionRequest>')
}


#<Dimensions>
#  <Length>', shipment.data$Length, '</Length>
#  <Width>', shipment.data$Width, '</Width>
#  <Height>', shipment.data$Height, '</Height>
#  </Dimensions>

# Function that allows to retry an operating a configurable number of times,
#  with a configurable wait between attempts
retry <- function(expr,
                  isError = function(x) "try-error" %in% class(x),
                  maxErrors = 5,
                  sleep = 0) {
  attempts = 0
  retval = try(eval(expr), silent = TRUE)
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      message("retry: too many retries. Return NULL")
      return(NULL)
    } else {
      message(sprintf("retry: error in attempt %i/%i", attempts, maxErrors))
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr), silent = TRUE)
  }
  return(retval)
}


# Function for rating all the shippings from a table, it constructs and sends 
#   the requests one by one in a loop, and stores in the table
rate_shippings <- function(data,
                           api.credentials,
                           request.type = "RATE",
                           save_copy = TRUE,
                           sleep_time = 1) {
  # Constructs and sends requests for each row in the table.
  #
  # Args:
  #   data: a data.frame, which contains the following fields:
  #     "Shipper.Country", "Shipper.Postal.Code", "Recipient.Country", 
  #     "Recipient.State.Province", "Recipient.Postal.Code", "Recipient.Residential",
  #     "Weight", "UPS.Service.Type", "UPS.Service.Description".
  #   api.credentials: a list, which contains 'UserID' (user id at UPS),
  #     'Password', 'AccessLicenseNumber', 'Account' (a shipper number at UPS).
  #   request.type: one of "RATE", or "SHOP".
  #   save_copy: a logical, if TRUE (default), a copy will be saved after 
  #     processing every 1000 rows.
  #   sleep_time: an integer, duration of a pause after every 500 requests.
  #
  # Returns:
  #   a data.frame, initial 'data' with added 'UPS.Charge', 'UPS.Charge.Currency',
  #     'UPS.RateType', and 'Error' columns.
  
  
  data.rated <- data %>%
    mutate(UPS.Charge = NA,
           UPS.Charge.Currency = NA,
           UPS.RateType = NA,
           Error = NA)
  
  for (i in 1:nrow(data)) {
    shipment.data <- data[i,]
    
    if (nrow(data) < 100 || i %% 100 == 1) {
      message(sprintf("Processing %s out of %s", i, nrow(data)))
    }
    
    
    xml.request <- construct_request(api.credentials = api.credentials,
                                     shipment.data = shipment.data,
                                     request.type = request.type)
    
    
    resp <- retry(expr = POST(url = api.base.url,
                              body = xml.request,
                              verbose = TRUE),
                  sleep = 0.1)
    
    if (!is.null(resp) && resp$status_code == '200') {
      
      content <- tryCatch(XML::xmlToList(rawToChar(resp$content)),
                          error = function(e) {
                            message(sprintf("%s: Unknown XML format", i));
                            return(NULL)
                          })
      
      if (!is.null(content) && content$Response$ResponseStatusCode == "1") {
        MonetaryValue <- tryCatch(content$RatedShipment$NegotiatedRates$NetSummaryCharges$GrandTotal$MonetaryValue,
                                  error = function(e) {
                                    message(sprintf("%s: No Negotiated Rates", i));
                                    return(NULL)
                                  })
        
        CurrencyCode <- tryCatch(content$RatedShipment$NegotiatedRates$NetSummaryCharges$GrandTotal$CurrencyCode,
                                 error = function(e) {
                                   message(sprintf("%s: No Negotiated Rates", i));
                                   return(NULL)
                                 })
        RateType <- "Neg"
        
        if (is.null(MonetaryValue)) {
          MonetaryValue <- tryCatch(content$RatedShipment$TotalCharges$MonetaryValue,
                                    error = function(e) {
                                      message(sprintf("%s: There is no field 'TotalCharges/MonetaryValue'", i));
                                      return(NULL)
                                    })
          CurrencyCode <- tryCatch(content$RatedShipment$TotalCharges$CurrencyCode,
                                   error = function(e) {
                                     message(sprintf("%s: There is no field 'TotalCharges/CurrencyCode'", i));
                                     return(NULL)
                                   })
          RateType <- "Pub"
        }
        
        data.rated$UPS.Charge[i] <- MonetaryValue
        data.rated$UPS.Charge.Currency[i] <- CurrencyCode
        data.rated$UPS.RateType[i] <- RateType
      } else if (content$Response$ResponseStatusCode != "1") {
        message(sprintf("%i: %s", i, toString(content$Response$Error$ErrorDescription)))
        data.rated$Error[i] <- toString(content$Response$Error$ErrorDescription)
      } else {
        message(sprintf("%s: Warning: NULL content the response", i))
      }
      
    } else  {
      message(sprintf("%s: Warning: Response status code is %s", i, resp$status_code))
    }
    
    # Plan a Pause after every 1000 requests
    if (i %% 1000 == 500) {
      Sys.sleep(sleep_time)
    }
    
    # Save an intermediate copy every 100 requests
    if (isTRUE(save_copy) && i %% 100 == 0) {
      write.csv(data.rated, file = "data.rated.copy.csv")
    }
  }
  
  return(data.rated)
}
