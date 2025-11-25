############################################################
# api.R - Plumber API for House Price Models
############################################################

library(plumber)
library(dplyr)

############################################################
# CORS FILTER (for browser-based frontends like Next.js)
############################################################

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  # Handle preflight OPTIONS request
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }

  plumber::forward()
}

############################################################
# LOAD MODELS
############################################################

# These .rds files must be in the same directory as api.R
linear_model <- readRDS("linear_model.rds")   # FinalModel (log-price regression)
logit_model  <- readRDS("logit_model.rds")    # logit_model (High vs Low)

# Extract factor levels from the models to keep them consistent
safe_xlevels <- function(model, name) {
  if (!is.null(model$xlevels[[name]])) {
    model$xlevels[[name]]
  } else {
    character(0)
  }
}

levels_type_lin        <- safe_xlevels(linear_model, "Type")
levels_university_lin  <- safe_xlevels(linear_model, "University")

levels_type_logit      <- safe_xlevels(logit_model, "Type")
levels_university_logit<- safe_xlevels(logit_model, "University")

# Use a common set of levels (union) across both models
levels_type       <- unique(c(levels_type_lin,       levels_type_logit))
levels_university <- unique(c(levels_university_lin, levels_university_logit))

############################################################
# API TITLE / DESCRIPTION
############################################################

#* @apiTitle House Price Prediction API
#* @apiDescription
#* Predict house sale prices (via log-price linear regression)
#* and classify homes as High/Low value (via logistic regression).

############################################################
# HELPER: BUILD NEWDATA ROW WITH CORRECT CLASSES
############################################################

build_newdata <- function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University) {
  Beds       <- as.numeric(Beds)
  Baths      <- as.numeric(Baths)
  Sqft_home  <- as.numeric(Sqft_home)
  Sqft_lot   <- as.numeric(Sqft_lot)
  Age        <- as.numeric(Age)

  Type       <- factor(Type,       levels = levels_type)
  University <- factor(University, levels = levels_university)

  data.frame(
    Beds       = Beds,
    Baths      = Baths,
    Sqft_home  = Sqft_home,
    Sqft_lot   = Sqft_lot,
    Age        = Age,
    Type       = Type,
    University = University,
    stringsAsFactors = FALSE
  )
}

############################################################
# HEALTH CHECK
############################################################

#* Health check for the API
#* @get /health
#* @serializer unboxedJSON
function() {
  list(
    status  = "ok",
    message = "House models API is running",
    models  = list(
      linear_model = !is.null(linear_model),
      logit_model  = !is.null(logit_model)
    )
  )
}

############################################################
# PRICE PREDICTION ENDPOINT
############################################################

#* Predict log price and sale price for a house
#*
#* @param Beds:number Number of bedrooms
#* @param Baths:number Number of bathrooms
#* @param Sqft_home:number Square footage of the home
#* @param Sqft_lot:number Square footage of the lot
#* @param Age:number Age of the home (years)
#* @param Type:string Property type (e.g., "Single Family")
#* @param University:string Nearest university (e.g., "Iowa State University")
#* @get /predict_price
#* @serializer unboxedJSON
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University) {

  newdata <- build_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University)

  # Validate factor levels
  if (is.na(newdata$Type)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid Type: '", Type,
        "'. Allowed values are: ",
        paste(levels_type, collapse = ", ")
      )
    ))
  }

  if (is.na(newdata$University)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid University: '", University,
        "'. Allowed values are: ",
        paste(levels_university, collapse = ", ")
      )
    ))
  }

  pred_log_price <- as.numeric(predict(linear_model, newdata = newdata))
  pred_price     <- exp(pred_log_price)

  list(
    error               = FALSE,
    inputs              = newdata,
    predicted_log_price = pred_log_price,
    predicted_price     = pred_price
  )
}

############################################################
# HIGH / LOW CLASSIFICATION ENDPOINT
############################################################

#* Classify a house as High / Low price
#*
#* @param Beds:number Number of bedrooms
#* @param Baths:number Number of bathrooms
#* @param Sqft_home:number Square footage of the home
#* @param Sqft_lot:number Square footage of the lot
#* @param Age:number Age of the home (years)
#* @param Type:string Property type (e.g., "Single Family")
#* @param University:string Nearest university
#* @param threshold:number Probability cutoff for High (default = 0.5)
#* @get /predict_highlow
#* @serializer unboxedJSON
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University, threshold = 0.5) {

  newdata <- build_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University)

  # Validate factor levels
  if (is.na(newdata$Type)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid Type: '", Type,
        "'. Allowed values are: ",
        paste(levels_type, collapse = ", ")
      )
    ))
  }

  if (is.na(newdata$University)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid University: '", University,
        "'. Allowed values are: ",
        paste(levels_university, collapse = ", ")
      )
    ))
  }

  threshold <- as.numeric(threshold)
  if (is.na(threshold) || threshold <= 0 || threshold >= 1) {
    threshold <- 0.5
  }

  prob_high <- as.numeric(
    predict(logit_model, newdata = newdata, type = "response")
  )

  predicted_class <- ifelse(prob_high >= threshold, "High", "Low")

  list(
    error           = FALSE,
    inputs          = newdata,
    threshold       = threshold,
    prob_high       = prob_high,
    predicted_class = predicted_class
  )
}
