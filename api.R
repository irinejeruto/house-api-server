############################################################
# api.R - Plumber API endpoints for house price models
############################################################

# Load required libraries
library(plumber)
library(dplyr)

# Load trained models (ensure these files exist in the working directory)
linear_model  <- readRDS("linear_model.rds")   # FinalModel (log-price regression)
logit_model   <- readRDS("logit_model.rds")    # logit_model (High vs Low)

# Extract factor levels from the models to keep them consistent
levels_type_lin        <- linear_model$xlevels$Type
levels_university_lin  <- linear_model$xlevels$University

levels_type_logit      <- logit_model$xlevels$Type
levels_university_logit<- logit_model$xlevels$University

# Use a common set of levels across both models (they should match, but just in case)
levels_type        <- unique(c(levels_type_lin, levels_type_logit))
levels_university  <- unique(c(levels_university_lin, levels_university_logit))

############################################################
#* @apiTitle House Price & High/Low Classification API
#* @apiDescription
#* Predict log house price (and back-transformed price) and
#* classify homes as High/Low price using existing models.
############################################################


#-----------------------------------------------------------
# Helper: build newdata row with correct classes
#-----------------------------------------------------------
build_newdata <- function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University) {
  # Convert numerics
  Beds       <- as.numeric(Beds)
  Baths      <- as.numeric(Baths)
  Sqft_home  <- as.numeric(Sqft_home)
  Sqft_lot   <- as.numeric(Sqft_lot)
  Age        <- as.numeric(Age)
  
  # Convert factors with correct levels
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

#-----------------------------------------------------------
#* Health check
#* @get /health
#* @serializer unboxedJSON
#-----------------------------------------------------------
function() {
  list(
    status = "ok",
    message = "House models API is running",
    models = c("linear_model" = !is.null(linear_model),
               "logit_model"  = !is.null(logit_model))
  )
}

#-----------------------------------------------------------
#* Predict house price (log-price regression model)
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
#-----------------------------------------------------------
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University) {
  
  newdata <- build_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University)
  
  # Basic validation for factor levels
  if (is.na(newdata$Type)) {
    return(list(
      error = TRUE,
      message = paste0(
        "Invalid Type: '", Type,
        "'. Allowed values are: ",
        paste(levels_type, collapse = ", ")
      )
    ))
  }
  if (is.na(newdata$University)) {
    return(list(
      error = TRUE,
      message = paste0(
        "Invalid University: '", University,
        "'. Allowed values are: ",
        paste(levels_university, collapse = ", ")
      )
    ))
  }
  
  # Predict log-price and convert back to original scale
  pred_log_price <- as.numeric(predict(linear_model, newdata = newdata))
  pred_price     <- exp(pred_log_price)

print('===============pred_price===============')
  print(pred_price)
  print('===============pred_log_price===============')
  print(pred_log_price)
  print('===============newdata===============')
  print(newdata)
  print('===============linear_model===============')
  print(linear_model)
  print('===============logit_model===============')
  print(logit_model)
  
  list(
    error          = FALSE,
    inputs         = newdata,
    predicted_log_price = pred_log_price,
    predicted_price     = pred_price
  )
}

#-----------------------------------------------------------
#* Predict High/Low price class (logistic regression model)
#*
#* @param Beds:number Number of bedrooms
#* @param Baths:number Number of bathrooms
#* @param Sqft_home:number Square footage of the home
#* @param Sqft_lot:number Square footage of the lot
#* @param Age:number Age of the home (years)
#* @param Type:string Property type (e.g., "Single Family")
#* @param University:string Nearest university (e.g., "Iowa State University")
#* @param threshold:number Classification cutoff for High (default = 0.5)
#* @get /predict_highlow
#* @serializer unboxedJSON
#-----------------------------------------------------------
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University, threshold = 0.5) {
  
  newdata <- build_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, University)
  
  # Basic validation for factor levels
  if (is.na(newdata$Type)) {
    return(list(
      error = TRUE,
      message = paste0(
        "Invalid Type: '", Type,
        "'. Allowed values are: ",
        paste(levels_type, collapse = ", ")
      )
    ))
  }
  if (is.na(newdata$University)) {
    return(list(
      error = TRUE,
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
  
  # Predict probability of High price
  prob_high <- as.numeric(
    predict(logit_model, newdata = newdata, type = "response")
  )
  
  predicted_class <- ifelse(prob_high >= threshold, "High", "Low")
  
  list(
    error          = FALSE,
    inputs         = newdata,
    threshold      = threshold,
    prob_high      = prob_high,
    predicted_class = predicted_class
  )
}
