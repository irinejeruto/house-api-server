############################################################
# api.R - Plumber API for House Price Models
# Final regression model:
#   Log_price ~ Log_Beds + Log_Baths + Log_Sqft_home +
#               Log_Sqft_lot + Age + Type + Town
# Classification model:
#   HighPrice ~ Beds + Baths + Sqft_home + Sqft_lot + Age +
#               Type + Town
############################################################

library(plumber)

############################################################
# CORS FILTER (for browser-based frontends like Next.js)
############################################################

#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  # Handle preflight OPTIONS requests
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
linear_model <- readRDS("loglin_model.rds")   # FinalModel
logit_model  <- readRDS("logit_model.rds")    # High vs Low logistic model

# Helper to safely extract factor levels from model$xlevels
safe_xlevels <- function(model, name) {
  if (!is.null(model$xlevels[[name]])) {
    model$xlevels[[name]]
  } else {
    character(0)
  }
}

# Get factor levels for Type and Town from both models
levels_type_lin   <- safe_xlevels(linear_model, "Type")
levels_town_lin   <- safe_xlevels(linear_model, "Town")

levels_type_logit <- safe_xlevels(logit_model, "Type")
levels_town_logit <- safe_xlevels(logit_model, "Town")

# Use a unified set of levels across models
levels_type <- unique(c(levels_type_lin, levels_type_logit))
levels_town <- unique(c(levels_town_lin, levels_town_logit))

############################################################
# API TITLE / DESCRIPTION
############################################################

#* @apiTitle House Price Prediction API
#* @apiDescription
#* Predict house sale prices (via log-price linear regression)
#* and classify homes as High/Low value (via logistic regression),
#* using structural features (Beds, Baths, Sqft, Age) and location (Type, Town).

############################################################
# HELPER: BUILD BASE NEWDATA WITH CORRECT TYPES
############################################################

build_base_newdata <- function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, Town) {
  Beds      <- as.numeric(Beds)
  Baths     <- as.numeric(Baths)
  Sqft_home <- as.numeric(Sqft_home)
  Sqft_lot  <- as.numeric(Sqft_lot)
  Age       <- as.numeric(Age)

  Type <- factor(Type, levels = levels_type)
  Town <- factor(Town, levels = levels_town)

  data.frame(
    Beds      = Beds,
    Baths     = Baths,
    Sqft_home = Sqft_home,
    Sqft_lot  = Sqft_lot,
    Age       = Age,
    Type      = Type,
    Town      = Town,
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
# PRICE PREDICTION ENDPOINT  (FinalModel: log–log + Age + Type + Town)
############################################################

#* Predict log price and sale price for a house
#*
#* @param Beds:number Number of bedrooms
#* @param Baths:number Number of bathrooms
#* @param Sqft_home:number Square footage of the home
#* @param Sqft_lot:number Square footage of the lot
#* @param Age:number Age of the home (years)
#* @param Type:string Property type (e.g., "Single Family")
#* @param Town:string Town (e.g., "Ames, IA")
#* @get /predict_price
#* @serializer unboxedJSON
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, Town) {

  newdata <- build_base_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, Town)

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

  if (is.na(newdata$Town)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid Town: '", Town,
        "'. Allowed values are: ",
        paste(levels_town, collapse = ", ")
      )
    ))
  }

  # Add log-transformed predictors to match FinalModel
  newdata$Log_Beds      <- log(newdata$Beds)
  newdata$Log_Baths     <- log(newdata$Baths)
  newdata$Log_Sqft_home <- log(newdata$Sqft_home)
  newdata$Log_Sqft_lot  <- log(newdata$Sqft_lot)

  # Predict log-price and convert to original scale
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
# HIGH / LOW CLASSIFICATION ENDPOINT (logit_model)
############################################################

#* Classify a house as High / Low price
#*
#* @param Beds:number Number of bedrooms
#* @param Baths:number Number of bathrooms
#* @param Sqft_home:number Square footage of the home
#* @param Sqft_lot:number Square footage of the lot
#* @param Age:number Age of the home (years)
#* @param Type:string Property type (e.g., "Single Family")
#* @param Town:string Town (e.g., "Ames, IA")
#* @param threshold:number Probability cutoff for High (default = 0.5)
#* @get /predict_highlow
#* @serializer unboxedJSON
function(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, Town, threshold = 0.5) {

  newdata <- build_base_newdata(Beds, Baths, Sqft_home, Sqft_lot, Age, Type, Town)

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

  if (is.na(newdata$Town)) {
    return(list(
      error   = TRUE,
      message = paste0(
        "Invalid Town: '", Town,
        "'. Allowed values are: ",
        paste(levels_town, collapse = ", ")
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
    error           = FALSE,
    inputs          = newdata,
    threshold       = threshold,
    prob_high       = prob_high,
    predicted_class = predicted_class
  )
}
