############################################################
# plumber.R - run the House Price API (local + Render)
############################################################

library(plumber)

# Create router from api.R
pr <- pr("api.R")

# Render injects the PORT env var. Default to 8000 for local dev.
port <- as.numeric(Sys.getenv("PORT", "8000"))

# Listen on 0.0.0.0 so Docker/Render can access it
pr_run(pr, host = "0.0.0.0", port = port)
