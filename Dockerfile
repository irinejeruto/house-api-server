# Use rstudio/plumber base image
FROM rstudio/plumber:latest

# Set working directory inside the container
WORKDIR /app

# Copy API code and model objects into the image
COPY api.R plumber.R linear_model.rds logit_model.rds ./

# Install any extra R packages your api.R needs
# (plumber is already in the base image)
RUN R -e "install.packages(c('dplyr'), repos = 'https://cloud.r-project.org')"

# Default port (Render will override with $PORT)
ENV PORT=8000

# Expose port for local docs / debugging
EXPOSE 8000

# Start the Plumber API
CMD ["Rscript", "plumber.R"]
