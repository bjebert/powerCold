# Submitter - useful for converting submission_hourly back to required submission_format


submit <- function(model_name = "LM_004") {
    if(file.exists(sprintf("submissions/%s.csv", model_name))) {
        stop(sprintf("Submission %s already exists", model_name))
    }
        
    train <- fread("data/train.csv")
    test <- fread("data/test.csv")
    sub_hourly <- fread("data/submission_hourly.csv")
    submission_format <- fread("data/submission_format.csv")
    
    model_env <- new.env()
    sys.source(sprintf("models/%s/%s.R", model_name, model_name), envir = model_env)
    
    predictions <- model_env$get_predictions(train[sub_split == "train"], train[sub_split == "test"], test, sub_hourly)
    sub_hourly[, consumption := predictions]
    
    submission_format[, i := 1:.N, by = series_id]
    consumption_agg <- sub_hourly[, .(consumption = sum(consumption)), by = c('series_id', 'i')]
    
    submission_format[, consumption := NULL]    
    submission_format <- merge(submission_format, consumption_agg, by = c('series_id', 'i'), all.x = T, sort = FALSE)
    submission_format[, i := NULL]
    
    # Reorder columns
    submission_format <- submission_format[, .(pred_id, series_id, timestamp, temperature, consumption, prediction_window)]
    
    fwrite(submission_format, sprintf("submissions/%s.csv", model_name), row.names = FALSE)
}