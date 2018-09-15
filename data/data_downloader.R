library(data.table)

data_dir <- sprintf("%s/data", getwd())

system(sprintf("wget https://s3.amazonaws.com/drivendata/data/55/public/cold_start_test.csv -O %s/test.csv", data_dir))
system(sprintf("wget https://s3.amazonaws.com/drivendata/data/55/public/meta.csv -P %s", data_dir))
system(sprintf("wget https://s3.amazonaws.com/drivendata/data/55/public/consumption_train.csv -O %s/train.csv", data_dir))
system(sprintf("wget https://s3.amazonaws.com/drivendata/data/55/public/submission_format.csv -P %s", data_dir))

