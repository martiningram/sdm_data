bbs_dataset <- function (csv_path) {

    names_of_interest <- c('x', 'in.train', 'in.test', 'latlon',
                           'route.presence.absence')

    files_of_interest <- paste0(file.path(csv_path, names_of_interest), '.csv')

    loaded <- lapply(files_of_interest, read.csv, stringsAsFactors = FALSE,
                     row.names = 1)
    names(loaded) <- names_of_interest

    # Squeeze the one-column dataframes into vectors
    loaded[['in.train']] <- loaded[['in.train']]$x
    loaded[['in.test']] <- loaded[['in.test']]$x

    # Add information about which variables are bioclim variables
    all_cols <- colnames(loaded$x)
    loaded[['bio_cols']] <- all_cols[grepl('bio', all_cols)]

    class(loaded) <- c('bbs_dataset', 'presence_absence_dataset')

    loaded

}

get_training_set.bbs_dataset <- function(x) {

    # We only want those that are bioclim variables
    covariates <- x$x[, x$bio_cols]

    # Also keep only those in the training set
    training_cov <- covariates[x$in.train, ]

    # Also find the latitude and longitude
    lat_lon <- x$latlon
    train_lat_lon <- x$latlon[x$in.train, ]

    outcomes <- x$route.presence.absence[x$in.train, ]

    list(covariates = training_cov, outcomes = outcomes,
         lat_lon = train_lat_lon)

}

get_test_set.bbs_dataset <- function(x) {

    covariates <- x$x[, x$bio_cols]
    test_cov <- covariates[x$in.test, ]
    outcomes <- x$route.presence.absence[x$in.test, ]
    test_lat_lon <- x$latlon[x$in.test, ]

    list(covariates = test_cov, outcomes = outcomes,
         lat_lon = test_lat_lon)

}
