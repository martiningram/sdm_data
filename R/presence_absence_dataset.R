not_implemented <- function(obj) {
    stop('This function is not implemented for this class!')
}

get_training_set <- function(obj) UseMethod("get_training_set")
get_test_set <- function(obj) UseMethod("get_test_set")
plot_species <- function(obj, species_name) UseMethod("plot_species")

presence_absence_dataset <- structure(data = list(), 'presence_absence_dataset')

get_training_set.presence_absence_dataset <- not_implemented
get_test_set.presence_absence_dataset <- not_implemented

summary.presence_absence_dataset <- function(x) {

    train_set <- get_training_set(x)
    test_set <- get_test_set(x)

    print(paste0('Presence/Absence dataset with ',
                 nrow(train_set$covariates),
                 ' training and ',
                 nrow(test_set$covariates),
                 ' test records.'))

}

plot.presence_absence_dataset <- function(x) {

    # Plot where the records fall
    train_set <- get_training_set(x)
    test_set <- get_test_set(x)

    train_lat_lon <- train_set$lat_lon
    test_lat_lon <- test_set$lat_lon

    train_lat_lon[, 'set'] <- 'train'
    test_lat_lon[, 'set'] <- 'test'

    combined <- rbind(train_lat_lon, test_lat_lon)

    p <- ggplot2::ggplot(combined, ggplot2::aes(x = Longitude, y = Latitude,
                                                colour = set)) +
        ggplot2::geom_point() +
        ggplot2::theme_classic()

    p
}

plot_species.presence_absence_dataset <- function(x, species_name) {

    # Plot where the records fall
    train_set <- get_training_set(x)
    test_set <- get_test_set(x)

    train_lat_lon <- train_set$lat_lon
    test_lat_lon <- test_set$lat_lon

    # Also fetch where this species lives
    relevant_train_outcomes <- train_set[['outcomes']][, species_name]
    relevant_test_outcomes <- test_set[['outcomes']][, species_name]

    train_data <- cbind(train_lat_lon, is_present = relevant_train_outcomes)
    test_data <- cbind(test_lat_lon, is_present = relevant_test_outcomes)

    train_data[, 'set'] <- 'train'
    test_data[, 'set'] <- 'test'
 
    combined <- rbind(train_data, test_data)

    p <- ggplot2::ggplot(combined,
                         ggplot2::aes(x = Longitude, y = Latitude,
                                      colour = is_present)) +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::facet_wrap(~set) +
        ggplot2::theme_classic()

    p

}
