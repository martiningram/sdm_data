library(sdmdata)

dataset <- bbs_dataset(Sys.getenv('BBS_PATH'))

p <- plot_species(dataset, 'Redhead')

print(p)
