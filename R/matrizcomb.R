#matriComb
matrizComb <-
  expand.grid(
    #sample_size = 10,
    sample_size = seq(4, 50, 5),
    mean_e = 10,
    sd_e = 3,
    mean_c = 8,
    sd_c = 3,
    n_experiments = 1,
    #n_experiments = seq(1, 101, 5),
    distribution = "normal",
    d = 1/3)
# filler <- rep(NA, nrow(matrizComb))
# matrizComb$d_empirico <- filler


matrizComb <-
  expand.grid(
    sample_size = 5,
    #sample_size = seq(4, 50, 5),
    mean_e = 10,
    sd_e = 3,
    mean_c = 8,
    sd_c = 3,
    #n_experiments = 1,
    n_experiments = seq(1, 21, 2),
    distribution = "lognormal",
    d = 1/3)
