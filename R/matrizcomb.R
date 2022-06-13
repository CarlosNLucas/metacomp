#matriComb
matrizComb <-
  expand.grid(
    #sample_size = 30,
    sample_size = seq(2, 30, 2),
    mean_e = 6,
    sd_e = 3,
    mean_c = 6,
    sd_c = 3,
    n_experiments = 1,
    #n_experiments = seq(1, 9, 2),
    d = 2/3)
# filler <- rep(NA, nrow(matrizComb))
# matrizComb$d_empirico <- filler
