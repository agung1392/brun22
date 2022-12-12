#' Sistem Kejadian Diskret dengan lebih dari satu server serial.
#'
#' Sebuah proses manufaktur dilakukan dalam dua tahap, pertama-tama material diperiksa oleh
#' seorang operator, dengan lama waktu pemeriksaan menyebar Triangular(min=0.2, max=0.8,
#' mode=0.4). Setelah selesai diperiksa, material diproses oleh sebuah mesin dengan lama
#' pemrosesan menyebar Uniform(1.5, 2.0). Pabrik tersebut hanya menyediakan satu orang
#' operator dan satu unit mesin pemroses.
#'
#' @param L_opr berbentuk fungsi yang memberikan nilai lamanya proses di operator.
#' @param L_mesin berbentuk fungsi yang memberikan nilai lamanya proses di mesin.
#' @param AK berbentuk fungsi nilai waktu antar kedatanagan entiti.
#' @param rn numerik yang memberikan rentang waktu simulasi.
#' @param rsc numerik yang memberikan kapasitas sumberdaya yang tersedia untuk operator dan mesin.
#' @return dataset Arrivals dan Resources
#' @examples
#' brun_sdhn_03(L_opr = function() rtri(1, 0.2, 0.8, 0.4), L_mesin = function() runif(1, 1.5, 2.0),
#'              AK = function() 2, src = c(1, 1), rn=100)

brun_sdhn_03 <- function(L_opr, L_mesin, AK, src = c(1, 1), rn=100)
  {
  if (!is.function(L_opr) || !is.function(L_mesin) || !is.function(AK))
  {
    stop("argumen L_opr, L_mesin dan AK harus merupakan fungsi")
  }
    env <- simmer("serial")

    traj <- trajectory() %>%
      seize("operator") %>%
      timeout(L_opr) %>%
      release("operator") %>%
      seize("mesin") %>%
      timeout(L_mesin) %>%
      release("mesin")

    env %>%
      add_resource("operator", src[1]) %>%
      add_resource("mesin", src[2]) %>%
      add_generator("material", traj, AK) %>%
      run(rn)

    out1 <- as_tibble(get_mon_resources(env))
    out2 <- as_tibble(get_mon_arrivals(env))
    out <- list(Arrivals = out2, Resources = out1)

    return(out)
  }

# brun_sdhn_03(L_opr = function() rtri(1, 0.2, 0.8, 0.4), L_mesin = function() runif(1, 1.5, 2.0),
#             AK = function() {2}, src = c(1, 1), rn=100)
