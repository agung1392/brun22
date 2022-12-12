#' Simulasi Melingkar (Sirkular)
#'
#' Sebuah perusahaan melakukan pengiriman barang dari pabrik ke gudang.
#' Perusahaan tersebut menggunakan dua buah truk yang identitik (TrukA dan TrukB) untuk melakukan
#' aktivitas tersebut. Truk melakukan proses muat di pabrik, yang
#' membutuhkan waktu 1 jam, kemudian berangkat ke gudang dengan waktu tempuh
#' 1.5 jam. Sampai di gudang, dilakukan proses bongkar, yang membutuhkan waktu
#' 0.5 jam. Selesai proses bongkar di gudang, truk kembali ke pabrik dengan lama
#' waktu tempuh 1 jam. Simulasi dijalankan sampai waktu 24 jam.
#'
#' @param MUAT sebuah fungsi yang memberikan nilai untuk lamanya aktivitas MUAT di pabrik
#' @param BONGKAR sebuah fungsi yang memberikan nilai untuk lamanya aktivitas BONGKAR di gudang
#' @param TravelMB sebuah fungsi yang memberikan nilai untuk lamanya waktu perjalanan dari pabrik (MUAT) ke gudang (BONGKAR)
#' @param TravelBM sebuah fungsi yang memberikan nilai untuk lamanya waktu perjalanan dari gudang (BONGKAR) ke pabrik (MUAT)
#' @param rsc sebuah vektor numerik yang memeberikan besarnya kapasitas tempat MUAT di pabrik dan tempat BONGKAR di gudang
#' @param rn sebuah nilai numerik yang memberikan rentang waktu simulasi, nilai terpasangnya adalh 24
#' @param wkA adalah sebuah fungsi yang memberikan waktu kedatangan TrukA, nilai terpasangnya adalah at(0)
#' @param wkB adalah sebuah fungsi yang memberikan waktu kedatangan TrukB, nilai terpasangnya adalah at(0)
#' @param ongoing sebuah nilai logical, yang mana nilai TRUE berarti mencatat kedatangan entiti yang masih sedang diproses di sistem.  Hal ini ditunjukkan oleh nilai kolom finished FALSE
#'
#' @return
#' \strong{dataset} Arrivals
#' @export
#'
#' @examples
#' brun_sirkular_01(MUAT = function() 1, BONGKAR = function() 0.5, TravelMB = function() 1.5,
#'                            TravelBM = function() 1, rsc=c(1, 1), rn=24, wkA = at(0), wkB = at(0),
#'                            ongoing=FALSE)
#'

brun_sirkular_01 <- function(MUAT, BONGKAR, TravelMB, TravelBM, rsc=c(1, 1), rn=24,
                           wkA = at(0), wkB = at(0), ongoing=FALSE)
{
  if (!is.function(MUAT) || !is.function(BONGKAR) || !is.function(TravelMB)|| !is.function(TravelBM))
  {
    stop("MUAT, BONGKAR, TravelMB, dan TravelBM harus berupa fungsi numeric")
  }

  sirk <- simmer("sirkular")

  gudang <- trajectory()

  for(j in 1:2){
    pabrik <- trajectory() %>%
      # log_("sampai di pabrik ..") %>%
      seize("pabrik") %>%
      timeout(MUAT) %>%
      release("pabrik") %>%
      # log_("keluar pabrik ..") %>%
      # log_("perjalanan menuju gudang ..") %>%
      timeout(TravelMB)

    gudang <- trajectory() %>%
      # log_("sampai di gudang ..") %>%
      seize("gudang") %>%
      timeout(BONGKAR) %>%
      release("gudang") %>%
      # log_("keluar gudang ..") %>%
      # log_("perjalanan menuju pabrik ..") %>%
      timeout(TravelBM)

    trajA <- trajectory() %>%
      join(pabrik, gudang) %>%
      activate("TrukA")

    trajB <- trajectory() %>%
      join(pabrik, gudang) %>%
      activate("TrukB")
  }

  sirk %>%
    add_resource("pabrik", rsc[1]) %>%
    add_resource("gudang", rsc[1]) %>%
    add_generator("TrukA", trajA, wkA, mon=2) %>%
    add_generator("TrukB", trajB, wkB, mon=2) %>%
    run(rn) %>% invisible()

  out1 <- as_tibble(get_mon_arrivals(sirk, ongoing))
  out <- list(Arrivals = out1)
  return(out)
}

# brun_sirkular_01(MUAT = function() 1, BONGKAR = function() 0.5, TravelMB = function() 1.5,
#                            TravelBM = function() 1, rsc=c(1, 1), rn=24, wkA = at(0), wkB = at(0),
#                            ongoing=FALSE)

