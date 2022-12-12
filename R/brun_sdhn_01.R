#' Sistem Antrian Sederhana.
#'
#' Kampus Binus mempunyai sebuah mesin ATM yang melayani nasabah untuk transaksi perbankan.
#' Waktu antar kedatangan pengguna ATM adalah merupakan peubah acak yang menyebar
#' Exponential, dengan rate (lambda : λ = 0.40 kedatangan per jam) dan disimpan dalam objek AK
#' (dibulatkan 3 digit dibelakang koma). Lamanya waktu yang diperlukan pengguna untuk bertransaksi
#' pada mesin ATM juga menyebar Eksponesial, dengan rate (lambda : λ = 0.45 pengguna per jam) dan
#' disimpan dalam objek Lama (dibulatkan 3 digit dibelakang koma).
#'
#' @param ak numerik yang merupakan laju kedatangan entiti.
#' @param lm numerik yang merupakan laju lamanya layanan.
#' @param rd numerik untuk nilai pembulatan.
#' @param rsc numerik yang memberikan kapasitas sumberdaya yang tersedia.
#' @param rn numerik rentang waktu simulasi.
#' @param ongoing = TRUE mencatat kedatangan entiti yang masih dalam proses di sistem.
#' @return data set arrivals
#' @examples
#' brun_sdhn_01(ak=0.40, lm=0.45, rd=3, rsc=1, rn=50, ongoing=TRUE)

brun_sdhn_01 <- function(ak=0.40, lm=0.45, rd=3, rsc=1, rn=50, ongoing=TRUE){

  env <- simmer("sederhana")

  # Waktu Antar Kedatangan (AK)
  AK <- function() round(rexp(n=1, rate=ak),rd)

  # Membangkitkan lamanya layanan
  Lama <- function() round(rexp(n=1, rate=lm),rd)

  lintas <- trajectory() %>%
    seize("ATM") %>%
    timeout(Lama) %>%
    release("ATM")

  env %>%
    add_resource("ATM", rsc) %>%
    add_generator("nasabah", lintas, AK) %>%
    run(rn) %>% invisible

  output.dat <- as_tibble(get_mon_arrivals(env, ongoing = ongoing))
  out <- subset(output.dat, start_time>0)
  return(out)
}





