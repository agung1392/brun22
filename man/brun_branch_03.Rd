% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brun_branch_03.R
\name{brun_branch_03}
\alias{brun_branch_03}
\title{Simulasi Sistem Kejadian Diskret dengan Percabangan (kasus 3)}
\usage{
brun_branch_03(
  L_insp,
  L_adjt,
  ak,
  chek = 0.15,
  rsc = c(2, 2),
  rn = 200,
  ongoing = FALSE
)
}
\arguments{
\item{L_insp}{adalah berupa fungsi yang memberikan nilai untuk lamanya aktivitas pemeriksaan oleh insp}

\item{L_adjt}{adalah berupa fungsi yang memberikan nilai untuk lamanya aktivitas perbaikan oleh adjt}

\item{ak}{adalah berupa fungsi yang digunakan untuk membangkitkan waktu antar kedatangan TV Set}

\item{chek}{adalah sebuah nilai numerik yang memberikan peluang sebuah TV Set yang telah diperiksa memerlukan perbaikan}

\item{rsc}{adalah vektor numerik integer, yang memberikan nilai besarnya kapasitas \emph{inspector} dan \emph{adjustor}}

\item{rn}{adalah sebuah nilai numerik untuk rentang waktu simulasi, nilai terpasangnya adalah 300}

\item{ongoing}{sebuah nilai logical, yang mana nilai TRUE, berarti mencatat kedatangan entiti yang masih sedang diproses di sistem.  Hal ini ditunjukkan oleh nilai kolom finished FALSE}
}
\value{
\strong{dataset} Arrivals dan Attributes
}
\description{
Pemrosesan akhir dari suatu pabrik perakitan TV, digambarkan sebagai berikut:
TV yang datang dari lini perakitan akan diperiksa kesesuaian mutunya oleh stasiun
pemeriksa (\emph{inspector}), dalam kasus ini tersedia dua stasiun pemeriksa yang bekerja
paralel dan identik, yang mana lamanya proses pemeriksaan menyebar \strong{Uniform(6, 12)}.
Berdasarkan pengalaman 15% dari hasil pemeriksaan memerlukan perbaikan, yang dilakukan
oleh stasiun perbaikan (\emph{adjustor}) dan sisanya 85% siap dikirim ke konsumen.
Tersedia dua stasiun perbaikan (identik) yang bekerja paralel, dengan lama proses perbaikan
merupakan peubah acak yang menyebar \strong{Uniform(20, 40)}. TV set yang berasal dari stasiun perbaikan
kembali dikirim ke antrian stasiun pemeriksaan. Waktu antar kedatangan TV set dari lini produksi
adalah \strong{Uniform(3.5, 7.5)}.
}
\examples{
brun_branch_03(function() runif(1, 6, 12), function() runif(1, 20, 40), function() runif(1, 3.5, 7.5))
}
