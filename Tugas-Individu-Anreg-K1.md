Tugas Individu Anreg K1
================
Yumna Salma
2024-03-05

# Uji Asumsi Regresi Linier Sederhana

## **Read data**

``` r
library(readxl)
data <- read_xlsx("C:/Analisis Regresi/Data Anreg Individu.xlsx")
y <- data$Y
x <- data$X

data<-data.frame(cbind(y,x))
head(data)
```

    ##    y  x
    ## 1 54  2
    ## 2 50  5
    ## 3 45  7
    ## 4 37 10
    ## 5 35 14
    ## 6 25 19

``` r
plot(x,y)
```

![](Tugas-Individu-Anreg-K1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
regresi <- lm(y~x, data)
summary(regresi)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.1628 -4.7313 -0.9253  3.7386  9.0446 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 46.46041    2.76218   16.82 3.33e-10 ***
    ## x           -0.75251    0.07502  -10.03 1.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.891 on 13 degrees of freedom
    ## Multiple R-squared:  0.8856, Adjusted R-squared:  0.8768 
    ## F-statistic: 100.6 on 1 and 13 DF,  p-value: 1.736e-07

## Pemeriksaan Asumsi Kenormalan Residual

Menggunakan Uji Kolmogorov-Smirnov

Hipotesis:

H0: Residual menyebar normal

H1: Residual tidak menyebar normal

Nilai alfa: 0.05

``` r
library(nortest)
lillie.test(regresi$residuals)
```

    ## 
    ##  Lilliefors (Kolmogorov-Smirnov) normality test
    ## 
    ## data:  regresi$residuals
    ## D = 0.12432, p-value = 0.7701

Nilai p-value \> 0.05 maka tidak tolak H0 sehingga dapat diketahui
dengan tingkat kepercayaan 95% sisaan dari model regresi menyebar
normal, sehingga uji kenormalan terpenuhi terpenuhi.

## Uji Autokorelasi

Menggunakan Uji hipotesis pada Durbin-Watson

Hipotesis:

H0: Tidak terdapat autokorelasi pada resiudal

H1: Terdapat autokorelasi pada residual

Nilai alfa: 0.05

``` r
library(lmtest)
```

    ## Warning: package 'lmtest' was built under R version 4.3.2

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.3.2

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
dwtest(regresi)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  regresi
    ## DW = 0.48462, p-value = 1.333e-05
    ## alternative hypothesis: true autocorrelation is greater than 0

Nilai p-value \< 0.05 tolak H0, maka pada taraf nyata 95% terdapat
autokorelai pada residual. Sehingga uji asumsi tidak ada autokorelasi
tidak terpenuhi.

## Uji Heterokedastisitas

Menguji apakah model regresi terjadi ketidaksamaan varians dari
resiudal/sisaan satu pengamatan ke pengamatan lain.

Menggunakan Breusch-Pagan-Godfrey test

Hipotesis:

H0: Asumsi kehomogenan variansi sisaan terpenuhi

H1: Asumsi kehomogenan variansi sisaan tidak terpenuhi

``` r
library(lmtest)
bptest(regresi, studentize = FALSE, data=data)
```

    ## 
    ##  Breusch-Pagan test
    ## 
    ## data:  regresi
    ## BP = 0.19628, df = 1, p-value = 0.6577

Nilai p-value = 0.6577 \> 0.05 maka tidak tolak H0 sehingga dapat
diketahui dengan tingkat kepercayaan 95% artinya asumsi kehomogenan
variansi sisaan terpenuhi.

## Transformasi Data

Terdapat satu asumsi yang tidak terpenuhi, yaitu uji asumsi autokorelasi
sehingga, diperlukan transformasi data sedemikian rupa agar asumsi
tersebut terpenuhi.

``` r
xtr <- sqrt(x)
xtr
```

    ##  [1] 1.414214 2.236068 2.645751 3.162278 3.741657 4.358899 5.099020 5.567764
    ##  [9] 5.830952 6.164414 6.708204 7.211103 7.280110 7.745967 8.062258

``` r
ytr <- sqrt(y)
ytr
```

    ##  [1] 7.348469 7.071068 6.708204 6.082763 5.916080 5.000000 4.472136 4.000000
    ##  [9] 4.242641 3.605551 2.828427 3.316625 2.828427 2.000000 2.449490

``` r
regresi2 = lm(ytr~xtr, data = data)
summary(regresi2)
```

    ## 
    ## Call:
    ## lm(formula = ytr ~ xtr, data = data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.42765 -0.17534 -0.05753  0.21223  0.46960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.71245    0.19101   45.61 9.83e-16 ***
    ## xtr         -0.81339    0.03445  -23.61 4.64e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2743 on 13 degrees of freedom
    ## Multiple R-squared:  0.9772, Adjusted R-squared:  0.9755 
    ## F-statistic: 557.3 on 1 and 13 DF,  p-value: 4.643e-12

## Uji Autokorelasi Hasil Transformasi

Menggunakan Uji hipotesis pada Durbin-Watson

Hipotesis:

H0: Tidak terdapat autokorelasi pada resiudal

H1: Terdapat autokorelasi pada residual

Nilai alfa: 0.05

``` r
library(lmtest)
dwtest(regresi2)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  regresi2
    ## DW = 2.6803, p-value = 0.8629
    ## alternative hypothesis: true autocorrelation is greater than 0

Hasil uji autokorelasi setelah data tersebut ditransformasi adalah
p-value = 0.8629 \> 0.05 yang artinya tidak tolak H0, maka dapat
dinyatakan pada taraf kepercayaan 95% ada cukup bukti untuk menyatakan
tidak terdapat autokorelasi pada resiudal sehingga asumsi terpenuhi.

Permasahalan asumsi autokorelasi telah teratasi dengan proses
transformasi peubah penjelas dan respons. Selain itu diketahui pula
bahwa model setelah trans meghasilkan nilai adjusted R-squared sebesar
97.55% lebih baik dari model sebelum transformasi.

Model terbaik tersebut dapat dituliskan sebagai berikut:

$$
\hat Y^* = 8.71245 - 0.81339X^*_1 
$$

``` r
plot(xtr,ytr)
```

![](Tugas-Individu-Anreg-K1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
