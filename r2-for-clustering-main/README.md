hclust\_r2
================

As a group project, we studied climatic data from meteo station in
Switzerland. From tutitiempo and MeteoSuisse we collected 68 observations
from 2019. We observed the temperature, the number of foggy, rainy,
snowy, stormy days and other variables.

The main goal was to classify every cities and create cluster with
k-means and hierarchical clustering algorithms. To help us with deciding
how many clusters are optimal, we used *R*<sup>2</sup> criteria which is :

$ R^2 = \frac{BSS}{TSS} $


where BSS defines the between sum square and TSS the total sum square.

The idea is, if k is the number of cluster, n our observations, if k
converges toward n then *R*<sup>2</sup> converges toward 1.

Then we need to observe which is the most suitable while input the less computation. Meaning if we add
one more cluster, how much do we gain?

Since hclust function didn’t gave *R*<sup>2</sup>, i just programmed it.

Before starting, let’s see a head of the data.

``` r
head(df)
```

    ##                         T   TM  Tm      PP   V  RA SN TS FG GR   Lat Long Alt
    ## AIGLE                11.1 16.1 6.0 1022.38 7.9 132 12  2  4  0 46.33 6.91 383
    ## ALTDORF              10.9 15.2 6.3 1244.85 8.5 143 15 12  4  0 46.86 8.63 451
    ## ALTENRHEIN-FLUGPLATZ 10.8 14.9 6.4 1152.40 8.0   0  0  0  0  0 47.48 9.38 398
    ## BASEL BINNINGEN      11.6 16.5 7.2  778.29 7.2  97  7  2  9  0 47.55 7.58 316
    ## BUCHS-AARAU           0.8 15.5 6.2  890.24 5.2 159 15  5 45  0 47.38 8.08 387
    ## CHANGINS             11.5 15.5 7.1 1054.85 9.5   0  0  0  0  0 46.40 6.23 430

Let’s see an example with three clusters, and how operate the function. We
will use Ward criteria.

``` r
df_hclust = hclust(d = dist(x = df) , method = "ward.D2" )
plot(df_hclust)
rect.hclust(tree = df_hclust, k= 3, border = "red")
```


<center>
<img src="https://raw.githubusercontent.com/ezulfica/School-Projects/main/r2-for-clustering-main/hclustdend.png" style="display: block; margin: auto;" />
</center>
That’s how our observations will be distributed.

Now let’s numerate our clusters. For each observation we will put a
number for the corresponding cluster (the cutree function from stats
package does it automatically).

We will directly input the vector in our database.

``` r
df$hclust = cutree(tree = df_hclust, k = 3)
head(df,10)
```

    ##                         T   TM  Tm      PP    V  RA SN TS FG GR   Lat Long  Alt
    ## AIGLE                11.1 16.1 6.0 1022.38  7.9 132 12  2  4  0 46.33 6.91  383
    ## ALTDORF              10.9 15.2 6.3 1244.85  8.5 143 15 12  4  0 46.86 8.63  451
    ## ALTENRHEIN-FLUGPLATZ 10.8 14.9 6.4 1152.40  8.0   0  0  0  0  0 47.48 9.38  398
    ## BASEL BINNINGEN      11.6 16.5 7.2  778.29  7.2  97  7  2  9  0 47.55 7.58  316
    ## BUCHS-AARAU           0.8 15.5 6.2  890.24  5.2 159 15  5 45  0 47.38 8.08  387
    ## CHANGINS             11.5 15.5 7.1 1054.85  9.5   0  0  0  0  0 46.40 6.23  430
    ## CHASSERAL             5.0  8.0 2.2 1436.37 30.5   0  0  0  0  0 47.08 7.03 1599
    ## CHUR-EMS             11.2 16.1 7.0 1218.91 10.5 132 26  5  1  0 46.86 9.53  556
    ## CIMETTA               6.6  9.6 3.9 1712.17  9.9   0  0  0  0  0 46.20 8.80 1648
    ## COMPROVASCO          11.2 16.5 6.9 1296.17  8.1   0  0  0  0  0 46.46 8.93  552
    ##                      hclust
    ## AIGLE                     1
    ## ALTDORF                   1
    ## ALTENRHEIN-FLUGPLATZ      1
    ## BASEL BINNINGEN           1
    ## BUCHS-AARAU               1
    ## CHANGINS                  1
    ## CHASSERAL                 2
    ## CHUR-EMS                  1
    ## CIMETTA                   2
    ## COMPROVASCO               1

Now we can calculate for each cluster, the barycenter, std and the
*R*<sup>2</sup>. Let’s see the evolution of the *R*<sup>2</sup> from one
cluster to 15

``` r
print(hclust_r2(data = df, col = 1:13, kmin = 1,kmax = 15))
```

    ##  [1] 0.0000000 0.4283217 0.6101955 0.7594276 0.8066128 0.8515218 0.8923180
    ##  [8] 0.9073920 0.9221483 0.9332886 0.9433325 0.9532297 0.9596635 0.9645998
    ## [15] 0.9689623

``` r
plot(hclust_r2(data = df, col = 1:13, kmin = 1,kmax = 15), type = "b", ylab = "R2", xlab = "number of cluster")
```
<center>
<img src="https://raw.githubusercontent.com/ezulfica/School-Projects/main/r2-for-clustering-main/rsquare.png" style="display: block; margin: auto;" />
</center>
