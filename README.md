

# Ponman : A package for BioArgo data Analysis

BioArgo floats are 


## Get data to ponman

Getting data to ponman can attain in two ways. Since API programing is not available, the database can be access by FTP(File transfer protocol) or you can manually download to the working directory. 

```
#extract data from a location and time
extractlocation_time<- get_data2ponman(mode = "geotime",location = "indian_ocean",year = "2010",month = "02")

```

You will get an output with a choice to download multiple to single files

```
get_data2ponman(mode = "geotime",location = "indian_ocean",year = "2010",month = "02")

 [1] "20100201_prof.nc" "20100202_prof.nc" "20100203_prof.nc" "20100204_prof.nc" "20100205_prof.nc" "20100206_prof.nc"
 [7] "20100207_prof.nc" "20100208_prof.nc" "20100209_prof.nc" "20100210_prof.nc" "20100211_prof.nc" "20100212_prof.nc"
[13] "20100213_prof.nc" "20100214_prof.nc" "20100215_prof.nc" "20100216_prof.nc" "20100217_prof.nc" "20100218_prof.nc"
[19] "20100219_prof.nc" "20100220_prof.nc" "20100221_prof.nc" "20100222_prof.nc" "20100223_prof.nc" "20100224_prof.nc"
[25] "20100225_prof.nc" "20100226_prof.nc" "20100227_prof.nc" "20100228_prof.nc"
Do you want download all these files? 

1: Yes
2: No

Selection: 

```

You can take single files by just typing name without ""


```
Selection: 2
Type the file name you want to download: 20100226_prof.nc

```

## Reading single files

After downloading the datasets in working directory. It could be simply readed by the function "ExtractBioArgo"

```
Readprofile_1 <- ExtractBioArgo(bioarg = "2902092_001_20180531065847123.nc")


Horray!!, All paramters available
          Date latitude longitude cycle.no pressure temperature salinity    oxygen chlorophyll backscatter
1   2013-02-24 19.08436  66.54972        1      0.6      26.051   36.294 182.54312    -0.02920     0.00028
2   2013-02-24 19.08436  66.54972        1      1.6      26.050   36.298 182.45311     0.31755     0.00040
3   2013-02-24 19.08436  66.54972        1      2.8      26.049   36.299 182.45979     0.29930     0.00108
4   2013-02-24 19.08436  66.54972        1      3.6      26.054   36.295 182.31361     0.29200     0.00107
5   2013-02-24 19.08436  66.54972        1      4.4      26.054   36.296 182.23773     0.32850     0.00116
6   2013-02-24 19.08436  66.54972        1      5.2      26.053   36.297 182.25000     0.32850     0.00104
7   2013-02-24 19.08436  66.54972        1      6.5      26.052   36.298 182.19359     0.31755     0.00108
8   2013-02-24 19.08436  66.54972        1      7.8      26.053   36.298 182.06905     0.30660     0.00104
9   2013-02-24 19.08436  66.54972        1      8.6      26.053   36.298 182.03868     0.32850     0.00116
10  2013-02-24 19.08436  66.54972        1      9.6      26.052   36.299 182.01314     0.31390     0.00097
11  2013-02-24 19.08436  66.54972        1     12.7      26.053   36.298 181.91534     0.35916     0.00128
12  2013-02-24 19.08436  66.54972        1     17.5      26.054   36.299 182.02058     0.42121     0.00109
[ reached getOption("max.print") -- omitted some rows ]

```












