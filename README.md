Repository for reproducing the paper **"Coherent streamflow variability in Monsoon Asia over the past eight centuries---links to oceanic drivers"** (Nguyen *et al*, submitted to Geophysical Research Letters).

# Reconstruction

I have provided all reconstruction results in the folder `results`. If you want to rerun the reconstruction code, please backup the `results` folder first.

If you have access to a computing cluster, you can write a shell script to run the file `KWF_ensemble_cluster.R`. I provided `reconstruction.sh` as an example. This script is written to run with a job array. You can customize it according to your cluster's specifications.

If you don't have access to a computing cluster, you can run the file `KWF_ensemble_local.R`. This script will overwrite the files in the folder `results`.

```{r}
source('KWF_ensemble_local.R')
```

**Note** Due to restrictions, I cannot share the instrumental data from China. Therefore, you will not be able to reproduce the results for Chinese stations, unfortunately.

# Results

The script `for_paper.R` reproduces the figures in the main paper.

```{r}
source('for_paper.R')
```

The script `for_SI.R` reproduces the figures in the Supporting Information.

```{r}
source('for_SI.R')
```
