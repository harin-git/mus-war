This repo contains code for the paper "**Global music discoveries reveal cultural shifts during the war in Ukraine**". To reproduce our analyses you will need some or all of these files.

> All data and materials are publicly archived at [OSF](https://osf.io/ra38k), with the exception of song lyrics data which belongs to the relevant right holders. All analysis scripts are available and maintained at <https://github.com/harin-git/mus-war>.

Please contact us if you run into any issues.

## Interactive visualizations

Interactive visualizations of figures in the paper can be found on [our website](https://musicdiscover.net)

![](images/interactive_ani-01.gif)

## Downloading the data

For faster performance, we recommend manually downloading all datasets (folders "Bootstraps" and "Dataset") from the OSF repository by following [this link](https://osf.io/ra38k/?view_only=32795758b14040cdb826d743023308fd). Once downloaded, unzip the folders and place them in the main working directory alongside the analysis scripts.

You can also download the data using the helper function `download_data.R`, which allows to automatically fetch data from the OSF repository using the [osfr](https://cran.r-project.org/web/packages/osfr/vignettes/getting_started.html) R package. You can define which type of data to download, corresponding directories will be created, and the download process will begin. Note, this can be slow depending on your region.

## Reproducing the analyses and plots

All analyses in the paper can be reproduced with the code posted here, in R and Python. Most of our computations are done using bootstrap simulations, which are often computationally costly. Therefore, pre-computed bootstrap files are available for downloaded by specifying `bootstrap_files = TRUE` using `download_data.R` . This will initiate in creating a directory called `/Boostraps` and download the bootstrap files.

Scripts to reproduce the statistics and plots of the main results are contained in the directory `/Analysis`, while secondary scripts for validation, extracting embeddings, etc are included in sub-directory `/Analysis/Extra`

When running all analyses and reproducing the figures, `SIMULATION` parameter in the scripts can be set to `FALSE` to bypass the bootstrap computation. Given the bootstrap data is downloaded, it will fetch the pre-computed bootstrap outcomes.

All analyses scripts begin by loading study-wide packages and high level functions through `utils.R`. These packages included in the script needs to be first installed to fully reproduce all analyses. See file `session_info.txt` for full specification used by the researcher.

All visualizations are produced using `ggplot2`, with the exception of diffusion networks, which was visualized using [Gephi](https://gephi.org/). Some figure elements are augmented manually (e.g., adding some labels) and/or include illustrations.

To infer the diffusion network, [NETINF](https://snap.stanford.edu/netinf/) needs to be installed. Once installed and pointed to the right path, `Main1d,e-infer_diffusion_network.R` function can be used as a wrapper to augment the raw data and format the cascades as input to NETINF.

## Codebook

### General

`nodeID` = unique identifier given to city

`trackID` = unique identifier given to song

`pre_post` = whether the dates are pre or post-invasion period (reference date = 2022-02-24)

`source` & `target` = used to identify the undirected and directed relations between cities (nodes). Corresponds to nodeID

`weight` = strength of the edge, defined by the frequency it appears in bootstraps

### Semantic features

`embedding` = word vectors extracted from Open AI embedding

`lyrics_language` = identified language of lyrics using [fasttext](https://fasttext.cc/docs/en/language-identification.html)

### Acoustic features

\*Full description of acoustic features can be found at [Essentia website](https://essentia.upf.edu/algorithms_reference.html).

`loudness` = EBU R128 loudness descriptors of an audio signal

`dynamic_complexity` = average absolute deviation from the global loudness level estimate on the dB scale

`spectral_complexity` = number of peaks in the input spectrum

`spectral_centroid` = the centroid is computed by dividing the norm of the resulting signal by the norm of the input signal. The centroid is given in hertz

`spectral_energy` = energy ratio of the specified band over the total energy

`zcr` = number of sign changes between consecutive signal values divided by the total number of values

`bpm` = beats per minute

`onset_rate` = number of onsets per second and their position in time for an audio signal

`chord_change_rate` = rate at which detected chords change

`major_minor` = detected major or minor key

`mfcc` = mel-frequency cepstrum coefficients of a spectrum by taking means across 12 bins
