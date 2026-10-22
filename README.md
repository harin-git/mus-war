This repo contains code for the paper "**Unsettled Times: Music Discovery Reveals Divergent Cultural Responses to War**". To reproduce our analyses you will need some or all of these files.

> All data and materials are publicly archived at [OSF](https://osf.io/ra38k), with the exception of song lyrics data which belongs to the relevant right holders. All analysis scripts are available and maintained at <https://github.com/harin-git/mus-war>.

Please contact us if you run into any issues.

## Interactive visualizations

Interactive visualizations of figures in the paper can be found on [our website](https://musicdiscover.net)

![](images/interactive_ani-01.gif)

## Downloading the data

For faster performance, we recommend manually downloading all datasets (folders "Bootstraps" and "Dataset") from the OSF repository by following [this link](https://osf.io/ra38k/?view_only=32795758b14040cdb826d743023308fd). Once downloaded, unzip the folders and place them in the main working directory alongside the analysis scripts.

You can also download the data using the helper function `download_data.R`, which allows to automatically fetch data from the OSF repository using the [osfr](https://cran.r-project.org/web/packages/osfr/vignettes/getting_started.html) R package. You can define which type of data to download, corresponding directories will be created, and the download process will begin. Note, this can be slow depending on your region.

## Reproducing the analyses and plots

All analyses in the paper can be reproduced with the code posted here, in R and Python. Most computations use bootstrap simulations, which can be computationally costly. Pre-computed bootstrap files can be downloaded by calling `download_data(bootstrapped_data = TRUE)` in `download_data.R`. They belong in the `Bootstraps/` directory.

Scripts to reproduce the statistics and plots of the main results are contained in the directory `/Analysis`, while secondary scripts for validation, extracting embeddings, etc are included in sub-directory `/Analysis/Extra`

When running all analyses and reproducing the figures, `SIMULATION` parameter in the scripts can be set to `FALSE` to bypass the bootstrap computation. Given the bootstrap data is downloaded, it will fetch the pre-computed bootstrap outcomes.

All analysis scripts begin by loading study-wide packages and high-level functions through `utils.R`. Install the packages used by the relevant scripts before running them. See `session_info.txt` for the original analysis environment.

### Figure exports

Run each `Analysis/Main…` script from the repository root with `Dataset/` and `Bootstraps/` available. Each script contains its analysis and one plotting/export path. The shared settings, statistical summaries and `plot_save()` are in `utils.R`.

`plot_save()` writes an editable SVG, a PDF generated from that SVG, a 300-dpi PNG and the plotted data as a CSV. Source Data names are explicitly set in each script with `source_name`, independently of artwork filenames, for example `Fig_2a.csv`, `Fig_4c_Ukraine.csv` and `Supplementary_Fig_4_Russia.csv`. Exploratory plots without a manuscript figure number write to `exploratory_data/`, not the numbered Source Data folder. Outputs go to `Plots/Main/`, `Plots/SI/` and `Plots/source_data/`; set `PLOT_DIR` to use another directory. Statistical results are printed by the relevant analysis script. `Main3d-acoustic_features.R` reports the 16 song-level feature comparisons with Bonferroni correction and Bayes factors, without producing a figure.

Plotting packages include `svglite`, `rsvg`, `sf`, `maps`, `uwot`, `quanteda`, `ggwordcloud`, `philentropy`, `MASS` and `patchwork`. `BayesFactor` is used for evidence about null results. UMAP coordinates and word clouds use fixed seeds; UMAP uses one thread. Saved bootstrap inputs must match the current data and code. In particular, the acoustic JSD bootstrap uses kernel width 1 and UMAP seed 42.

`Dataset/`, `Bootstraps/`, `out/`, `Plots/` and `logs/` are local inputs/outputs excluded from git. Running the scripts does not upload them. Editorial diagnostics and working audit scripts are not part of this repository.

All visualisations are produced using `ggplot2`, except the diffusion networks in Fig. 5c,d, which are exported separately from [Gephi](https://gephi.org/). Manual figure assembly, illustrations and labels added outside R are not recreated by the runner.

To infer the diffusion network, [NETINF](https://snap.stanford.edu/netinf/) needs to be installed. Once installed and pointed to the right path, `Main5c,d-infer_diffusion_network.R` function can be used as a wrapper to augment the raw data and format the cascades as input to NETINF.

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
