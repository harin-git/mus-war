All data used in the study are deposited here. Analysis scripts to reproduce the results and figures are maintained at https://github.com/harin-git/mus-war. Data comes in two formats: csv & rds. Bootstrapped data only includes the rds version as only used for reproducing the results.

Below lists all informations regarding the column headings of the data. If you run into any issue or require further clarification, do not hesitate to reach out at harin.lee@ae.mpg.de

## Codebook

### General

`nodeID` = unique identifier given to city

`trackID` = unique identifier given to song

`pre_post` = whether the dates are pre or post-invasion period (reference date = 2022-02-24)

`source` & `target` = used to identify the undirected and directed relations between cities (nodes). Corresponds to nodeID

`weight` = strength of the edge, defined by the frequency it appears in bootstraps

### Semantic features

`embedding` = word vectors extracted from Open AI embedding

`lyrics_language` = identified language of lyrics using fasttext (https://fasttext.cc/docs/en/language-identification.html)

### Acoustic features

Full description of acoustic features can be found at https://essentia.upf.edu/algorithms_reference.html

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