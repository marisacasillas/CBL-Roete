# Contents of CBL-Roete

This repository contains the python code for our implementation of the CBL-model, the input- and output files used in the paper, as well as the R scripts for analysis of the model output.

**About the files**

If you wish to run our implementation of the CBL-model:
    1) First use run.sh to run CBLmodel.py, it using the input files from the model_input folder. This generates the model output, but includes an older version of our controlled scoring measure, saved in the model_output folder.
    2) Run modifyscores.py over the model output to compute the final version of the controlled scoring measure.

If you wish to run our analysis-scripts over model_ouput you've generated yourself, copy the model_output files to the correct subfolders in the analysis-folder. You can then run our R-scripts over the new model output.

The script preprocess_corpusfiles.py was used to extract the model input files from the corpus on Childes.

**How to cite**

I. Roete, S.L. Frank, P. Fikkert and M. Casillas (in prep). Modeling the influence of language input statistics on children's speech production
