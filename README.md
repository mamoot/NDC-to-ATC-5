# NDC-to-ATC-5
Converts NDC to ATC 5 codes, annotates. visualizes, and infers clinical conditions from a list of ndc codes.
This process requires inputs: 
(sample_ndc.csv, atc_class.csv, atc_level_2.csv, disease_atc5.csv),
and can be run inidividually via running ndc_map.R, ATC_5_count_annotation_and_QC.R,
  visualization.R, and Medical_condition_infer.R, or collectively via ndc_atc5_visualization_conditions.R script.
The outputs are an ATC count file, a map of NDC to ATC codes, an annotated table with NDC, ATC5 codes and levels, and the original
input file annotated with ATC5 codes, names, levels, and inferred clinical conditions.
