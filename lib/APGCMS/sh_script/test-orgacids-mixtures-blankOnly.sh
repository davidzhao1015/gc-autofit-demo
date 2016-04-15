#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/orgacids_mix2'
FILES="example/FEB18-16-CDF"
rm -r $OUT_DIR/*

# Setup
# mkdir -p $FILES
# ln -s "../../../example/orgacids_mix1/alk8-40.CDF" "${FILES}/ALKSTD.CDF"
# ln -s "../../../example/orgacids_mix1/Blank.mzXML" "${FILES}/Blank.mzXML"
# ln -s "../../../example/orgacids_mix1/mix1-1.CDF" "${FILES}/mix1-1.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-2.CDF" "${FILES}/mix1-2.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-3.CDF" "${FILES}/mix1-3.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-4.CDF" "${FILES}/mix1-4.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-5.CDF" "${FILES}/mix1-5.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-6.CDF" "${FILES}/mix1-6.CDF"
# ln -s "../../../example/orgacids_mix1/mix1-7.CDF" "${FILES}/mix1-7.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --useblank=TRUE --process='PREPROCESSING' --outdir="${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --internalstd='Cholesterol (ISTD)'  
#  --internalstd='Succinic acid-ISTD'  

