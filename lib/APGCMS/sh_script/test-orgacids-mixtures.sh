#!/bin/bash
APGCMS_DIR=.
# OUT_DIR='test/orgacids_mix2/oneISTD'
# FILES="example/organicacid_calib_Feb1816/oneISTD"
OUT_DIR='test/orgacids_mix2/twoISTD'
FILES="example/organicacid_calib_Feb1816/twoISTD"
rm -r $OUT_DIR/*

# Setup
# mkdir -p $FILES
# ln -s "../../../example/orgacids_mix1/alk8-40.CDF" "${FILES}/ALKSTD.CDF"
# ln -s "../../../example/orgacids_mix1/Blank.mzXML" "${FILES}/Blank.mzXML"
# ln -s "../../../example/orgacids_mix1/mix1-1.CDF" "${FILES}/mix1-1.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --useblank=TRUE --process='PREPROCESSING' --outdir="${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --internalstd='Cholesterol (ISTD)' 
  # --internalstd='Succinic acid-ISTD'  

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --outdir="${OUT_DIR}/profiling" \
  --internalstd='Cholesterol (ISTD)' --process='PROFILING' \
  --MFscore=100
  # --internalstd='Succinic acid-ISTD' --process='PROFILING' \
  # --MFscore=100

