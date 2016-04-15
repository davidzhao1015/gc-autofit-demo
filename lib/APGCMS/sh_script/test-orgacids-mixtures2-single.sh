#!/bin/bash
APGCMS_DIR=..
OUT_DIR='../test/orgacids_mix2/oneISTD'
FILES="../example/organicacid_lib_Mix2_ISTD2_Feb1816/oneISTD"
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
  --internalstd='Succinic acid'  

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --outdir="${OUT_DIR}/profiling" \
  --internalstd='Succinic acid' --process='PROFILING' \
  --MFscore=100

