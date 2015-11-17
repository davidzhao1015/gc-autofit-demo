#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/saliva'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/saliva/ALKS.CDF" "${FILES}/ALKS.CDF"
ln -s "../../../example/saliva/BLK2.CDF" "${FILES}/BLK2.CDF"
ln -s "../../../example/saliva/S1.CDF" "${FILES}/S1.CDF"
ln -s "../../../example/saliva/S2.CDF" "${FILES}/S2.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='SALIVA' \
  --internalstd='Ribitol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing" \
  --AlkaneRT='10,11,12,13,14,15,16,17,18,19,20'

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='SALIVA' \
  --internalstd='Ribitol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling" \
  --AlkaneRT='10,11,12,13,14,15,16,17,18,19,20'
