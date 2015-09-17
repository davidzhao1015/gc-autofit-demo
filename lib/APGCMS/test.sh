#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/serum'
rm -r $OUT_DIR
# Prepeocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${APGCMS_DIR}/example/serum" \
  --lib.internal='SERUM' --internalstd='Ribitol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing"

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${APGCMS_DIR}/example/serum" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='SERUM' --internalstd='Ribitol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"
