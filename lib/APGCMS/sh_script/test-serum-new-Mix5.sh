#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/serum_Mix5'
FILES="example/serum_new_20150225/Mix5_mzXML"
# FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
# mkdir -p $FILES
# ln -s "../../../example/serum/ALKSTD.CDF" "${FILES}/ALKSTD.CDF"
# ln -s "../../../example/serum/GSS-BLANK.CDF" "${FILES}/GSS-BLANK.CDF"
# ln -s "../../../example/serum/GSS-1R.CDF" "${FILES}/GSS-1R.CDF"
# ln -s "../../../example/serum/GSS-2R.CDF" "${FILES}/GSS-2R.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --useblank=TRUE --process='PREPROCESSING' --outdir="${OUT_DIR}/preprocessing" \
  --lib.internal='SERUM' \
  --internalstd='Ribitol' \

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='SERUM' \
  --internalstd='Ribitol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling" --MFscore=100

