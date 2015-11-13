#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/serum'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/serum/ALKSTD.CDF" "${FILES}/ALKSTD.CDF"
ln -s "../../../example/serum/GSS-BLANK.CDF" "${FILES}/GSS-BLANK.CDF"
ln -s "../../../example/serum/GSS-1R.CDF" "${FILES}/GSS-1R.CDF"
# ln -s "../../../example/serum/GSS-2R.CDF" "${FILES}/GSS-2R.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='SERUM' \
  --selectedCmpd='HMDB00067,HMDB00094,HMDB00115,HMDB00673,HMDB00827,HMDB02142,HMDB02329,HMDB00508' \
  --internalstd='Ribitol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing" 

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='SERUM' \
  --selectedCmpd='HMDB00067,HMDB00094,HMDB00115,HMDB00673,HMDB00827,HMDB02142,HMDB02329,HMDB00508' \
  --internalstd='Ribitol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"
