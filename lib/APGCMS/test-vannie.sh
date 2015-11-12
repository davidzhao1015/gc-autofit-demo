#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/vannie_Oct29'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/vannie_Oct29/ALKSTD.CDF" "${FILES}/ALKSTD.CDF"
ln -s "../../../example/vannie_Oct29/BLK.CDF" "${FILES}/BLK.CDF"
ln -s "../../../example/vannie_Oct29/PATA1B.CDF" "${FILES}/PATA1B.CDF"
ln -s "../../../example/vannie_Oct29/PATA3A.CDF" "${FILES}/PATA3A.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='URINE' --internalstd='Cholesterol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing"

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' --internalstd='Cholesterol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"
