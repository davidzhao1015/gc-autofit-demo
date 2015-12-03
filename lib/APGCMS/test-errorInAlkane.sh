#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/errorInAlkane'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/errorInAlkane/urine_alkane.CDF" "${FILES}/urine_alkane.CDF"
ln -s "../../../example/errorInAlkane/blank2_sp5_inj280.CDF" "${FILES}/blank2_sp5_inj280.CDF"
ln -s "../../../example/errorInAlkane/urine1b_sp5_inj280.CDF" "${FILES}/urine1b_sp5_inj280.CDF"
ln -s "../../../example/errorInAlkane/urine2b_sp5_inj280.CDF" "${FILES}/urine2b_sp5_inj280.CDF"
ln -s "../../../example/errorInAlkane/sample.CDF" "${FILES}/sample.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='URINE' --internalstd='Cholesterol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing"

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' --internalstd='Cholesterol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"

