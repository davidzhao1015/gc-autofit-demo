#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/errorInSample'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
# ln -s "../../../example/errorInSample/alk8-40_sp5_inj280.CDF" "${FILES}/alk8-40_sp5_inj280.CDF"
ln -s "../../../example/errorInSample/Alkstd.mzXML" "${FILES}/Alkstd.mzXML"
ln -s "../../../example/errorInSample/blank2_sp5_inj280.CDF" "${FILES}/blank2_sp5_inj280.CDF"
ln -s "../../../example/errorInSample/urine1b_sp5_inj280.CDF" "${FILES}/urine1b_sp5_inj280.CDF"
ln -s "../../../example/errorInSample/urine2b_sp5_inj280.CDF" "${FILES}/urine2b_sp5_inj280.CDF"
ln -s "../../../example/errorInSample/sample.CDF" "${FILES}/sample.CDF"
ln -s "../../../example/errorInSample/usample.CDF" "${FILES}/usample.CDF"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='URINE' --internalstd='Cholesterol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing"

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' --internalstd='Cholesterol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"

