#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/urine'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/urine/Alkstd.mzXML" "${FILES}/Alkstd.mzXML"
ln -s "../../../example/urine/Blank.mzXML" "${FILES}/Blank.mzXML"
ln -s "../../../example/urine/C001.mzXML" "${FILES}/C001.mzXML"
# ln -s "../../../example/urine/C002.mzXML" "${FILES}/C002.mzXML"
# ln -s "../../../example/urine/C003.mzXML" "${FILES}/C003.mzXML"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='URINE' \
  --internalstd='Cholesterol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing" 

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --internalstd='Cholesterol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling"
