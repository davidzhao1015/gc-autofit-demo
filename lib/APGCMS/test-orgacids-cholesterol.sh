#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/orgacids_cholesterol'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/Example_Jul0914_mzXML/Alkstd.mzXML" "${FILES}/ALKSTD.mzXML"
ln -s "../../../example/Example_Jul0914_mzXML/Blank2.mzXML" "${FILES}/Blank.mzXML"
ln -s "../../../example/Example_Jul0914_mzXML/C001.mzXML" "${FILES}/Coo1.mzXML"

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
  --outdir="${OUT_DIR}/profiling" \
  --MFscore=100
