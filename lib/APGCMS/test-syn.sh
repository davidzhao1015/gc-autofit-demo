#!/bin/bash
APGCMS_DIR=.
OUT_DIR='test/syt'
FILES="${OUT_DIR}/infiles"
rm -r $OUT_DIR/*

# Setup
mkdir -p $FILES
ln -s "../../../example/syt/Alk16Jan6.mzXML" "${FILES}/ALKSTD.mzXML"
ln -s "../../../example/syt/blk_BSTFA.mzXML" "${FILES}/blk.mzXML"
ln -s "../../../example/syt/syt_41_44.mzXML" "${FILES}/syt_41_44.mzXML"

# Prepoocessing
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --lib.internal='URINE' \
  --internalstd='NONE' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="${OUT_DIR}/preprocessing" 

# Profiling
 Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${APGCMS_DIR}/${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --internalstd='NONE' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling" \
  --MFscore=100
