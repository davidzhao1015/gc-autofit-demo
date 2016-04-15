#!/bin/bash
APGCMS_DIR=..
OUT_DIR='../test/urine_performance/EoE_Jul1114'
# FILES="${OUT_DIR}/infiles"
FILES="../example/performance_organicacid/EOE_Jul1114"
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
  --lib.internal='URINE' \
  --internalstd='Cholesterol' \

# Profiling
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${FILES}" \
  --infoFileDir="${OUT_DIR}/preprocessing" \
  --lib.internal='URINE' \
  --internalstd='Cholesterol' --process='PROFILING' \
  --outdir="${OUT_DIR}/profiling" --MFscore=100

