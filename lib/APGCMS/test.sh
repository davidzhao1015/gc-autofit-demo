#!/bin/bash
APGCMS_DIR=.
Rscript "${APGCMS_DIR}/APGCMS/apgcms_main.R" --infiledir="${APGCMS_DIR}/example/serum" \
  --lib.internal='SERUM' --internalstd='Cholesterol' --useblank=TRUE --process='PREPROCESSING' \
  --outdir="test"
