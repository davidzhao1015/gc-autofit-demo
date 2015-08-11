# Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process='PREPROCESSINGa'/'PROFILING' --outdir=<user_defined_output_directory>
# Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process=$1 $2 $3
Rscript ../APGCMS/apgcms_main.R --infiledir='sample' --lib.internal='SERUM' --internalstd='Ribitol' --process=$1 $2 $3
