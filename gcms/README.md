This is the core program of GC-autofit program.



preprocessing is generating spectrum plot:
	spectrum file is .CDF


profiling is finding the potential match molecule



Using http to store the peak from annotating raw spectrum data
https://stackoverflow.com/questions/28924820/send-a-post-request-using-httr-r-package/28926498




Test run:
Rscript ../APGCMS/apgcms_main.R --infiledir='inputfile_dir' --lib.internal='SERUM' --internalstd='Ribitol' --process='PREPROCESSING --outdir=<user_defined_output_directory>

# Profiling

Rscript ../APGCMS/apgcms_main.R --infiledir='inputfile_dir' --lib.internal='SERUM' --internalstd='Ribitol' --process='PROFILING' --outdir='user_defined_output_dir' --infoFileDir='user_defined_output_directory_for_preprocessing'
