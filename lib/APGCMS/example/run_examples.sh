#!/bin/sh

# Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process='PREPROCESSINGa'/'PROFILING' --outdir=<user_defined_output_directory>
# Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process=$1 $2 $3

if [ $# -ne 2 ] 
then
   echo "\n\t[Usage] sh run_examples.sh SERUM [0:Preprocessing, 1:Profiling]\n\n"
   exit 1
fi


if [ $1 = "SERUM" ] 
then
    echo "\n## Running SERUM Example with Internal Library\n\n"
    if [ $2 = 0 ] 
    then
        echo "##     Preprocessing"
        Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process='PREPROCESSING'
    elif [ $2 = 1 ] 
    then
        echo "##     Profiling"
        Rscript ../APGCMS/apgcms_main.R --infiledir='serum' --lib.internal='SERUM' --internalstd='Ribitol' --process='PROFILING' --infoFileDir='/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/example/serum_result'
    else
        echo "\n\t[Usage] sh run_examples.sh SERUM [0:Preprocessing, 1:Profiling]\n"
        exit 1
    fi
elif [ $1 = "URINE" ] 
then
    echo "\n## Running URINE Example with Internal Library\n\n"
    if [ $2 = 0 ] 
    then
        echo "##     Preprocessing"
        Rscript ../APGCMS/apgcms_main.R --infiledir='urine' --lib.internal='URINE' --internalstd='Cholesterol' --process='PREPROCESSING' --infoFileDir='serum_result'
    elif [ $2 = 1 ] 
    then
        echo "##     Profiling"
        Rscript ../APGCMS/apgcms_main.R --infiledir='urine' --lib.internal='URINE' --internalstd='Cholesterol' --process='PROFILING' --infoFileDir='/Users/beomsoo/gcmsProfiling/gc-autofit/lib/APGCMS/example/serum_result'
    else
        echo "\n\t[Usage] sh run_examples.sh SERUM [0:Preprocessing, 1:Profiling]\n"
        exit 1
    fi
else
    echo "\n\t[Usage] sh run_examples.sh SERUM [0:Preprocessing, 1:Profiling]\n\n"
    exit 1
fi
