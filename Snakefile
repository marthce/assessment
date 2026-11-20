rule all:
    "The default rule"
    input:
        "visualisation.png"

rule data_download:
    "Download and clean data"
    input:
        "begin_snakefile.txt"
    output:
        "clean/raw_data_clean.xml"
    shell:
        """
        cd code
        bash 1_data_download.sh
        """

rule convert_data:
    "Convert xml to tsv"
    input:
        "clean/raw_data_clean.xml"
    output:
        "clean/extract.tsv"
    shell:
        """
        cd code
        Rscript 2_convert_data.R
        """

rule check:
    "Check tsv file"
    input:
        "clean/extract.tsv"
    output:
        "logs/check.log"
    shell:
        """
        cd code
        bash 3_check_tsv.sh
        """


rule visualise:
    "Make data visualisation"
    input:
        "clean/extract.tsv"
        "logs/check.log"
    output:
        "results/visualisation.png"
    shell:
        """
        cd code
        mkdir results
        Rscript 4_make_visualisation.R
        """

