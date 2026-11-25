rule all:
    "The default rule"
    input:
        "results/finalplot.png"

rule data_download:
    "Download and clean data"
    input:
        "begin_snakefile.txt"
    output:
        "clean/clean_data.xml"
    shell:
        """
        cd code
        bash 1_data_download.sh
        """

rule convert_data:
    "Convert xml to tsv in R"
    input:
        "clean/clean_data.xml"
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
        bash 3_check_tsv.sh > ../logs/check.log
        """


rule visualise:
    "Make data visualisation"
    input:
        "clean/extract.tsv",
        "logs/check.log"
    output:
        "results/finalplot.png"
    shell:
        """
        cd code
        Rscript 4_visualisation.R
        """

