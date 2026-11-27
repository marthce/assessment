configfile: "snakemake-config.yml"

rule all:
    "The default rule"
    input:
        "results/finalplot.png"

rule get_pmids:
    "Get list of pmids"
    input:
        "begin_snakefile.txt"
    output:
        "raw/pmids_all.txt"
        "raw/pmids_20.txt"
    shell:
        """
        cd code
        bash 0_get_pmids.sh
        """

rule data_download:
    "Download and clean data"
    input:
        all = "raw/pmids_all.txt",
        twenty = "raw/pmids_20.txt"
    output:
        "clean/clean_data.xml"
    shell:
        """
        cd code
        if [ {config[test]} = "True" ]; then
        bash 1_data_download.sh ../{input.twenty}
        else
        bash 1_data_download.sh ../{input.all}
        fi
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

