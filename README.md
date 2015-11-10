# The Rarecoal package
This software implements tools to process and analyse genome sequencing data. The key program is called `rarecoal`, other programs in this package help with the data processing necessary to use `rarecoal`. A preprint on a project using this software can be found [here](http://biorxiv.org/content/early/2015/07/17/022723), and the mathematical derivations can be found under `doc/rarecoal.pdf`.

## Rarecoal program
Rarecoal reads data in the form of a joint allele frequency histogram for rare alleles. An example file can be found under `testData/5popSplit_combined.txt`. The first lines of this file are:

    N=200,200,200,200,200
    MAX_M=10
    0,0,0,0,0 1980590858
    HIGHER 9115605
    0,0,0,0,1 2068825
    0,0,0,1,0 1342429
    0,1,0,0,0 1329776
    0,0,1,0,0 900338
    0,2,0,0,0 317628

The first line denotes the number of haploid samples in each subpopulations. In this case there are five subpopulations, each with a hundred diploid individuals, so 200 haplotypes each, and 1000 haplotypes in total. The second line denotes the maximum total allele count of variants, here 10, so up to allele freuqency 1%. The following lines are pairs of patterns and numbers. The patterns describe how a mutation is distributed across the five populations. For example, for a variant of allele frequency 6, there could be a pattern of the form `0,2,2,1,1`, which means that of there are zero mutations in the first, two individuals in the second and third population, one in the fourth and one in the fifth population carrying that mutation. The special pattern `HIGHER` summarizes all variants with allele frequency higher than denoted in the second line of the file. The number after each pattern is the number of sites in the data set that exhibit this particular pattern.

A typical workflow of how to generate this format from VCF files is described below using `vcf2FreqSum` and `freqSum2Histogram`.

You can list the command line options for `rarecoal` by typing `rarecoal -h`, and for a specific subcommand, say `view` by typing `rarecoal view -h`.

### rarecoal view
You can use this subcommand to read in a histogram file and view it up to a specific allele count, using the `-m` option. For example, you can view the number of all singletons and doubletons in the example data set via:

    rarecoal view -m2 -i testData/5pop

## Utilities for processing

### vcf2FreqSum

### mergeFreqSum

### groupFreqSum

### extractInFreqSum

### downSampleFreqSum

### freqSum2Histogram

### ms2hist

### sampleHist

### combineHistograms