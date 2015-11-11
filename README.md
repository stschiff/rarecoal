# The Rarecoal package
This software implements tools to process and analyse genome sequencing data. The key program is called rarecoal, other programs in this package help with the data processing necessary to use rarecoal. A preprint on a project using this software can be found [here](http://biorxiv.org/content/early/2015/07/17/022723), and the mathematical derivations can be found under `doc/rarecoal.pdf`.

## Installation instructions
* Download the Haskell install tool stack, available [here](https://github.com/commercialhaskell/stack).
* Download this repository and unpack it.
* Change directory into this repository
* (Optional) run `stack setup` to install the correct compiler. If you are not sure whether you have the compiler already, you can directly go to the next step. It will tell you to run `stack setup` if you need to.
* Run `stack build`. This will download all the dependencies and may take a while. You may get an error about missing software such as pkg-config, or gsl, which then need to be installed using your standard package manager, such as `apt-get` on Linux or `brew` on a Mac. As soon as you have installed those, just run `stack build` again to continue where you left before the error.
* Run `stack install` to copy the executables into "~/.local/bin". You should then add this directory to your path.

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

I provide tools for generating this format from VCF files within this package, notably `vcf2FreqSum` and `freqSum2Histogram`.

You can list the command line options for `rarecoal` by typing `rarecoal -h`, and for a specific subcommand, say `view` by typing `rarecoal view -h`.

All time inputs and outputs in rarecoal are measured in units of 2N<sub>0</sub> generations, and all population sizes are measured
in units of N<sub>0</sub>. Here, N<sub>0</sub> is indirectly set using the `--theta` option in any rarecoal command. Here, `--theta`
is defined as 2N<sub>0</sub>µ, where µ is the per-generation mutation rate. (Sorry for this factor 2 difference to the standard
definition of theta in population genetics. It was a historical mistake which I am reluctant to fix because I defined it this way in all my mathematical derivations). By default, theta is set to 0.0005, which corresponds to N<sub>0</sub>=20,000 assuming a per-generation mutation rate of µ=1.25e-8.

A note on multithreading: Without specifying the number of threads via `--nrThreads`, rarecoal will run on parallel using all the processors on your system. In my experience, the speed increases fairly linearly up to around 8 processors. Up to around 16 processors, there is still some improvement, but much slower than linear. So I would usually recommend running on 8 processors at most, although you can try to go higher if you can afford it.

### rarecoal view
You can use this subcommand to read in a histogram file and view it up to a specific allele count, using the `-m` option. For example, you can view the number of all singletons and doubletons in the example data set via:

    rarecoal view -m2 -i testData/5popSplit_combined.txt

You can also use the `-N` option to to adjust the total number of called sites, which will affect the number of non-variant sites (the special pattern `0,0,0,0,0` in the case for five populations) such that the total number of sites matches the number given.

### rarecoal maxl
This command instructs rarecoal to perform a numerical optimization of the parameters of a model given the data. You first need to specify a model for your data, which is done by writing a template. As an example, consider the template in `testData/5popSimTemplateFixedPop.txt `:

    t01,t23,t24,t02,P
    P 0.0,0,<P>
    P 0.0,1,<P>
    P 0.0,2,<P>
    P 0.0,3,<P>
    P 0.0,4,<P>
    J <t01>,0,1
    J <t23>,2,3
    J <t24>,2,4
    J <t02>,0,2

The first line of a template file contains all the parameters, comma-separated. A parameter has to start with a letter, but can than also contain numbers. The second and following lines are event specifications which fully define your model. Events always occur at a particular time, which is always counted backwards into the past. The first five lines above specify initial population sizes. For example, the third line `P 0.0,2,<P>` says that at time 0.0 (in the present), the population size at branch 2 should be set to `P`, which is a parameter of your model. You could also specify population size changes for non-zero times, which would then set a new population size in the given branch at the given time. The last four lines in the example specify branch-joins, defined backwards in time: At time `<t01>`, we move all lineages from branch 1 into branch 0. At time `<t23>` we move all lineages from branch 3 into branch 2, and so on. These events then specify your tree topology.

There are three event types:

* Population size changes. The syntax for a population size change is `P t,i,p`, where `P` is the capital letter `P`, `t` is the time at which the new population size should be set, `i` is the index of the branch, starting with 0, and `p` is the new population size. You can put parameters of your model as time and/or as new population sizes using the `<>` brackets as shown in the example.
* Population Joins: The syntax is `J t,k,l`, where `J` is the capital letter `J`, `t` is the time of the join, `k` is the target branch and `l` is the source branch, looking backwards in time. Again, parameters are allowed using the `<>`-syntax for the time of the event.
* Population Joins with Population size change: The syntax is `K t,k,l,p`, where `t`, `k` and `l` are the same as above, and `p` also specifies a new population size for the target branch at that time.

An example using the last event type is given in `testData/5popSimTemplate.txt`. The model in these templates described the tree ((0,1),((2,3),4)) in Newick Format, which is also the tree used to simulate the data in `testData/5popSplit_combined.txt`.

The `rarecoal maxl` program takes a number of command line options, you can list them using `rarecoal maxl -h`.
A usage example using the example data is:

    rarecoal maxl -T testData/5popSimTemplateFixedPop.txt -x [0.001,0.002,0.003,0.004,1] -m 4 -i testData/5popSplit_combined.txt

This starts the maximization with the parameters given by `-x`, which correspond to the parameters listed in the first line of the template file. It is important to set the option `-m` to something not too high (`-m 4` was used throughout the publication), otherwise the run time will be very long since rarecoal needs to compute the likelihood of all possible patterns up to higher allele counts.




### rarecoal mcmc
This works very similarly to the `maxl` command. However, instead of a heuristic optimization, we perform a full MCMC, refines maximum likelihood estimates obtained by `maxl`, and also gives the median and 95% range of the posterior distribution for each parameter. Again, type `rarecoal mcmc -h` to get an overview about all the command line options. An example usage is

    rarecoal mcmc -T testData/5popSimTemplateFixedPop.txt -x [0.001,0.002,0.003,0.004,1] -m 4 -i testData/5popSplit_combined.txt 

A particularly useful parameter for this command is the `--paramsFile` option, which lets you specify the output of a previous maxl or mcmc run as the starting point for MCMC. A typical work flow is to first run `maxl` on a particular model to get close to the maximum, and then use `mcmc` with `--paramsFile` to begin at the point found by `maxl`.

### rarecoal logl
The command simply takes a model and a histogram and spits out the log likelihood for that particular model. In addition, you can specify an output file for the complete joint allele frequency spectrum, which is useful if you want to compare empirical with theoretical spectra (fits). A typical command line is:

    rarecoal logl -T testData/5popSimTemplateFixedPop.txt -x [0.001,0.002,0.003,0.004,1] -m 4 -i testData/5popSplit_combined.txt -s model_spectrum.txt

which gives the likelihood and writes the probability for each allele frequency pattern into the filed specified by `-s`. Note that you can again use `--paramsFile` to input the output of a previous `mcmc` or `maxl` run.

In contrast to `maxl` and `mcmc`, which need a model template, `logl` also accepts model specifications via the command line, given by the options `-j` and `-p`, as explained in the help text. Note that for setting events of type "K" (see model template syntax above) on the command line, you need to be give both of `-j` and `-p`.

### rarecoal prob
A little program which lets you compute the probability for a particular pattern using rarecoal. Similarly to `logl`, it accepts models via a template and via command line options. An example is:

    rarecoal prob -j 0.02,0,1 [100,100] [1,1]

which computes the probability to observe a shared doubleton in two populations, each with 100 sampled haplotypes.

### rarecoal find
Rarecoal find performs a brute force search for the branch point of a subpopulation branch, given a partial model. There are two use cases for this program. First, you can use `rarecoal find` to iteratively find the optimal model topology for a number of populations. For example, let's say I already know how the first four populations in my five-population example data set are related, and let's say their so-far best four-population model is given by a series of joins, specified on the command line by `-j 0.0025,0,1 -j 0.005,2,3 -j 0.01,0,2`. We can then use `rarecoal find` to find the maximum likelihood merge-point for the fifth population onto that model, via:

    rarecoal find -j 0.0025,0,1 -j 0.005,2,3 -j 0.01,0,2 -m 4 -q 4 -f out_eval.txt -b 0 -i testData/testData/5popSplit_combined.txt

Here, the parameter `-q 4` specifies that we are trying to find the merge-point of branch 4 (which is 0-based indexed, so the fifth population), `-f` gives the file where to write the likelihood of each tried point to, and `-b` gives the (scaled) sampling age of the branch at question, which should be 0 for modern data, and a positive number for ancient samples.

The second use case is for placing individual samples onto a tree. Let's say you have optimized a full model including population sizes in each branch for your five populations, with the final MCMC output stored in `mcmc_out.txt`. You have generated a new histogram for these five populations plus one individual of unknown ancestry. Let's say your individual is an ancient sample, with a sampling age 2,000 years before present. Scaling with 2N<sub>0</sub>=20,000 (see scaling note above), and a generation time of 29 years, the scaled age is then 0.00172.Then for mapping the individual, you would run:

    rarecoal find -T testData/5popSimTemplate.txt --paramsFile mcmc_out.txt -m 4 -q 4 -f mapping_out.txt -b 0.00172 -i testData/testData/5popSplit_combined.txt

## Utilities for processing

All tools described here have an online help, as shown by typing the command name and then `-h`.

### vcf2FreqSum
This tool converts a multi-sample VCF file, read from stdin, to a simpler file which I call "freqSum". The first lines of an example are:

1	10539	C	A	0	0	0	0	0	0	0	0	0	0	0	0	0	0
1	11008	C	G	0	0	0	0	1	0	1	0	0	0	0	0	0	0
1	11012	C	G	0	0	0	0	1	0	1	0	0	0	0	0	0	0
1	13110	G	A	1	1	0	0	0	0	0	0	0	0	0	0	0	0
1	13116	T	G	0	0	0	0	0	1	0	1	0	0	0	1	0	0


Here, the first four columns are chromosome, position, reference allele, alternative allele. The following columns give the alternative allele count in each individual. Note that this tool fails if you pass indels or multi-allelic SNPs. You should therefore combine this script with a filtering step, for example using bcftools (v1.2):

    bcftools view -m2 -M2 -c1 -v snps -S <sample_list.txt> <vcf_file> | vcf2FreqSum > output.txt

### groupFreqSum
This tools groups freqSum columns (read from stdin) into sample groups, giving the total allele count of all samples in that group. It expects a comma-separated list, surrounded by square-brackets, of how to merge samples. For example, if you have a freqSum file for 500 individuals, as generated from vcf2freqSum, and you would like to merge them into 5 groups of 100 samples each, you would use 

    groupFreqSum -n [100,100,100,100,100] < freqSumFile.txt > groupedFreqSumFile.txt

The output from this command line will still be a freqSum file, but with groups instead of individuals given in the columns. The advantage of this data format is that it is very general with respect to individuals vs. groups. In the first example above (at vcf2FreqSum), the output contained a single column for each individual, with allele counts not exceeding 2 (for homozygous non-ref genotype), naturally. However, once you used `groupFreqSum`, you end up with columns describing the allele counts in a whole group. The format is still the same.

### mergeFreqSum
This tools merges two freqSum files. It takes four arguments, as shown by typing `mergeFreqSum -h`:

    Usage: mergeFreqSum freqSumFile1 freqSumFile2 <n1> <n2>
    Available options:
      -h,--help                Show this help text
      freqSumFile1             file 1, put - for stdin
      freqSumFile2             file 2
      <n1>                     number of populations in file 1
      <n2>                     number of populations in file 2

Here, n1 and n2 are the number of sample/group columns (which can also denote populations or groups if `groupFreqSum` has been used).
If a site is present in one file but not the other, all missing genotypes are assumed to be homozygous reference.

### downSampleFreqSum
This can be used to downsample the number of samples in a particular sample or group in the input file, read from stdin. The three arguments are the 0-based index of the column to downsample, the number of haplotypes in that column, and the (lower) new number of haplotypes in that column.

### freqSum2Histogram
This is the key tool to convert a freqSumFile to an allele sharing histogram, as used by rarecoal. Type `-h` for getting help.

One key ingredient in this tool is the total number of sites, specified via `-N`. This is an important input, as it will set the number of non-variant counts in your histogram, specified by the pattern consisting of zeros only, e.g. 0,0,0,0,0 in five populations. This number is important for estimating population sizes, which relies on knowing the ratio of variants and non-variants. If you are processing modern sequencing data (say from the 1000 Genomes Project), you can more or less assume that the entire mappable and callable genome is covered in all individuals. For humans, the number in all autosomes is close to 2,500,000,000, but the details depend on your specific data set and processing. For the 1000 Genomes Project, you can have a look at the `/vol1/ftp/release/20130502/supporting/accessible_genome_masks` directory on the FTP site and count all accessible sites if you don't apply further filtering.

### extractInFreqSum
This tool can be used to move a column in a freqSumFile behind the last column. Useful for extracting individual samples before grouping multiple samples with `groupFreqSum`.

### ms2hist
This converts the output from the `ms` and `scrm` simulation tools to a histogram. The input is read from stdin and should only contain the line starting with "POSITIONS:" and the following lines specifying the genotypes. You should use the `tail` command in UNIX to select the respective lines of the `ms` or `scrm` outputs. It cannot be used to process multiple chromosomes simultaneously.
This tools expects the total number of sites simulated as a parameter `-N`. Note that this parameter should give the entire length of the chromosome that you simulated, not just the number of segregating sites!

### sampleHist
This extracts samples from a subpopulation in the histogram, by sampling without replacement independently at every site underlying the histogram. The extracted samples therefore do not represent real individuals, but "average" individuals with genotypes sampled independently at every site from a full population. This can be useful if you need to extract individuals from histograms which were generated from data for which only allele frequencies but not genotypes are given. 

### combineHistograms
This simply adds up multiple histograms.