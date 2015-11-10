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

A typical workflow of how to generate this format from VCF files is described below using `vcf2FreqSum` and `freqSum2Histogram`.

You can list the command line options for `rarecoal` by typing `rarecoal -h`, and for a specific subcommand, say `view` by typing `rarecoal view -h`.

### rarecoal view
You can use this subcommand to read in a histogram file and view it up to a specific allele count, using the `-m` option. For example, you can view the number of all singletons and doubletons in the example data set via:

    rarecoal view -m2 -i testData/5popSplit_combined.txt

You can also use the `-N` option to to adjust the total number of called sites, which will affect the number of non-variant sites (the special pattern `0,0,0,0,0` in the case for five populations).

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

This starts the maximization with the parameters given by `-x`, which correspond to the parameters listed in the first line of the template file. It is important to set the option `-m` to something not too high (`-m 4` was used throughout the publication), otherwise the run time will be very long. 

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