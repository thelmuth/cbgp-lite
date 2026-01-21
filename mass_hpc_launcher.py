import os

# Which benchmark suite. Options: "psb", "composite", "sobania"
suite = "sobania"

# Used to identify runs in qstat. Change to make easy to identify
qstat_suffix = "-sobania"

# Your email address for receiving alerts
email = "thelmuth@hamilton.edu"

# Directory to hold results for each problem. Change for each set of runs
basedir = "/usr/local/research/compsci/helmuth/thelmuth/Results/sobania-try-1/"

# Directory where input data either can be found or will be downloaded.
# Can either be local (default) or global (will use less HDD if multiple projects use it)
data_dir = "/usr/local/research/compsci/helmuth/data/psb/"

# Any command line parameters for cbgp. Empty string for all defaults.
cmd_line_params = """ """

# Numbers of runs. "0-99" is good for normal of doing 100 runs. Can
# also be a list of runs to re-run, such as "32,39,84,91"
run_nums = "0-99"

if suite == "psb":
    problems = [
                ### PSB1
                "checksum",
                "collatz-numbers",
                "compare-string-lengths",
                "count-odds",
                "digits",
                "double-letters",
                "even-squares",
                "for-loop-index",
                "grade",
                "last-index-of-zero",
                "median",
                "mirror-image",
                "negative-to-zero",
                "number-io",
                "pig-latin",
                "replace-space-with-newline",
                "scrabble-score",
                "small-or-large",
                "smallest",
                "string-differences",
                "string-lengths-backwards",
                "sum-of-squares",
                "super-anagrams",
                "syllables",
                "vector-average",
                "vectors-summed",
                "x-word-lines",

                ### PSB2
                "basement",
                "bouncing-balls",
                "bowling",
                "camel-case",
                "cut-vector",
                "dice-game",
                "find-pair",
                "fizz-buzz",
                "fuel-cost",
                "gcd",
                "indices-of-substring",
                "leaders",
                "luhn",
                "middle-character",
                "paired-digits",
                "shopping-list",
                "snow-day",
                "solve-boolean",
                "spin-words",
                "square-digits",
                "substitution-cipher",
                "twitter",
                "vector-distance"
                ]
elif suite == "composite":
    problems = [
                ### Composite
                "area-of-rectangle",
                "centimeters-to-meters",
                "count-true",
                "filter-bounds",
                "first-index-of-true",
                "get-vals-of-key",
                "max-applied-fn",
                "min-key",
                "set-cartesian-product",
                "set-symmetric-difference",
                "sets-with-element",
                "simple-encryption",
                "sum-2-vals",
                "sum-2-vals-polymorphic",
                "sum-2D",
                "sum-vector-vals",
                "time-sheet"
                ]
elif suite == "sobania":
    problems = [
                ### Sobania
                "instance1_loc5_cc1_any",
                "instance2_loc7_cc1_any",
                "instance3_loc9_cc1_any",
                "instance4_loc11_cc1_any",
                "instance5_loc13_cc1_any",
                "instance6_loc5_cc2_if_only",
                "instance7_loc9_cc2_if_only",
                "instance8_loc11_cc2_if_only",
                "instance9_loc7_cc2_for_only",
                "instance10_loc13_cc2_for_only",
                "instance11_loc10_cc3_two_ifs_not_nested",
                "instance12_loc10_cc3_nested_if",
                "instance13_loc10_cc3_if_in_for",
                "instance14_loc10_cc3_two_fors_not_nested",
                "instance15_loc10_cc3_nested_for"
    ]

### You shouldn't need to change anything below here, unless you add another
### token to replace in template_replacements

with open('hpc_launcher.template', 'r') as hpc_template:
    hpc_launcher_template = hpc_template.read()

for problem in problems:
    template_replacements = {"#qsub-name#": problem + qstat_suffix,
                             "#email#": email,
                             "#namespace#": problem,
                             "#suite#": suite,
                             "#dir#": basedir,
                             "#data-dir#": data_dir,
                             "#run-nums#": run_nums,
                             "#cmd-line-params#": cmd_line_params}

    hpc_launcher = hpc_launcher_template
    for k in template_replacements:
        hpc_launcher = hpc_launcher.replace(k, template_replacements[k])

    temp_filename = "temp_launcher.run"
    with open(temp_filename, 'w') as temp_launcher:
        temp_launcher.write(hpc_launcher)

    os.system("qsub " + temp_filename)
    os.remove(temp_filename)