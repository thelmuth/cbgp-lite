import os

#template_file = 'hpc_launcher.template'
template_file = 'hpc_launcher.psb2.template'

name = "-CBGP-comp1" ## Initial benchmarking of Code Building GP on Hamilton's HPC
basedir = "/usr/local/research/compsci/helmuth/thelmuth/Results/cbgp/composite-v1/"

cmd_line_params = """ """
#cmd_line_params = """--ast-strategy :newest-out"""
#cmd_line_params = """--ast-strategy :biggest-out """

problems = [
            ### PSB1
    # "checksum",
    # "collatz-numbers",
    # "compare-string-lengths",
    # "count-odds",
    # "digits",
    # "double-letters",
    # "even-squares",
    # "for-loop-index",
    # "grade",
    # "last-index-of-zero",
    # "median",
    # "mirror-image",
    # "negative-to-zero",
    # "number-io",
    # "pig-latin",
    # "replace-space-with-newline",
    # "scrabble-score",
    # "small-or-large",
    # "smallest",
    # "string-differences",
    # "string-lengths-backwards",
    # "sum-of-squares",
    # "super-anagrams",
    # "syllables",
    # "vector-average",
    # "vectors-summed",
    # "x-word-lines",

            ### PSB2
    # "basement",
    # "bouncing-balls",
    # "bowling",
    # "camel-case",
    # "dice-game",
    "find-pair",
    "cut-vector",
    # "fizz-buzz",
    # "fuel-cost",
    # "gcd",
    # "indices-of-substring",
    # "leaders",
    # "luhn",
    # "middle-character",
    # "paired-digits",
    # "shopping-list",
    # "snow-day",
    # "solve-boolean",
    # "spin-words",
    # "square-digits",
    # "substitution-cipher",
    # "twitter",
    # "vector-distance"

            ### Composite
    # "area-of-rectangle",
    # "centimeters-to-meters",
    # "count-true",
    # "filter-bounds",
    # "first-index-of-true",
    # "max-applied-fn",
    # "set-cartesian-product",
    # "set-symmetric-difference",
    # "sets-with-element",
    # "sum-2-vals",
    # "sum-2-vals-polymorphic",
    # "sum-2D",
    # "sum-vector-vals",
    # "timesheet",

]

with open(template_file, 'r') as hpc_template:
    hpc_launcher_template = hpc_template.read()


for problem in problems:

    hpc_launcher = hpc_launcher_template.replace("#qsub-name#", problem + name)
    hpc_launcher = hpc_launcher.replace("#namespace#", problem)
    hpc_launcher = hpc_launcher.replace("#dir#", basedir)
    hpc_launcher = hpc_launcher.replace("#cmd-line-params#", cmd_line_params)


    temp_filename = "temp_launcher.run"
    with open(temp_filename, 'w') as temp_launcher:
        temp_launcher.write(hpc_launcher)

    os.system("qsub " + temp_filename)
    os.remove(temp_filename)
