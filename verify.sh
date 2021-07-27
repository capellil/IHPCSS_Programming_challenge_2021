#!/bin/bash


function echo_good
{
	echo -e "\033[32m$1\033[0m\c"
}

function echo_bad
{
	echo -e "\033[31m$1\033[0m\c"
}

function echo_success
{
	echo_good "[SUCCESS]"
	echo " $1"
}

function echo_failure
{
	echo_bad "[FAILURE]"
	echo " $1"
	exit -1
}

# Function taken from Meta Stack Overflow (https://meta.stackoverflow.com)
# Author: Glenn Jackman (profile: https://stackoverflow.com/users/7552/glenn-jackman)
# Original article: https://stackoverflow.com/a/14367368
function is_in_array
{ 
    local array="$1[@]"
    local seeking=$2
    local in=1
    for element in "${!array}"; do
        if [[ $element == $seeking ]]; then
            in=0
            break
        fi
    done
    return $in
}

######################
# Display quick help #
######################
clear;
echo "Quick help:";
echo "  - This script is meant to be submit as follows: './submit.sh LANGUAGE IMPLEMENTATION SIZE OUTPUT_FILE'";
echo "  - LANGUAGE = 'c' | 'f'";
echo "  - IMPLEMENTATION = 'cpu' | 'gpu'";
echo "  - SIZE = 'small' | 'big'";
echo "  - OUTPUT_FILE = the path to the file in which store the output.";
echo "  - Example: to submit the C CPU version on the small dataset, submit './submit.sh c cpu small'.";
echo "";

echo "Command executing this script: $0 $@"

#################################
# Check the number of arguments #
#################################
if [ "$#" -eq "4" ]; then
	echo_success "Correct number of arguments received."
else
	echo_failure "Incorrect number of arguments received; $# passed whereas 4 are expected. Please refer to the quick help above."
fi

#############################################
# Check that the language passed is correct #
#############################################
languages=("c" "f");
all_languages=`echo ${languages[@]}`;
is_in_array languages $1
language_retrieved=$?;
if [ "${language_retrieved}" == "0" ]; then
	echo_success "The language passed is correct ('$1').";
else
	echo_failure "The language '$1' is unknown. It must be one of: ${all_languages}.";
fi

###################################################
# Check that the implementation passed is correct #
###################################################
implementations=("cpu" "gpu");
all_implementations=`echo ${implementations[@]}`;
is_in_array implementations $2
implementation_retrieved=$?;
if [ "${implementation_retrieved}" == "0" ]; then
	echo_success "The implementation passed is correct (\"$2\").";
else
	echo_failure "The implementation '$2' is unknown. It must be one of: ${all_implementations}.";
fi

#########################################
# Check that the size passed is correct #
#########################################
sizes=("small" "big");
all_sizes=`echo ${sizes[@]}`;
is_in_array sizes $3
size_retrieved=$?;
if [ "${size_retrieved}" == "0" ]; then
	echo_success "The size passed is correct (\"$3\").";
else
	echo_failure "The size '$3' is unknown. It must be one of: ${all_sizes}.";
fi

#########################################################
# Check that the corresponding submission script exists #
#########################################################
reference_file="./reference/$1/$2_$3.txt";
if [ -f "${reference_file}" ]; then
	echo_success "The corresponding reference file \"${reference_file}\" has been found."
else
	echo_failure "The corresponding reference file \"${reference_file}\" has not been found."
fi

reference_iterations_string=`cat ${reference_file} | grep "Iteration" | cut -d ' ' -f 3`;
reference_iterations=($reference_iterations_string)
reference_iterations_count=${#reference_iterations[@]}
reference_iteration_achieved=`cat ${reference_file} | grep "iterations" | cut -d ' ' -f 10`;

iterations_to_verify_string=`cat $4 | grep "Iteration" | cut -d ' ' -f 3`;
iterations_to_verify=($iterations_to_verify_string)
iterations_to_verify_count=${#reference_iterations[@]}
iterations_to_verify_achieved=`cat $4 | grep "iterations" | cut -d ' ' -f 10`;

iteration=0
for i in ${!iterations_to_verify[@]}; do
	if [ ${i} -gt ${reference_iterations_count} ]; then
		echo_success "Your version ran more iterations than the reference; keep in mind that the extra iterations your program has run could not be checked against the reference. Please inform the programming challenge organiser to provide longer reference files."
		break;
	fi
	if [ ! "${reference_iterations[$i]}" = "${iterations_to_verify[$i]}" ]; then
		echo_failure "Iteration ${iteration} differs: ${reference_iterations[$i]} (reference) vs ${iterations_to_verify[$i]} (you)";
	fi
	let iteration="${iteration} + 25";
done

echo_success "All iterations compared are identical to the reference."
echo_success "Number of iterations achieved: ${reference_iteration_achieved} (reference) vs ${iterations_to_verify_achieved} (you).";
