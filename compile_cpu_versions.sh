have_nvhpc_loaded="`module list 2>&1 | grep nvhpc | wc -l`"
have_mvapich_loaded="`module list 2>&1 | grep mvapich2/2.3.5-gcc8.3.1 | wc -l`"

if [ "${have_nvhpc_loaded}" -eq "1" ]; then
	if [ "${have_mvapich_loaded}" -eq "0" ]; then
		echo "You have the nvhpc module loaded and not the mvapich module. Please issue this command:"
		echo "module unload nvhpc && module load mvapich2/2.3.5-gcc8.3.1 && $0"
		exit
	else
		echo "You have both the nvhpc and mvapich module loaded. Please issue this command"
		echo "module unload nvhpc && $0"
		exit
	fi
else
	if [ "${have_mvapich_loaded}" -eq "0" ]; then
		echo "You do not have the mvapich2/2.3.5-gcc8.3.1 module loaded. Please issue this command:"
		echo "module load mvapich2/2.3.5-gcc8.3.1 && $0"
		exit
	fi
fi
make clean_cpu
make all_cpu
