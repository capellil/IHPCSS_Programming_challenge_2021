have_nvhpc_loaded="`module list 2>&1 | grep nvhpc | wc -l`"
have_mvapich_loaded="`module list 2>&1 | grep mvapich2/2.3.5-gcc8.3.1 | wc -l`"

if [ "${have_mvapich_loaded}" -eq "1" ]; then
	if [ "${have_nvhpc_loaded}" -eq "0" ]; then
		echo "You have the mvapich module loaded and not the nvhpc module. Please issue this command:"
		echo "module unload mvapich2/2.3.5-gcc8.3.1 && module load nvhpc/21.2 && $0"
		exit
	else
		echo "You have both the nvhpc and mvapich module loaded. Please issue this command"
		echo "module unload mvapich2/2.3.5-gcc8.3.1 && $0"
		exit
	fi
else
	if [ "${have_nvhpc_loaded}" -eq "0" ]; then
		echo "You do not have the nvhpc module loaded. Please issue this command:"
		echo "module load nvhpc/21.2 && $0"
		exit
	fi
fi
make clean_gpu
make all_gpu
