if [ "`module list 2>&1 | grep nvhpc | wc -l`" -eq "0" ]; then
	echo "You do not have the nvhpc module loaded, which is needed for GPUs. Please issue this command :)"
	echo "module unload mvapich2/2.3.5-gcc8.3.1; module load nvhpc;" 
	exit
fi
make clean_gpu
make all_gpu
