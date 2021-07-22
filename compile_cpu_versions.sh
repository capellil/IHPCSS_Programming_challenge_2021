if [ "`module list 2>&1 | grep nvhpc | wc -l`" -eq "1" ]; then
	echo "You have the nvhpc module loaded, which is more for GPUs. Please issue this command :)"
	echo "module unload nvhpc; module load mvapich2/2.3.5-gcc8.3.1"
	exit
fi
make clean_cpu
make all_cpu
