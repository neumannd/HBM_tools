HDF_ROOT=/sw/dataformats/hdf5/1.8.17/smp1/intel.16.0.3.210_threadsafe
HDF_LIB=$(HDF_ROOT)/lib
HDF_INC=$(HDF_ROOT)/include
NETCDF_ROOT=/sw/dataformats/netcdf/4.3.3.1/smp1/intel.16.0.3.210_threadsafe
NETCDF_LIB=$(NETCDF_ROOT)/lib
NETCDF_INC=$(NETCDF_ROOT)/include

FC=ifort
FCFLAGS=-I$(HDF_INC) -I$(NETCDF_INC) -Wl,-rpath,$(NETCDF_LIB) -Wl,-rpath,$(HDF_LIB)
LDFLAGS=-L$(HDF_LIB) -L$(NETCDF_LIB) -Wl,-rpath,$(NETCDF_LIB) -Wl,-rpath,$(HDF_LIB)
LIBS=-lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lz -lm
# not used
# -g debug
# -stand f03
# -qopenmp

## use
# module load intel.studio/16.0.3.210

all: integrate_layer_depth.x
	echo "doing all"

integrate_layer_depth.x: integrate_layer_depth.o
	$(FC) integrate_layer_depth.o -o integrate_layer_depth.x $(LDFLAGS) $(LIBS)

integrate_layer_depth.o: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -c $(FCFLAGS) -o integrate_layer_depth.o

wiOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_wiOpenMP.x -qopenmp $(FCFLAGS) $(LDFLAGS) $(LIBS)

noOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_noOpenMP.x $(FCFLAGS) $(LDFLAGS) $(LIBS)

clean:
	rm *.x *.o