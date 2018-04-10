HDF_ROOT=/media/neumannd/sw/linux/packages/hdf5/hdf5-1.8.18/gcc-5.4.0/noMPI
HDF_LIB=$(HDF_ROOT)/lib
HDF_INC=$(HDF_ROOT)/include
NETCDF_ROOT=/media/neumannd/sw/linux/packages/netcdf/netcdf-4.4.1.1/gcc-5.4.0/noMPI
NETCDF_LIB=$(NETCDF_ROOT)/lib
NETCDF_INC=$(NETCDF_ROOT)/include

FC=gfortran-5
FCFLAGS=-I$(HDF_INC) -I$(NETCDF_INC) -Wl,-rpath,$(NETCDF_LIB) -Wl,-rpath,$(HDF_LIB)
LDFLAGS=-L$(HDF_LIB) -L$(NETCDF_LIB) -Wl,-rpath,$(NETCDF_LIB) -Wl,-rpath,$(HDF_LIB)
LIBS=-lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lz -lm
# not used
# -g debug
# -std=f2003 
# -fopenmp

all: integrate_layer_depth.x
	echo "doing all"

integrate_layer_depth.x: integrate_layer_depth.o
	$(FC) integrate_layer_depth.o -o integrate_layer_depth.x $(LDFLAGS) $(LIBS)

integrate_layer_depth.o: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -c $(FCFLAGS) -o integrate_layer_depth.o

full: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth.x $(FCFLAGS) $(LDFLAGS) $(LIBS)

wiOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_wiOpenMP.x -fopenmp $(FCFLAGS) $(LDFLAGS) $(LIBS)

noOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_noOpenMP.x $(FCFLAGS) $(LDFLAGS) $(LIBS)

old.20180408: integrate_layer_depth.old.v01.20180408.f90
	$(FC) integrate_layer_depth.old.v01.20180408.f90 -o integrate_layer_depth.old.v01.20180408_wiOpenMP.x -fopenmp $(FCFLAGS) $(LDFLAGS) $(LIBS)

clean:
	rm *.x *.o
