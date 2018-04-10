FC=ifort
FCFLAGS=-I/sw/dataformats/netcdf/4.3.3.1/smp1/intel.16.0.3.210_threadsafe/include -Wl,-rpath,/sw/dataformats/netcdf/4.3.3.1/smp1/intel.16.0.3.210_threadsafe/lib
LDFLAGS=-L/sw/dataformats/netcdf/4.3.3.1/smp1/intel.16.0.3.210_threadsafe/lib
LIBS=-lnetcdff -lnetcdf
MYFILE=max_name_length_netcdf

all: $(MYFILE).f90
	$(FC) $(FCFLAGS) $(LDFLAGS) $(LIBS) $(MYFILE).f90 -o $(MYFILE).x

test: $(MYFILE).f90
	$(FC) $(FCFLAGS) $(MYFILE).f90 -o $(MYFILE).x

clean:
	rm $(MYFILE).x
