all: integrate_layer_depth.x
	echo "doing all"

integrate_layer_depth.x: integrate_layer_depth.o
	$(FC) integrate_layer_depth.o -o integrate_layer_depth.x $(LDFLAGS) $(LIBS)

integrate_layer_depth.o: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -c $(FCFLAGS) -o integrate_layer_depth.o

wiOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_wiOpenMP.x -fopenmp $(FCFLAGS) $(LDFLAGS) $(LIBS)

noOpenMP: integrate_layer_depth.f90
	$(FC) integrate_layer_depth.f90 -o integrate_layer_depth_noOpenMP.x $(FCFLAGS) $(LDFLAGS) $(LIBS)

clean:
	rm *.x *.o

dir := .
include Makefile.include.gfortran
# include Makefile.include.ifort