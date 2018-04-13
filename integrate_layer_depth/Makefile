all: allobs
	$(FC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(SRC)/integrate_layer_depth.x

allobs : $(OBJS)

%.o : %.f90
	\cd $(<D) && $(FC) $(<F) -c $(FCFLAGS)

install: all
	mv $(SRC)/integrate_layer_depth.x $(BIN)/integrate_layer_depth.x

clean:
	cd $(SRC) && rm *.mod *.o

dir := .
include Makefile.include.gfortran
# include Makefile.include.ifort

dir := $(SRC)
include $(dir)/.Makefile.src
