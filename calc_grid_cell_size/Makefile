all: allobs
	$(FC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(SRC)/calc_grid_cell_size.x

allobs : $(OBJS)

%.o : %.f90
	\cd $(<D) && $(FC) $(<F) -c $(FCFLAGS)

install: all
	mv $(SRC)/calc_grid_cell_size.x $(BIN)/calc_grid_cell_size.x

clean:
	cd $(SRC) && rm *.mod *.o

dir := .
include Makefile.include.gfortran
# include Makefile.include.ifort

dir := $(SRC)
include $(dir)/.Makefile.src
