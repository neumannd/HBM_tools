all: allobs
	$(FC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(SRC)/create_mask.x

allobs : $(OBJS)

%.o : %.f90
	\cd $(<D) && $(FC) $(<F) -c $(FCFLAGS)

install: all
	mv $(SRC)/create_mask.x $(BIN)/create_mask.x

clean:
	cd $(SRC) && rm *.mod *.o


dir := .
include Makefile.include.gfortran
# include Makefile.include.ifort

dir := $(SRC)
include $(dir)/.Makefile.src