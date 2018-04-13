.PHONY : all install clean

all: allobs
	$(FC) $(OBJS) $(LDFLAGS) $(LIBS) -o $(SRC)/calc_n_distribution.x

install: all
	mv $(SRC)/calc_n_distribution.x $(BIN)/calc_n_distribution.x

clean:
	cd $(SRC) && rm *.o *.mod

dir := .
include Makefile.include.gfortran
# include Makefile.include.ifort

dir := $(SRC)
include sub.mk
include $(dir)/.Makefile.src