.SUFFIXES: .f90 .F90 .o
.PHONY: allobs

allobs : $(OBJS)

%.o : %.f90
	\cd $(<D) && $(FC) $(<F) -c $(FCFLAGS)