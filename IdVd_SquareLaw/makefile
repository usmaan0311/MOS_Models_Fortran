SRC=mos
FXX=gfortran -o

all:
	$(FXX) ${SRC}.exe ${SRC}.f08

vi:
	vim ${SRC}.f08

run:
	./${SRC}.exe

plot:
	python PlotF.py 

clean:
	$(RM) ~* *~ *.exe *.txt *.csv *.dat
