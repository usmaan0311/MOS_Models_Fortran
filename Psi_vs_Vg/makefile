SRC=main 
MOD=Newton.f08 Diff.f08
DEP=main.o Newton.o Diff.o
CXX= gfortran - c
FXX=gfortran - o


all:
	gfortran -c InitCond.f08 Psi.f08 Diff.f08 Newton.f08 WriteSol.f08 main.f08
	gfortran -o main.exe InitCond.o Psi.o Diff.o Newton.o WriteSol.o main.o



vi: 
	vim ${SRC}.f08

run:
	./${SRC}.exe

plot:
	python PsiPlot.py

clean:
	$(RM) *~ ~* *.exe *.o *.mod *.exe *.dat
