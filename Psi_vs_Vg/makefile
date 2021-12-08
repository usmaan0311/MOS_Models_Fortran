SRC=main 
CXX= gfortran - c
FXX=gfortran - o


all:
	$(CXX) InitCond.f08 Psi.f08 Diff.f08 Newton.f08 WriteSol.f08 main.f08
	$(FXX) main.exe InitCond.o Psi.o Diff.o Newton.o WriteSol.o main.o



vi: 
	vim ${SRC}.f08

run:
	./${SRC}.exe

plot:
	python PsiPlot.py

clean:
	$(RM) *~ ~* *.exe *.o *.mod *.exe *.dat
