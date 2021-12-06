import numpy as np, pandas as pd, matplotlib.pyplot as plt, glob
file=glob.glob('*.dat')
Vt=1.14344
vi=input('Enter the value of Vg\n the shown legends will be Vg + Vt\n')
V=float(vi)
Vg=np.linspace(Vt+0.1,Vt+0.1+V,5)
for (i,v) in zip(file,Vg):
    df=pd.read_csv(i, sep='\s+', header=None).dropna(axis=1)
    V,I=df.iloc[:,0], df.iloc[:,1]
    plt.plot(V,I, label=f'Vg={round(v,2)} V')
    plt.ylabel('Id (A)')
    plt.xlabel('Vd (V)')

plt.legend()
plt.savefig('IdVd.png',bbox_inches='tight')
plt.show()
