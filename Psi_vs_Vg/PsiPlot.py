import pandas as pd, glob, matplotlib.pyplot as plt
file=glob.glob("*.dat")
dfig=pd.read_csv(file[0], sep='\s+', header=None).dropna()
dfit=pd.read_csv(file[1], sep='\s+', header=None).dropna()
dfp=pd.read_csv(file[2], sep='\s+', header=None).dropna()

Vgp, psi=dfp.iloc[:,0], dfp.iloc[:,1]
Vgig, ig=dfig.iloc[:,0], dfig.iloc[:,1]
Vgit, iter=dfit.iloc[:,0], dfit.iloc[:,1]

fig,ax=plt.subplots()
ax1=ax.twinx()
ax.plot(Vgp,psi,'o',c='cyan',label=r"$\Psi_{s}$")
ax.plot(Vgig,ig,'+',c='red',label="Initial Guess")
ax.set_ylabel(r"$\Psi_{s}$")
ax.set_xlabel("Vgs (V)")
ax1.plot(Vgit,iter,'.', label="iterations")
ax1.set_ylabel("iterations")
ax.legend()
ax1.legend()
plt.show()
