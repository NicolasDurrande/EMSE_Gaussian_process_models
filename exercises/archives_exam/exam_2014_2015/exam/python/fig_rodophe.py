import numpy as np
import pylab as pb
import GPy

np.random.seed(1)
pb.ion()
pb.close('all')

x = np.linspace(0,6*np.pi,201)[:,None]
X = np.round(np.linspace(0.,4,4)[:,None],2)+0.0001
Y = np.sin(X) + np.cos(2*X)

k = GPy.kern.periodic_Matern52(input_dim=1,variance=10,lengthscale=2.,n_freq=40)
# k = GPy.kern.Matern52(input_dim=1,variance=4.,lengthscale=3.)
K = k.K(x,x)

######### traj
pb.figure()
Z = np.random.multivariate_normal(0*x[:,0],K,3)
pb.plot(x,Z.T)
pb.ylim((-6,6))
pb.savefig('Fig_traj_per',bbox_inches='tight')

######### modele
pb.figure()
ax = pb.subplot(111)
m = GPy.models.GPRegression(X,Y,kernel = k)
m['noise'] = 0.
m.plot(ax=ax,plot_limits=(0,6*np.pi),resolution=201)
pb.savefig('Fig_model_per',bbox_inches='tight')

######### traj cond
pb.figure()
m,K,c,d = m.predict(x,full_cov=True)

Z = np.random.multivariate_normal(0*x[:,0],K,3).T
Z += m
pb.plot(x,Z)
pb.plot(X,Y,'kx',mew=1.5)
pb.ylim((-6,6))
pb.savefig('Fig_trajcond_per',bbox_inches='tight')

#pb.savefig('Ex3',bbox_inches='tight')
