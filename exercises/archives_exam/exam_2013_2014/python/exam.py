import numpy as np
import pylab as pb
import GPy

pb.ion()
pb.close('all')

###############################################
## Ex1
x = np.linspace(0,1,500)[:,None]

pb.figure(figsize=(20,5))

pb.subplot(141)
k = GPy.kern.Matern52(input_dim=1,variance=1.,lengthscale=.2)
K = k.K(x)
Z = np.random.multivariate_normal(0*x[:,0],K,10)
pb.plot(x,Z[0:3,:].T)
pb.ylim((-2.5,2.5))
pb.title('a)')

pb.subplot(142)
k = GPy.kern.rbf(input_dim=1,variance=1.,lengthscale=.2)
K = k.K(x)
Z = np.random.multivariate_normal(0*x[:,0],K,10)
pb.plot(x,Z[0:3,:].T)
pb.ylim((-2.5,2.5))
pb.title('b)')

pb.subplot(143)
k = GPy.kern.exponential(input_dim=1,variance=1.,lengthscale=.2)
K = k.K(x)
Z = np.random.multivariate_normal(0*x[:,0],K,10)
pb.plot(x,Z[0:3,:].T)
pb.ylim((-2.5,2.5))
pb.title('c)')

pb.subplot(144)
k = GPy.kern.linear(input_dim=1,variances=1.)
K = k.K(x)
Z = np.random.multivariate_normal(0*x[:,0],K,10)
pb.plot(x,Z[0:3,:].T)
pb.ylim((-2.5,2.5))
pb.title('d)')

pb.savefig('Ex1',bbox_inches='tight')

###############################################
## Ex2
x = np.linspace(0,1,101)[:,None]
X = np.round(np.linspace(0.,1.,4)[:,None],2)
Y = X + 5*X**2 + 0.5 *np.sin(X) 

pb.figure(figsize=(20,5))

ax = pb.subplot(141)
k = GPy.kern.rbf(input_dim=1,variance=1.,lengthscale=.2)
l = GPy.kern.linear(input_dim=1,variances=1e5)
m = GPy.models.GPRegression(X,Y,kernel = k+l)
m['noise'] = 0.
m.plot(ax=ax,plot_limits=(-.2,2.2),resolution=101)
pb.title('a)')

ax = pb.subplot(142)
k = GPy.kern.exponential(input_dim=1,variance=1.,lengthscale=.2)
m = GPy.models.GPRegression(X,Y,kernel = k)
m['noise'] = .2
m.plot(ax=ax,plot_limits=(-.2,2.2),resolution=101)
pb.title('b)')

ax = pb.subplot(143)
k = GPy.kern.rbf(input_dim=1,variance=1.,lengthscale=.2)
m = GPy.models.GPRegression(X,0.5 *np.sin(X),kernel = k)
m['noise'] = 0.
xp = np.linspace(-.2,2.2,200)[:,None]
a,b,c,d = m.predict(xp)
GPy.util.plot.gpplot(xp,a+xp+5*xp**2,c+xp+5*xp**2,d+xp+5*xp**2)
pb.plot(X,Y,'kx',mew=1.5)
pb.xlim((-.2,2.2))
pb.title('c)')

ax = pb.subplot(144)
k = GPy.kern.rbf(input_dim=1,variance=1.,lengthscale=.2)
b = GPy.kern.bias(input_dim=1,variance=1e5)
m = GPy.models.GPRegression(X,Y,kernel = k+b)
m['noise'] = 0.
m.plot(ax=ax,plot_limits=(-.2,2.2),resolution=101)
pb.title('d)')

pb.savefig('Ex2',bbox_inches='tight')

###############################################
## Ex3

x = np.linspace(0,4*np.pi,201)[:,None]
X = np.round(np.linspace(0.,4,5)[:,None],2)
Y = np.sin(X) + np.cos(2*X) + 0.2*np.sin(X/4.)

pb.figure(figsize=(10,5))

ax = pb.subplot(111)
k = GPy.kern.periodic_Matern52(input_dim=1,variance=4.,lengthscale=2.,n_freq=40)
m = GPy.models.GPRegression(X,Y,kernel = k)
m['noise'] = 0.
m.plot(ax=ax,plot_limits=(0,4*np.pi),resolution=201)
X = 2*np.pi
pb.plot(X,np.sin(X) + np.cos(2*X) + 0.2*np.sin(X/4.),'ro',mew = 1.5)

pb.savefig('Ex3',bbox_inches='tight')

###############################################
## Ex4

x = np.linspace(0,5,201)[:,None]

kZZ = GPy.kern.rbf(input_dim=1,variance=1.,lengthscale=2.)

def kZZ(X,Y):
	return(np.exp(-(X-Y.T)**2))

def kdZZ(X,Y):
	return(2*(Y.T - X) * np.exp(-(X-Y.T)**2))

def kdZdZ(X,Y):
	return( 2*np.exp(-(X-Y.T)**2)*(1-2*(X-Y.T)**2) )

pb.figure(figsize=(15,3.5))
#pb.subplots_adjust(top=0.8)

pb.subplot(131)
pb.plot(x,kZZ(x,x[100]),'k',linewidth=2)
pb.title('$k_{Z,Z}(x,2.5)$', fontsize=20)
pb.ylim((-0.2,1.2))

pb.subplot(132)
pb.plot(x,kdZZ(x,x[100]),'k',linewidth=2)
pb.title('$k_{Z,Z\'}(x,2.5)$', fontsize=20)

pb.subplot(133)
pb.plot(x,kdZdZ(x,x[100]),'k',linewidth=2)
pb.title('$k_{Z\',Z\'}(x,2.5)$', fontsize=20)
pb.ylim((-2.7,2.2))

pb.savefig('Ex4',bbox_inches='tight')

#######################################
def m_multi(x,c,k1,k2,k12,X1,X2,Y1,Y2):
	K1 = k1(X1,X1)
	K2 = k2(X2,X2)
	K12 = k12(X1,X2)
	K = np.hstack((np.vstack((K1,K12.T)),np.vstack((K12,K2))))
	K_1 = np.linalg.inv(K)
	if c == 0:
		k = np.hstack((k1(x,X1),k12(x,X2)))
	else: 
		k = np.hstack((k12(x,X1),k2(x,X2)))
	Y = np.vstack((Y1,Y2))
	return( np.dot(np.dot(k,K_1),Y))

def c_multi(x,c,k1,k2,k12,X1,X2):
	K1 = k1(X1,X1)
	K2 = k2(X2,X2)
	K12 = k12(X1,X2)
	K = np.hstack((np.vstack((K1,K12.T)),np.vstack((K12,K2))))
	K_1 = np.linalg.inv(K)
	if c == 0:
		k = np.hstack((k1(x,X1),k12(x,X2)))
		kc = k1(x,x)
	else:
		k = np.hstack((k12(x,X1),k2(x,X2)))
		kc = k2(x,x)
	Y = np.vstack((Y1,Y2))
	return( kc - np.dot(np.dot(k,K_1),k.T))

x = np.linspace(0,14,501)[:,None]

X1 = 100 + np.linspace(2,5,1)[:,None]
X2 = np.linspace(7,100,2)[:,None]

Y2 = 2*np.ones((2,1))
Y2[0] = -2

m1 = m_multi(x,0,kZZ,kdZdZ,kdZZ,X1,X2,Y1,Y2)
c1 = c_multi(x,0,kZZ,kdZdZ,kdZZ,X1,X2)
v1 = np.diag(c1)[:,None]
v1 = (v1+abs(v1))/2.
up1 = m1 + 2*np.sqrt(v1)
lo1 = m1 - 2*np.sqrt(v1)

Z = np.random.multivariate_normal(0*x[:,0],c1,100)

pb.figure()
pb.plot(x,m1 +Z.T,'k')
GPy.util.plot.gpplot(x,m1,lo1,up1)
pb.savefig('Ex4bis',bbox_inches='tight')

############################
pb.figure(figsize=(15,5))

x = np.linspace(0,14,501)[:,None]

X1 = 100 + np.linspace(2,5,1)[:,None]
X2 = np.linspace(7,100,2)[:,None]

Y2 = 2*np.ones((2,1))
Y2[0] = -2

c1 = c_multi(x,0,kZZ,kdZdZ,kdZZ,X1,X2)
v1 = np.diag(c1)[:,None]

pb.subplot(121)
pb.plot(x,v1,'k',linewidth=2)
pb.ylim((0,1.2))
pb.title('a) variance de prediction')

x = np.linspace(0,14,501)[:,None]

X1 = np.linspace(2,5,2)[:,None]
X2 = np.linspace(5,11,2)[:,None]

Y1 = np.sin(2*X1) + X1
Y2 = 2*np.cos(2*X2) + 1

m1 = m_multi(x,0,kZZ,kdZdZ,kdZZ,X1,X2,Y1,Y2)
c1 = c_multi(x,0,kZZ,kdZdZ,kdZZ,X1,X2)
v1 = np.diag(c1)[:,None]
v1 = (v1+abs(v1))/2.
up1 = m1 + 2*np.sqrt(v1)
lo1 = m1 - 2*np.sqrt(v1)

pb.subplot(122)
GPy.util.plot.gpplot(x,m1,lo1,up1)
pb.title('b) meilleur predicteur et intervalles de confiance')

pb.savefig('Ex4ter',bbox_inches='tight')
