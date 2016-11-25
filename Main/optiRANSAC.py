import sys
import numpy as np
import re
import math

from operator import itemgetter
from sklearn.linear_model import SGDRegressor, RANSACRegressor
from sklearn.preprocessing import MinMaxScaler
from sklearn import cross_validation
from sklearn.grid_search import GridSearchCV

class DataReader:
	def readData(self, fname, maxl=None):

		# The lines to parse are like this (no space before):
		# 84 71 [-74,0,0,1,0,-312,17,0,-1,-16,8,7,0,-5,-2,0,2,0,-1,-165,-5,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,199]:
		# i.e.: score in cp, phase in 256 scale and feats in 8/cp scale
		rs = re.compile(',| \[| |\]$')
		ps = fs = 0
		with open(fname, 'r') as f:
			for l in f:
				if maxl <> None and ps >= maxl:
					break
				ps = ps + 1
				if fs == 0:
					ns = rs.split(l)
					fs = ns.__len__() - 3
		self.samples = ps
		self.features = fs

		# Create the 3 arrays with positions, subpositions and indexes
		X  = np.ndarray(shape=(ps, fs), dtype=np.float);
		y  = np.ndarray(shape=(ps,), dtype=np.float);
		ph = np.ndarray(shape=(ps,1), dtype=np.float);

		ps = 0
		with open(fname, 'r') as f:
			for l in f:
				if maxl <> None and ps >= maxl:
					break
				ns = rs.split(l)
				y[ps]  = ns[0]
				ph[ps] = ns[1]
				for i, v in enumerate(ns[2:-1]):
					X[ps, i] = v
				ps = ps + 1
		Xm  = ph * X / 256
		print "Shape Xm = ", Xm.shape
		print "Shape ph = ", ph.shape
		Xe = (256-ph) * X / 256
		print "Shape Xe = ", Xe.shape
		X = np.hstack((Xm, Xe))
		print "Shape X  = ", X.shape
		#scl = MinMaxScaler(feature_range=(-1,1))
		#X = scl.fit_transform(X)
		self.X = X
		self.y = y * 8
		#self.scl = scl

#def score(X, ph, wm, we):
#	y = X * ph * wm + X * (256-ph) * we
#
#def scale(x, ph):
#	return np.hstack(x * ph, x * (256 - ph))

#
# Here we begin:
#

noSamples = 7000000

#
# This is the order of the features:
#
featsOrder = {
    'ewMaterialDiff'    :  1,
    'ewKingSafe'        :  2,
    'ewKingOpen'        :  3,
    'ewKingPlaceCent'   :  4,
    'ewKingPlacePwns'   :  5,
    'ewRookHOpen'       :  6,
    'ewRookOpen'        :  7,
    'ewRookConn'        :  8,
    'ewMobilityKnight'  :  9,
    'ewMobilityBishop'  : 10,
    'ewMobilityRook'    : 11,
    'ewMobilityQueen'   : 12,
    'ewCenterPAtts'     : 13,
    'ewCenterNAtts'     : 14,
    'ewCenterBAtts'     : 15,
    'ewCenterRAtts'     : 16,
    'ewCenterQAtts'     : 17,
    'ewCenterKAtts'     : 18,
    'ewSpace'           : 19,
    'ewAdvAtts'         : 20,
    'ewIsolPawns'       : 21,
    'ewIsolPassed'      : 22,
    'ewBackPawns'       : 23,
    'ewBackPOpen'       : 24,
    'ewEnpHanging'      : 25,
    'ewEnpEnPrise'      : 26,
    'ewEnpAttacked'     : 27,
    'ewLastLinePenalty' : 28,
    'ewBishopPair'      : 29,
    'ewRedundanceRook'  : 30,
    'ewRookPawn'        : 31,
    'ewAdvPawn5'        : 32,
    'ewAdvPawn6'        : 33,
    'ewPawnBlockP'      : 34,
    'ewPawnBlockO'      : 35,
    'ewPawnBlockA'      : 36,
    'ewPassPawnLev'     : 37
    }

iniend = {
    'ewMaterialDiff'    : 8,
    'ewKingSafe'        : 0,
    'ewKingOpen'        : 0,
    'ewKingPlaceCent'   : 0,
    'ewKingPlacePwns'   : 6,
    'ewRookHOpen'       : 202,
    'ewRookOpen'        : 221,
    'ewRookConn'        :  78,
    'ewMobilityKnight'  : 71,
    'ewMobilityBishop'  : 33,
    'ewMobilityRook'    : 26,
    'ewMobilityQueen'   :  6,
    'ewCenterPAtts'     : 68,
    'ewCenterNAtts'     : 45,
    'ewCenterBAtts'     : 39,
    'ewCenterRAtts'     : 34,
    'ewCenterQAtts'     : 59,
    'ewCenterKAtts'     : 53,
    'ewSpace'           :  0,
    'ewAdvAtts'         : 16,
    'ewIsolPawns'       : (-122),
    'ewIsolPassed'      : (-160),
    'ewBackPawns'       : (-180),
    'ewBackPOpen'       :    0,
    'ewEnpHanging'      : (-33),
    'ewEnpEnPrise'      : (-21),
    'ewEnpAttacked'     : (-13),
    'ewLastLinePenalty' : 0,
    'ewBishopPair'      :  388,
    'ewRedundanceRook'  : (-105),
    'ewRookPawn'        : (-40),
    'ewAdvPawn5'        :  130,
    'ewAdvPawn6'        :  500,
    'ewPawnBlockP'      : (-110),
    'ewPawnBlockO'      : (-27),
    'ewPawnBlockA'      : (-73),
    'ewPassPawnLev'     : 9
    }

marker = [1,30,29,31,2,3,5,4,28,12,11,10,9,18,17,16,15,14,13,19,20,8,7,6,27,26,25,36,35,34,22,21,24,23,33,32,37]

fo = ["" for i in marker]
for f, i in featsOrder.iteritems():
	fo[i-1] = f

# Data reader:
dr = DataReader()
#dr.readData('c:\\Engines\\Barbarossa\\alle11mb.txt', maxl=noSamples)
dr.readData('alle900k.txt', maxl=noSamples)

opt_phase = 3

#X_tr, X_te, y_tr, y_te = cross_validation.train_test_split(dr.X, dr.y, test_size=0.4)

if opt_phase == 1:
	### This is for grid searching the parameters:
	print "Searching for the best model:"
	sys.stdout.flush()
	clf = SGDRegressor(learning_rate='optimal')

	pars = {
			'loss': ['squared_loss', 'huber', 'epsilon_insensitive', 'squared_epsilon_insensitive'],
			'penalty': ['none','l1','l2'],
			'alpha': [0.001, 0.01, 0.1, 1.],
			'epsilon': [1., 0.1, 0.01, 0.001 ],
			'eta0': [0.1, 0.01],
		}
	#pars = {
	#		'loss': ['squared_loss'],
	#		'alpha': [0.01, 0.1, 1., 10.],
	#		'epsilon': [1., 0.1]
	#	}

	grid_search = GridSearchCV(clf, param_grid=pars)

	grid_search.fit(dr.X, dr.y)

	top_scores = sorted(grid_search.grid_scores_, key=itemgetter(1), reverse=True)[:3]
	for i, score in enumerate(top_scores):
		print("Model rank {0}:".format(i+1))
		print("Mean validation score: {0:.3f} (std: {1:.3f})".format(
			score.mean_validation_score,
			np.std(score.cv_validation_scores)))
		print("Parameters: {0}".format(score.parameters))
		print

### This is for after finding the best model:
# But don't forget to change the optimal hyperparameters!
if opt_phase >= 2:
	est = SGDRegressor(loss='epsilon_insensitive', penalty='none', alpha=0.001,
			eta0=0.01, learning_rate='optimal', epsilon=0.1)
	clf = RANSACRegressor(base_estimator=est, min_samples=0.75)

if opt_phase == 2:
	### This is for cross validating with the best model:
	print "Cross validating the best model:"
	sys.stdout.flush()
	scores = cross_validation.cross_val_score(clf, dr.X, dr.y, cv=5)
	print ("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

#inil = [iniend[fo[i-1]] for i in marker]
#inil = [random.randint(-2, 2) for i in marker]
#xini = np.array(inil, dtype=np.float)
#xini = np.zeros(shape=marker.__len__(), dtype=np.float)

# Minimize:
#xbest, vbest, dic = fmin_l_bfgs_b(calcErr, xini, args=(x0, x1, ix), maxls=5,
#		approx_grad=1, epsilon=1, factr=1e8, pgtol=0.001, iprint=100, maxfun=noFuncs)
#xbest, vbest, dic = fmin_l_bfgs_b(calcErr, xini, args=(x0, x1, y0, y1, ix),
#		approx_grad=1, iprint=100, maxfun=noFuncs)
#print "Dict: ", dic

#xbest, vbest, fc, gc, warn = fmin_cg(calcErr, xini, args=(x0, x1, ix),
#				epsilon=1, maxiter=noIter, full_output=1, disp=1, callback=print_xk)
#print "Warn: ", warn

#print "Best error: ", vbest

if opt_phase == 3:
	### This is to print the found best params:
	print "Find best params with the best model:"
	sys.stdout.flush()
	try:
		clf.fit(dr.X, dr.y)
	except ValueError:
		print "Did not catch!"
		print "Trials: ", clf.n_trials

	else:
		est = clf.estimator_
		n = marker.__len__()
		print
		print '============================='
		print "Intercept:", est.intercept_
		print '================='
		print "Best weights mid:"
		print '================='
		for i, v in zip(marker, est.coef_[:n]):
			print fo[i-1], "\t=", round(v), " (", iniend[fo[i-1]], ")"
		print '================='
		print "Best weights end:"
		print '================='
		for i, v in zip(marker, est.coef_[n:]):
			print fo[i-1], "\t=", round(v), " (", iniend[fo[i-1]], ")"
		print '============================='
