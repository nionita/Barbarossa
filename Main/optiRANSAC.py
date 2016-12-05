import sys
import numpy as np
import re
import math

from operator import itemgetter
from sklearn.linear_model import SGDRegressor, RANSACRegressor
from sklearn.preprocessing import MinMaxScaler
from sklearn import cross_validation
from sklearn.grid_search import GridSearchCV

debug = False

class DataReader:
	def __init__(self):
		# We store code, mid value & end value for the features
		self.featCVs = {
		    'ewMaterialDiff'    : ( 1, 8, 8),
		    'ewKingSafe'        : ( 2, 1, 0),
		    'ewKingOpen'        : ( 3, 5, 0),
		    'ewKingPlaceCent'   : ( 4, 6, 0),
		    'ewKingPlacePwns'   : ( 5, 0, 6),
		    'ewRookHOpen'       : ( 6, 171, 202),
		    'ewRookOpen'        : ( 7, 219, 221),
		    'ewRookConn'        : ( 8, 96,  78),
		    'ewMobilityKnight'  : ( 9, 50, 71),
		    'ewMobilityBishop'  : (10, 57, 33),
		    'ewMobilityRook'    : (11, 28, 26),
		    'ewMobilityQueen'   : (12,  4,  6),
		    'ewCenterPAtts'     : (13, 84, 68),
		    'ewCenterNAtts'     : (14, 49, 45),
		    'ewCenterBAtts'     : (15, 57, 39),
		    'ewCenterRAtts'     : (16, 10, 34),
		    'ewCenterQAtts'     : (17,  4, 59),
		    'ewCenterKAtts'     : (18,  0, 53),
		    'ewSpace'           : (19,  1,  0),
		    'ewAdvAtts'         : (20,  3, 16),
		    'ewIsolPawns'       : (21, -42, -122),
		    'ewIsolPassed'      : (22, -60, -160),
		    'ewBackPawns'       : (23, -120, -180),
		    'ewBackPOpen'       : (24, -35,   0),
		    'ewEnpHanging'      : (25, -23, -33),
		    'ewEnpEnPrise'      : (26, -25, -21),
		    'ewEnpAttacked'     : (27, -9, -13),
		    'ewLastLinePenalty' : (28, 115, 0),
		    'ewBishopPair'      : (29, 363, 388),
		    'ewRedundanceRook'  : (30,   0, -105),
		    'ewRookPawn'        : (31, -50, -40),
		    'ewAdvPawn5'        : (32,  10, 130),
		    'ewAdvPawn6'        : (33, 440, 500),
		    'ewPawnBlockP'      : (34, -124, -110),
		    'ewPawnBlockO'      : (35, -23, -27),
		    'ewPawnBlockA'      : (36, -14, -73),
		    'ewPassPawnLev'     : (37, 0, 9)
		    }

		self.XOrder = [1,30,29,31,2,3,5,4,28,12,11,10,9,18,17,16,15,14,13,19,20,8,7,6,27,26,25,36,35,34,22,21,24,23,33,32,37]

		fo = ["" for i in self.XOrder]
		for f, (i, _, _) in self.featCVs.iteritems():
			fo[i-1] = f
		self._ctf = fo

		maxc = max(self.XOrder)
		io = list(range(maxc+1))
		for i, c in enumerate(self.XOrder):
			io[c] = i
		self._cti = io

	"""Transform a feature code to a feature name"""
	def codeToFeature(self, code):
		return self._ctf[code-1]

	"""For a given feature name give the index (in xorder)"""
	def featureToIndex(self, feat):
		return self._cti[self.featCVs[feat][0]]

	"""For a given index (in xorder) give the feature name"""
	def indexToFeature(self, index):
		return self.codeToFeature(self.XOrder[index])

	"""Get a dictionary of feature values"""
	def featureVals(self):
		fw = {}
		for f, (_, mi, en) in self.featCVs.iteritems():
			fw[f] = mi, en
		return fw

	"""
	Read features/scores from a file to optimize the given features
	The features to optimize must be given as lists (mid, end) of feature names
	The rest will be used to compute the "rest" of the score
	"""
	def readData(self, fname, midFeatList=[], endFeatList=[], maxl=None, shift=0.1):

		# The lines to parse are now (no space before):
		# 68 71 16251 84 [-74,0,0,1,0,-312,17,0,-1,-16,8,7,0,-5,-2,0,2,0,-1,-165,-5,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,199]
		# i.e.: (static score in cp, phase in 256 scale, nodes searched,
		# score in cp and feats in 8/cp scale
		rs = re.compile(',| \[| |\]$')
		ps = fs = 0
		with open(fname, 'r') as f:
			for l in f:
				if maxl <> None and ps >= maxl:
					break
				ps = ps + 1
				if fs == 0:
					ns = rs.split(l)
					fs = ns.__len__() - 5
		self.samples = ps
		self.features = fs

		#assert(self.features == len(self.XOrder)

		# Use numpy arrays with holes for mid & end weights
		# The holes are the features that have to be optimized,
		# and must be set to 0 in these arrays:
		wmid = np.zeros(shape=(fs,))
		wend = np.zeros(shape=(fs,))

		# Put all known weights there
		for f, (_, mi, en) in self.featCVs.iteritems():
			i = self.featureToIndex(f)
			wmid[i] = mi
			wend[i] = en

		# Zero the ones to optimize and collect information abt them:
		optMidIdxs = {}
		omi = 0
		for f in midFeatList:
			i = self.featureToIndex(f)
			wmid[i] = 0
			optMidIdxs[i] = omi
			omi += 1
		optEndIdxs = {}
		oen = 0
		for f in endFeatList:
			i = self.featureToIndex(f)
			wend[i] = 0
			optEndIdxs[i] = oen
			oen += 1

		assert omi + oen > 0	# must have some features to optimize

		# Create the arrays with features, scores and phase
		x  = np.ndarray(shape=(fs,), dtype=np.float)	# one line of features for calculation
		if omi > 0:
			Xm = np.ndarray(shape=(ps, omi), dtype=np.float)	# mid features to optimize
		if oen > 0:
			Xe = np.ndarray(shape=(ps, oen), dtype=np.float)	# end features to optimize
		st = np.ndarray(shape=(ps,), dtype=np.float)	# static scores
		sn = np.ndarray(shape=(ps,), dtype=np.float)	# search scores
		al = np.ndarray(shape=(ps,), dtype=np.float)	# searched nodes alpha
		ph = np.ndarray(shape=(ps,1), dtype=np.float)	# phases
		km = np.ndarray(shape=(ps,), dtype=np.float)	# known part of score, mid
		ke = np.ndarray(shape=(ps,), dtype=np.float)	# known part of score, end
		k  = np.ndarray(shape=(ps,), dtype=np.float)	# known part of score, combined

		# The target is the static score modified towards the search score
		# by a quantity which depends on the number of searched nodes:
		# shift = math.exp(-scale * 100)	- shift towards dynamic score for 100 nodes
		scale = -math.log(shift) / 100

		markerMidFeats = []
		markerEndFeats = []

		ps = 0
		with open(fname, 'r') as f:
			for l in f:
				if maxl <> None and ps >= maxl:
					break
				if debug:
					print l
				ns = rs.split(l)
				st[ps] = float(ns[0])
				ph[ps] = float(ns[1])
				al[ps] = math.exp(-scale*float(ns[2]))
				sn[ps] = float(ns[3])
				jm = je = 0	# index in feature vectors to optimize
				for i, v in enumerate(ns[4:-1]):
					v = float(v)
					x[i] = v
					f = self.indexToFeature(i)
					if i in optMidIdxs:
						Xm[ps, jm] = v
						markerMidFeats.append(f)
						jm += 1
					if i in optEndIdxs:
						Xe[ps, je] = v
						markerEndFeats.append(f)
						je += 1
				km[ps] = np.dot(x, wmid)
				ke[ps] = np.dot(x, wend)
				ps = ps + 1

		self.markerMidFeats = markerMidFeats
		self.markerEndFeats = markerEndFeats
		if debug:
			print 'Mid feats:', self.markerMidFeats
			print 'End feats:', self.markerEndFeats
			print 'Xm = ', Xm
			print 'Xe = ', Xe
		Xm = ph * Xm / (256 * 8)
		Xe = (256-ph) * Xe / (256 * 8)
		X  = np.hstack((Xm, Xe))
		ph = ph[:,0]
		k  = (ph * km + (256-ph) * ke) / (256 * 8)
		print "Shape X  = ", X.shape
		#scl = MinMaxScaler(feature_range=(-1,1))
		#self.X = scl.fit_transform(X, y)
		y = st + al * (sn - st)	# the target score
		self.y = y - k		# the known part is subtracted, optimization explains only the rest
		self.X = X
		#self.scl = scl
		if debug:
			print 'ph = ', ph
			print 'al = ', al
			print 'st = ', st
			print 'sn = ', sn
			print 'y  = ', y
			print 'km = ', km
			print 'ke = ', ke
			print 'k  = ', k
			print 'X  = ', X
			print 'y  = ', self.y

#def score(X, ph, wm, we):
#	y = (X * ph * wm + X * (256-ph) * we) / (256 * 8)	# because of the 8/cp scale
#
#def scale(x, ph):
#	return np.hstack(x * ph, x * (256 - ph))

#
# Here we begin:
#
if __name__ == '__main__':

	noSamples = None	# 1000000

	ofm = [
		    #'ewKingSafe'    ,
		    #'ewKingOpen'    ,
		    #'ewKingPlaceCent',
		    #'ewKingPlacePwns',
		    'ewRookHOpen'    ,
		    'ewRookOpen'     ,
		    'ewRookConn'     ,
		    'ewMobilityKnight',
		    'ewMobilityBishop',
		    'ewMobilityRook'  ,
		    'ewMobilityQueen' ,
		    'ewCenterPAtts'   ,
		    'ewCenterNAtts'   ,
		    'ewCenterBAtts'   ,
		    'ewCenterRAtts'   ,
		    'ewCenterQAtts'   ,
		    'ewCenterKAtts'   ,
		    'ewSpace'         ,
		    'ewAdvAtts'       ,
		    'ewIsolPawns'     ,
		    'ewIsolPassed'    ,
		    'ewBackPawns'     ,
		    'ewBackPOpen'     ,
		    'ewEnpHanging'    ,
		    'ewEnpEnPrise'    ,
		    'ewEnpAttacked'   ,
		    'ewLastLinePenalty',
		    'ewBishopPair'     ,
		    'ewRedundanceRook' ,
		    'ewRookPawn'       ,
		    'ewAdvPawn5'       ,
		    'ewAdvPawn6'       ,
		    'ewPawnBlockP'     ,
		    'ewPawnBlockO'     ,
		    'ewPawnBlockA'     ,
		    'ewPassPawnLev'
	    ]
	ofe = [
		    'ewMaterialDiff',
		    #'ewKingSafe'    ,
		    #'ewKingOpen'    ,
		    #'ewKingPlaceCent',
		    #'ewKingPlacePwns',
		    'ewRookHOpen'    ,
		    'ewRookOpen'     ,
		    'ewRookConn'     ,
		    'ewMobilityKnight',
		    'ewMobilityBishop',
		    'ewMobilityRook'  ,
		    'ewMobilityQueen' ,
		    'ewCenterPAtts'   ,
		    'ewCenterNAtts'   ,
		    'ewCenterBAtts'   ,
		    'ewCenterRAtts'   ,
		    'ewCenterQAtts'   ,
		    'ewCenterKAtts'   ,
		    'ewSpace'         ,
		    'ewAdvAtts'       ,
		    'ewIsolPawns'     ,
		    'ewIsolPassed'    ,
		    'ewBackPawns'     ,
		    'ewBackPOpen'     ,
		    'ewEnpHanging'    ,
		    'ewEnpEnPrise'    ,
		    'ewEnpAttacked'   ,
		    'ewLastLinePenalty',
		    'ewBishopPair'     ,
		    'ewRedundanceRook' ,
		    'ewRookPawn'       ,
		    'ewAdvPawn5'       ,
		    'ewAdvPawn6'       ,
		    'ewPawnBlockP'     ,
		    'ewPawnBlockO'     ,
		    'ewPawnBlockA'     ,
		    'ewPassPawnLev'
	    ]

	# Data reader:
	dr = DataReader()
	#dr.readData('c:\\Engines\\Barbarossa\\alle11mb.txt', maxl=noSamples)
	dr.readData('optim/nobe-3-11M.txt', midFeatList=ofm, endFeatList=ofe,
			maxl=noSamples, shift=0.05)

	opt_phase = 3

	#X_tr, X_te, y_tr, y_te = cross_validation.train_test_split(dr.X, dr.y, test_size=0.4)

	if opt_phase == 1:
		### This is for grid searching the parameters:
		print "Searching for the best model:"
		sys.stdout.flush()
		clf = SGDRegressor(loss='huber', penalty='none', fit_intercept=True, alpha=0.001,
				n_iter=50, learning_rate='optimal')

		pars = {
				'epsilon': [5, 10., 20., 40.]
			}
		#pars = {
		#		'loss': ['squared_loss'],
		#		'alpha': [0.01, 0.1, 1., 10.],
		#		'epsilon': [1., 0.1]
		#	}

		grid_search = GridSearchCV(clf, param_grid=pars)

		grid_search.fit(dr.X, dr.y)

		top_scores = sorted(grid_search.grid_scores_, key=itemgetter(1), reverse=True)[:5]
		for i, score in enumerate(top_scores):
			print("Model rank {0}:".format(i+1))
			print("Mean validation score: {0:.6f} (std: {1:.6f})".format(
				score.mean_validation_score,
				np.std(score.cv_validation_scores)))
			print("Parameters: {0}".format(score.parameters))
			print

	### This is for after finding the best model:
	# But don't forget to change the optimal hyperparameters!
	if opt_phase >= 2:
		#est = SGDRegressor(loss='epsilon_insensitive', penalty='none', alpha=0.001,
		clf = SGDRegressor(loss='huber', penalty='none', fit_intercept=True,
				alpha=0.001, epsilon=5, n_iter=100, learning_rate='optimal')
		#clf = RANSACRegressor(base_estimator=est, min_samples=0.75)

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
			#est = clf.estimator_
			est = clf
			n = len(ofm)
			#coef = dr.scl.transform(est.coef_)
			coef = est.coef_
			mid = coef[:n]
			end = coef[n:]
			print
			print '============================='
			print "Intercept:", est.intercept_
			print '================='
			print "Best weights mid / end:"
			print '================='
			for f, v in zip(dr.markerMidFeats, mid):
				print("%20s = %5d" % (f, round(v)))
			print '================='
			print "Best weights mid / end:"
			print '================='
			for f, v in zip(dr.markerEndFeats, end):
				print("%20s = %5d" % (f, round(v)))
			print '================='
