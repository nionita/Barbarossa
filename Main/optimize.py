import sys
import numpy as np
import re
#from random import randint
from scipy.optimize import fmin_l_bfgs_b

def readMats(fname, maxl=None):
	rs = re.compile('.+\[|,|\]$')
	r0 = re.compile('^Feats ')
	r1 = re.compile('^  - Feats ')

	p0 = p1 = fs = 0
	with open(fname, 'r') as f:
		for l in f:
			if r0.match(l):
				if maxl <> None and p0 >= maxl:
					break
				p0 = p0 + 1
				if fs == 0:
					ns0 = rs.split(l)
					fs = ns0.__len__() - 2
			else:
				if r1.match(l):
					p1 = p1 + 1
	print "Pos: {0}, Subpos: {1}, Feats: {2}\n".format(p0, p1, fs)

	# Create the 3 arrays with positions, subpositions and indexes
	x0 = np.ndarray(shape=(p0, fs), dtype=np.int32);
	x1 = np.ndarray(shape=(p1, fs), dtype=np.int32);
	ix = np.ndarray(shape=p0, dtype=np.int32);

	p0 = p1 = 0
	with open(fname, 'r') as f:
		for l in f:
			if r0.match(l):
				if maxl <> None and p0 >= maxl:
					break
				ns0 = rs.split(l)
				for i, v in enumerate(ns0[1:-1]):
					x0[p0, i] = v
				ix[p0] = p1
				p0 = p0 + 1
			else:
				if r1.match(l):
					ns1 = rs.split(l)
					for i, v in enumerate(ns1[1:-1]):
						x1[p1, i] = v
					p1 = p1 + 1
	return x0, x1, ix

def calcErr(f, x0, x1, ix):
	#xxs = np.vsplit(x1, ix[1:])
	ps = x0.shape[0]
	err = 0
	for i in range(ps):
		if i == ps - 1:
			j = x1.shape[0]
		else:
			j = ix[i+1]
		xxs = x1[ix[i]:j-1]
		s0 = np.inner(x0, f)
		#s1 = np.inner(xxs[i], f)
		s1 = np.inner(xxs, f)
		mi = np.amin(s1)
		ma = np.amax(s1)
		err = err + np.sum(abs(s0 - ma) / (1 + ma - mi), dtype=np.float)
	return err / ps

#
# Here we begin:
#

x0, x1, ix = readMats('nocami.end', maxl=1000)

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

inil = [iniend[fo[i-1]] for i in marker]

xini = np.array(inil, dtype=np.int32)

# Minimize:
xbest, vbest, dic = fmin_l_bfgs_b(calcErr, xini, args=(x0, x1, ix),
		approx_grad=1, iprint=100, maxfun=2000)

print "Dict: ", dic
print "Best error: ", vbest
print "Best weights:"
for i, v in zip(marker, xbest):
	print fo[i-1], " = ", v
