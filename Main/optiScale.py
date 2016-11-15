import sys
import numpy as np
import re
import math
import random
from scipy.optimize import fmin_l_bfgs_b
from numpy.linalg import norm
from sklearn.preprocessing import MinMaxScaler
#from scipy.optimize import fmin_cg

def readMats(fname, maxl=None):

	# We get such lines to parse:
	# Feats 232 [200,0,0,0,0,0,0,0,-1,0,0,-7,0,0,0,0,-1,0,-1,146,6,0,0,0,0,0,0,0,0,0,1,-1,0,0,0,0,356]
	# or
	# - Feats 232 [-200,0,0,0,0,0,0,0,1,0,0,2,0,0,0,0,1,0,2,-233,-7,0,0,0,1,3,0,0,0,0,-1,1,0,0,0,-1,-369]
	#
	rs0 = re.compile(',|^Feats | \[|\]$')
	rs1 = re.compile(',|^- Feats | \[|\]$')
	r0  = re.compile('^F')

	p0 = p1 = fs = 0
	with open(fname, 'r') as f:
		for l in f:
			if r0.match(l):
				if maxl <> None and p0 >= maxl:
					break
				p0 = p0 + 1
				if fs == 0:
					ns0 = rs0.split(l)
					fs = ns0.__len__() - 3
			else:
				p1 = p1 + 1
	print "Pos: {0}, Subpos: {1}, Feats: {2}\n".format(p0, p1, fs)
	sys.stdout.flush()

	# Create the 3 arrays with positions, subpositions and indexes
	x0 = np.ndarray(shape=(p0, fs), dtype=np.float);
	y0 = np.ndarray(shape=(p0,), dtype=np.float);
	x1 = np.ndarray(shape=(p1, fs), dtype=np.float);
	y1 = np.ndarray(shape=(p1,), dtype=np.float);
	ix = np.ndarray(shape=(p0,), dtype=np.int32);

	p0 = p1 = 0
	with open(fname, 'r') as f:
		for l in f:
			if r0.match(l):
				if maxl <> None and p0 >= maxl:
					break
				ns0 = rs0.split(l)
				y0[p0] = ns0[1]
				for i, v in enumerate(ns0[2:-1]):
					x0[p0, i] = v
				ix[p0] = p1
				p0 = p0 + 1
			else:
				ns1 = rs1.split(l)
				y1[p1] = ns1[1]
				for i, v in enumerate(ns1[2:-1]):
					x1[p1, i] = v
				p1 = p1 + 1
	return x0, x1, y0, y1, ix

def normalize(x0, x1):
	scaler = MinMaxScaler()
	xx = np.vstack((x0, x1))
	scaler.fit(xx)
	x2 = scaler.transform(x0)
	x3 = scaler.transform(x1)
	return x2, x3

def calcErr(f, x0, x1, y0, y1, ix):
	global printed
	ps = x0.shape[0]
	f[0]  = materWeight	# we keep this always fix
	no = norm(f)
	s0 = np.inner(x0, f)
	s1 = np.inner(x1, f)
	errtot = 0
	c = 0
	for i in range(ps):
		if i == ps - 1:
			j = x1.shape[0]
		else:
			j = ix[i+1]
		if ix[i] < j-1:
			xxs = s1[ix[i]:j-1]
			ima = np.argmax(xxs)
			#imi = np.argmin(xxs)
			#errtot = errtot + abs(s0[i] - ma) / (1 + ma - mi)
			dif = s0[i] + xxs[ima]
			# L2 error:
			#errtot = errtot + dif * dif + zeroPenalty / (1 + ma - mi)
			# L1 error:
			#errtot = errtot + abs(dif) + zeroPenalty / (1 + ma - mi)
			# Tanh error:
			#errtot = errtot + (ma - mi) * 100 * math.tanh(abs(dif)/100) + zeroPenalty / (1 + no)
			# The error is between s0 and xxs, but we give it the average weight
			# from the phases of the two:
			errloc  = diffScale * math.tanh(abs(dif)/diffScale)
			w = (y0[i]+y1[ix[i]+ima])/2
			errtot += errloc * w
			c += 1
			if no > 8 and printed < 4:
				printed += 1
				print "========== ", printed, " ==========="
				print "x0[i]: ", x0[i]
				print "c = ", c, " ps = ", ps, " f: ", f
				print "xxs: ", xxs
				print "ima = ", ima, ", ma = ", xxs[ima]
				print "dif = ", dif
				print "no = ", no
				print "errloc = ", errloc, ", w = ", w, ", errtot = ", errtot
				print "=========================="
				sys.stdout.flush()
	return errtot / c + zeroPenalty / no + bigPenalty * no

def print_xk(x):
	print "Called with: ", x
	sys.stdout.flush()

#
# Here we begin:
#

printed = 0

materWeight = 8
noSamples = 40000
noFuncs   = 5000
noIter    = 100

diffScale = 800
zeroPenalty = 8 * 200000
bigPenalty = 200000 / 800

x0, x1, y0, y1, ix = readMats('alle.end', maxl=noSamples)
#x0, x1 = normalize(x0, x1)
#x0, x1, ix = readMats('alle.end')
y0 = y0 / 256
y1 = y1 / 256

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

# First value should be always 1: it is material, which we do not optimize and fix it to materWeight
marker = [1,30,29,31,2,3,5,4,28,12,11,10,9,18,17,16,15,14,13,19,20,8,7,6,27,26,25,36,35,34,22,21,24,23,33,32,37]

fo = ["" for i in marker]
for f, i in featsOrder.iteritems():
	fo[i-1] = f

inil = [iniend[fo[i-1]] for i in marker]
#inil = [random.randint(-2, 2) for i in marker]
xini = np.array(inil, dtype=np.float)
#xini = np.zeros(shape=marker.__len__(), dtype=np.float)

# Minimize:
#xbest, vbest, dic = fmin_l_bfgs_b(calcErr, xini, args=(x0, x1, ix), maxls=5,
#		approx_grad=1, epsilon=1, factr=1e8, pgtol=0.001, iprint=100, maxfun=noFuncs)
xbest, vbest, dic = fmin_l_bfgs_b(calcErr, xini, args=(x0, x1, y0, y1, ix),
		approx_grad=1, iprint=100, maxfun=noFuncs)
print "Dict: ", dic

#xbest, vbest, fc, gc, warn = fmin_cg(calcErr, xini, args=(x0, x1, ix),
#				epsilon=1, maxiter=noIter, full_output=1, disp=1, callback=print_xk)
#print "Warn: ", warn

xbest[0] = materWeight

print "Best error: ", vbest
print "Best weights:"
for i, v in zip(marker, xbest):
	print fo[i-1], "\t=", round(v), " (", iniend[fo[i-1]], ")"
