#include "stdafx.h"
#include <vector>
#include <iostream>
#include <math.h>
#include <algorithm>
#include <string>
#include <iterator>

using namespace std;

#define u64 unsigned long long
#define u32 unsigned int

#define MAX 100000000

u32 nodigs(u32 x)
{
	u32 out = 0;
	while(x > 0) {
		x /= 10;
		++out;
	}
	return out;
}

template <typename T, u32 S>
struct Array
{
	Array()
	{
	}
	
	Array(T arg[S]) {
		memcpy(arr, arg, S * sizeof(T));
	}

	T& operator[](u32 x) {
		return arr[x];
	}

	T arr[S];
};

bool isPrime(u32 x, vector<bool>& isPrime, vector<u32>& primesSoFar, vector<u32>& digs)
{
	if (x > MAX) throw "exception";
	if (x % 2 == 0) return false;
	if (isPrime.size() > x / 2) return isPrime[x / 2];

	u32 iter = isPrime.size() * 2 + 1;
	while (iter <= x) {
		bool prime = true;
		for (vector<u32>::iterator it = primesSoFar.begin(); it != primesSoFar.end(); ++it) {
			if (iter % *it == 0) {
				isPrime.push_back(false);
				prime = false;
				break;
			}
		}
		if (prime) {
			primesSoFar.push_back(iter);
			isPrime.push_back(true);
			digs.push_back(nodigs(iter));
		}
		iter += 2;
	}
	return isPrime[x / 2];
}

bool isPrime(u32 x, vector<u32>& primesSoFar)
{
	u32 aux = u32(sqrt(double(x)));
	for (vector<u32>::iterator it = primesSoFar.begin(); it != primesSoFar.end(); ++it) {
		if (*it > aux) break;
		if (x % *it == 0) return false;
	}
	return true;
}

u32 concat(u32 x, u32 y, u32 digs)
{
	for (u32 i = 0; i < digs; ++i) x *= 10;
	return x + y;
}

template<int S>
u32 checkSetAndStoreIf(u32 x, u32 xdigs, back_insert_iterator< vector< Array<u32, S + 1> > > out, vector< Array<u32, S> >& sets,
					   vector<bool>& prime, vector<u32>& primesSoFar, vector<u32>& digs)
{
	u32 outsize = 0;
	for (u32 i = 0; i < sets.size(); ++i) {
		bool isOk = true;
		for (u32 j = 0; j < S; ++j) {
			u32 temp = concat(sets[i][j], x, xdigs);
			if (!isPrime(temp, primesSoFar)) {
				isOk = false;
				break;
			}
		}
		if (!isOk) continue;

		u32 newSet[S + 1];

		for (u32 j = 0; j < S; ++j) {
			u32 temp = concat(x, sets[i][j], nodigs(sets[i][j]));
			if (!isPrime(temp, primesSoFar)) {
				isOk = false;
				break;
			}
			newSet[j] = sets[i][j];
		}
		if (!isOk) continue;
		newSet[S] = x;
		out++ = Array<u32, S + 1>(newSet);
		++outsize;
	}
	return outsize;
}

bool checkAndStoreIf(u32 x, vector<bool>& prime, vector<u32>& primesSoFar,
					 vector< Array<u32, 2> >& twoSets, vector< Array<u32, 3> >& threeSets, vector< Array<u32, 4> >& fourSets,
					 vector<u32>& digs, vector< Array<u32, 5> >& out)
{
	if (!isPrime(x, prime, primesSoFar, digs)) return false;
	isPrime(x * 11, prime, primesSoFar, digs);
	u32 xdigs = nodigs(x);

	if (checkSetAndStoreIf(x, xdigs, back_inserter(out), fourSets, prime, primesSoFar, digs) > 0) return true;
	u32 aux = 0;
	aux = checkSetAndStoreIf(x, xdigs, back_inserter(fourSets), threeSets, prime, primesSoFar, digs);
	aux = checkSetAndStoreIf(x, xdigs, back_inserter(threeSets), twoSets, prime, primesSoFar, digs);

	aux = 0;
	for (u32 i = 0; i < primesSoFar.size(); ++i) {
		if (primesSoFar[i] >= x) break;
		u32 temp1 = concat(x, primesSoFar[i], digs[i]);
		u32 temp2 = concat(primesSoFar[i], x, xdigs);
		if (isPrime(temp1, primesSoFar) && isPrime(temp2, primesSoFar)) {
			u32 temp[2] = {x, primesSoFar[i]};
			twoSets.push_back(Array<u32, 2>(temp));
			++aux;
		}
	}
	return false;
}

int _tmain(int argc, _TCHAR* argv[])
{
	vector<bool> prime;
	vector<u32> primesSoFar;
	vector<u32> digs;
	prime.reserve(MAX);
	digs.reserve(MAX);
	primesSoFar.reserve(int(sqrt(double(MAX))));
	primesSoFar.push_back(2);
	prime.push_back(true);

	vector< Array<u32, 2> > twoSets;
	vector< Array<u32, 3> > threeSets;
	vector< Array<u32, 4> > fourSets;
	twoSets.reserve(int(sqrt(sqrt(double(MAX)))));
	threeSets.reserve(int(sqrt(sqrt(double(MAX)))));
	fourSets.reserve(int(sqrt(sqrt(double(MAX)))));

	for (u32 i = 0; i < MAX; ++i) {
		vector< Array<u32, 5> > out;
		if (i % 100 == 0) cerr << i << endl;
		if (checkAndStoreIf(i, prime, primesSoFar, twoSets, threeSets, fourSets, digs, out)) {
			for (u32 j = 0; j < 5; ++j) {
				cerr << out[0][j] << " ";
			}
			cerr << endl;
			return 0;
		}
	}
	return 0;
}