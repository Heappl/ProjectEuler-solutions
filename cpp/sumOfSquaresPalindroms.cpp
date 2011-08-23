#include "stdafx.h"
#include <vector>
#include <iostream>
#include <math.h>
#include <algorithm>
#include <string>

using namespace std;

#define u64 unsigned long long

u64 countDigs(u64 x)
{
	u64 out = 0;
	while(x) {
		x /= 10;
		++out;
	}
	return out;
}

bool isPalindrom(u64 x)
{
	u64 digs = countDigs(x);
	u64 half = digs / 2;
	u64 aux = 0;
	for (u64 i = 0; i < half; ++i) {
		aux = aux * 10 + x % 10;
		x /= 10;
	}
	if (digs % 2 == 1) x /= 10;
	return (aux == x);
}

u64 countPalindroms(u64 top, u64 start, vector<u64>& visited)
{
	u64 iter = start + 1;
	u64 aux = start * start;
	u64 out = 0;
	while (true) {
		aux += iter * iter;
		if (aux > top) break;
		if (isPalindrom(aux)) {
			bool seen = false;
			for (unsigned i = 0; i < visited.size(); ++i) {
				if (visited[i] == aux) {
					seen = true;
					break;
				}
			}
			if (!seen) {
				out += aux;
				visited.push_back(aux);
			}
		}
		++iter;
	}
	return out;
}

u64 countPalindroms(u64 top)
{
	vector<u64> visited;
	u64 out = 0;
	for (u64 i = 1; i < u64(sqrt(double(top))); ++i) {
		out += countPalindroms(top, i, visited);
	}
	return out;
}

int _tmain(int argc, _TCHAR* argv[])
{
	cout << countPalindroms(100000000) << endl;
	return 0;
}

