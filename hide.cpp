#include<set>
#include<complex>

typedef num unsigned int
typedef point complex<num>

set<num> hide(set<num> A, vector<num> locn_tag) {
	set<point> V, P;
	auto p = make_poly(locn_tag);

  for (auto it = A.begin(); it != A.end(); it++) {
		V.insert(point(*it, p(*it)));
	}
	V = P;

	num x, y;
	for (num i = V.size(); i < v; i++) {
		x = rand();
		y = rand();
		while (V.find(point(x, y)) != V.end()) y = rand();
		V.insert(point(x, y))
	}
}

make_poly(vector<num> locn_tag) {
}

