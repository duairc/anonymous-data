#ifdef DataPolyKinds
#define KBool Bool
#define KEither(a, b) Either a b
#define KList(a) [a]
#define KMaybe(a) Maybe a
#define KNatural Nat
#define KOrdering Ordering
#define KPair(a, b) (a, b)
#define KTriplet(a, b, c) (a, b, c)
#define KQuartet(a, b, c, d) (a, b, c, d)
#define KQuintet(a, b, c, d, e) (a, b, c, d, e)
#define KSextet(a, b, c, d, e, f) (a, b, c, d, e, f)
#define KSeptet(a, b, c, d, e, f, g) (a, b, c, d, e, f, g)
#define KPoly1 k
#define KPoly2 k1
#define KPoly3 k2
#define KPoly4 k3
#define KPoly5 k4
#define KPoly6 k5
#define KPoly7 k6
#define KString Symbol
#else
#define KBool *
#define KChar *
#define KEither(a, b) *
#define KList(a) *
#define KMaybe(a) *
#define KNatural *
#define KOrdering *
#define KPair(a, b) *
#define KTriplet(a, b, c) *
#define KQuartet(a, b, c, d) *
#define KQuintet(a, b, c, d, e) *
#define KSextet(a, b, c, d, e, f) *
#define KSeptet(a, b, c, d, e, f, g) *
#define KPoly1 *
#define KPoly2 *
#define KPoly3 *
#define KPoly4 *
#define KPoly5 *
#define KPoly6 *
#define KPoly7 *
#define KString *
#endif