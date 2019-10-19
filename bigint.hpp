#ifndef BIGINT_HPP
#define BIGINT_HPP

/**
 * Wrapper around gmp's mpz_class, with a fast path for values that fit in a long.
 */
class bigint {
private:
	void assign_from(const mpz_class& x) const {
		large.reset(new mpz_class(x));
		small = LONG_MIN;
	}

public:
	// Invariant: if 'large' is set, 'small' must be set to LONG_MIN and not used.
	// ('small' is allowed to be LONG_MIN if 'large' is null however.)
	mutable long small;
	mutable std::unique_ptr<mpz_class> large;

	bigint(): small(0) {}
	bigint(long x): small(x) {}
	bigint(const mpz_class& x) { assign_from(x); }
	bigint(bigint&& other): small(other.small), large(std::move(other.large)) {}
	bigint(const bigint& other): small(other.small) {
		if (other.large) {
			assign_from(*other.large);
		}
	}
	bigint(const std::string& str) {
		bool neg = false;
		size_t i = 0;
		if (str[0] == '-') {
			neg = true;
			i = 1;
		}
		long val = 0;
		for (; i < str.size(); i++) {
			int dig = str[i] - '0';
			if (__builtin_smull_overflow(val, 10, &val) ||
			    __builtin_saddl_overflow(val, dig, &val)) {
				assign_from(mpz_class(str));
				return;
			}
		}
		small = neg ? -val : val;
	}
	mpz_class to_mpz() const {
		if (!large) {
			assign_from(mpz_class(small));
		}
		return *large;
	};
	void shrink() const {
		if (large && large->fits_slong_p()) {
			small = large->get_si();
			if (small != LONG_MIN) {
				large.reset();
			}
		}
	}
	bigint& operator=(const bigint& other) {
		small = other.small;
		if (other.large) {
			assign_from(*other.large);
		} else {
			large = nullptr;
		}
		return *this;
	}
	bigint& operator=(bigint&& other) {
		small = other.small;
		large = std::move(other.large);
		return *this;
	}
	std::string get_str() const {
		return large ? large->get_str() : std::to_string(small);
	}
	bool fits_ulong_p() const {
		return large ? large->fits_ulong_p() : (0 <= small && (unsigned long) small < ULONG_MAX);
	}
	unsigned long get_ui() const {
		return large ? large->get_ui() : (unsigned long) small;
	}
	long get_si() const {
		return large ? large->get_si() : (long) small;
	}
};

inline bigint operator+(const bigint& a, const bigint& b) {
	long res;
	if (!a.large && !b.large && !__builtin_saddl_overflow(a.small, b.small, &res)) {
		return {res};
	}
	return {a.to_mpz() + b.to_mpz()};
}

inline bigint operator-(const bigint& a, const bigint& b) {
	long res;
	if (!a.large && !b.large && !__builtin_ssubl_overflow(a.small, b.small, &res)) {
		return {res};
	}
	return {a.to_mpz() - b.to_mpz()};
}

inline bigint operator-(const bigint& a) {
	if (!a.large && a.small != LONG_MIN) {
		return {-a.small};
	}
	return {-*a.large};
}

inline bigint operator*(const bigint& a, const bigint& b) {
	long res;
	if (!a.large && !b.large && !__builtin_smull_overflow(a.small, b.small, &res)) {
		return {res};
	}
	return {a.to_mpz() * b.to_mpz()};
}

inline bigint operator/(const bigint& a, const bigint& b) {
	if (!a.large && !b.large && a.small != LONG_MIN) {
		return {a.small / b.small};
	}
	return {a.to_mpz() / b.to_mpz()};
}

inline bigint operator%(const bigint& a, const bigint& b) {
	if (!a.large && !b.large && a.small != LONG_MIN) {
		return {a.small % b.small};
	}
	return {a.to_mpz() % b.to_mpz()};
}

inline bigint& operator%=(bigint& a, const bigint& b) { a = a % b; return a; }

#define BINOP(op, opeq) \
	inline bigint operator op(const bigint& a, int b) { return a op bigint(b); } \
	inline bigint operator op(int a, const bigint& b) { return bigint(a) op b; } \
	inline mpf_class operator op(const bigint& a, const mpf_class& b) { return a.to_mpz() op b; } \
	inline mpf_class operator op(const mpf_class& a, const bigint& b) { return a op b.to_mpz(); } \
	inline bigint& operator opeq(bigint& a, const bigint& b) { a = a op b; return a; }

#define RELOP(op) \
	inline bool operator op(const bigint& a, const mpf_class& b) { return a.to_mpz() op b; } \
	inline bool operator op(const mpf_class& a, const bigint& b) { return a op b.to_mpz(); } \
	inline bool operator op(const bigint& a, const bigint& b) { \
		return !a.large && !b.large ? a.small op b.small : a.to_mpz() op b.to_mpz(); \
	} \
	inline bool operator op(const bigint& a, int b) { \
		return !a.large ? a.small op b : a.to_mpz() op b; \
	} \
	inline bool operator op(int a, const bigint& b) { \
		return !b.large ? a op b.small : a op b.to_mpz(); \
	}


BINOP(+, +=)
BINOP(-, -=)
BINOP(*, *=)
BINOP(/, /=)

RELOP(<)
RELOP(>)
RELOP(<=)
RELOP(>=)
RELOP(==)
RELOP(!=)

#undef BINOP
#undef RELOP

inline std::ostream& operator<<(std::ostream& os, const bigint& x) {
	if (x.large) {
		return os << *x.large;
	} else {
		return os << x.small;
	}
}

#endif /* BIGINT_HPP */
