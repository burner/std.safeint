/** This $(D module) holds functions to safely compare integers of different
sizes and signeness without any unwanted implicit casts.

Additionally, this $(D module) implements a $(D SafeInt!T) type.
This type has an explicit $(D nan) value, checks binary operation for over-
and underflows, checks division by zero, and checks if values to assign can be
represented by $(D T) where $(D T) is any integer type.
*/
module std.safeint;

import std.traits : isFloatingPoint, isIntegral, isUnsigned, isNumeric, 
	   isSigned, Unqual;
import std.typetuple : TypeTuple;

@safe pure:

/** This function compares two integer of arbitrary type for equality.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is equal to the value of $(D s),
	false otherwise.
*/
bool equal(T,S)(in T t, in S s) @nogc nothrow if(isIntegral!T && isIntegral!S) {
	return impl!("a == b",false,false)(t,s);
}

/** This function compares two integer of arbitrary type for no-equality.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is not equal to the value of $(D s),
	false otherwise.
*/
bool notEqual(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a != b",true,true)(t,s);
}

/** This function checks if the value of the first parameter is smaller than 
the value of the second parameter.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is smaller than the value of $(D s),
	false otherwise.
*/
bool less(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a < b",true,false)(t,s);
}

/** This function checks if the value of the first parameter is less or equal
to the value of the second parameter.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is smaller or equal than the value
	of $(D s), false otherwise.
*/
bool lessEqual(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a <= b",true,false)(t,s);
}

/** This function checks if the value of the first parameter is greater than 
the value of the second parameter.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is greater than the value of $(D s),
	false otherwise.
*/
bool greater(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a > b",false,true)(t,s);
}

/** This function checks if the value of the first parameter is greater or equal
to the value of the second parameter.

This function makes sure no implicit value propagation falsifies the result of
the comparison.

Params:
	t = an integer value
	s = an integer value

Returns: $(D true) if the value of $(D t) is greater or equal than the value
	of $(D s), false otherwise.
*/
bool greaterEqual(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a >= b",false,true)(t,s);
}

private bool impl(string op, bool A, bool B, T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	import std.functional : binaryFun;
	alias func = binaryFun!op;

	static if((isUnsigned!T && isUnsigned!S) || (isSigned!T && isSigned!S)) {
		return func(t,s);
	} else static if(isSigned!T && isUnsigned!S) {
		return t < 0 ? A : func(t,s);
	} else static if(isUnsigned!T && isSigned!S) {
		return s < 0 ? B : func(t,s);
	} else {
		static if(isSigned!T) {
			string Ts = "signed!T";
		} else {
			string Ts = "unSigned!T";
		}
		static if(isSigned!S) {
			string Ss = "signed!S";
		} else {
			string Ss = "unSigned!S";
		}
		static assert(false, T.stringof ~ " " ~ S.stringof ~ " " ~ Ts 
			~ " " ~ Ss);
	}
}

private alias TTest = TypeTuple!(byte,short,int,long,ubyte,ushort,uint,ulong);

unittest {
	import std.conv : to;
	foreach(T; TypeTuple!(byte,short,int,long)) {
		foreach(S; TypeTuple!(ubyte,ushort,uint,ulong)) {
			assert( equal(to!T(1),to!T(1)));
			assert( equal(to!S(1),to!S(1)));
			assert( equal(to!T(1),to!S(1)));
			assert(!equal(to!T(-1),to!S(1)));
			assert(!notEqual(to!T(1),to!T(1)));
			assert(!notEqual(to!S(1),to!S(1)));
			assert(!notEqual(to!T(1),to!S(1)));
			assert( notEqual(to!T(-1),to!S(1)));
			assert( notEqual(to!T(0),to!S(1)));
			assert(!notEqual(to!T(0),to!S(0)));

			assert(!less(to!T(1),to!T(1)));
			assert(!less(to!S(1),to!S(1)));
			assert(!less(to!T(1),to!S(1)));
			assert( less(to!T(-1),to!S(1)));
			assert(!less(to!T(1),to!S(0)));

			assert( lessEqual(to!T(1),to!T(1)));
			assert( lessEqual(to!S(1),to!S(1)));
			assert( lessEqual(to!T(1),to!S(1)));
			assert( lessEqual(to!T(-1),to!S(1)));
			assert(!lessEqual(to!T(1),to!S(0)));

			assert(!greater(to!T(1),to!T(1)));
			assert( greater(to!T(2),to!T(1)));
			assert(!greater(to!S(1),to!S(1)));
			assert( greater(to!S(2),to!S(1)));
			assert(!greater(to!T(1),to!S(1)));
			assert(!greater(to!T(-1),to!S(1)));

			assert( greaterEqual(to!T(1),to!T(1)));
			assert( greaterEqual(to!T(2),to!T(1)));
			assert(!greaterEqual(to!S(1),to!S(2)));
			assert(!greaterEqual(to!T(-1),to!S(1)));
		}
	}
}

private auto getValue(T)(T t) {
	static if(isIntegral!T)
		return t;
	else
		return t.value;
}

/* This functions checks if the value of $(D s) can be stored by a variable of
type $(D T).

Params:
	s = the value to check

Returns:
	$(D true) if the value can be stored, false otherwise.
*/
bool canConvertTo(T,S)(in S s) nothrow @nogc if(isIntegral!(Unqual!T) 
		&& isIntegral!(SafeIntType!S)) {
	return (less(getValue(s), T.min) || greater(getValue(s), T.max)) 
		? false : true;
}

///
unittest {
	assert( canConvertTo!int(1337));
	assert( canConvertTo!uint(1337));
	assert( canConvertTo!int(-1337));
	assert(!canConvertTo!uint(-1337));
	assert(!canConvertTo!byte(1337));
	assert(!canConvertTo!ubyte(1337));
	assert(!canConvertTo!byte(-1337));
	assert(!canConvertTo!ubyte(-1337));
}

unittest {
	foreach(T; TTest) {
		foreach(S; TTest) {
			assert(canConvertTo!T(SafeInt!S(0)));
		}
	}
}

/** This template returns the integer type used by a $(D SafeInt) to store the
value.

If an integer type is passed this type will be returned.
*/
template SafeIntType(T) {
	static if(isIntegral!T)
		alias SafeIntType = T;
	else
		alias SafeIntType = typeof(T.value);
}

///
unittest {
	static assert(is(SafeIntType!(SafeInt!int) == int));
	static assert(is(SafeIntType!int == int));
}

/** This template checks if the passed type is a $(D SafeInt).

Returns:
	$(D true) if the passed type $(D T) is an $(D SafeInt), false
	overwise.
*/
template isSafeInt(T) {
	static if(is(T : SafeInt!S, S))
		enum isSafeInt = true;
	else
		enum isSafeInt = false;
}

///
unittest {
	static assert( isSafeInt!(SafeInt!int));
	static assert(!isSafeInt!int);
}

unittest {
	static assert(!isSafeInt!int);
	foreach(T; TTest) {
		alias ST = SafeInt!T;
		static assert(isSafeInt!ST);
	}
}

/** $(B SafeInt) implements a safe integer type.

Safe in the sense that:
$(UL 
	$(LI over and underflows are not ignored)
	$(LI no unchecked implicit casts are performed)
	$(LI assigned values are checked if they fit into the value range of the 
		underlaying type)
	$(LI default initialization to NaN)
	$(LI no bitwise operations are implemented.)
)

Every SafeInt must be specialized with one integer type.
The integer type also defines the NaN value.
For unsigned integer $(D U) the NaN value is $(D U.max).
For signed integer $(D S) the NaN value is $(D U.min).

This limits the value ranges of $(D SafeInt!U) where $(D U) is an unsigned
integer to $(D SafeInt!U >= 0 && SafeInt!U < U.max).
This limits the value ranges of $(D SafeInt!S) where $(D S) is an signed
integer to $(D SafeInt!S > S.min && SafeInt!S < S.max).
*/
nothrow @nogc struct SafeInt(T) if(isIntegral!T) {
	import core.checkedint;

	alias Signed = isSigned!T;

	T value = nan;

	/** Whenever a operation is not defined by the SafeInt struct this alias
	converts the SafeInt to the underlaying integer.
	*/
	alias value this;

	/** The constructor for SafeInt.
	
	The passed value must either be an basic numeric or another SafeInt value.
	The value of the passed parameter must fit into the value range defined by
	the template specialization of the SafeInt.

	Params:
		v = the value to construct the SafeInt from.
	*/
	this(V)(in V v) if(isNumeric!V || is(V : SafeInt!S, S) ) {
		this.safeAssign(v);
	}

	static if(isUnsigned!T) {
		/// The minimal value storable by this SafeInt.
		enum min = 0u;
		/// The maximal value storable by this SafeInt.
		enum max = T.max - 1;
		// The NaN value defined by this SafeInt.
		enum nan = T.max;
	} else {
		/// The minimal value storable by this SafeInt.
		enum min = T.min + 1;
		/// The maximal value storable by this SafeInt.
		enum max = T.max;
		// The NaN value defined by this SafeInt.
		enum nan = T.min;
	}

	private void safeAssign(V)(in V v) {
		if(greaterEqual(getValue(v), this.min) 
				&& lessEqual(getValue(v), this.max)) 
		{
			this.value = cast(T)v;
		} else {
			this.value = this.nan;
		}
	}

	/** Check if this SafeInts value is NaN.

	Returns:
		true is value is NaN, false otherwise.
	*/
	@property bool isNaN() const {
		return this.value == nan;
	}

	private static auto getValue(V)(V vIn) {
		static if(is(V : SafeInt!S, S)) {
			return vIn.value;
		} else {
			return vIn;
		}
	}

	/** This implements $(D +=, -=, %=, /=, *=) for this SafeInt.

	Returns:
		a copy of this SafeInt.	
	*/
	SafeInt!T opOpAssign(string op,V)(V vIn) {
		enum call = "this = this " ~ op ~ " vIn;";
		mixin(call);
		return this;
	}

	/** This implements $(D +, -, %, /, *) for this SafeInt.

	If the result of the operation can not be stored by the SafeInt!T,
	the resulting value is nan.

	Returns:
		a new SafeInt!T with the result of the operation.
	*/
	SafeInt!T opBinary(string op,V)(V vIn) const {
		auto v = getValue(vIn);

		static if(typeof(v).sizeof > 4 || typeof(this.value).sizeof > 4) {
			alias SignedType = long;
			alias UnsignedType = ulong;
		} else {
			alias SignedType = int;
			alias UnsignedType = uint;
		}

		bool overflow = false;

		if(this.isNaN()) {
			return SafeInt!T();
		}

		static if(op == "+") {
			static if(T.sizeof > 4 || typeof(v).sizeof > 4) {
				long function(long,long,ref bool) 
					@safe @nogc pure nothrow sOp = &adds;
				ulong function(ulong,ulong,ref bool) 
					@safe @nogc pure nothrow uOp = &addu;
			} else {
				int function(int,int,ref bool) 
					@safe @nogc pure nothrow sOp = &adds;
				uint function(uint,uint,ref bool) 
					@safe @nogc pure nothrow uOp = &addu;
			}
		} else static if(op == "-") {
			static if(T.sizeof > 4 || typeof(v).sizeof > 4) {
				long function(long,long,ref bool) 
					@safe @nogc pure nothrow sOp = &subs;
				ulong function(ulong,ulong,ref bool) 
					@safe @nogc pure nothrow uOp = &subu;
			} else {
				int function(int,int,ref bool) 
					@safe @nogc pure nothrow sOp = &subs;
				uint function(uint,uint,ref bool) 
					@safe @nogc pure nothrow uOp = &subu;
			}
		} else static if(op == "*") {
			static if(T.sizeof > 4 || typeof(v).sizeof > 4) {
				long function(long,long,ref bool) 
					@safe @nogc pure nothrow sOp = &muls;
				ulong function(ulong,ulong,ref bool) 
					@safe @nogc pure nothrow uOp = &mulu;
			} else {
				int function(int,int,ref bool) 
					@safe @nogc pure nothrow sOp = &muls;
				uint function(uint,uint,ref bool) 
					@safe @nogc pure nothrow uOp = &mulu;
			}
		} else static if(op == "/") {
			auto sOp = function(long v1, long v2 , ref bool overflow) 
					@safe pure nothrow @nogc
			{
				T ret = this.nan;
				if(notEqual(v2, 0)) {
					return v1 / cast(T)v2;
				} else {
					return SafeInt!T().nan;
				}
			};

			auto uOp = function(ulong v1, ulong v2, ref bool overflow) 
					@safe pure nothrow @nogc 
			{
				T ret = this.nan;
				if(notEqual(v2, 0)) {
					return cast(T)v1 / v2;
				} else {
					return SafeInt!T().nan;
				}
			};
		} else static if(op == "%") {
			auto sOp = function(T v1, T v2 , ref bool overflow) 
					@safe pure nothrow @nogc
			{
				T ret = this.nan;
				return v1 % v2;
			};

			auto uOp = sOp;
		}

		/*auto executer(A,B,T)(A a, B b, ref bool overflow) {
			A ret = T.nan;
			if(canConvertTo!A(v)) {
				ret = sOp(this.value, cast(A)v, overflow);
			} else if(canConvertTo!B(a)) {
				auto tmp = uOp(cast(B)this.value, v, overflow);
				if(canConvertTo!A(tmp)) {
					ret = cast(A)tmp;
				}
			}
		}*/

		static if(Signed && isSigned!(typeof(v))) { 
			auto ret = sOp(this.value, v, overflow
			);
		} else static if(!Signed && isUnsigned!(typeof(v))) {
			auto ret = uOp(this.value, v, overflow
			);
		}

		static if(Signed && isUnsigned!(typeof(v))) {
			T ret = this.nan;
			if(canConvertTo!SignedType(v)) {
				ret = cast(T)sOp(this.value, cast(T)v, overflow);
			} else if(canConvertTo!UnsignedType(this.value)) {
				auto tmp = cast(V)uOp(cast(typeof(v))this.value, v, overflow);
				if(canConvertTo!T(tmp)) {
					ret = cast(T)tmp;
				}
			}
		}

		static if(!Signed && isSigned!(typeof(v))) {
			T ret = this.nan;
			if(canConvertTo!UnsignedType(v)) {
				ret = cast(T)uOp(this.value, cast(T)v, overflow);
			} else if(canConvertTo!SignedType(this.value)) {
				auto tmp = cast(V)sOp(cast(typeof(v))this.value, v, overflow);
				if(canConvertTo!(SafeIntType!T)(tmp)) {
					ret = cast(T)tmp;
				}
			}
		}

		if(overflow) {
			ret = this.nan;
		}

		return typeof(this)(ret);
	}
	
	/** Implements the assignment operation for the SafeInt type.

	Every numeric value and every SafeInt can be assigned.
	If the passed value can not be stored by the SafeInt, the value of the
	SafeInt will be set to NaN.

	Params:
		vIn = the value to assign

	Returns:
		a copy of this SafeInt.	
	*/
	SafeInt!T opAssign(V)(V vIn) 
			if(isNumeric!T && is(V : SafeInt!S, S)) 
	{
		this.safeAssign(getValue(vIn));
		return this;
	}

	/** Implements the equality comparison function for the SafeInt type.
	
	Params:
		vIn = the value to compare the SafeInt with
	
	Returns:
		$(D true) if the passed value is equal to the value stored in the
		SafeInt, false otherwise.
	*/
	bool opEquals(V)(auto ref V vIn) const {
		static if(isFloatingPoint!V) {
			return this.value == vIn;
		} else {
			auto v = getValue(vIn);

			return equal(this.value, v);
		}
	}

	/** Implements the comparison function for the SafeInt type.
	
	Params:
		vIn = the value to compare the SafeInt with
	
	Returns:
		-1 if the SafeInt is less than $(D vIn), 1 if the SafeInt is greater
		than $(D vIn), 0 otherwise.
	*/
	int opCmp(V)(auto ref V vIn) const {
		static if(isFloatingPoint!V) {
			return this.value < vIn ? -1 : this.value > vIn ? 1 : 0;
		} else {
			auto v = getValue(vIn);

			return less(this.value, v) ? -1 :
				equal(this.value, v) ? 0 : 1;
		}
	}
}

///
@safe @nogc pure nothrow unittest {
	SafeInt!uint s0 = -1;
	assert(s0.isNaN);

	SafeInt!int s0_1 = s0  + 4;
	assert(s0_1.isNaN);
	auto s1 = SafeInt!int(1);
	auto s2 = s1 + 1;

	SafeInt!int s2_1 = s0 = s2;
	assert(!s2.isNaN);
	assert(s2 == 2);
	assert(s2 == 2);
	assert(s2 == SafeInt!byte(2));
	assert(s2 < SafeInt!byte(3));
	assert(s2 > SafeInt!byte(1));
	assert(s2 > 1.0);

	s2 += 1;
	assert(s2 == 3);

	auto s3 = SafeInt!int(2);
	auto s4 = s1 + s3;
	static assert(is(typeof(s4) == SafeInt!int));
	assert(!s4.isNaN);
	assert(s4 == 3);

	assert(SafeInt!int(0) == 0.0);
}

@nogc @safe pure nothrow:

unittest {
	foreach(T; TTest) {
		auto s1 = SafeInt!T(127);
		assert(s1 == 127);
		assert(!s1.isNaN);
		auto s1_2 = s1 + 1;

		SafeInt!T s2;
		assert(s2.isNaN);
	}
}

unittest {
	auto s1 = SafeInt!ubyte(1);

	auto r = s1 + SafeInt!ubyte(2);
	assert(r == 3);
	auto r1 = s1 + 2;
	assert(r1 == 3);
}

unittest {
	foreach(T; TTest) {
		auto s1 = SafeInt!T(1);
		auto s2 = SafeInt!T(1);
		auto s3 = SafeInt!T(5);
		auto s4 = SafeInt!T(2);

		auto sp = s1 + s2;
		auto sm = s1 - s2;
		auto sx = s1 * s2;
		auto sd = s1 / s2;
		auto sdn = s1 / 0;
		auto sdn2 = s1 / SafeInt!int(0);
		auto smo = s3 % s4;

		static assert(is(typeof(sp) == SafeInt!T));
		static assert(is(typeof(sm) == SafeInt!T));
		static assert(is(typeof(sx) == SafeInt!T));
		static assert(is(typeof(sd) == SafeInt!T));
		static assert(is(typeof(smo) == SafeInt!T));
		static assert(is(typeof(sdn) == SafeInt!T));
		static assert(is(typeof(sdn2) == SafeInt!T));

		assert(sp == 2);
		assert(sm == 0);
		assert(sx == 1);
		assert(sd == 1);
		assert(smo == 1);
		assert(sdn == sdn.nan);
		assert(sdn2 == sdn.nan);
	}
}

unittest {
	foreach(T; TTest) {
		foreach(S; TTest) {
			auto s0 = SafeInt!T(0);
			auto s1 = SafeInt!T(1);
			auto s2 = SafeInt!S(1);
			auto s3 = SafeInt!S(2);

			assert(s1 == 1);
			assert(s1 == s2);
			assert(s1 < s3);
			assert(s1 < 2);
			assert(s1 < s3);
			assert(s1 < 2);
			assert(s1 > 0);
			assert(s1 > s0);
		}
	}
}

unittest {
	auto s1 = SafeInt!int(1);
	auto s2 = SafeInt!int(1);

	auto sd = s1 / s2;
}

unittest {
	import std.conv : to;
	foreach(T; TTest) {
		SafeInt!T minT = SafeInt!(T).min;
		SafeInt!T maxT = SafeInt!(T).max;
		SafeInt!T zeroT = 0;

		static if(isUnsigned!T) {
			assert(minT == 0);
			assert(maxT == T.max - 1);
			assert(zeroT == 0);

			zeroT -= 1;
			assert(zeroT.isNaN);
		} else {
			assert(minT == T.min + 1);
			assert(maxT == T.max);
			assert(zeroT == 0);
		}

		minT -= 1;
		assert(minT.isNaN);

		maxT += 1;
		assert(maxT.isNaN);

	}
}

unittest {
	foreach(T; TTest) {
		foreach(S; TypeTuple!(byte,short,int,long)) {
			auto s0 = SafeInt!T(2);
			auto s1 = s0 + cast(S)-1;
		}
	}
}
