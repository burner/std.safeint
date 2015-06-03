module std.safeint;

import std.traits : isFloatingPoint, isIntegral, isUnsigned, isSigned, Unqual;
import std.typetuple : TypeTuple;

@safe pure:

bool equal(T,S)(in T t, in S s) @nogc nothrow if(isIntegral!T && isIntegral!S) {
	return impl!("a == b",false,false)(t,s);
}

bool notEqual(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a != b",true,true)(t,s);
}

bool less(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a < b",true,false)(t,s);
}

bool lessEqual(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a <= b",true,false)(t,s);
}

bool greater(T,S)(in T t, in S s) @nogc nothrow 
		if(isIntegral!T && isIntegral!S) 
{
	return impl!("a > b",false,true)(t,s);
}

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

bool canConvertTo(T,S)(in S s) nothrow @nogc if(isIntegral!(Unqual!T) 
		&& isIntegral!(SafeIntType!S)) {
	return (less(getValue(s), T.min) || greater(getValue(s), T.max)) 
		? false : true;
}

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

template SafeIntType(T) {
	static if(isIntegral!T)
		alias SafeIntType = T;
	else
		alias SafeIntType = typeof(T.value);
}

unittest {
	static assert(is(SafeIntType!int == int));
	static assert(is(SafeIntType!(SafeInt!int) == int));
}

nothrow @nogc struct SafeInt(T) if(isIntegral!T) {
	import core.checkedint;

	alias Signed = isSigned!T;

	T value = nan;

	alias value this;

	this(V)(in V v) if(isIntegral!V) {
		this.safeAssign(v);
	}

	static if(isUnsigned!T) {
		enum minValue = 0u;
		enum maxValue = T.max - 1;
		enum nan = T.max;
	} else {
		enum minValue = T.min + 1;
		enum maxValue = T.max;
		enum nan = T.min;
	}

	private void safeAssign(V)(in V v) {
		if(greaterEqual(v, this.minValue) && lessEqual(v, this.maxValue)) {
			this.value = cast(T)v;
		} else {
			this.value = this.nan;
		}
	}

	@property bool isNaN() const {
		return this.value == nan;
	}

	private static auto getValue(V)(V vIn) @nogc nothrow {
		static if(is(V : SafeInt!S, S)) {
			return vIn.value;
		} else {
			return vIn;
		}
	}

	SafeInt!T opBinary(string op,V)(V vIn) const @nogc nothrow {
		auto v = getValue(vIn);

		bool overflow = false;

		if(this.isNaN()) {
			return SafeInt!T();
		}

		static if(op == "+") {
			static if(T.sizeof > 4) {
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
			static if(T.sizeof > 4) {
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
			static if(T.sizeof > 4) {
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

		static if(Signed && isSigned!(typeof(v))) { 
			auto ret = sOp(this.value, v, overflow);
		} else static if(!Signed && isUnsigned!(typeof(v))) {
			auto ret = uOp(this.value, v, overflow);
		}

		static if(Signed && isUnsigned!(typeof(v))) {
			T ret = this.nan;
			if(canConvertTo!T(v)) {
				ret = cast(T)sOp(this.value, cast(T)v, overflow);
			} else if(canConvertTo!(typeof(v))(this.value)) {
				auto tmp = cast(V)uOp(cast(typeof(v))this.value, v, overflow);
				if(canConvertTo!T(tmp)) {
					ret = cast(T)tmp;
				}
			}
		}

		static if(!Signed && isSigned!(typeof(v))) {
			T ret = this.nan;
			if(canConvertTo!T(v)) {
				ret = cast(T)uOp(this.value, cast(T)v, overflow);
			} else if(canConvertTo!(typeof(v))(this.value)) {
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

	bool opEquals(V)(auto ref V vIn) const @nogc nothrow {
		static if(isFloatingPoint!V) {
			return this.value == vIn;
		} else {
			auto v = getValue(vIn);

			return equal(this.value, v);
		}
	}

	int opCmp(V)(auto ref V vIn) const @nogc nothrow {
		static if(isFloatingPoint!V) {
			return this.value < vIn ? -1 : this.value > vIn ? 1 : 0;
		} else {
			auto v = getValue(vIn);

			return less(this.value, v) ? -1 :
				equal(this.value, v) ? 0 : 1;
		}
	}
}

@safe pure nothrow @nogc:

unittest {
	auto s1 = SafeInt!int(1);
	auto s2 = s1 + 1;
	assert(!s2.isNaN);
	assert(s2 == 2);
	assert(s2 == 2);
	assert(s2 == SafeInt!byte(2));
	assert(s2 < SafeInt!byte(3));
	assert(s2 > SafeInt!byte(1));
	assert(s2 > 1.0);

	auto s3 = SafeInt!int(2);
	auto s4 = s1 + s3;
	static assert(is(typeof(s4) == SafeInt!int));
	assert(!s4.isNaN);
	assert(s4 == 3);

	assert(SafeInt!int(0) == 0.0);
}

unittest {
	foreach(T; TypeTuple!(byte,short,int,long,ubyte,ushort,uint,ulong)) {
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
}

unittest {
	foreach(T; TypeTuple!(byte,short,int,long,ubyte,ushort,uint,ulong)) {
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
	foreach(T; TypeTuple!(byte,short,int,long,ubyte,ushort,uint,ulong)) {
		foreach(S; TypeTuple!(byte,short,int,long,ubyte,ushort,uint,ulong)) {
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
