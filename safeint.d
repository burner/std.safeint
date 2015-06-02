module std.safeint;

import std.traits : isIntegral, isUnsigned, isSigned;
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

bool canConvertTo(T,S)(in S s) nothrow @nogc if(isIntegral!T && isIntegral!S) {
	return (less(s, T.min) || greater(s, T.max)) ? false : true;
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
			auto sOp = function(T v1, T v2 , ref bool overflow) 
					@safe pure nothrow @nogc
			{
				T ret = this.nan;
				if(notEqual(v2, 0)) {
					return v1 / cast(T)v2;
				} else {
					return SafeInt!T().nan;
				}
			};

			auto uOp = function(T v1, T v2, ref bool overflow) 
					@safe pure nothrow @nogc 
			{
				T ret = this.nan;
				if(notEqual(v1, 0)) {
					return cast(T)v1 / v2;
				} else {
					return SafeInt!T().nan;
				}
			};
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
			} else if(canConvertTo!V(this.value)) {
				auto tmp = cast(T)uOp(cast(typeof(v))this.value, v, overflow);
				if(canConvertTo!T(tmp)) {
					ret = cast(T)tmp;
				}
			}
		}

		static if(!Signed && isSigned!(typeof(v))) {
			T ret = this.nan;
			if(canConvertTo!T(v)) {
				ret = cast(T)uOp(this.value, cast(T)v, overflow);
			} else if(canConvertTo!V(this.value)) {
				auto tmp = cast(T)sOp(cast(typeof(v))this.value, v, overflow);
				if(canConvertTo!T(tmp)) {
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
		auto v = getValue(vIn);

		return equal(this.value, v);
	}

	int opCmp(V)(auto ref V vIn) const @nogc nothrow {
		auto v = getValue(vIn);

		return less(this.value, v) ? -1 :
			equal(this.value, v) ? 0 : 1;
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

	auto s3 = SafeInt!int(2);
	auto s4 = s1 + s3;
	static assert(is(typeof(s4) == SafeInt!int));
	assert(!s4.isNaN);
	assert(s4 == 3);
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

		auto sp = s1 + s2;
		auto sm = s1 - s2;
		auto sx = s1 * s2;
		auto sd = s1 / s2;

		static assert(is(typeof(sp) == SafeInt!T));
		static assert(is(typeof(sm) == SafeInt!T));
		static assert(is(typeof(sx) == SafeInt!T));
		assert(is(typeof(sd) == SafeInt!T));

		assert(sp == 2);
		assert(sm == 0);
		assert(sx == 1);
		assert(sd == 1);
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
