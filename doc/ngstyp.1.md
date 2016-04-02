% NGSTYP(1) NGS User Manual
% Ilya Sher
% 2015

# NAME

ngstyp - Next Generation Shell language types.

# LANGUAGE DATA TYPES

## Basic types

Basic types are implemented in C. User-defined types *can not* inherit from a basic type.

* Null
* Bool
* Int
* Str
* Arr
* Fun
* Any
	* BasicTypeInstance
	* NormalTypeInstance
* Seq
* Type
	* BasicType
	* NormalType
* Hash
* CLib
* CSym


## Normal types

User-defined types *can* inherit from a normal type.

* Exception
	* Error
		* LookupFail
			* KeyNotFound
			* IndexNotFound
			* AttrNotFound
			* GlobalNotFound
		* InvalidArgument
		* CompileFail
		* CallFail
			* DontKnowHowToCall
			* ImplNotFound
* Backtrace
* Command
* Range
	* InclusiveRange
	* ExclusiveRange

... and all types defined with `type`.

## Defining your types

TODO, WIP

	type T1
	type T2
	T2.inherit(T1)
	t2 = T2()
	echo(t2 is T2)
	echo(t2 is T1)
	# Outputs one per line: true, true
