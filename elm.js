(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$Landing = F2(
	function (a, b) {
		return {$: 'Landing', a: a, b: b};
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		A2($author$project$Main$Landing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Main$EnteringNameDone = {$: 'EnteringNameDone'};
var $author$project$Main$Next = {$: 'Next'};
var $author$project$Main$NoOp = {$: 'NoOp'};
var $author$project$Main$Prev = {$: 'Prev'};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$ArrowLeft = {$: 'ArrowLeft'};
var $author$project$Main$ArrowRight = {$: 'ArrowRight'};
var $author$project$Main$Enter = {$: 'Enter'};
var $author$project$Main$Other = {$: 'Other'};
var $author$project$Main$toKey = function (key) {
	switch (key) {
		case 'Enter':
			return $author$project$Main$Enter;
		case 'ArrowLeft':
			return $author$project$Main$ArrowLeft;
		case 'ArrowRight':
			return $author$project$Main$ArrowRight;
		default:
			return $author$project$Main$Other;
	}
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$toKey,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $author$project$Main$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Platform$Sub$map,
				function (key) {
					switch (key.$) {
						case 'Enter':
							return $author$project$Main$EnteringNameDone;
						case 'ArrowRight':
							return $author$project$Main$Next;
						case 'ArrowLeft':
							return $author$project$Main$Prev;
						default:
							return $author$project$Main$NoOp;
					}
				},
				$elm$browser$Browser$Events$onKeyDown($author$project$Main$keyDecoder))
			]));
};
var $author$project$Main$BinaryExpression = function (a) {
	return {$: 'BinaryExpression', a: a};
};
var $author$project$Main$CoursePage = F2(
	function (a, b) {
		return {$: 'CoursePage', a: a, b: b};
	});
var $author$project$Main$CoursesOverview = F2(
	function (a, b) {
		return {$: 'CoursesOverview', a: a, b: b};
	});
var $author$project$Main$FinishedQuiz = F5(
	function (a, b, c, d, e) {
		return {$: 'FinishedQuiz', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Main$FunctionExpression = function (a) {
	return {$: 'FunctionExpression', a: a};
};
var $author$project$Main$GuardExpression = function (a) {
	return {$: 'GuardExpression', a: a};
};
var $author$project$Main$LearningContentPage = F4(
	function (a, b, c, d) {
		return {$: 'LearningContentPage', a: a, b: b, c: c, d: d};
	});
var $author$project$Main$LecturePage = F3(
	function (a, b, c) {
		return {$: 'LecturePage', a: a, b: b, c: c};
	});
var $author$project$Main$PatternMatchingExpression = function (a) {
	return {$: 'PatternMatchingExpression', a: a};
};
var $author$project$Main$RunningQuiz = F5(
	function (a, b, c, d, e) {
		return {$: 'RunningQuiz', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Main$ShuffleAnswers = function (a) {
	return {$: 'ShuffleAnswers', a: a};
};
var $author$project$Main$SingleExpression = function (a) {
	return {$: 'SingleExpression', a: a};
};
var $author$project$Main$StartQuiz = function (a) {
	return {$: 'StartQuiz', a: a};
};
var $author$project$Main$User = F2(
	function (name, badges) {
		return {badges: badges, name: name};
	});
var $author$project$Main$UsernameIncorrect = {$: 'UsernameIncorrect'};
var $author$project$Main$WinningQuiz = F3(
	function (a, b, c) {
		return {$: 'WinningQuiz', a: a, b: b, c: c};
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Main$checkUsername = function (name) {
	if (name.$ === 'Just') {
		var n = name.a;
		return $elm$core$String$length(n) > 2;
	} else {
		return false;
	}
};
var $author$project$Main$exercise1 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: 'Int', isCorrect: true},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: '1',
		id: 1,
		title: 'Zahlenausdruck'
	});
var $author$project$Main$exercise2 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: 'Int', isCorrect: false},
				{code: 'String', isCorrect: true},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: '\"Hallo\"',
		id: 2,
		title: 'Stringausdruck'
	});
var $author$project$Main$exercise3 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: 'Int', isCorrect: false},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'Bool', isCorrect: true}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: 'True',
		id: 3,
		title: 'Boolausdruck'
	});
var $author$project$Main$exercise4 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: '[Int]', isCorrect: true},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: '[1, 2, 3]',
		id: 4,
		title: 'Listenausdruck'
	});
var $author$project$Main$exercise5 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: '(Int, String)', isCorrect: true},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: '(1, \"Hallo\")',
		id: 5,
		title: 'Tupleausdruck'
	});
var $author$project$Main$exercise6 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: 'Maybe Int', isCorrect: true},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: 'Just 1',
		id: 6,
		title: 'Maybeausdruck'
	});
var $author$project$Main$exercise7 = $author$project$Main$SingleExpression(
	{
		answers: _List_fromArray(
			[
				{code: 'Maybe a', isCorrect: true},
				{code: 'String', isCorrect: false},
				{code: 'Float', isCorrect: false},
				{code: 'SomeType', isCorrect: false}
			]),
		description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
		expression: 'Nothing',
		id: 7,
		title: 'Maybeausdruck'
	});
var $author$project$Main$lecture1 = {
	badge: {id: 'singleexpression', image: $elm$core$Maybe$Nothing, name: 'Single Expression'},
	description: 'In dieser Lektion wird dein Wissen über Typen von einfachen Ausdrücken getestet.',
	exercises: _List_fromArray(
		[$author$project$Main$exercise1, $author$project$Main$exercise2, $author$project$Main$exercise3, $author$project$Main$exercise4, $author$project$Main$exercise5, $author$project$Main$exercise6, $author$project$Main$exercise7]),
	id: 1,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('In Haskell ist alles ein Ausdruck. Um Wikipedia zu zitieren: \"Ein Ausdruck ist in vielen Programmiersprachen ein Konstrukt, das gemäß einer gegebenen Semantik in Bezug auf einen Kontext ausgewertet werden kann, also einen Wert liefert.\".'),
				expression: '',
				id: 1,
				title: 'Ausdrücke'
			},
				{description: $elm$core$Maybe$Nothing, expression: 'x :: Int\nx = 1', id: 1, title: 'Ganzzahliger Ausdruck'},
				{description: $elm$core$Maybe$Nothing, expression: 'x :: Float\nx = 1.0', id: 2, title: 'Gleitkommazahl-Ausdruck'},
				{description: $elm$core$Maybe$Nothing, expression: 'x :: String\nx = \"Hallo\"', id: 3, title: 'String-Ausdruck'},
				{description: $elm$core$Maybe$Nothing, expression: 'x :: Float\nx = 1.0', id: 4, title: 'Gleitkommazahl-Ausdruck'},
				{description: $elm$core$Maybe$Nothing, expression: 'x :: [String]\nx = [\"Hallo\", \"Welt\"]', id: 5, title: 'Listen-Ausdruck'},
				{
				description: $elm$core$Maybe$Just('Ein boolescher Ausdruck kann entweder \'True\' oder \'False\' sein. In diesem Beispiel ist der Wert \'True\'.'),
				expression: 'x :: Bool\nx = True',
				id: 6,
				title: 'Boolescher Ausdruck'
			}
			]),
		id: 1,
		title: 'Einfache Ausdrücke bzw. Typen'
	},
	title: 'Typen von einfachen Ausdrücken'
};
var $author$project$Main$lecture2 = {
	badge: {id: 'binaryexpression', image: $elm$core$Maybe$Nothing, name: 'Zweistellige Ausdrücke'},
	description: 'Diese Lektion beinhaltet Aufgaben mit zweistelligen Ausdrücken, die über einfache Operatoren miteinander verbunden sind.',
	exercises: _List_fromArray(
		[
			$author$project$Main$BinaryExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Int', isCorrect: true},
						{code: 'String', isCorrect: false},
						{code: 'Float', isCorrect: false},
						{code: 'SomeType', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
				id: 8,
				leftExpression: '1',
				operator: '+',
				rightExpression: '2',
				title: 'Zahlenausdruck'
			}),
			$author$project$Main$BinaryExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Int', isCorrect: false},
						{code: 'String', isCorrect: true},
						{code: 'Float', isCorrect: false},
						{code: 'SomeType', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
				id: 9,
				leftExpression: '\"Hallo\"',
				operator: '++',
				rightExpression: '\"Welt\"',
				title: 'Stringausdruck'
			}),
			$author$project$Main$BinaryExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Int', isCorrect: false},
						{code: 'String', isCorrect: false},
						{code: 'Float', isCorrect: false},
						{code: 'Bool', isCorrect: true}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
				id: 10,
				leftExpression: 'True',
				operator: '&&',
				rightExpression: 'False',
				title: 'Boolausdruck'
			}),
			$author$project$Main$BinaryExpression(
			{
				answers: _List_fromArray(
					[
						{code: '[Int]', isCorrect: true},
						{code: 'String', isCorrect: false},
						{code: 'Float', isCorrect: false},
						{code: 'SomeType', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
				id: 11,
				leftExpression: '[1, 2]',
				operator: '++',
				rightExpression: '[3, 4]',
				title: 'Listenausdruck'
			}),
			$author$project$Main$BinaryExpression(
			{
				answers: _List_fromArray(
					[
						{code: '[(1, String)]', isCorrect: true},
						{code: '(1, String)', isCorrect: false},
						{code: 'Float', isCorrect: false},
						{code: 'SomeType', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Ausdruck?'),
				id: 12,
				leftExpression: '[(1, \"Joscha\"), (4, \"Test\")]',
				operator: '++',
				rightExpression: '[(5, \"Noch\"), (2, \"Konstantin\")]',
				title: 'Listenausdruck'
			})
		]),
	id: 2,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('In Haskell gibt es viele Operatoren, die über zwei Ausdrücke gelegt werden können. Diese Operatoren sind z.B. +, -, *, /, ++, &&, || und viele mehr.'),
				expression: 'x :: Int\nx = 1 + 2',
				id: 1,
				title: 'Zweistelliger Ausdruck'
			}
			]),
		id: 1,
		title: 'Zweistellige Ausdrücke'
	},
	title: 'Typen von zweistelligen Ausdrücken'
};
var $author$project$Main$lecture3 = {
	badge: {id: '', image: $elm$core$Maybe$Nothing, name: 'Funktionen'},
	description: 'In dieser Lektion wird dein Wissen über Funktionen getestet.',
	exercises: _List_fromArray(
		[
			$author$project$Main$FunctionExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'add :: Int -> Int -> Int', isCorrect: true},
						{code: 'add :: Int -> Int -> String', isCorrect: true},
						{code: 'add :: Float', isCorrect: false},
						{code: 'Int', isCorrect: false}
					]),
				_arguments: _List_fromArray(
					['x', 'y']),
				description: $elm$core$Maybe$Just('Welchen Typ hat die folgende Funtkion?'),
				functionName: 'add',
				id: 13,
				title: 'Funktion'
			})
		]),
	id: 3,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('Funktionen sind ein zentraler Bestandteil von Haskell. Funktionen können mehrere Argumente haben und verschiedene Typen zurückgeben. In diesem Beispiel wird eine Funktion `add` definiert, die zwei Ganzzahlen addiert und das Ergebnis zurückgibt.'),
				expression: 'add :: Int -> Int -> Int\nadd x y = x + y',
				id: 1,
				title: 'Einfache Funktion'
			}
			]),
		id: 1,
		title: 'Funktionen in Haskell'
	},
	title: 'Funktionen'
};
var $author$project$Main$lecture4 = {
	badge: {id: 'guards', image: $elm$core$Maybe$Nothing, name: 'Guards'},
	description: 'In dieser Lektion wird dein Wissen über Guards getestet.',
	exercises: _List_fromArray(
		[
			$author$project$Main$GuardExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'guardFunction :: Int -> String', isCorrect: true},
						{code: 'guardFunction :: String', isCorrect: false},
						{code: 'guardFunction :: Float', isCorrect: false},
						{code: 'guardFunction :: SomeType', isCorrect: false}
					]),
				_arguments: _List_fromArray(
					['x']),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Guard-Ausdruck?'),
				expression: '\n\t | x > 0 = \"größer 0\"' + ('\n\t | x < 0 = \"kleiner 0\"' + '\n\t | otherwise = \"gleich 0\"'),
				functionName: 'guardFunction',
				id: 14,
				title: 'Guard-Ausdruck'
			})
		]),
	id: 4,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('Guards sind eine Möglichkeit, Bedingungen in Haskell übersichtlich und lesbar zu gestalten. Sie ermöglichen es, verschiedene Fälle einer Funktion durch Bedingungen zu unterscheiden. Guards werden mit einem senkrechten Strich (|) eingeleitet und können mehrere Bedingungen enthalten, die nacheinander geprüft werden.'),
				expression: 'absolute :: Int -> Int\nabsolute x\n    | x >= 0 = x\n    | otherwise = -x',
				id: 0,
				title: 'Guard-Ausdrücke'
			},
				{
				description: $elm$core$Maybe$Just('Diese Funktion berechnet den absoluten Wert einer Zahl. Wenn die Zahl größer oder gleich 0 ist, wird sie direkt zurückgegeben. Andernfalls wird ihr negatives Pendant zurückgegeben.'),
				expression: 'absolute :: Int -> Int\nabsolute x\n    | x >= 0 = x\n    | otherwise = -x',
				id: 1,
				title: 'Einfacher Guard-Ausdruck'
			}
			]),
		id: 1,
		title: 'Guards in Haskell'
	},
	title: 'Guards'
};
var $author$project$Main$lecture5 = {
	badge: {id: 'pattern-matching', image: $elm$core$Maybe$Nothing, name: 'Pattern Matching'},
	description: 'In dieser Lektion wird dein Wissen über Pattern Matching getestet.',
	exercises: _List_fromArray(
		[
			$author$project$Main$PatternMatchingExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'f :: Int -> Int -> Int', isCorrect: true},
						{code: 'f :: Int -> String -> Int', isCorrect: false},
						{code: 'f :: Int -> Int -> String', isCorrect: false},
						{code: 'f :: Int -> Int -> Float', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat der folgende Pattern Matching Ausdruck?'),
				id: 15,
				patterns: _List_fromArray(
					['f _ 0 = 0', 'f 1 _ = 1', 'f x y = x + y']),
				title: 'Pattern Matching'
			})
		]),
	id: 5,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('Pattern Matching ist eine leistungsstarke Funktion in Haskell, die es ermöglicht, Werte anhand ihrer Struktur zu zerlegen und zu analysieren. Es wird häufig in Funktionen verwendet, um verschiedene Fälle zu behandeln.'),
				expression: '',
				id: 1,
				title: 'Einfaches Pattern Matching'
			},
				{
				description: $elm$core$Maybe$Just('In diesem Beispiel wird die Funktion `f` definiert, die eine Ganzzahl als Eingabe nimmt und einen String zurückgibt. Für die Eingabe `0` gibt sie \"Null\" zurück, für `1` \"Eins\" und für alle anderen Werte \"Andere Zahl\".'),
				expression: 'f :: Int -> String\nf 0 = \"Null\"\nf 1 = \"Eins\"\nf _ = \"Andere Zahl\"',
				id: 1,
				title: 'Einfaches Pattern Matching'
			}
			]),
		id: 1,
		title: 'Pattern Matching in Haskell'
	},
	title: 'Pattern Matching'
};
var $author$project$Main$course1 = {
	description: 'Lerne unterschiedliche Arten von Ausdrücken in Haskell kennen.',
	id: 1,
	lectures: _List_fromArray(
		[$author$project$Main$lecture1, $author$project$Main$lecture2, $author$project$Main$lecture3, $author$project$Main$lecture4, $author$project$Main$lecture5]),
	title: 'Ausdrücke'
};
var $author$project$Main$dataTypesLecture = {
	badge: {id: 'data-types', image: $elm$core$Maybe$Nothing, name: 'Datentypen'},
	description: 'In dieser Lektion lernst du Datentypen in Haskell kennen und wie man sie selbst definiert.',
	exercises: _List_fromArray(
		[
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Ja', isCorrect: true},
						{code: 'Nein', isCorrect: false},
						{code: 'Weiß nicht', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Bist du bereit weiter zu machen?'),
				expression: '42',
				id: 20,
				title: 'Verstanden?'
			})
		]),
	id: 8,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('Haskell erlaubt es, eigene Datentypen zu definieren. Diese können komplexe Strukturen repräsentieren und ermöglichen eine klare und präzise Modellierung von Daten.'),
				expression: '',
				id: 0,
				title: 'Datentypen in Haskell'
			},
				{
				description: $elm$core$Maybe$Just('Das Schlüsselwort `data` leitet einen neuen Datentyp ein, gefolgt vom Namen. Daann folgenden die möglichen Werte (Typkonstruktoren) des Datentyps, die durch das Pipe-Zeichen (`|`) getrennt sind.'),
				expression: 'data Auto = Audi \n | BMW \n | Mercedes \n | Tesla \n\nx = Mercedes',
				id: 0,
				title: 'Auto-Datentyp'
			},
				{
				description: $elm$core$Maybe$Just('Hier wird ein Datentyp `Person` mit einem Typkonstruktor definiert, der zwei Felder hat: einen `String` für den Namen und einen `Int` für das Alter. Der Ausdruck `joscha = Person \"Joscha\" 20` erstellt eine Instanz des Datentyps `Person` mit dem Namen \"Joscha\" und dem Alter 32.'),
				expression: 'data Person = Person String Int\n\njoscha = Person \"Joscha\" 32',
				id: 1,
				title: 'Person-Datentyp mit Feldern'
			},
				{
				description: $elm$core$Maybe$Just('Hier wird ein generischer Datentyp `Maybe` definiert, der entweder den Wert `Nothing` oder einen Wert des Typs `a` enthalten kann. Der Ausdruck `x = Just 42` erstellt eine Instanz des Datentyps `Maybe Int`, die den Wert `42` enthält. Der Typ `Maybe` ist ein Beispiel für einen parametrisierten Datentyp, der es ermöglicht, verschiedene Typen zu verwenden.'),
				expression: 'data Maybe a = Nothing | Just a\n\nx = Just 42',
				id: 2,
				title: 'Generische Datentypen - Beispiel Maybe'
			},
				{
				description: $elm$core$Maybe$Just('Hier wird ein generischer Datentyp `List` definiert, der entweder leer (`Nil`) oder ein Element (`Cons a (List a)`) enthalten kann. Der Ausdruck `x = Cons 1 (Cons 2 (Cons 3 Nil))` erstellt eine Instanz des Datentyps `List Int`, die die Werte `1`, `2` und `3` enthält. Dieser Datentyp ist zugleich generisch und rekursiv, da der Typkonstruktor Cons als zweiten Parameter den Typen List selbst enthält.'),
				expression: 'data List a = Nil | Cons a (List a)\n\nx = Cons 1 (Cons 2 (Cons 3 Nil))',
				id: 3,
				title: 'Generische Datentypen - Liste'
			}
			]),
		id: 0,
		title: 'Datentypen'
	},
	title: 'Weitere Datentypen'
};
var $author$project$Main$operatorsLecture = {
	badge: {id: 'operators', image: $elm$core$Maybe$Nothing, name: 'Operatoren'},
	description: 'In dieser Lektion lernst du einige Operatoren in Haskell kennen.',
	exercises: _List_fromArray(
		[
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Ja', isCorrect: true}
					]),
				description: $elm$core$Maybe$Just('Bist du bereit weiter zu machen?'),
				expression: '42',
				id: 19,
				title: 'Verstanden?'
			})
		]),
	id: 6,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('Haskell bietet eine Vielzahl von Operatoren, die auf verschiedene Datentypen angewendet werden können. Diese Operatoren ermöglichen es, Ausdrücke zu kombinieren und zu manipulieren.'),
				expression: '',
				id: 0,
				title: 'Operatoren in Haskell'
			},
				{
				description: $elm$core$Maybe$Just('`+` nimmt zwei Zahlen und rechnet diese zusammen. Weitere arithmetische Operatoren wie `-`, `*` und `/` gibt es ebenfalls.'),
				expression: '1 + 1',
				id: 1,
				title: 'Plus-Operator (Addition)'
			},
				{
				description: $elm$core$Maybe$Just('Vergleichen Werte miteinander (in diesem Fall Zahlen) und liefert einen booleschen Wert zurück.'),
				expression: '2 < 3',
				id: 2,
				title: 'Vergleichsoperatoren `<`, `>`, `==`'
			},
				{
				description: $elm$core$Maybe$Just('`++` nimmt zwei Listen und fügt diese zusammen. Da String gleichbedeutend ist mit [Char] (List von Char), kann `++` auch auf diese angewendet werden.'),
				expression: '[1, 2] ++ [3, 4]',
				id: 3,
				title: '++-Operator (Konkatenation)'
			},
				{
				description: $elm$core$Maybe$Just('`:` ist ein spezieller Operator, der ein Element an den Anfang einer Liste anfügt. Er wird auch als `cons` bezeichnet.'),
				expression: '1 : [2, 3]',
				id: 4,
				title: ':-Operator (cons)'
			},
				{
				description: $elm$core$Maybe$Just('`&&` nimmt zwei Boolesche Werte und gibt `True` zurück, wenn beide `True` sind. `||` als oder-Operator ist ebenfalls vorhanden.'),
				expression: 'True && False',
				id: 5,
				title: '&&-Operator (und)'
			},
				{
				description: $elm$core$Maybe$Just('Mit diesem Beispiel soll verdeutlicht werden, dass man Operatoren miteinander verknüpfen kann.'),
				expression: '(1 < 3) && (5 < 6)',
				id: 6,
				title: '&&-Operator (und)'
			}
			]),
		id: 0,
		title: 'Operatoren in Haskell'
	},
	title: 'Operatoren'
};
var $author$project$Main$simpleDataTypesLecture = {
	badge: {id: 'simple-data-types', image: $elm$core$Maybe$Nothing, name: 'Einfache Datentypen'},
	description: 'In dieser Lektion lernst du einfache Datentypen in Haskell kennen.',
	exercises: _List_fromArray(
		[
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'Ja', isCorrect: true},
						{code: 'Nein', isCorrect: false},
						{code: 'Weiß nicht', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Bist du bereit weiter zu machen?'),
				expression: '42',
				id: 19,
				title: 'Verstanden?'
			})
		]),
	id: 7,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('In Haskell gibt es verschiedene einfache Datentypen wie `Int`, `Float`, `Bool` und `String`. Diese Datentypen sind die Bausteine für komplexere Datenstrukturen und ermöglichen es, verschiedene Arten von Werten zu repräsentieren.'),
				expression: '',
				id: 0,
				title: 'Einfache Datentypen in Haskell'
			},
				{
				description: $elm$core$Maybe$Just('Eine Ganzzahl ist ein einfacher Datentyp, der ganze Zahlen repräsentiert.'),
				expression: 'x :: Int\nx = 42',
				id: 0,
				title: 'Ganzzahl'
			},
				{
				description: $elm$core$Maybe$Just('Haskell kann einen Typ ableiten (in den meisten Fällen), auch wenn er nicht explizit angegeben wird. Das nennt sich Typinferenz.'),
				expression: 'x = 42',
				id: 1,
				title: 'Ganzzahl'
			},
				{
				description: $elm$core$Maybe$Just('Eine Gleitkommazahl ist ein Datentyp, der Fließkommazahlen repräsentiert.'),
				expression: 'x :: Float\nx = 3.14',
				id: 2,
				title: 'Gleitkommazahl'
			},
				{
				description: $elm$core$Maybe$Just('Strings sind Zeichenketten, die Text repräsentieren.'),
				expression: 'x :: String\nx = \"Hallo Welt\"',
				id: 3,
				title: 'String'
			},
				{
				description: $elm$core$Maybe$Just('Strings sind in Haskell Listen von Zeichen, die auch als `[Char]` dargestellt werden können. Das bedeutet, dass ein String in Haskell eine Liste von `Char`-Elementen ist.'),
				expression: 'x :: [Char]\nx =  \"Hallo Welt\"',
				id: 4,
				title: 'String bzw. Liste von Zeichen'
			},
				{
				description: $elm$core$Maybe$Just('Ein Boolescher Datentyp kann entweder `True` oder `False` sein.'),
				expression: 'x :: Bool\nx = True',
				id: 5,
				title: 'Bool'
			}
			]),
		id: 0,
		title: 'Einfache Datentypen'
	},
	title: 'Einfache Datentypen'
};
var $author$project$Main$simpleHaskellProgramLecture = {
	badge: {id: 'simple-haskell-program', image: $elm$core$Maybe$Nothing, name: 'Einfaches Haskell-Programm'},
	description: 'In dieser Lektion wird ein einfaches Haskell-Programm vorgestellt.',
	exercises: _List_fromArray(
		[
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'IO ()', isCorrect: true},
						{code: 'String', isCorrect: false},
						{code: 'Int', isCorrect: false},
						{code: 'SomeType', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welchen Typ hat die folgende Funktion?'),
				expression: 'main :: IO ()\nmain = do\n    putStrLn \"Hallo, Welt!\"',
				id: 16,
				title: 'Einfaches Haskell-Programm'
			}),
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'In der main-Funktion', isCorrect: false},
						{code: 'In den Klammern nach dem Modulnamen', isCorrect: true},
						{code: 'In der Datei, die das Programm ausführt', isCorrect: false},
						{code: 'In der Datei, die die Funktion importiert', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Wo müssen Funktionen eines Moduls stehen, damit sie in anderen Modulen verwendet werden können?'),
				expression: 'module Main (main) where',
				id: 17,
				title: 'Einfaches Haskell-Programm'
			}),
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: 'String -> IO ()', isCorrect: true},
						{code: 'IO ()', isCorrect: false},
						{code: 'String', isCorrect: false},
						{code: 'Int', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Was ist der Typ der Funktion `putStrLn`?'),
				expression: 'putStrLn :: String -> IO ()',
				id: 18,
				title: 'Einfaches Haskell-Programm'
			})
		]),
	id: 6,
	learningContent: {
		examples: _List_fromArray(
			[
				{description: $elm$core$Maybe$Nothing, expression: 'module Main (main) where\n\nmain :: IO ()\nmain = do\n    putStrLn \"Hallo, Welt!\"', id: 0, title: 'module'},
				{
				description: $elm$core$Maybe$Just('Diese Zeile definiert ein Haskell-Modul namens `Main`, das die Funktion `main` exportiert. Das Modul ist der Einstiegspunkt für das Programm. In den Klammern sind all die Funktionen aufgelistet, die von diesem Modul exportiert werden und somit von anderen Modulen verwendet werden können.'),
				expression: 'module Main (main) where',
				id: 0,
				title: 'module'
			},
				{
				description: $elm$core$Maybe$Just('Die `main`-Funktion ist der Einstiegspunkt für das Programm. Sie hat den Typ `IO ()`, was bedeutet, dass innerhalb dieser Funktion Eingabe/Ausgabe-Operation verwendet werden.'),
				expression: 'main :: IO ()',
				id: 1,
				title: 'main-Funktion'
			},
				{
				description: $elm$core$Maybe$Just('Die `do`-Notation ermöglicht es, mehrere IO-Operationen in einer Sequenz auszuführen. In diesem Fall wird die Funktion `putStrLn` verwendet, um den Text \"Hallo, Welt!\" auf der Konsole auszugeben. Diese `do`-Notation ist eine spezielle Syntax in Haskell, die es ermöglicht, mehrere IO-Operationen in einer lesbaren Weise zu kombinieren und wird hier nicht weiter behandelt.'),
				expression: 'main = do\n    putStrLn \"Hallo, Welt!\"',
				id: 2,
				title: 'main-Funktion'
			},
				{
				description: $elm$core$Maybe$Just('Die Beispiele in dieser Anwendung werden außerhalb der main-Funktion defininiert und können dann in der main-Funktion oder in anderen Funktionen verwendet werden. In diesem Beispiel wird eine Funktion `add` definiert, die zwei Ganzzahlen addiert und das Ergebnis zurückgibt. Der Typ der Funktion ist `Int -> Int -> Int`, was bedeutet, dass sie zwei Ganzzahlen als Eingabe nimmt und eine Ganzzahl zurückgibt.'),
				expression: 'add :: Int -> Int -> Int\nadd x y = x + y',
				id: 3,
				title: 'earn you a haskell'
			}
			]),
		id: 0,
		title: 'Einfaches Haskell-Programm'
	},
	title: 'Einfaches Haskell-Programm'
};
var $author$project$Main$syntaxLecture = {
	badge: {id: 'let-in-where', image: $elm$core$Maybe$Nothing, name: 'Let-In und Where'},
	description: 'In dieser Lektion lernst du die Unterschiede und Anwendungsfälle von `let-in` und `where` in Haskell kennen.',
	exercises: _List_fromArray(
		[
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: '12', isCorrect: true},
						{code: '7', isCorrect: false},
						{code: '0', isCorrect: false},
						{code: 'Fehler', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Was ist das Ergebnis des folgenden Ausdrucks?'),
				expression: 'let x = 3\n    y = 4\nin x * y',
				id: 21,
				title: 'Let-In Übung'
			}),
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: '5', isCorrect: true},
						{code: '2', isCorrect: false},
						{code: '3', isCorrect: false},
						{code: 'Fehler', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Was ist das Ergebnis des Aufrufs der Funktion `f 2`?'),
				expression: 'f x = x + y\n  where y = 3',
				id: 22,
				title: 'Where Übung'
			}),
			$author$project$Main$SingleExpression(
			{
				answers: _List_fromArray(
					[
						{code: '`let-in` kann überall verwendet werden, `where` nur in Funktionsdefinitionen.', isCorrect: true},
						{code: '`where` kann überall verwendet werden, `let-in` nur in Funktionsdefinitionen.', isCorrect: false},
						{code: '`let-in` und `where` sind identisch.', isCorrect: false},
						{code: '`let-in` ist schneller als `where`.', isCorrect: false}
					]),
				description: $elm$core$Maybe$Just('Welcher der folgenden Aussagen ist korrekt?'),
				expression: '',
				id: 23,
				title: 'Let-In und Where Vergleich'
			})
		]),
	id: 9,
	learningContent: {
		examples: _List_fromArray(
			[
				{
				description: $elm$core$Maybe$Just('`let-in` wird verwendet, um lokale Bindungen innerhalb eines Ausdrucks zu definieren. In diesem Beispiel werden `x` und `y` innerhalb des `let`-Blocks definiert und im `in`-Block verwendet.'),
				expression: 'let x = 5\n    y = 6\nin x + y',
				id: 1,
				title: 'Let-In Beispiel'
			},
				{
				description: $elm$core$Maybe$Just('`where` wird verwendet, um lokale Bindungen am Ende einer Funktion zu definieren. In diesem Beispiel wird `y` im `where`-Block definiert und in der Funktion `f` verwendet.'),
				expression: 'f x = x + y\n  where y = 5',
				id: 2,
				title: 'Where Beispiel'
			},
				{
				description: $elm$core$Maybe$Just('`let-in` ist ein Ausdruck und kann überall verwendet werden, während `where` nur in Funktionsdefinitionen vorkommt. `let-in` ist nützlich für temporäre Berechnungen, während `where` für Klarheit in Funktionsdefinitionen sorgt.'),
				expression: 'let x = 5 in x * 2  -- Gültig nur innerhalb des Ausdrucks\nf x = x + y\n  where y = 5  -- Gültig für die gesamte Funktion',
				id: 3,
				title: 'Unterschiede zwischen Let-In und Where'
			}
			]),
		id: 1,
		title: 'Let-In und Where in Haskell'
	},
	title: 'Let-In und Where'
};
var $author$project$Main$foundations = {
	description: 'Lerne die Grundlagen von Haskell kennen.',
	id: 0,
	lectures: _List_fromArray(
		[$author$project$Main$simpleHaskellProgramLecture, $author$project$Main$simpleDataTypesLecture, $author$project$Main$dataTypesLecture, $author$project$Main$operatorsLecture, $author$project$Main$syntaxLecture]),
	title: 'Grundlagen'
};
var $author$project$Main$coursesExamples = _List_fromArray(
	[$author$project$Main$foundations, $author$project$Main$course1]);
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$random$Random$constant = function (value) {
	return $elm$random$Random$Generator(
		function (seed) {
			return _Utils_Tuple2(value, seed);
		});
};
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0.a;
		var genB = _v1.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v2 = genA(seed0);
				var a = _v2.a;
				var seed1 = _v2.b;
				var _v3 = genB(seed1);
				var b = _v3.a;
				var seed2 = _v3.b;
				return _Utils_Tuple2(
					A2(func, a, b),
					seed2);
			});
	});
var $author$project$Main$sequence = function (gens) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (gen, acc) {
				return A3($elm$random$Random$map2, $elm$core$List$cons, gen, acc);
			}),
		$elm$random$Random$constant(_List_Nil),
		gens);
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Main$shuffle = function (list) {
	var generatePairs = $author$project$Main$sequence(
		A2(
			$elm$core$List$map,
			function (item) {
				return A2(
					$elm$random$Random$map,
					function (r) {
						return _Utils_Tuple2(r, item);
					},
					A2($elm$random$Random$float, 0, 1));
			},
			list));
	return A2(
		$elm$random$Random$map,
		A2(
			$elm$core$Basics$composeL,
			$elm$core$List$map($elm$core$Tuple$second),
			$elm$core$List$sortBy($elm$core$Tuple$first)),
		generatePairs);
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Basics$xor = _Basics_xor;
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'NoOp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'GoToCoursesOverview':
				switch (model.$) {
					case 'CoursesOverview':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'CoursePage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'LecturePage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'LearningContentPage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'RunningQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'FinishedQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					case 'WinningQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'EnteringName':
				var name = msg.a;
				if (model.$ === 'Landing') {
					return _Utils_Tuple2(
						A2(
							$author$project$Main$Landing,
							$elm$core$Maybe$Just(name),
							($author$project$Main$checkUsername(
								$elm$core$Maybe$Just(name)) !== (!$elm$core$String$length(name))) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just($author$project$Main$UsernameIncorrect)),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'EnteringNameDone':
				if (model.$ === 'Landing') {
					var maybeUsername = model.a;
					var maybeError = model.b;
					if (maybeError.$ === 'Just') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						if (maybeUsername.$ === 'Just') {
							var name = maybeUsername.a;
							return _Utils_Tuple2(
								A2(
									$author$project$Main$CoursesOverview,
									A2($author$project$Main$User, name, _List_Nil),
									$author$project$Main$coursesExamples),
								$elm$core$Platform$Cmd$none);
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'SelectCourse':
				var course = msg.a;
				switch (model.$) {
					case 'CoursesOverview':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'LecturePage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'LearningContentPage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'RunningQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'FinishedQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'WinningQuiz':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'SelectLecture':
				var lecture = msg.a;
				if (model.$ === 'CoursePage') {
					var user = model.a;
					var course = model.b;
					return _Utils_Tuple2(
						A3($author$project$Main$LecturePage, user, course, lecture),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'StartLecture':
				if (model.$ === 'LecturePage') {
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					return _Utils_Tuple2(
						A4($author$project$Main$LearningContentPage, user, course, lecture, 0),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'SelectAnswer':
				var exercise = msg.a;
				var answer = msg.b;
				if (model.$ === 'RunningQuiz') {
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					var remainingExercises = model.d;
					var answeredExercises = model.e;
					var newRemainingExercises = A2(
						$elm$core$List$filter,
						function (e) {
							return !_Utils_eq(e, exercise);
						},
						remainingExercises);
					var newAnswers = _Utils_ap(
						answeredExercises,
						_List_fromArray(
							[
								_Utils_Tuple2(exercise, answer)
							]));
					if (!$elm$core$List$length(newRemainingExercises)) {
						if (A2(
							$elm$core$List$all,
							function (_v10) {
								var a = _v10.b;
								return a.isCorrect;
							},
							newAnswers)) {
							return _Utils_Tuple2(
								A3($author$project$Main$WinningQuiz, user, course, lecture),
								$elm$core$Platform$Cmd$none);
						} else {
							var wrongExercises = A2(
								$elm$core$List$filter,
								function (_v11) {
									var a = _v11.b;
									return !a.isCorrect;
								},
								newAnswers);
							return _Utils_Tuple2(
								A5($author$project$Main$FinishedQuiz, user, course, lecture, wrongExercises, 0),
								$elm$core$Platform$Cmd$none);
						}
					} else {
						return _Utils_Tuple2(
							A5($author$project$Main$RunningQuiz, user, course, lecture, newRemainingExercises, newAnswers),
							function () {
								var _v12 = $elm$core$List$head(newRemainingExercises);
								if (_v12.$ === 'Just') {
									var h = _v12.a;
									return A2(
										$elm$random$Random$generate,
										$author$project$Main$ShuffleAnswers,
										$author$project$Main$shuffle(
											function () {
												switch (h.$) {
													case 'SingleExpression':
														var m = h.a;
														return m.answers;
													case 'BinaryExpression':
														var m = h.a;
														return m.answers;
													case 'FunctionExpression':
														var m = h.a;
														return m.answers;
													case 'GuardExpression':
														var m = h.a;
														return m.answers;
													default:
														var m = h.a;
														return m.answers;
												}
											}()));
								} else {
									return $elm$core$Platform$Cmd$none;
								}
							}());
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'Next':
				switch (model.$) {
					case 'LecturePage':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						return _Utils_Tuple2(
							A4($author$project$Main$LearningContentPage, user, course, lecture, 0),
							$elm$core$Platform$Cmd$none);
					case 'FinishedQuiz':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						var wrongExercises = model.d;
						var i = model.e;
						var nextIndex = A2(
							$elm$core$Basics$min,
							$elm$core$List$length(wrongExercises) - 1,
							i + 1);
						return _Utils_Tuple2(
							A5($author$project$Main$FinishedQuiz, user, course, lecture, wrongExercises, nextIndex),
							$elm$core$Platform$Cmd$none);
					case 'LearningContentPage':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						var i = model.d;
						var nextIndex = A2(
							$elm$core$Basics$min,
							$elm$core$List$length(lecture.learningContent.examples),
							i + 1);
						return (_Utils_cmp(
							nextIndex,
							$elm$core$List$length(lecture.learningContent.examples)) > -1) ? _Utils_Tuple2(
							A5($author$project$Main$RunningQuiz, user, course, lecture, lecture.exercises, _List_Nil),
							$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
							A4($author$project$Main$LearningContentPage, user, course, lecture, nextIndex),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'Prev':
				switch (model.$) {
					case 'LecturePage':
						var user = model.a;
						var course = model.b;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none);
					case 'FinishedQuiz':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						var wrongExercises = model.d;
						var i = model.e;
						return _Utils_Tuple2(
							A5(
								$author$project$Main$FinishedQuiz,
								user,
								course,
								lecture,
								wrongExercises,
								A2($elm$core$Basics$max, 0, i - 1)),
							$elm$core$Platform$Cmd$none);
					case 'LearningContentPage':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						var i = model.d;
						return (i <= 0) ? _Utils_Tuple2(
							A2($author$project$Main$CoursePage, user, course),
							$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
							A4(
								$author$project$Main$LearningContentPage,
								user,
								course,
								lecture,
								A2($elm$core$Basics$max, 0, i - 1)),
							$elm$core$Platform$Cmd$none);
					case 'CoursePage':
						var user = model.a;
						return _Utils_Tuple2(
							A2($author$project$Main$CoursesOverview, user, $author$project$Main$coursesExamples),
							$elm$core$Platform$Cmd$none);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'AddBadge':
				var badge = msg.a;
				if (model.$ === 'WinningQuiz') {
					var user = model.a;
					var course = model.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Main$CoursePage,
							_Utils_update(
								user,
								{
									badges: A2(
										$elm$core$List$any,
										function (b) {
											return _Utils_eq(b, badge);
										},
										user.badges) ? user.badges : A2($elm$core$List$cons, badge, user.badges)
								}),
							course),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'ShuffleExercises':
				switch (model.$) {
					case 'LearningContentPage':
						var lecture = model.c;
						return _Utils_Tuple2(
							model,
							A2(
								$elm$random$Random$generate,
								$author$project$Main$StartQuiz,
								$author$project$Main$shuffle(lecture.exercises)));
					case 'FinishedQuiz':
						var lecture = model.c;
						return _Utils_Tuple2(
							model,
							A2(
								$elm$random$Random$generate,
								$author$project$Main$StartQuiz,
								$author$project$Main$shuffle(lecture.exercises)));
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'StartQuiz':
				var shuffledExercises = msg.a;
				var command = function () {
					var _v19 = $elm$core$List$head(shuffledExercises);
					if (_v19.$ === 'Just') {
						var h = _v19.a;
						return A2(
							$elm$random$Random$generate,
							$author$project$Main$ShuffleAnswers,
							$author$project$Main$shuffle(
								function () {
									switch (h.$) {
										case 'SingleExpression':
											var m = h.a;
											return m.answers;
										case 'BinaryExpression':
											var m = h.a;
											return m.answers;
										case 'FunctionExpression':
											var m = h.a;
											return m.answers;
										case 'GuardExpression':
											var m = h.a;
											return m.answers;
										default:
											var m = h.a;
											return m.answers;
									}
								}()));
					} else {
						return $elm$core$Platform$Cmd$none;
					}
				}();
				switch (model.$) {
					case 'LearningContentPage':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						return _Utils_Tuple2(
							A5($author$project$Main$RunningQuiz, user, course, lecture, shuffledExercises, _List_Nil),
							command);
					case 'FinishedQuiz':
						var user = model.a;
						var course = model.b;
						var lecture = model.c;
						return _Utils_Tuple2(
							A5($author$project$Main$RunningQuiz, user, course, lecture, shuffledExercises, _List_Nil),
							command);
					default:
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			default:
				var shuffled = msg.a;
				if (model.$ === 'RunningQuiz') {
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					var exercises = model.d;
					var studentAnswers = model.e;
					return _Utils_Tuple2(
						A5(
							$author$project$Main$RunningQuiz,
							user,
							course,
							lecture,
							function () {
								var _v22 = $elm$core$List$head(exercises);
								if (_v22.$ === 'Just') {
									var h = _v22.a;
									return A2(
										$elm$core$List$cons,
										function () {
											switch (h.$) {
												case 'SingleExpression':
													var m = h.a;
													return $author$project$Main$SingleExpression(
														_Utils_update(
															m,
															{answers: shuffled}));
												case 'BinaryExpression':
													var m = h.a;
													return $author$project$Main$BinaryExpression(
														_Utils_update(
															m,
															{answers: shuffled}));
												case 'FunctionExpression':
													var m = h.a;
													return $author$project$Main$FunctionExpression(
														_Utils_update(
															m,
															{answers: shuffled}));
												case 'GuardExpression':
													var m = h.a;
													return $author$project$Main$GuardExpression(
														_Utils_update(
															m,
															{answers: shuffled}));
												default:
													var m = h.a;
													return $author$project$Main$PatternMatchingExpression(
														_Utils_update(
															m,
															{answers: shuffled}));
											}
										}(),
										function () {
											var _v24 = $elm$core$List$tail(exercises);
											if (_v24.$ === 'Just') {
												var tail = _v24.a;
												return tail;
											} else {
												return _List_Nil;
											}
										}());
								} else {
									return _List_Nil;
								}
							}(),
							studentAnswers),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Main$AddBadge = function (a) {
	return {$: 'AddBadge', a: a};
};
var $author$project$Main$EnteringName = function (a) {
	return {$: 'EnteringName', a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$autofocus = $elm$html$Html$Attributes$boolProperty('autofocus');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Main$SelectLecture = function (a) {
	return {$: 'SelectLecture', a: a};
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $author$project$Images$Images$genericBadgeSvg = function (h) {
	return A2(
		$elm$svg$Svg$svg,
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$viewBox('0 0 80 80'),
				$elm$svg$Svg$Attributes$height(h)
			]),
		_List_fromArray(
			[
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#59C5EF'),
						$elm$svg$Svg$Attributes$d('M30.432 39.985 20.474 57.2l8.428-1.008 7.57-13.088z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#2BA0BF'),
						$elm$svg$Svg$Attributes$d('m36.472 43.104-7.57 13.088L32.229 64l10.282-17.777z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#59C5EF'),
						$elm$svg$Svg$Attributes$d('M39.107 49.022 47.771 64l3.327-7.808-5.679-9.819z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#2BA0BF'),
						$elm$svg$Svg$Attributes$d('m51.73 43.723-6.311 2.65 5.679 9.819 8.428 1.008z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#FFBD66'),
						$elm$svg$Svg$Attributes$d('m57.38 32.571-.916-.664a1.72 1.72 0 0 1-.457-2.294l.592-.964a1.72 1.72 0 0 0-1.067-2.574l-1.1-.263a1.723 1.723 0 0 1-1.3-1.945l.178-1.116a1.71 1.71 0 0 0-.482-1.488 1.711 1.711 0 0 0-1.488-.483l-1.117.178a1.721 1.721 0 0 1-1.945-1.3l-.262-1.1a1.72 1.72 0 0 0-2.575-1.067l-.963.593a1.72 1.72 0 0 1-2.294-.456l-.664-.916a1.721 1.721 0 0 0-2.787 0l-.664.916a1.72 1.72 0 0 1-2.294.456l-.964-.592a1.72 1.72 0 0 0-2.574 1.066l-.263 1.1a1.722 1.722 0 0 1-1.945 1.3l-1.117-.178a1.72 1.72 0 0 0-1.986 1.546l-.003.035c-.008.126-.003.257.018.388l.179 1.117a1.72 1.72 0 0 1-1.3 1.945l-1.1.263a1.72 1.72 0 0 0-1.066 2.574l.592.964a1.721 1.721 0 0 1-.457 2.295l-.916.663a1.72 1.72 0 0 0 0 2.786l.916.664a1.72 1.72 0 0 1 .457 2.294l-.592.964a1.72 1.72 0 0 0 1.066 2.574l1.1.264a1.721 1.721 0 0 1 1.3 1.944l-.179 1.116a1.711 1.711 0 0 0 .483 1.488v.002c.371.371.91.574 1.488.482l1.117-.179a1.72 1.72 0 0 1 1.945 1.3l.262 1.1a1.72 1.72 0 0 0 2.575 1.066l.964-.592a1.721 1.721 0 0 1 2.294.457l.664.915c.687.948 2.1.948 2.787 0l.664-.915a1.721 1.721 0 0 1 2.294-.457l.964.592a1.72 1.72 0 0 0 2.574-1.066l.263-1.1a1.722 1.722 0 0 1 1.945-1.3l1.117.179c.132.021.262.026.388.018l.035-.002a1.722 1.722 0 0 0 1.546-1.986l-.178-1.117a1.72 1.72 0 0 1 1.3-1.945l1.1-.263a1.72 1.72 0 0 0 1.067-2.574l-.592-.964a1.72 1.72 0 0 1 .457-2.294l.916-.664a1.72 1.72 0 0 0 0-2.786z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#FDDC85'),
						$elm$svg$Svg$Attributes$d('M51.604 33.845c0-6.339-5.138-11.478-11.477-11.478-6.338 0-11.477 5.139-11.477 11.478 0 6.339 5.138 11.476 11.477 11.476s11.477-5.137 11.477-11.476z')
					]),
				_List_Nil),
				A2(
				$elm$svg$Svg$path,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$fill('#FFBD66'),
						$elm$svg$Svg$Attributes$d('m40.128 25.373 2.467 4.998 5.517.802-3.993 3.892.943 5.494-4.934-2.594-4.934 2.594.942-5.494-3.992-3.892 5.516-.802z')
					]),
				_List_Nil)
			]));
};
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$small = _VirtualDom_node('small');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$coursePage = F2(
	function (user, course) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('m-1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h4,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('display-5 text-center')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(course.title)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('album')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('container')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('row row-cols-1 row-cols-sm-2 row-cols-md-2 g-3')
										]),
									A2(
										$elm$core$List$map,
										function (lecture) {
											return A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('col-md-6')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('card shadow-sm h-100'),
																$elm$html$Html$Events$onClick(
																$author$project$Main$SelectLecture(lecture)),
																A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('card-title text-center')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text(lecture.title)
																	])),
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('card-body')
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$div,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('card-text')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text(lecture.description)
																			]))
																	])),
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('card-footer')
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$div,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('d-flex justify-content-between align-items-center')
																			]),
																		_List_fromArray(
																			[
																				A2(
																				$elm$html$Html$small,
																				_List_fromArray(
																					[
																						$elm$html$Html$Attributes$class('muted')
																					]),
																				_List_Nil),
																				A2(
																				$elm$html$Html$small,
																				_List_fromArray(
																					[
																						$elm$html$Html$Attributes$class('muted')
																					]),
																				_List_fromArray(
																					[
																						A2(
																						$elm$core$List$any,
																						function (b) {
																							return _Utils_eq(b, lecture.badge);
																						},
																						user.badges) ? A3($elm$core$Maybe$withDefault, $author$project$Images$Images$genericBadgeSvg, lecture.badge.image, '2em') : $elm$html$Html$text(
																						function () {
																							var l = $elm$core$List$length(lecture.exercises);
																							return _Utils_ap(
																								$elm$core$String$fromInt(
																									$elm$core$List$length(lecture.learningContent.examples)) + ' Beispiele | ',
																								_Utils_ap(
																									$elm$core$String$fromInt(l),
																									(l === 1) ? ' Aufgabe' : ' Aufgaben'));
																						}())
																					]))
																			]))
																	]))
															]))
													]));
										},
										A2(
											$elm$core$List$sortBy,
											function (l) {
												return A2(
													$elm$core$List$any,
													function (b) {
														return _Utils_eq(b, l.badge);
													},
													user.badges) ? 1 : 0;
											},
											course.lectures)))
								]))
						]))
				]));
	});
var $author$project$Main$SelectCourse = function (a) {
	return {$: 'SelectCourse', a: a};
};
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$Images$Images$checkSvg = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$width('16'),
			$elm$svg$Svg$Attributes$height('16'),
			$elm$svg$Svg$Attributes$fill('currentColor'),
			$elm$svg$Svg$Attributes$class('bi bi-check-square-fill text-success'),
			$elm$svg$Svg$Attributes$viewBox('0 0 16 16')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M2 0a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2zm10.03 4.97a.75.75 0 0 1 .011 1.05l-3.992 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.75.75 0 0 1 1.08-.022z')
				]),
			_List_Nil)
		]));
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $author$project$Main$progressBarView = F2(
	function (progress, height) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('progress'),
					A2($elm$html$Html$Attributes$style, 'height', height)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('progress-bar progress-bar'),
							A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
							A2($elm$html$Html$Attributes$attribute, 'role', 'progressbar'),
							A2(
							$elm$html$Html$Attributes$attribute,
							'aria-valuenow',
							$elm$core$String$fromInt(progress)),
							A2($elm$html$Html$Attributes$attribute, 'aria-valuemin', '0'),
							A2($elm$html$Html$Attributes$attribute, 'aria-valuemax', '100'),
							A2(
							$elm$html$Html$Attributes$style,
							'width',
							$elm$core$String$fromInt(progress) + '%')
						]),
					_List_Nil)
				]));
	});
var $author$project$Main$coursesOverview = F2(
	function (user, courses) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('m-1')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h1,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('display-5 text-center')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Kursübersicht')
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('text-center')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Dir stehen ' + ($elm$core$String$fromInt(
								$elm$core$List$length(courses)) + ' Kurse zur Verfügung: '))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('album')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('container')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('row row-cols-1 row-cols-sm-2 row-cols-md-2 g-3')
										]),
									A2(
										$elm$core$List$map,
										function (course) {
											return A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('col-md-6')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$div,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('card shadow-sm'),
																$elm$html$Html$Events$onClick(
																$author$project$Main$SelectCourse(course)),
																A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('card-title text-center')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text(course.title)
																	])),
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('card-body')
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$div,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('card-text')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text(course.description)
																			]))
																	])),
																function () {
																var progress = (($elm$core$List$length(
																	A2(
																		$elm$core$List$filter,
																		function (l) {
																			return A2(
																				$elm$core$List$any,
																				function (b) {
																					return _Utils_eq(b, l.badge);
																				},
																				user.badges);
																		},
																		course.lectures)) * 100) / $elm$core$List$length(course.lectures)) | 0;
																return A2(
																	$elm$html$Html$div,
																	_List_fromArray(
																		[
																			$elm$html$Html$Attributes$class('card-footer'),
																			$elm$html$Html$Attributes$classList(
																			_List_fromArray(
																				[
																					_Utils_Tuple2('d-none', !progress)
																				]))
																		]),
																	_List_fromArray(
																		[
																			(progress < 100) ? A2($author$project$Main$progressBarView, progress, '2em') : A2(
																			$elm$html$Html$div,
																			_List_fromArray(
																				[
																					$elm$html$Html$Attributes$class('d-flex justify-content-between align-items-center')
																				]),
																			_List_fromArray(
																				[
																					A2($elm$html$Html$div, _List_Nil, _List_Nil),
																					A2(
																					$elm$html$Html$div,
																					_List_Nil,
																					_List_fromArray(
																						[$author$project$Images$Images$checkSvg]))
																				]))
																		]));
															}()
															]))
													]));
										},
										courses))
								]))
						]))
				]));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$code = _VirtualDom_node('code');
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$HCode = function (a) {
	return {$: 'HCode', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1 = {$: 'Style1'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2 = {$: 'Style2'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3 = {$: 'Style3'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4 = {$: 'Style4'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5 = {$: 'Style5'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6 = {$: 'Style6'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'elm-s');
		case 'BasicSymbol':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'elm-bs');
		case 'GroupSymbol':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'elm-gs');
		case 'Capitalized':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6, 'elm-c');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'elm-k');
		case 'Function':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'elm-f');
		case 'TypeSignature':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'elm-ts');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'elm-n');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine = function (fragments) {
	return {fragments: fragments, highlight: $elm$core$Maybe$Nothing};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak = {$: 'LineBreak'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Comment = {$: 'Comment'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default = {$: 'Default'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment = F2(
	function (toStyle, _v0) {
		var syntax = _v0.a;
		var text = _v0.b;
		switch (syntax.$) {
			case 'Normal':
				return {additionalClass: '', requiredStyle: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, text: text};
			case 'Comment':
				return {additionalClass: '', requiredStyle: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Comment, text: text};
			case 'LineBreak':
				return {additionalClass: '', requiredStyle: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, text: text};
			default:
				var c = syntax.a;
				var _v2 = toStyle(c);
				var requiredStyle = _v2.a;
				var additionalClass = _v2.b;
				return {additionalClass: additionalClass, requiredStyle: requiredStyle, text: text};
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLinesHelp = F3(
	function (toStyle, _v0, _v1) {
		var syntax = _v0.a;
		var text = _v0.b;
		var lines = _v1.a;
		var fragments = _v1.b;
		var maybeLastSyntax = _v1.c;
		if (_Utils_eq(syntax, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak)) {
			return _Utils_Tuple3(
				A2(
					$elm$core$List$cons,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine(fragments),
					lines),
				_List_fromArray(
					[
						A2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
						toStyle,
						_Utils_Tuple2(syntax, text))
					]),
				$elm$core$Maybe$Nothing);
		} else {
			if (_Utils_eq(
				$elm$core$Maybe$Just(syntax),
				maybeLastSyntax)) {
				if (fragments.b) {
					var headFrag = fragments.a;
					var tailFrags = fragments.b;
					return _Utils_Tuple3(
						lines,
						A2(
							$elm$core$List$cons,
							_Utils_update(
								headFrag,
								{
									text: _Utils_ap(text, headFrag.text)
								}),
							tailFrags),
						maybeLastSyntax);
				} else {
					return _Utils_Tuple3(
						lines,
						A2(
							$elm$core$List$cons,
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
								toStyle,
								_Utils_Tuple2(syntax, text)),
							fragments),
						maybeLastSyntax);
				}
			} else {
				return _Utils_Tuple3(
					lines,
					A2(
						$elm$core$List$cons,
						A2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
							toStyle,
							_Utils_Tuple2(syntax, text)),
						fragments),
					$elm$core$Maybe$Just(syntax));
			}
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines = F2(
	function (toStyle, revTokens) {
		return function (_v0) {
			var lines = _v0.a;
			var frags = _v0.b;
			return A2(
				$elm$core$List$cons,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine(frags),
				lines);
		}(
			A3(
				$elm$core$List$foldl,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLinesHelp(toStyle),
				_Utils_Tuple3(_List_Nil, _List_Nil, $elm$core$Maybe$Nothing),
				revTokens));
	});
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0.a;
			var _v1 = parse(s0);
			if (_v1.$ === 'Good') {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (step.$ === 'Loop') {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$parser$Parser$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment = {$: 'Comment'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr('\n'));
};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile = F2(
	function (isNotRelevant, previousParser) {
		return A2(
			$elm$parser$Parser$ignorer,
			previousParser,
			$elm$parser$Parser$chompWhile(isNotRelevant));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$inlineComment = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b)
			]);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
			$elm$parser$Parser$symbol('--'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen = F3(
	function (f, list, plist) {
		return A2(
			$elm$parser$Parser$andThen,
			function (n) {
				return f(
					_Utils_ap(n, list));
			},
			plist);
	});
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen = F3(
	function (f, list, pn) {
		return A2(
			$elm$parser$Parser$andThen,
			function (n) {
				return f(
					A2($elm$core$List$cons, n, list));
			},
			pn);
	});
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable = F2(
	function (options, revAList) {
		var defaultMap = options.defaultMap;
		var isNotRelevant = options.isNotRelevant;
		var end = options.end;
		var innerParsers = options.innerParsers;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							defaultMap(end),
							revAList)),
					$elm$parser$Parser$symbol(end)),
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(revAList),
					$elm$parser$Parser$end),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					$elm$parser$Parser$oneOf(innerParsers)),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$always(true))))))
				]));
	});
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable = F3(
	function (nestLevel, options, revAList) {
		var defaultMap = options.defaultMap;
		var isNotRelevant = options.isNotRelevant;
		var start = options.start;
		var end = options.end;
		var innerParsers = options.innerParsers;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return (nestLevel === 1) ? $elm$parser$Parser$succeed(n) : A3($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel - 1, options, n);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								defaultMap(end),
								revAList)),
						$elm$parser$Parser$symbol(end))),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel + 1, options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$symbol(start))))),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					$elm$parser$Parser$oneOf(innerParsers)),
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(revAList),
					$elm$parser$Parser$end),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel, options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$always(true))))))
				]));
	});
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedHelp = F2(
	function (options, revAList) {
		var start = options.start;
		var end = options.end;
		var isNotRelevant = options.isNotRelevant;
		var _v0 = _Utils_Tuple2(
			$elm$core$String$uncons(options.start),
			$elm$core$String$uncons(options.end));
		if (_v0.a.$ === 'Nothing') {
			var _v1 = _v0.a;
			return $elm$parser$Parser$problem('Trying to parse a delimited helper, but the start token cannot be an empty string!');
		} else {
			if (_v0.b.$ === 'Nothing') {
				var _v2 = _v0.b;
				return $elm$parser$Parser$problem('Trying to parse a delimited helper, but the end token cannot be an empty string!');
			} else {
				var _v3 = _v0.a.a;
				var startChar = _v3.a;
				var _v4 = _v0.b.a;
				var endChar = _v4.a;
				return options.isNestable ? A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable,
					1,
					_Utils_update(
						options,
						{
							isNotRelevant: function (c) {
								return isNotRelevant(c) && ((!_Utils_eq(c, startChar)) && (!_Utils_eq(c, endChar)));
							}
						}),
					revAList) : A2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable,
					_Utils_update(
						options,
						{
							isNotRelevant: function (c) {
								return isNotRelevant(c) && (!_Utils_eq(c, endChar));
							}
						}),
					revAList);
			}
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited = function (options) {
	var start = options.start;
	var isNotRelevant = options.isNotRelevant;
	var defaultMap = options.defaultMap;
	return A2(
		$elm$parser$Parser$andThen,
		function (n) {
			return A2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedHelp,
				options,
				_List_fromArray(
					[n]));
		},
		A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				defaultMap(start)),
			$elm$parser$Parser$symbol(start)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$multilineComment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		defaultMap: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		end: '-}',
		innerParsers: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList]),
		isNestable: true,
		isNotRelevant: function (c) {
			return !$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
		},
		start: '{-'
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$inlineComment, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$multilineComment]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$BasicSymbol = {$: 'BasicSymbol'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C = function (a) {
	return {$: 'C', a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Capitalized = {$: 'Capitalized'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$GroupSymbol = {$: 'GroupSymbol'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword = {$: 'Keyword'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal = {$: 'Normal'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Number = {$: 'Number'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile = function (isNotRelevant) {
	return A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompIf(isNotRelevant)),
		$elm$parser$Parser$chompWhile(isNotRelevant));
};
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbols = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr('|'),
			_Utils_chr('.'),
			_Utils_chr('='),
			_Utils_chr('\\'),
			_Utils_chr('/'),
			_Utils_chr('('),
			_Utils_chr(')'),
			_Utils_chr('-'),
			_Utils_chr('>'),
			_Utils_chr('<'),
			_Utils_chr(':'),
			_Utils_chr('+'),
			_Utils_chr('!'),
			_Utils_chr('$'),
			_Utils_chr('%'),
			_Utils_chr('&'),
			_Utils_chr('*')
		]));
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbols);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbol = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbols = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr(','),
			_Utils_chr('['),
			_Utils_chr(']'),
			_Utils_chr('{'),
			_Utils_chr('}')
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbols);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isStringLiteralChar = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr('\"')) || _Utils_eq(
		c,
		_Utils_chr('\''));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr(' ')) || _Utils_eq(
		c,
		_Utils_chr('\t'));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isStringLiteralChar(c))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$capitalized = $elm$parser$Parser$getChompedString(
	A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar,
		$elm$parser$Parser$chompIf($elm$core$Char$isUpper)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbol = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function = {$: 'Function'};
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0.a;
	return $elm$parser$Parser$Advanced$Parser(
		function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 'Bad') {
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, false, x);
			} else {
				var a = _v1.b;
				var s1 = _v1.c;
				return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
			}
		});
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixSet = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr('+'),
			_Utils_chr('-'),
			_Utils_chr('/'),
			_Utils_chr('*'),
			_Utils_chr('='),
			_Utils_chr('.'),
			_Utils_chr('$'),
			_Utils_chr('<'),
			_Utils_chr('>'),
			_Utils_chr(':'),
			_Utils_chr('&'),
			_Utils_chr('|'),
			_Utils_chr('^'),
			_Utils_chr('?'),
			_Utils_chr('%'),
			_Utils_chr('#'),
			_Utils_chr('@'),
			_Utils_chr('~'),
			_Utils_chr('!'),
			_Utils_chr(',')
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isInfixChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function),
			b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(_Utils_Tuple0),
					$elm$parser$Parser$backtrackable(
						$elm$parser$Parser$symbol('('))),
				$elm$parser$Parser$backtrackable(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isInfixChar))),
			$elm$parser$Parser$backtrackable(
				$elm$parser$Parser$symbol(')')))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$keywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['as', 'where', 'let', 'in', 'if', 'else', 'then', 'case', 'of', 'type', 'alias']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$keywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber = function (c) {
	return $elm$core$Char$isDigit(c) || _Utils_eq(
		c,
		_Utils_chr('.'));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(_Utils_Tuple0),
		$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber)),
	$elm$parser$Parser$chompWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$negativeNumber = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(_Utils_Tuple0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('-'))),
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$negativeNumber]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable = $elm$parser$Parser$getChompedString(
	A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar,
		$elm$parser$Parser$chompIf($elm$core$Char$isLower)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$weirdText = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Number),
					b);
			},
			$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number)),
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Capitalized),
					'()')),
			$elm$parser$Parser$symbol('()')),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser,
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$BasicSymbol),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbol),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$GroupSymbol),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbol),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Capitalized),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$capitalized),
			A2(
			$elm$parser$Parser$map,
			function (n) {
				return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword(n) ? _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
					n) : _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, n);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$weirdText)
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$String = {$: 'String'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapableSet = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr('\''),
			_Utils_chr('\"'),
			_Utils_chr('\\'),
			_Utils_chr('n'),
			_Utils_chr('r'),
			_Utils_chr('t'),
			_Utils_chr('b'),
			_Utils_chr('f'),
			_Utils_chr('v')
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapableChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapableSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(_Utils_Tuple0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('\\'))),
	$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapableChar));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$elmEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Capitalized),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr('\\'));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter = {
	defaultMap: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$String),
			b);
	},
	end: '\"',
	innerParsers: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$elmEscapable]),
	isNestable: false,
	isNotRelevant: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	start: '\"'
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter,
		{end: '\'', start: '\''}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$tripleDoubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter,
		{end: '\"\"\"', start: '\"\"\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$tripleDoubleQuote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$doubleQuote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$quote]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n');
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContext = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContext,
				A2(
					$elm$parser$Parser$map,
					function (n) {
						return A2($elm$core$List$cons, n, revTokens);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$TypeSignature = {$: 'TypeSignature'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || (_Utils_eq(
		c,
		_Utils_chr('(')) || (_Utils_eq(
		c,
		_Utils_chr(')')) || (_Utils_eq(
		c,
		_Utils_chr('-')) || _Utils_eq(
		c,
		_Utils_chr(','))))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContentHelp = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$TypeSignature),
					'()')),
			$elm$parser$Parser$symbol('()')),
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$BasicSymbol),
					'->')),
			$elm$parser$Parser$symbol('->')),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
					function (c) {
						return _Utils_eq(
							c,
							_Utils_chr('(')) || (_Utils_eq(
							c,
							_Utils_chr(')')) || (_Utils_eq(
							c,
							_Utils_chr('-')) || _Utils_eq(
							c,
							_Utils_chr(','))));
					}))),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$TypeSignature),
					b);
			},
			$elm$parser$Parser$getChompedString(
				A2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant,
					$elm$parser$Parser$chompIf($elm$core$Char$isUpper)))),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant)))
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContent = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContentHelp),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$andThen,
					function (ns) {
						return A2($elm$parser$Parser$loop, ns, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContent);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$BasicSymbol),
									':'),
								revTokens)),
						$elm$parser$Parser$symbol(':')))),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2($elm$parser$Parser$loop, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar = function (c) {
	return _Utils_eq(
		c,
		_Utils_chr('-')) || _Utils_eq(
		c,
		_Utils_chr('{'));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar = $elm$parser$Parser$getChompedString(
	$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar));
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 'ExpectingKeyword', a: a};
};
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return (_Utils_eq(newOffset, -1) || (0 <= A3(
				$elm$parser$Parser$Advanced$isSubChar,
				function (c) {
					return $elm$core$Char$isAlphaNum(c) || _Utils_eq(
						c,
						_Utils_chr('_'));
				},
				newOffset,
				s.src))) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || _Utils_eq(
		c,
		_Utils_chr('('))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || (_Utils_eq(
		c,
		_Utils_chr('(')) || (_Utils_eq(
		c,
		_Utils_chr(')')) || (_Utils_eq(
		c,
		_Utils_chr(',')) || _Utils_eq(
		c,
		_Utils_chr('.')))))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpnIsSpecialChar = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || (_Utils_eq(
		c,
		_Utils_chr('(')) || _Utils_eq(
		c,
		_Utils_chr(')'))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContextNested = function (_v1) {
	var nestLevel = _v1.a;
	var revTokens = _v1.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested(
				_Utils_Tuple2(nestLevel, revTokens)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested = function (_v0) {
	var nestLevel = _v0.a;
	var revTokens = _v0.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens)));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContextNested,
				A2(
					$elm$parser$Parser$map,
					function (n) {
						return _Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens));
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							_Utils_ap(n, revTokens)));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParNest = function (_v0) {
	var nestLevel = _v0.a;
	var revTokens = _v0.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested(
				_Utils_Tuple2(nestLevel, revTokens)),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(nestLevel + 1, ns));
				},
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
							revTokens)),
					$elm$parser$Parser$symbol('('))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return (!nestLevel) ? $elm$parser$Parser$Done(ns) : $elm$parser$Parser$Loop(
						_Utils_Tuple2(nestLevel - 1, ns));
				},
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, ')'),
							revTokens)),
					$elm$parser$Parser$symbol(')'))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens)));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							function (s) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, s);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
									A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpnIsSpecialChar))))
						]))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParentheses = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, ')'),
							revTokens)),
					$elm$parser$Parser$symbol(')'))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser,
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
									function (c) {
										return _Utils_eq(
											c,
											_Utils_chr(',')) || _Utils_eq(
											c,
											_Utils_chr('.'));
									}))),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$TypeSignature),
									b);
							},
							$elm$parser$Parser$getChompedString(
								A2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant,
									$elm$parser$Parser$chompIf($elm$core$Char$isUpper)))),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function),
									b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant)))
						]))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return A2(
							$elm$parser$Parser$loop,
							_Utils_Tuple2(0, n),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParNest);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens)),
						$elm$parser$Parser$symbol('(')))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return A2($elm$parser$Parser$loop, n, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParentheses);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens)),
						$elm$parser$Parser$symbol('(')))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							$elm$core$Basics$always(
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
									'exposing')),
							$elm$parser$Parser$keyword('exposing')),
							A2(
							$elm$parser$Parser$map,
							$elm$core$Basics$always(
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
									'as')),
							$elm$parser$Parser$keyword('as')),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecIsNotRelevant)))
						]))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclarationHelp = F2(
	function (revTokens, str) {
		return (str === 'module') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
					str),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration) : A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function),
					str),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclaration = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclarationHelp(revTokens),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2($elm$parser$Parser$loop, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineStartVariable = F2(
	function (revTokens, n) {
		return ((n === 'module') || (n === 'import')) ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration) : ((n === 'port') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclaration) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword(n) ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody) : A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature)));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineStartVariable(revTokens),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (s) {
						return A2(
							$elm$parser$Parser$loop,
							_Utils_ap(s, revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (s) {
						return A2(
							$elm$parser$Parser$loop,
							A2($elm$core$List$cons, s, revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$elm = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toLines,
	$elm$core$Result$map($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$HCode));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme = function (a) {
	return {$: 'Theme', a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex = function (a) {
	return {$: 'Hex', a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor = {$: 'DefaultColor'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor = function (background) {
	return {background: background, isBold: false, isItalic: false, isUnderline: false, text: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$noEmphasis = F2(
	function (text, background) {
		return {background: background, isBold: false, isItalic: false, isUnderline: false, text: text};
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor = function (text) {
	return {background: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor, isBold: false, isItalic: false, isUnderline: false, text: text};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$requiredStyles = {
	addition: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#eaffea')),
	comment: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#969896')),
	_default: A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$noEmphasis,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#24292e'),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#ffffff')),
	deletion: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#ffecec')),
	highlight: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#fffbdd')),
	style1: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#005cc5')),
	style2: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#df5000')),
	style3: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#d73a49')),
	style4: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#0086b3')),
	style5: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#63a35c')),
	style6: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#005cc5')),
	style7: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#795da3'))
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$theme = {customStyles: _List_Nil, requiredStyles: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$requiredStyles};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleToFragment = function (a) {
	switch (a.$) {
		case 'Identifier':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-ar-i');
		case 'Prefix':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'css-ar-p');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-ar-k');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'css-ar-v');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7 = {$: 'Style7'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorToFragment = function (att) {
	switch (att.$) {
		case 'AttributeName':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'css-s-a-an');
		case 'AttributeValue':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'css-s-a-av');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-s-a-o');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorToFragment = function (s) {
	switch (s.$) {
		case 'Element':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-s-e');
		case 'Id':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'css-s-i');
		case 'Class':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'css-s-cl');
		case 'Combinator':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7, 'css-s-c');
		case 'Universal':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-s-u');
		case 'AttributeSelector':
			var att = s.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorToFragment(att);
		case 'PseudoElement':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, 'css-s-pe');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, 'css-s-pc');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'css-s');
		case 'AtRule':
			var a = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleToFragment(a);
		case 'Selector':
			var s = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorToFragment(s);
		case 'Property':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'css-p');
		case 'PropertyValue':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'css-pv');
		case 'Number':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'css-n');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'css-u');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'Number':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'js-n');
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'js-s');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'js-k');
		case 'DeclarationKeyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'js-dk');
		case 'FunctionEval':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'js-fe');
		case 'Function':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'js-f');
		case 'LiteralKeyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6, 'js-lk');
		case 'Param':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7, 'js-p');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'js-ce');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Nix$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'Number':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'nix-n');
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'nix-s');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'nix-k');
		case 'Operator':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'nix-o');
		case 'Function':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'nix-f');
		case 'Punctuation':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6, 'nix-p');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7, 'nix-l');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$NoLang$syntaxToStyle = function (syntax) {
	return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, 'nolang');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'Number':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'py-n');
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'py-s');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'py-k');
		case 'DeclarationKeyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'py-dk');
		case 'Function':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'py-f');
		case 'LiteralKeyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6, 'py-lk');
		case 'Param':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7, 'py-p');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default, 'py-fe');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'Number':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1, 'sql-n');
		case 'String':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'sql-s');
		case 'Keyword':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'sql-k');
		case 'Operator':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4, 'sql-o');
		case 'Function':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'sql-f');
		case 'Punctuation':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6, 'sql-p');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7, 'sql-l');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 'Tag':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3, 'xml-t');
		case 'Attribute':
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5, 'xml-a');
		default:
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2, 'xlm-av');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxToSelector = function (syntax) {
	switch (syntax.$) {
		case 'Elm':
			var elmSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle(elmSyntax).b;
		case 'Xml':
			var xmlSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$syntaxToStyle(xmlSyntax).b;
		case 'Javascript':
			var jsSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$syntaxToStyle(jsSyntax).b;
		case 'Css':
			var cssSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$syntaxToStyle(cssSyntax).b;
		case 'Python':
			var pythonSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$syntaxToStyle(pythonSyntax).b;
		case 'Sql':
			var sqlSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$syntaxToStyle(sqlSyntax).b;
		case 'Nix':
			var nixSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Nix$syntaxToStyle(nixSyntax).b;
		default:
			var noLangSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$NoLang$syntaxToStyle(noLangSyntax).b;
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxesToSelectors = function (syntaxes) {
	return $elm$core$String$concat(
		A2(
			$elm$core$List$intersperse,
			', ',
			A2(
				$elm$core$List$map,
				$elm$core$Basics$append('.elmsh-'),
				A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxToSelector, syntaxes))));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss = F2(
	function (property, color) {
		switch (color.$) {
			case 'DefaultColor':
				return '';
			case 'Hex':
				var hex = color.a;
				return property + (hex + ';');
			case 'Rgb':
				var r = color.a;
				var g = color.b;
				var b = color.c;
				return $elm$core$String$concat(
					_List_fromArray(
						[
							property,
							'rgb(',
							$elm$core$String$fromInt(r),
							', ',
							$elm$core$String$fromInt(g),
							',',
							$elm$core$String$fromInt(b),
							');'
						]));
			default:
				var r = color.a;
				var g = color.b;
				var b = color.c;
				var a = color.d;
				return $elm$core$String$concat(
					_List_fromArray(
						[
							property,
							'rgba(',
							$elm$core$String$fromInt(r),
							', ',
							$elm$core$String$fromInt(g),
							',',
							$elm$core$String$fromInt(b),
							', ',
							$elm$core$String$fromFloat(a),
							');'
						]));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse = F2(
	function (bool, str) {
		return bool ? str : '';
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$styleToCss = function (_v0) {
	var isBold = _v0.isBold;
	var isItalic = _v0.isItalic;
	var isUnderline = _v0.isUnderline;
	var text = _v0.text;
	var background = _v0.background;
	return $elm$core$String$concat(
		_List_fromArray(
			[
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isBold, 'font-weight: bold;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isItalic, 'font-style: italic;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isUnderline, 'text-decoration: underline;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss, 'color: ', text),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss, 'background: ', background)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCssClass = function (_v0) {
	var selectors = _v0.a;
	var style = _v0.b;
	return $elm$core$String$isEmpty(selectors) ? '' : (selectors + (' {' + ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$styleToCss(style) + '}')));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCss = function (classes) {
	return $elm$core$String$concat(
		A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCssClass, classes));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$toCss = function (_v0) {
	var requiredStyles = _v0.requiredStyles;
	var customStyles = _v0.customStyles;
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCss(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2('.elmsh', requiredStyles._default),
					_Utils_Tuple2('.elmsh-hl', requiredStyles.highlight),
					_Utils_Tuple2('.elmsh-add', requiredStyles.addition),
					_Utils_Tuple2('.elmsh-del', requiredStyles.deletion),
					_Utils_Tuple2('.elmsh-comm', requiredStyles.comment),
					_Utils_Tuple2('.elmsh1', requiredStyles.style1),
					_Utils_Tuple2('.elmsh2', requiredStyles.style2),
					_Utils_Tuple2('.elmsh3', requiredStyles.style3),
					_Utils_Tuple2('.elmsh4', requiredStyles.style4),
					_Utils_Tuple2('.elmsh5', requiredStyles.style5),
					_Utils_Tuple2('.elmsh6', requiredStyles.style6),
					_Utils_Tuple2('.elmsh7', requiredStyles.style7)
				]),
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$mapFirst($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxesToSelectors),
				customStyles)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$css = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$toCss($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$theme);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$gitHub = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$GitHub$css;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$gitHub = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$gitHub);
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Add = {$: 'Add'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Del = {$: 'Del'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Normal = {$: 'Normal'};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$requiredStyleToString = function (required) {
	return 'elmsh' + function () {
		switch (required.$) {
			case 'Default':
				return '0';
			case 'Comment':
				return '-comm';
			case 'Style1':
				return '1';
			case 'Style2':
				return '2';
			case 'Style3':
				return '3';
			case 'Style4':
				return '4';
			case 'Style5':
				return '5';
			case 'Style6':
				return '6';
			default:
				return '7';
		}
	}();
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView = function (_v0) {
	var text = _v0.text;
	var requiredStyle = _v0.requiredStyle;
	var additionalClass = _v0.additionalClass;
	return (_Utils_eq(requiredStyle, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default) && $elm$core$String$isEmpty(additionalClass)) ? $elm$html$Html$text(text) : A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$requiredStyleToString(requiredStyle),
						!_Utils_eq(requiredStyle, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default)),
						_Utils_Tuple2('elmsh-' + additionalClass, additionalClass !== '')
					]))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(text)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$lineView = F3(
	function (start, index, _v0) {
		var fragments = _v0.fragments;
		var highlight = _v0.highlight;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('elmsh-line', true),
							_Utils_Tuple2(
							'elmsh-hl',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Normal))),
							_Utils_Tuple2(
							'elmsh-add',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Add))),
							_Utils_Tuple2(
							'elmsh-del',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Del)))
						])),
					A2(
					$elm$html$Html$Attributes$attribute,
					'data-elmsh-lc',
					$elm$core$String$fromInt(start + index))
				]),
			A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments));
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toInlineHtml = function (lines) {
	return A2(
		$elm$html$Html$code,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('elmsh')
			]),
		$elm$core$List$concat(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var highlight = _v0.highlight;
					var fragments = _v0.fragments;
					return _Utils_eq(highlight, $elm$core$Maybe$Nothing) ? A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments) : _List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$classList(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'elmsh-hl',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Normal))),
											_Utils_Tuple2(
											'elmsh-add',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Add))),
											_Utils_Tuple2(
											'elmsh-del',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Del)))
										]))
								]),
							A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments))
						]);
				},
				lines)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toBlockHtml = F2(
	function (maybeStart, lines) {
		if (maybeStart.$ === 'Nothing') {
			return A2(
				$elm$html$Html$pre,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elmsh')
					]),
				_List_fromArray(
					[
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toInlineHtml(lines)
					]));
		} else {
			var start = maybeStart.a;
			return A2(
				$elm$html$Html$pre,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elmsh')
					]),
				$elm$core$List$singleton(
					A2(
						$elm$html$Html$code,
						_List_Nil,
						A2(
							$elm$core$List$indexedMap,
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$lineView(start),
							lines))));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toBlockHtml = F2(
	function (maybeStart, _v0) {
		var lines = _v0.a;
		return A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toBlockHtml, maybeStart, lines);
	});
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$useTheme = function (_v0) {
	var theme = _v0.a;
	return A3(
		$elm$html$Html$node,
		'style',
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(theme)
			]));
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$highlightedExpressionView = F2(
	function (expression, line) {
		return _List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$useTheme($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$gitHub),
				A2(
				$elm$core$Result$withDefault,
				A2(
					$elm$html$Html$pre,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$code,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(expression)
								]))
						])),
				A2(
					$elm$core$Result$map,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toBlockHtml(line),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$elm(expression)))
			]);
	});
var $author$project$Main$SelectAnswer = F2(
	function (a, b) {
		return {$: 'SelectAnswer', a: a, b: b};
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toInlineHtml = function (_v0) {
	var lines = _v0.a;
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toInlineHtml(lines);
};
var $author$project$Main$highlightedInlineView = function (expression) {
	return _List_fromArray(
		[
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$useTheme($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$gitHub),
			A2(
			$elm$core$Result$withDefault,
			A2(
				$elm$html$Html$pre,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$code,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(expression)
							]))
					])),
			A2(
				$elm$core$Result$map,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toInlineHtml,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$elm(expression)))
		]);
};
var $author$project$Main$runningExerciseAnswerView = F2(
	function (exercise, answers) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('card-footer btn-toolbar d-flex gap-2')
				]),
			A2(
				$elm$core$List$map,
				function (answer) {
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn btn-lg bg-white btn-outline-dark w-100'),
								$elm$html$Html$Events$onClick(
								A2($author$project$Main$SelectAnswer, exercise, answer))
							]),
						$author$project$Main$highlightedInlineView(answer.code));
				},
				answers));
	});
var $author$project$Main$exerciseView = function (exercise) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card')
			]),
		function () {
			switch (exercise.$) {
				case 'SingleExpression':
					var singleExpression = exercise.a;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(singleExpression.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-title')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function () {
												var _v1 = singleExpression.description;
												if (_v1.$ === 'Just') {
													var d = _v1.a;
													return d;
												} else {
													return '';
												}
											}())
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-content')
										]),
									A2($author$project$Main$highlightedExpressionView, singleExpression.expression, $elm$core$Maybe$Nothing))
								])),
							A2($author$project$Main$runningExerciseAnswerView, exercise, singleExpression.answers)
						]);
				case 'BinaryExpression':
					var binaryExpression = exercise.a;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(binaryExpression.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-title')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function () {
												var _v2 = binaryExpression.description;
												if (_v2.$ === 'Just') {
													var d = _v2.a;
													return d;
												} else {
													return '';
												}
											}())
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-content')
										]),
									A2(
										$author$project$Main$highlightedExpressionView,
										A2(
											$elm$core$String$join,
											' ',
											_List_fromArray(
												[binaryExpression.leftExpression, binaryExpression.operator, binaryExpression.rightExpression])),
										$elm$core$Maybe$Nothing))
								])),
							A2($author$project$Main$runningExerciseAnswerView, exercise, binaryExpression.answers)
						]);
				case 'FunctionExpression':
					var functionExpression = exercise.a;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(functionExpression.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-title')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function () {
												var _v3 = functionExpression.description;
												if (_v3.$ === 'Just') {
													var d = _v3.a;
													return d;
												} else {
													return '';
												}
											}())
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-content')
										]),
									A2(
										$author$project$Main$highlightedExpressionView,
										functionExpression.functionName + (' ' + A2($elm$core$String$join, ' ', functionExpression._arguments)),
										$elm$core$Maybe$Nothing))
								])),
							A2($author$project$Main$runningExerciseAnswerView, exercise, functionExpression.answers)
						]);
				case 'GuardExpression':
					var guardExpression = exercise.a;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(guardExpression.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-title')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function () {
												var _v4 = guardExpression.description;
												if (_v4.$ === 'Just') {
													var d = _v4.a;
													return d;
												} else {
													return '';
												}
											}())
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-content')
										]),
									A2(
										$author$project$Main$highlightedExpressionView,
										guardExpression.functionName + (' ' + (A2($elm$core$String$join, ' ', guardExpression._arguments) + guardExpression.expression)),
										$elm$core$Maybe$Nothing))
								])),
							A2($author$project$Main$runningExerciseAnswerView, exercise, guardExpression.answers)
						]);
				default:
					var patternMatchingExpression = exercise.a;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(patternMatchingExpression.title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-title')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											function () {
												var _v5 = patternMatchingExpression.description;
												if (_v5.$ === 'Just') {
													var d = _v5.a;
													return d;
												} else {
													return '';
												}
											}())
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-content')
										]),
									A2(
										$author$project$Main$highlightedExpressionView,
										A2($elm$core$String$join, '\n', patternMatchingExpression.patterns),
										$elm$core$Maybe$Nothing))
								])),
							A2($author$project$Main$runningExerciseAnswerView, exercise, patternMatchingExpression.answers)
						]);
			}
		}());
};
var $author$project$Main$finishedExerciseAnswerView = F2(
	function (answers, studentAnswer) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn-toolbar d-flex gap-2')
				]),
			A2(
				$elm$core$List$map,
				function (answer) {
					return _Utils_eq(answer, studentAnswer) ? A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn btn-lg bg-white m-1 pe-none w-100'),
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('btn-outline-success', answer.isCorrect),
										_Utils_Tuple2('btn-outline-danger', !answer.isCorrect)
									]))
							]),
						$author$project$Main$highlightedInlineView(answer.code)) : (answer.isCorrect ? A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn btn-lg bg-white m-1 pe-none w-100'),
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('btn-outline-danger', !answer.isCorrect),
										_Utils_Tuple2('btn-outline-success', answer.isCorrect)
									]))
							]),
						$author$project$Main$highlightedInlineView(answer.code)) : $elm$html$Html$text(''));
				},
				answers));
	});
var $author$project$Main$ShuffleExercises = {$: 'ShuffleExercises'};
var $author$project$Main$finishedLectureFooter = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			$elm$html$Html$Attributes$class('card-footer d-flex justify-content-between align-items-center')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-lg text-white'),
					A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
					$elm$html$Html$Events$onClick($author$project$Main$Prev)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('<')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-lg btn-outline-warning'),
					$elm$html$Html$Events$onClick($author$project$Main$ShuffleExercises)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Quiz wiederholen')
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('btn btn-lg text-white'),
					A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
					$elm$html$Html$Events$onClick($author$project$Main$Next)
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('>')
				]))
		]));
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $author$project$Main$finishedExerciseView = F2(
	function (exercise, answer) {
		switch (exercise.$) {
			case 'SingleExpression':
				var singleExpressionModel = exercise.a;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-header text-center')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h5,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(singleExpressionModel.title)
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_Utils_ap(
								A2(
									$elm$core$List$cons,
									function () {
										var _v1 = singleExpressionModel.description;
										if (_v1.$ === 'Just') {
											var d = _v1.a;
											return A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('card-text')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(d)
													]));
										} else {
											return $elm$html$Html$text('');
										}
									}(),
									A2($author$project$Main$highlightedExpressionView, singleExpressionModel.expression, $elm$core$Maybe$Nothing)),
								_List_fromArray(
									[
										A2($author$project$Main$finishedExerciseAnswerView, singleExpressionModel.answers, answer)
									]))),
							$author$project$Main$finishedLectureFooter
						]));
			case 'BinaryExpression':
				var binaryExpressionModel = exercise.a;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('card')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-title text-center')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h5,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(binaryExpressionModel.title)
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card-body')
								]),
							_Utils_ap(
								A2(
									$elm$core$List$cons,
									function () {
										var _v2 = binaryExpressionModel.description;
										if (_v2.$ === 'Just') {
											var d = _v2.a;
											return A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('card-text')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(d)
													]));
										} else {
											return $elm$html$Html$text('');
										}
									}(),
									A2($author$project$Main$highlightedExpressionView, binaryExpressionModel.leftExpression + (' ' + (binaryExpressionModel.operator + (' ' + binaryExpressionModel.rightExpression))), $elm$core$Maybe$Nothing)),
								_List_fromArray(
									[
										A2($author$project$Main$finishedExerciseAnswerView, binaryExpressionModel.answers, answer)
									]))),
							$author$project$Main$finishedLectureFooter
						]));
			case 'FunctionExpression':
				var functionExpressionModel = exercise.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-header text-center')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h5,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(functionExpressionModel.title)
												]))
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-body')
										]),
									_List_fromArray(
										[
											function () {
											var _v3 = functionExpressionModel.description;
											if (_v3.$ === 'Just') {
												var d = _v3.a;
												return A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('card-text')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(d)
														]));
											} else {
												return $elm$html$Html$text('');
											}
										}(),
											A2(
											$elm$html$Html$code,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(
													functionExpressionModel.functionName + (' ' + A2($elm$core$String$join, ' ', functionExpressionModel._arguments)))
												]))
										])),
									A2($author$project$Main$finishedExerciseAnswerView, functionExpressionModel.answers, answer),
									$author$project$Main$finishedLectureFooter
								]))
						]));
			case 'GuardExpression':
				var guardExpressionModel = exercise.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-header text-center')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h5,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(guardExpressionModel.title)
												]))
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-body')
										]),
									A2(
										$elm$core$List$cons,
										function () {
											var _v4 = guardExpressionModel.description;
											if (_v4.$ === 'Just') {
												var d = _v4.a;
												return A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('card-text')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(d)
														]));
											} else {
												return $elm$html$Html$text('');
											}
										}(),
										A2(
											$author$project$Main$highlightedExpressionView,
											A2(
												$elm$core$String$join,
												' ',
												A2(
													$elm$core$List$cons,
													guardExpressionModel.functionName,
													_Utils_ap(
														guardExpressionModel._arguments,
														_List_fromArray(
															[guardExpressionModel.expression])))),
											$elm$core$Maybe$Nothing))),
									A2($author$project$Main$finishedExerciseAnswerView, guardExpressionModel.answers, answer),
									$author$project$Main$finishedLectureFooter
								]))
						]));
			default:
				var patternExpressionModel = exercise.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('card')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-header text-center')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h5,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(patternExpressionModel.title)
												]))
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('card-body')
										]),
									A2(
										$elm$core$List$cons,
										function () {
											var _v5 = patternExpressionModel.description;
											if (_v5.$ === 'Just') {
												var d = _v5.a;
												return A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('card-text')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(d)
														]));
											} else {
												return $elm$html$Html$text('');
											}
										}(),
										A2(
											$author$project$Main$highlightedExpressionView,
											A2($elm$core$String$join, '\n', patternExpressionModel.patterns),
											$elm$core$Maybe$Nothing))),
									A2($author$project$Main$finishedExerciseAnswerView, patternExpressionModel.answers, answer),
									$author$project$Main$finishedLectureFooter
								]))
						]));
		}
	});
var $elm$html$Html$footer = _VirtualDom_node('footer');
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $author$project$Main$get = F2(
	function (n, xs) {
		return $elm$core$List$head(
			A2($elm$core$List$drop, n, xs));
	});
var $author$project$Main$GoToCoursesOverview = {$: 'GoToCoursesOverview'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$header = _VirtualDom_node('header');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$svg$Svg$g = $elm$svg$Svg$trustedNode('g');
var $elm$svg$Svg$Attributes$preserveAspectRatio = _VirtualDom_attribute('preserveAspectRatio');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$transform = _VirtualDom_attribute('transform');
var $elm$svg$Svg$Attributes$version = _VirtualDom_attribute('version');
var $author$project$Images$Images$logo = A2(
	$elm$svg$Svg$svg,
	_List_fromArray(
		[
			$elm$svg$Svg$Attributes$version('1.0'),
			$elm$svg$Svg$Attributes$width('3em'),
			$elm$svg$Svg$Attributes$height('3em'),
			$elm$svg$Svg$Attributes$viewBox('0 0 1024.000000 1024.000000'),
			$elm$svg$Svg$Attributes$preserveAspectRatio('xMidYMid meet')
		]),
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$g,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$transform('translate(0.000000,1024.000000) scale(0.100000,-0.100000)'),
					$elm$svg$Svg$Attributes$fill('#000000'),
					$elm$svg$Svg$Attributes$stroke('none')
				]),
			_List_fromArray(
				[
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M2000 7440\n            c-11 -11 -20 -31 -20 -44\n            0 -16 228 -365 630 -967\n            387 -579 630 -952 630 -966\n            0 -14 -221 -356 -570 -879\n            -434 -651 -570 -862 -570 -885\n            0 -19 10 -39 25 -55\n            l25 -25\n            369 3\n            c355 3 371 4 398 24\n            15 11 295 418 621 905\n            582 866 593 885 589 926\n            -4 35 -101 186 -633 984\n            -346 518 -641 955 -655 970\n            l-27 29\n            -396 0\n            c-383 0 -397 -1 -416 -20\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M3261 7434\n            c-12 -15 -21 -35 -21 -44\n            0 -10 284 -442 630 -961\n            408 -611 630 -952 630 -968\n            0 -15 -201 -325 -567 -874\n            -312 -468 -569 -863 -571 -878\n            -3 -20 3 -35 21 -53\n            l26 -26\n            359 0\n            c357 0 359 0 393 23\n            24 16 145 187 397 562\n            200 297 368 550 373 563\n            11 27 56 31 66 5\n            15 -38 727 -1090 759 -1120\n            l34 -33\n            169 0\n            169 0\n            50 -103\n            c250 -511 824 -773 1359 -622\n            112 32 288 118 378 186\n            481 361 615 1030 310 1549\n            -146 249 -368 431 -633 520\n            -170 57 -197 58 -865 51\n            l-609 -6\n            -24 -28\n            c-42 -49 -34 -72 90 -257\n            l114 -170\n            -53 -85\n            c-45 -72 -80 -146 -120 -250\n            -6 -15 -9 -17 -13 -5\n            -3 8 -60 95 -127 195\n            -67 99 -265 394 -440 655\n            -1146 1708 -1431 2129 -1465 2163\n            l-39 37\n            -380 0\n            -380 0\n            -20 -26\n            z\n\n            m4194 -2484\n            c297 -77 541 -304 641 -597\n            45 -132 58 -223 51 -368\n            -17 -369 -236 -682 -572 -816\n            -169 -67 -351 -85 -520 -50\n            -194 40 -348 123 -485 261\n            -137 136 -218 286 -255 469\n            -24 115 -16 324 15 431\n            82 285 282 517 539 625\n            66 28 142 49 251 69\n            57 10 258 -4 335 -24\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M6935 4376\n            c-37 -17 -70 -52 -84 -89\n            -38 -98 37 -207 142 -207\n            87 0 148 57 155 144\n            4 56 -18 101 -69 139\n            -34 25 -105 31 -144 13\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M7415 4376\n            c-99 -43 -125 -175 -50 -251\n            79 -78 212 -52 251 50\n            30 81 -2 160 -80 196\n            -48 22 -80 23 -121 5\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M6935 3896\n            c-65 -29 -105 -107 -91 -177\n            26 -130 200 -166 278 -57\n            31 44 33 134 2 174\n            -48 64 -126 89 -189 60\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M7415 3896\n            c-37 -16 -83 -68 -91 -102\n            -10 -39 2 -100 26 -134\n            60 -85 190 -84 252 2\n            31 44 33 134 2 174\n            -48 64 -126 89 -189 60\n            z\n            ')
						]),
					_List_Nil),
					A2(
					$elm$svg$Svg$path,
					_List_fromArray(
						[
							$elm$svg$Svg$Attributes$d('\n            M5459 6131\n            c-48 -48 -41 -79 47 -212\n            161 -241 245 -359 266 -374\n            20 -13 127 -15 924 -15\n            l903 0\n            20 26\n            c20 26 21 39 21 299\n            0 251 -1 273 -18 288\n            -17 16 -106 17 -1076 17\n            l-1058 0\n            -29 -29\n            z\n            ')
						]),
					_List_Nil)
				]))
		]));
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$header = F2(
	function (user, course) {
		return A2(
			$elm$html$Html$header,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('sticky-top')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$nav,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('navbar navbar-expand-sm bg-body-tertiary')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('container-fluid')
								]),
							A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('navbar-brand')
										]),
									_List_fromArray(
										[$author$project$Images$Images$logo])),
								function () {
									if (user.$ === 'Just') {
										var u = user.a;
										return _List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('navbar-toggler'),
														A2($elm$html$Html$Attributes$attribute, 'data-bs-toggle', 'collapse'),
														A2($elm$html$Html$Attributes$attribute, 'data-bs-target', '#navbarNav')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$span,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('navbar-toggler-icon')
															]),
														_List_Nil)
													])),
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('collapse navbar-collapse justify-content-between'),
														$elm$html$Html$Attributes$id('navbarNav')
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$ul,
														_List_fromArray(
															[
																$elm$html$Html$Attributes$class('navbar-nav mr-auto border-right pr-3')
															]),
														_List_fromArray(
															[
																A2(
																$elm$html$Html$li,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('nav-item'),
																		A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$a,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('nav-link'),
																				$elm$html$Html$Events$onClick($author$project$Main$GoToCoursesOverview)
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Kursübersicht')
																			]))
																	])),
																function () {
																if (course.$ === 'Just') {
																	var c = course.a;
																	return A2(
																		$elm$html$Html$li,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('nav-item'),
																				A2($elm$html$Html$Attributes$style, 'cursor', 'pointer')
																			]),
																		_List_fromArray(
																			[
																				A2(
																				$elm$html$Html$a,
																				_List_fromArray(
																					[
																						$elm$html$Html$Attributes$class('nav-link'),
																						$elm$html$Html$Attributes$classList(
																						_List_fromArray(
																							[
																								_Utils_Tuple2('nav-link', true),
																								_Utils_Tuple2('active', true)
																							])),
																						$elm$html$Html$Events$onClick(
																						$author$project$Main$SelectCourse(c))
																					]),
																				_List_fromArray(
																					[
																						$elm$html$Html$text(c.title)
																					]))
																			]));
																} else {
																	return $elm$html$Html$text('');
																}
															}()
															])),
														A2(
														$elm$html$Html$div,
														_List_Nil,
														_List_fromArray(
															[
																A2(
																$elm$html$Html$div,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$class('btn-lg rounded text-white p-1'),
																		A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1')
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text(u.name),
																		function () {
																		var _v2 = u.badges;
																		if (!_v2.b) {
																			return $elm$html$Html$text('');
																		} else {
																			var badges = _v2;
																			return A2(
																				$elm$html$Html$span,
																				_List_fromArray(
																					[
																						$elm$html$Html$Attributes$class('badge badge-pill')
																					]),
																				_List_fromArray(
																					[
																						$elm$html$Html$text(
																						$elm$core$String$fromInt(
																							$elm$core$List$length(badges))),
																						$author$project$Images$Images$genericBadgeSvg('2em')
																					]));
																		}
																	}()
																	]))
															]))
													]))
											]);
									} else {
										return _List_Nil;
									}
								}()))
						]))
				]));
	});
var $elm$html$Html$Attributes$hidden = $elm$html$Html$Attributes$boolProperty('hidden');
var $elm$html$Html$input = _VirtualDom_node('input');
var $author$project$Main$StartLecture = {$: 'StartLecture'};
var $author$project$Main$lectureView = function (l) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('fixed-bottom m-2')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h4,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(l.title)
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(l.description)
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										'Diese Lektion beinhaltet ' + ($elm$core$String$fromInt(
											$elm$core$List$length(l.exercises)) + (' ' + (($elm$core$List$length(l.exercises) === 1) ? 'Aufgabe' : 'Aufgaben'))))
									]))
							]))
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('btn btn-lg text-white w-100'),
						A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
						$elm$html$Html$Events$onClick($author$project$Main$StartLecture)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Lektion starten')
					]))
			]));
};
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $author$project$Main$runningLearningContentView = F2(
	function (lecture, exampleIndex) {
		var _v0 = A2($author$project$Main$get, exampleIndex, lecture.learningContent.examples);
		if (_v0.$ === 'Just') {
			var example = _v0.a;
			return A2(
				$elm$html$Html$footer,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('mt-auto m-2')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('card')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-header')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(example.title)
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-body')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('card-title')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												A2($elm$core$Maybe$withDefault, '', example.description))
											])),
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('card-content')
											]),
										A2($author$project$Main$highlightedExpressionView, example.expression, $elm$core$Maybe$Nothing))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('card-footer d-flex justify-content-between align-items-center')
									]),
								_List_fromArray(
									[
										function () {
										var _v1 = $elm$core$List$head(lecture.learningContent.examples);
										if (_v1.$ === 'Just') {
											var head = _v1.a;
											return _Utils_eq(head, example) ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-lg text-white'),
														A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
														$elm$html$Html$Events$onClick($author$project$Main$Prev)
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('<<')
													]));
										} else {
											return $elm$html$Html$text('');
										}
									}(),
										function () {
										var lastIndex = $elm$core$List$length(lecture.learningContent.examples) - 1;
										var _v2 = A2($author$project$Main$get, lastIndex, lecture.learningContent.examples);
										if (_v2.$ === 'Just') {
											var last = _v2.a;
											return _Utils_eq(last, example) ? A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-lg text-white'),
														A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
														$elm$html$Html$Events$onClick($author$project$Main$ShuffleExercises)
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Quiz starten')
													])) : A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-lg text-white'),
														A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
														$elm$html$Html$Events$onClick($author$project$Main$Next)
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('>>')
													]));
										} else {
											return $elm$html$Html$text('');
										}
									}()
									]))
							])),
						function () {
						var progress = (((exampleIndex + 1) * 100) / $elm$core$List$length(lecture.learningContent.examples)) | 0;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('mt-1')
								]),
							_List_fromArray(
								[
									A2($author$project$Main$progressBarView, progress, '1em')
								]));
					}()
					]));
		} else {
			return A2(
				$elm$html$Html$footer,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('mt-auto m-2')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn btn-lg text-white w-100'),
								A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
								$elm$html$Html$Events$onClick($author$project$Main$ShuffleExercises)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Quiz \"' + (lecture.title + '\" starten'))
							]))
					]));
		}
	});
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('d-flex flex-column min-vh-100')
			]),
		function () {
			switch (model.$) {
				case 'CoursesOverview':
					var user = model.a;
					var courses = model.b;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Nothing),
							A2($author$project$Main$coursesOverview, user, courses)
						]);
				case 'CoursePage':
					var user = model.a;
					var course = model.b;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							A2($author$project$Main$coursePage, user, course)
						]);
				case 'LecturePage':
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							$author$project$Main$lectureView(lecture)
						]);
				case 'RunningQuiz':
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					var remainingExercises = model.d;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							A2(
							$elm$html$Html$main_,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('m-2')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h4,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(lecture.title)
										]))
								])),
							function () {
							var _v1 = $elm$core$List$head(remainingExercises);
							if (_v1.$ === 'Just') {
								var e = _v1.a;
								return A2(
									$elm$html$Html$footer,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('mt-auto m-2')
										]),
									_List_fromArray(
										[
											$author$project$Main$exerciseView(e),
											function () {
											if ($elm$core$List$length(lecture.exercises) === 1) {
												return $elm$html$Html$text('');
											} else {
												var l = $elm$core$List$length(lecture.exercises);
												var progress = ((((l - $elm$core$List$length(remainingExercises)) + 1) * 100) / l) | 0;
												return A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('mt-1')
														]),
													_List_fromArray(
														[
															A2($author$project$Main$progressBarView, progress, '1em')
														]));
											}
										}()
										]));
							} else {
								return $elm$html$Html$text('');
							}
						}()
						]);
				case 'WinningQuiz':
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							A2(
							$elm$html$Html$main_,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('m-2')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h4,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(lecture.title)
										]))
								])),
							A2(
							$elm$html$Html$footer,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('footer mt-auto m-2')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Herzlichen Glückwunsch, ' + (user.name + '! Du hast die Lektion erfolgreich abgeschlossen.'))
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-lg text-white w-100'),
											A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
											$elm$html$Html$Events$onClick(
											$author$project$Main$AddBadge(lecture.badge))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Belohnung einsacken'),
											A3($elm$core$Maybe$withDefault, $author$project$Images$Images$genericBadgeSvg, lecture.badge.image, '3em')
										]))
								]))
						]);
				case 'FinishedQuiz':
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					var wrongExercises = model.d;
					var i = model.e;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							function () {
							if (!wrongExercises.b) {
								return $elm$html$Html$text('FinishedLecture: Hier stimmt was nicht!');
							} else {
								var w = wrongExercises;
								var _v3 = A2($author$project$Main$get, i, w);
								if (_v3.$ === 'Just') {
									var _v4 = _v3.a;
									var exercise = _v4.a;
									var answer = _v4.b;
									return A2(
										$elm$html$Html$footer,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('footer m-2 mt-auto')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$div,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														'Du hast ' + ($elm$core$String$fromInt(
															$elm$core$List$length(lecture.exercises) - $elm$core$List$length(wrongExercises)) + (' von ' + ($elm$core$String$fromInt(
															$elm$core$List$length(lecture.exercises)) + ' Aufgaben richtig gelöst.'))))
													])),
												A2($author$project$Main$finishedExerciseView, exercise, answer)
											]));
								} else {
									return $elm$html$Html$text('');
								}
							}
						}()
						]);
				case 'LearningContentPage':
					var user = model.a;
					var course = model.b;
					var lecture = model.c;
					var i = model.d;
					return _List_fromArray(
						[
							A2(
							$author$project$Main$header,
							$elm$core$Maybe$Just(user),
							$elm$core$Maybe$Just(course)),
							A2(
							$elm$html$Html$main_,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('container-fluid')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$h4,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(lecture.learningContent.title)
										]))
								])),
							A2($author$project$Main$runningLearningContentView, lecture, i)
						]);
				default:
					var user = model.a;
					var error = model.b;
					return _List_fromArray(
						[
							A2($author$project$Main$header, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
							A2(
							$elm$html$Html$main_,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('main fill-height')
								]),
							_List_Nil),
							A2(
							$elm$html$Html$footer,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('footer mt-auto m-2')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('alert bg-danger-subtle'),
											$elm$html$Html$Attributes$hidden(
											function () {
												if (error.$ === 'Just') {
													var err = error.a;
													return false;
												} else {
													return true;
												}
											}())
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Dein Name muss mindestens drei Zeichen lang sein.')
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('mb-1')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$input,
											_List_fromArray(
												[
													$elm$html$Html$Events$onInput($author$project$Main$EnteringName),
													$elm$html$Html$Attributes$placeholder('Gib deinen Namen ein'),
													$elm$html$Html$Attributes$type_('text'),
													$elm$html$Html$Attributes$class('form-control form-control-lg'),
													$elm$html$Html$Attributes$autofocus(true)
												]),
											_List_Nil)
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick($author$project$Main$EnteringNameDone),
											$elm$html$Html$Attributes$class('btn btn-lg w-100 text-white'),
											A2($elm$html$Html$Attributes$style, 'background-color', '#6f42c1'),
											$elm$html$Html$Attributes$disabled(
											!$author$project$Main$checkUsername(user))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Start')
										]))
								]))
						]);
			}
		}());
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));