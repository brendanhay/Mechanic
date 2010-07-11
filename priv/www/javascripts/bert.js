// bert.js
//
// Copyright (c) 2009 Ryan Tomayko <tomayko.com/about>
// See COPYING for licensing information.
// <http://github.com/rtomayko/node-bertrpc>
//
// Based heavily on the BERT-JS library by Rusty Klophaus:
// <http://github.com/rklophaus/BERT-JS>
//
// Copyright (c) 2009 Rusty Klophaus (@rklophaus)
// Contributions by Ben Browning (@bbrowning)
//
// TODO time
// TODO regex
// TODO push / streaming
 
 
// BERT types are mapped to JavaScript types as follows:
//
//     +--------------+----------------+
//     | BERT         | JavaScript     |
//     +--------------+----------------+
//     | atom         | bert.Atom      |
//     | binary       | String         |
//     | boolean      | true, false    |
//     | bytelist     | bert.Bytelist  |
//     | dictionary   | Object         |
//     | float        | Number         |
//     | integer      | Number         |
//     | list         | Array          |
//     | nil          | null           |
//     | regex        | NOT SUPPORTED  |
//     | tuple        | bert.Tuple     |
//     | time         | NOT SUPPORTED  |
//     +--------------+----------------+
//
 
 
// frequently used atom objects; set after BERT is defined.
var _bert, _dict, _nil, _true, _false, _reply;
 
var BERT = {
   /* WIRE PROTOCOL CODES */
 
   BERT_START:    String.fromCharCode(131),
   SMALL_ATOM:    String.fromCharCode(115),
   ATOM:          String.fromCharCode(100),
   BINARY:        String.fromCharCode(109),
   SMALL_INTEGER: String.fromCharCode(97),
   INTEGER:       String.fromCharCode(98),
   SMALL_BIG:     String.fromCharCode(110),
   LARGE_BIG:     String.fromCharCode(111),
   FLOAT:         String.fromCharCode(99),
   STRING:        String.fromCharCode(107),
   LIST:          String.fromCharCode(108),
   SMALL_TUPLE:   String.fromCharCode(104),
   LARGE_TUPLE:   String.fromCharCode(105),
   NIL:           String.fromCharCode(106),
   ZERO:          String.fromCharCode(0),
 
   /* BERT TYPE WRAPPER CLASSES */
 
   Atom: function (string) {
      this.type = "atom";
      this.value = string;
      this.repr = function () { return string }
      this.toString = function () { return string };
   },
 
   Bytelist: function (string) {
      this.type = "bytelist";
      this.value = string;
      this.toString = function () { this.value }
      this.repr = function () {
         var bytes = BERT.string_to_bytelist(this.value),
             string= "";
         for (var i=0; i < bytes.length; i++) {
            if (i > 0) string += ",";
            string += bytes[i];
         }
         return "<<" + string + ">>";
      }
   },
 
   Tuple: function (array) {
      this.type = "tuple";
      this.length = array.length;
      this.value = array;
      for (var i=0; i < array.length; i++)
         this[i] = array[i];
      this.repr = function () {
         var s = "";
         for (var i=0; i < this.length; i++) {
            if (i > 0) s += ", ";
            s += BERT.repr(this.value[i]);
         }
         return "{" + s + "}";
      }
      this.toString = this.repr;
   },
 
   /* CASTING TO BERT TYPES */
 
   atom: function (string) { return new BERT.Atom(string); },
   tuple: function () { return new BERT.Tuple(arguments); },
   tup: function (array) { return new BERT.Tuple(array); },
   bytelist: function (string) { return new BERT.Bytelist(string); },
   binary: function (string) { return string; },
   list: function (array) { return list; },
   dictionary: function (object) { return object; },
 
   /* BASIC INTERFACE */
 
   encode: function (obj) {
      return BERT.BERT_START + BERT.encode_inner(obj);
   },
 
   decode: function (data) {
       if (data[0] != BERT.BERT_START) throw("Not a valid BERT. Code: " + data.charCodeAt(0));
      var obj = BERT.decode_inner(data.substring(1));
      if (obj.rest != "") throw("Invalid BERT.");
      return obj.value;
   },
 
   /* ENCODING */
 
   encode_inner: function (obj) {
      var type = typeof(obj);
      return this["encode_" + type].call(this, obj);
   },
 
   encode_string: function (obj) {
      return this.BINARY +
         this.int_to_bytes(obj.length, 4) +
         obj;
   },
 
   encode_bytelist: function (obj) {
      return this.STRING +
         this.int_to_bytes(obj.value.length, 2) +
         obj.value;
   },
 
   encode_boolean: function (obj) {
      if (obj) {
         return this.encode_tup(_bert, _true);
      } else {
         return this.encode_tup(_bert, _false);
      }
   },
 
   encode_number: function (obj) {
      var remainder = (obj % 1 != 0);
 
      if (remainder)
         return this.encode_float(obj);
 
      // small int...
      if (obj >= 0 && obj < 256)
         return this.SMALL_INTEGER + this.int_to_bytes(obj, 1);
 
      // 4 byte int...
      if (obj >= -134217728 && obj <= 134217727)
         return this.INTEGER + this.int_to_bytes(obj, 4);
 
      // bignum...
      var s = this.bignum_to_bytes(obj);
      if (s.length < 256) {
         return this.SMALL_BIG + this.int_to_bytes(s.length - 1, 1) + s;
      } else {
         return this.LARGE_BIG + this.int_to_bytes(s.length - 1, 4) + s;
      }
   },
 
   encode_float: function (obj) {
      var s = obj.toExponential();
      while (s.length < 31)
        s += this.ZERO;
      return this.FLOAT + s;
   },
 
   encode_object: function (obj) {
      if (obj == null)
         return this.encode_null(obj);
      if (obj.type == 'atom')
         return this.encode_atom(obj);
      if (obj.type == 'tuple')
         return this.encode_tuple(obj);
      if (obj.type == 'bytelist')
         return this.encode_bytelist(obj);
      if (obj.constructor.toString().indexOf("Array") >= 0)
         return this.encode_list(obj);
      return this.encode_dictionary(obj);
   },
 
   encode_atom: function (obj) {
      return this.ATOM +
         this.int_to_bytes(obj.value.length, 2) +
         obj.value;
   },
 
   encode_binary: function (obj) {
      return this.BINARY +
         this.int_to_bytes(obj.value.length, 4) +
         obj.value;
   },
 
   encode_tuple: function (obj) {
      var s = "";
 
      if (obj.length < 256) {
         s += this.SMALL_TUPLE + this.int_to_bytes(obj.length, 1);
      } else {
         s += this.LARGE_TUPLE + this.int_to_bytes(obj.length, 4);
      }
      for (var i=0; i < obj.length; i++) {
         s += this.encode_inner(obj[i]);
      }
      return s;
   },
 
   encode_tup: function () {
      return this.encode_tuple(this.tup(arguments));
   },
 
   encode_list: function (obj) {
      var s = this.LIST + this.int_to_bytes(obj.length, 4);
      for (var i=0; i < obj.length; i++) {
         s += this.encode_inner(obj[i]);
      }
      s += this.NIL;
      return s;
   },
 
   encode_dictionary: function (obj) {
      var array = new Array();
      for (var key in obj)
         array.push(this.tuple(this.atom(key), obj[key]));
      return this.encode_tup(_bert, _dict, array);
   },
 
   encode_null: function (obj) {
      return this.encode_tup(_bert, _nil);
   },
 
   /* DECODING */
 
   decode_inner: function (data) {
      var type = data[0];
      data = data.substring(1);
      if (type == this.SMALL_ATOM) return this.decode_atom(data, 1);
      if (type == this.ATOM) return this.decode_atom(data, 2);
      if (type == this.BINARY) return this.decode_binary(data);
      if (type == this.SMALL_INTEGER) return this.decode_integer(data, 1);
      if (type == this.INTEGER) return this.decode_integer(data, 4);
      if (type == this.SMALL_BIG) return this.decode_big(data, 1);
      if (type == this.LARGE_BIG) return this.decode_big(data, 4);
      if (type == this.FLOAT) return this.decode_float(data);
      if (type == this.STRING) return this.decode_bytelist(data);
      if (type == this.LIST) return this.decode_list(data);
      if (type == this.SMALL_TUPLE) return this.decode_tuple(data, 1);
      if (type == this.LARGE_TUPLE) return this.decode_large_tuple(data, 4);
      if (type == this.NIL) return this.decode_nil(data);
      throw("Unexpected BERT type: " + String.charCodeAt(type));
   },
 
   decode_atom: function (data, count) {
      var size = this.bytes_to_int(data, count);
      data = data.substring(count);
      var value = data.substring(0, size);
      if (value == "true") {
         value = _true;
      } else if (value == "false") {
         value = _false;
      } else if (value == "bert") {
         value = _bert;
      } else if (value == "nil") {
         value = _nil;
      } else if (value == "dict") {
         value = _dict;
      } else {
         value = this.atom(value);
      }
      return {
         value: value,
         rest:  data.substring(size)
      };
   },
 
   decode_binary: function (data) {
      var size = this.bytes_to_int(data, 4);
      data = data.substring(4);
      return {
         value: data.substring(0, size),
         rest:  data.substring(size)
      };
   },
 
   decode_integer: function (data, count) {
      var value = this.bytes_to_int(data, count);
      data = data.substring(count);
      return {
         value: value,
         rest:  data
      };
   },
 
   decode_big: function (data, count) {
      var size = this.bytes_to_int(data, count);
      data = data.substring(count);
      var value = this.bytes_to_bignum(data, size);
      return {
         value: value,
         rest:  data.substring(size + 1)
      };
   },
 
   decode_float: function (data) {
      var size = 31;
      return {
         value: parseFloat(data.substring(0, size)),
         rest: data.substring(size)
      };
   },
 
   decode_bytelist: function (data) {
      var size = this.bytes_to_int(data, 2);
      data = data.substring(2);
      return {
         value: this.bytelist(data.substring(0, size)),
         rest:  data.substring(size)
      };
   },
 
   decode_list: function (data) {
      var size = this.bytes_to_int(data, 4);
      data = data.substring(4);
      var array = new Array();
      for (var i=0; i < size; i++) {
         var element = this.decode_inner(data);
         array.push(element.value);
         data = element.rest;
      }
      var last = data[0];
      if (last != this.NIL) throw("List does not end with NIL!");
      data = data.substring(1);
      return {
         value: array,
         rest:  data
      };
   },
 
   decode_tuple: function (data, count) {
      var size  = this.bytes_to_int(data, count),
          array = new Array(),
          value = null;
      data = data.substring(count);
      for (var i=0; i < size; i++) {
         var element = this.decode_inner(data);
         array.push(element.value);
         data = element.rest;
      }
      if (array[0] == _bert) {
         if ( array[1] == _dict )   {
            var list = array[2],
                dict = {},
                item = null;
            for(var i=0; i < list.length; i++) {
               item = list[i];
               if ( item[0] === null ) {
                  dict[null] = item[1];
               } else if ( item[0].type == 'atom' ) {
                  dict[item[0].toString()] = item[1];
               } else {
                  dict[item[0]] = item[1];
               }
            }
            value = dict;
         }
         else if ( array[1] == _nil )    { value = null;  }
         else if ( array[1] == _true )   { value = true;  }
         else if ( array[1] == _false )  { value = false; }
         else
            throw 'unsupported complex tuple: {bert, ' + array[1] + '}';
      }else{
         value = this.tup(array);
      }
      return {value:value, rest:data};
   },
 
   decode_nil: function (data) {
      return {
         value: null,
         rest:  data
      };
   },
 
   /* UTILITY FUNCTIONS */
 
   // Encode an integer to a big-endian byte-string
   // of the length specified. Throw an exception if
   // the integer is too large to fit into the specified
   // number of bytes.
   int_to_bytes: function (int, length) {
      var negative = (int < 0),
          data = "",
          orig = int;
      if (negative) { int = ~int; }
      for (var i=0; i < length; i++) {
         var remainder = int % 256;
         if (negative) { remainder = 255 - remainder };
         data = String.fromCharCode(remainder) + data;
         int = Math.floor(int / 256);
      }
      if (int > 0) throw("Argument out of range: " + orig);
      return data;
   },
 
   // Read a big-endian encoded integer from the first length
   // bytes of the supplied string. When length is a single byte,
   // just return the unsigned byte value.
   bytes_to_int: function (data, length) {
      if ( length == 1 )
         return data.charCodeAt(i);
 
      var num = 0,
          negative = (length > 1 && data.charCodeAt(0) > 128),
          n = null;
 
      for (var i=0; i < length; i++) {
         n = data.charCodeAt(i);
         if (negative) n = 255 - n;
         if (num == 0) num = n;
         else num = num * 256 + n;
      }
      if (negative) num = ~num;
      return num;
   },
 
   // Encode an integer into an Erlang bignum, which is a
   // byte of 1 or 0 representing whether the number is
   // negative or positive, followed by little-endian bytes.
   bignum_to_bytes: function (int) {
      var negative = int < 0,
          data = "";
      if (negative) {
         int *= -1;
         data += String.fromCharCode(1);
      } else {
         data += String.fromCharCode(0);
      }
      while (int != 0) {
         var remainder = int % 256;
         data += String.fromCharCode(remainder);
         int = Math.floor(int / 256);
      }
      return data;
   },
 
   // Encode a list of bytes into an Erlang bignum.
   bytes_to_bignum: function (data, count) {
      var negative = (data.charCodeAt(0) == 1),
          num = 0,
          n = null;
      data = data.substring(1);
      for (var i = count - 1; i >= 0; i--) {
         n = data.charCodeAt(i);
         if (num == 0) num = n;
         else num = num * 256 + n;
      }
      if (negative) num *= -1;
      return num;
   },
 
   // Convert an array of bytes into a string.
   bytelist_to_string: function (bytes) {
      var string = "";
      for (var i=0; i < bytes.length; i++)
         string += String.fromCharCode(bytes[i]);
      return string;
   },
 
   string_to_bytelist: function (string) {
      var bytelist = new Array();
      for (var i=0; i < string.length; i++)
         bytelist[i] = string.charCodeAt(i);
      return bytelist;
   },
 
   /* FORMATTING */
 
   bin_repr: function (obj) {
      return BERT.repr(BERT.bytelist(obj));
   },
 
   // pretty print a JS object in erlang term form
   repr: function (obj) {
      if (obj === null)
         return "<nil>";
 
      if (obj === true)
         return "<true>";
 
      if (obj === false)
         return "<false>";
 
      if (typeof(obj) == 'string')
         return "<<\"" + obj + "\">>";
 
      // numbers, booleans, stuff like that
      if (typeof(obj) != 'object')
         return obj.toString();
 
      // BERT special types: atom, tuple, bytelist
      if (obj.repr)
         return obj.repr();
 
      // arrays
      if (obj.constructor.toString().indexOf("Array") >= 0) {
         var s = "";
         for (var i = 0; i < obj.length; i++) {
            if (i > 0) s += ", ";
            s += BERT.repr(obj[i])
         }
         return "[" + s + "]";
      }
 
      // Assume it's a dictionary
      var s = "", prev = null;
      for (var key in obj) {
         var val = obj[key];
         if ( typeof(key) == 'string' )
            key = BERT.atom(key);
         if ( prev ) s += ", ";
         s += BERT.repr(BERT.tuple(key, val));
         prev = val;
      }
      return "[" + s + "]";
   }
};
 
_bert  = BERT.atom('bert');
_nil   = BERT.atom('nil');
_dict =  BERT.atom('dict');
_true  = BERT.atom('true');
_false = BERT.atom('false');
_reply = BERT.atom('reply');

// // BERT-JS
// // Copyright (c) 2009 Rusty Klophaus (@rklophaus)
// // Contributions by Ben Browning (@bbrowning)
// // See MIT-LICENSE for licensing information.


// // BERT-JS is a Javascript implementation of Binary Erlang Term Serialization.
// // - http://github.com/rklophaus/BERT-JS
// //
// // References:
// // - http://www.erlang-factory.com/upload/presentations/36/tom_preston_werner_erlectricity.pdf
// // - http://www.erlang.org/doc/apps/erts/erl_ext_dist.html#8


// // - CLASSES -

// function BertClass() { 
//     this.BERT_START = String.fromCharCode(131);
//     this.SMALL_ATOM = String.fromCharCode(115);
//     this.ATOM = String.fromCharCode(100);
//     this.BINARY = String.fromCharCode(109);
//     this.SMALL_INTEGER = String.fromCharCode(97);
//     this.INTEGER = String.fromCharCode(98);
//     this.SMALL_BIG = String.fromCharCode(110);
//     this.LARGE_BIG = String.fromCharCode(111);
//     this.FLOAT = String.fromCharCode(99);
//     this.STRING = String.fromCharCode(107);
//     this.LIST = String.fromCharCode(108);
//     this.SMALL_TUPLE = String.fromCharCode(104);
//     this.LARGE_TUPLE = String.fromCharCode(105);
//     this.NIL = String.fromCharCode(106);
//     this.ZERO = String.fromCharCode(0);	
// }

// function BertAtom(Obj) {
//     this.type = "Atom";
//     this.value = Obj;
//     this.toString = function() { return Obj };
// }

// function BertBinary(Obj) {
//     this.type = "Binary";
//     this.value = Obj;
//     this.toString = function() { return "<<\"" + Obj + "\">>" };
// }

// function BertTuple(Arr) {
//     this.type = "Tuple";
//     this.length = Arr.length;
//     this.value = Arr;
//     for (var i=0; i<Arr.length; i++) {
// 	this[i] = Arr[i];
//     }
//     this.toString = function() {
// 	var s = "";
// 	for (var i=0; i<this.length; i++) {
// 	    if (s != "") s += ", ";
// 	    s += this[i].toString();
// 	}
	
// 	return "{" + s + "}";
//     }
// }



// // - INTERFACE -

// BertClass.prototype.encode = function(Obj) {
//     return this.BERT_START + this.encode_inner(Obj);
// }

// BertClass.prototype.decode = function(S) {
//     if (S[0] != this.BERT_START) throw("Not a valid BERT.");
//     var Obj = this.decode_inner(S.substring(1));
//     if (Obj.rest != "") throw("Invalid BERT.");
//     return Obj.value;
// }

// BertClass.prototype.atom = function(Obj) {
//     return new BertAtom(Obj);
// }

// BertClass.prototype.binary = function(Obj) {
//     return new BertBinary(Obj);
// }

// BertClass.prototype.tuple = function() {
//     return new BertTuple(arguments);
// }



// // - ENCODING - 

// BertClass.prototype.encode_inner = function(Obj) {
//     var type = typeof(Obj);
//     return eval("this.encode_" + type + "(Obj)");
// }

// BertClass.prototype.encode_string = function(Obj) {
//     return this.STRING + this.int_to_bytes(Obj.length, 2) + Obj;
// }

// BertClass.prototype.encode_boolean = function(Obj) {
//     if (Obj) return this.encode_inner(this.atom("true"));
//     else return this.encode_inne(this.atom("false"));
// }

// BertClass.prototype.encode_number = function(Obj) {
//     IsInteger = (Obj % 1 == 0)
    
//     // Handle floats...
//     if (!IsInteger) {
// 	return this.encode_float(Obj);
//     }
    
//     // Small int...
//     if (IsInteger && Obj >= 0 && Obj < 256) { 
// 	return this.SMALL_INTEGER + this.int_to_bytes(Obj, 1);
//     }
    
//     // 4 byte int...
//     if (IsInteger && Obj >= -134217728 && Obj <= 134217727) {
// 	return this.INTEGER + this.int_to_bytes(Obj, 4);
//     } 
    
//     // Bignum...
//     var s = this.bignum_to_bytes(Obj);
//     if (s.length < 256) { 
// 	return this.SMALL_BIG + this.int_to_bytes(s.length - 1, 1) + s;
//     } else {
// 	return this.LARGE_BIG + this.int_to_bytes(s.length - 1, 4) + s;
//     }
// }

// BertClass.prototype.encode_float = function(Obj) {
//     // float...
//     var s = Obj.toExponential();
//     while (s.length < 31) {
// 	s += this.ZERO;
//     }
//     return this.FLOAT + s;
// }

// BertClass.prototype.encode_object = function(Obj) {
//     // Check if it's an atom, binary, or tuple...
//     if (Obj.type == "Atom") return this.encode_atom(Obj);
//     if (Obj.type == "Binary") return this.encode_binary(Obj);
//     if (Obj.type == "Tuple") return this.encode_tuple(Obj);
    
//     // Check if it's an array...
//     var isArray = Obj.constructor.toString().indexOf("Array") != -1;
//     if (isArray) return this.encode_array(Obj);
    
//     // Treat the object as an associative array...
//     return this.encode_associative_array(Obj);
// }

// BertClass.prototype.encode_atom = function(Obj) {
//     return this.ATOM + this.int_to_bytes(Obj.value.length, 2) + Obj.value;
// }

// BertClass.prototype.encode_binary = function(Obj) {
//     return this.BINARY + this.int_to_bytes(Obj.value.length, 4) + Obj.value;
// }

// BertClass.prototype.encode_tuple = function(Obj) {
//     var s = "";
//     if (Obj.length < 256) {
// 	s += this.SMALL_TUPLE + this.int_to_bytes(Obj.length, 1);
//     } else {
// 	s += this.LARGE_TUPLE + this.int_to_bytes(Obj.length, 4);
//     }
//     for (var i=0; i<Obj.length; i++) {
// 	s += this.encode_inner(Obj[i]);
//     }
//     return s;
// }

// BertClass.prototype.encode_array = function(Obj) {
//     var s = this.LIST + this.int_to_bytes(Obj.length, 4);
//     for (var i=0; i<Obj.length; i++) {
// 	s += this.encode_inner(Obj[i]);
//     }
//     s += this.NIL;
//     return s;
// }

// BertClass.prototype.encode_associative_array = function(Obj) {
//     var Arr = new Array();
//     for (var key in Obj) {
// 	Arr.push(Bert.tuple(Bert.atom(key), Obj[key]));
//     }
//     return this.encode_array(Arr);
// }



// // - DECODING -

// BertClass.prototype.decode_inner = function(S) {
//     var Type = S[0];
//     S = S.substring(1);
//     if (Type == this.SMALL_ATOM) return this.decode_atom(S, 1);
//     if (Type == this.ATOM) return this.decode_atom(S, 2);
//     if (Type == this.BINARY) return this.decode_binary(S);
//     if (Type == this.SMALL_INTEGER) return this.decode_integer(S, 1);
//     if (Type == this.INTEGER) return this.decode_integer(S, 4);
//     if (Type == this.SMALL_BIG) return this.decode_big(S, 1);
//     if (Type == this.LARGE_BIG) return this.decode_big(S, 4);
//     if (Type == this.FLOAT) return this.decode_float(S);
//     if (Type == this.STRING) return this.decode_string(S);
//     if (Type == this.LIST) return this.decode_list(S);
//     if (Type == this.SMALL_TUPLE) return this.decode_tuple(S, 1);
//     if (Type == this.LARGE_TUPLE) return this.decode_large_tuple(S, 4);
//     if (Type == this.NIL) return this.decode_nil(S);
//     throw("Unexpected BERT type: " + Type);
// }

// BertClass.prototype.decode_atom = function(S, Count) { 
//     var Size = this.bytes_to_int(S, Count);
//     S = S.substring(Count);
//     var Value = S.substring(0, Size);
//     if (Value == "true") Value = true;
//     if (Value == "false") Value = false;
//     return {
// 	value: Bert.atom(Value),
// 	rest:  S.substring(Size)
//     };
// }

// BertClass.prototype.decode_binary = function(S) { 
//     var Size = this.bytes_to_int(S, 4);
//     S = S.substring(4);
//     return {
// 	value: Bert.binary(S.substring(0, Size)),
// 	rest:  S.substring(Size)
//     };	
// }

// BertClass.prototype.decode_integer = function(S, Count) { 
//     var Value = this.bytes_to_int(S, Count);
//     S = S.substring(Count);
//     return {
// 	value: Value,
// 	rest:  S
//     };	
// }

// BertClass.prototype.decode_big = function(S, Count) { 
//     var Size = this.bytes_to_int(S, Count);
//     S = S.substring(Count);
//     var Value = this.bytes_to_bignum(S, Size);
//     return {
// 	value : Value,
// 	rest: S.substring(Size + 1)
//     }
// }

// BertClass.prototype.decode_float = function(S) { 
//     var Size = 31;
//     return {
// 	value: parseFloat(S.substring(0, Size)),
// 	rest: S.substring(Size)
//     };
// }

// BertClass.prototype.decode_string = function(S) { 
//     var Size = this.bytes_to_int(S, 2);
//     S = S.substring(2);
//     return {
// 	value: S.substring(0, Size),
// 	rest:  S.substring(Size)
//     };	
// }

// BertClass.prototype.decode_list = function(S) { 
//     var Size = this.bytes_to_int(S, 4);
//     S = S.substring(4);
//     var Arr = new Array();
//     for (var i=0; i<Size; i++) {
// 	var El = this.decode_inner(S);
// 	Arr.push(El.value);
// 	S = El.rest;
//     }
//     LastChar = S[0];
//     if (LastChar != this.NIL) throw("List does not end with NIL!");
//     S = S.substring(1);
//     return {
// 	value: Arr,
// 	rest: S
//     }
// }

// BertClass.prototype.decode_tuple = function(S, Count) { 
//     var Size = this.bytes_to_int(S, Count);
//     S = S.substring(Count);
//     var Arr = new Array();
//     for (var i=0; i<Size; i++) {
// 	var El = this.decode_inner(S);
// 	Arr.push(El.value);
// 	S = El.rest;
//     }
//     return {
// 	value: Bert.tuple(Arr),
// 	rest: S
//     }	
// }

// BertClass.prototype.decode_nil = function(S) {
//     // nil is an empty list
//     return {
// 	value: new Array(),
// 	rest: S
//     };
// }



// // - UTILITY FUNCTIONS -

// // Encode an integer to a big-endian byte-string of length Length.
// // Throw an exception if the integer is too large
// // to fit into the specified number of bytes.
// BertClass.prototype.int_to_bytes = function(Int, Length) {
//     var isNegative = (Int < 0);
//     if (isNegative) { Int = ~Int; }
//     var s = "";
//     var OriginalInt = Int;
//     for (var i=0; i<Length; i++) {
// 	var Rem = Int % 256;
// 	if (isNegative) Rem = 255 - Rem;
// 	s = String.fromCharCode(Rem) + s;
// 	Int = Math.floor(Int / 256);
//     }
//     if (Int > 0) throw("Argument out of range: " + OriginalInt);
//     return s;
// }

// // Read a big-endian encoded integer from the first Length bytes
// // of the supplied string.
// BertClass.prototype.bytes_to_int = function(S, Length) {
//     var Num = 0;
//     var isNegative = (S.charCodeAt(0) > 128);
//     for (var i=0; i<Length; i++) {
// 	var n = S.charCodeAt(i);
// 	if (isNegative) n = 255 - n;
// 	if (Num == 0) Num = n;
// 	else Num = Num * 256 + n;
//     }	
//     if (isNegative) Num = ~Num;
//     return Num;
// }

// // Encode an integer into an Erlang bignum,
// // which is a byte of 1 or 0 representing
// // whether the number is negative or positive,
// // followed by little-endian bytes. 
// BertClass.prototype.bignum_to_bytes = function(Int) {
//     var isNegative = Int < 0;
//     var s = "";
//     if (isNegative) { 
// 	Int *= -1; 
// 	s += String.fromCharCode(1);
//     } else {
// 	s += String.fromCharCode(0);
//     }
    
//     while (Int != 0) {
// 	var Rem = Int % 256;
// 	s += String.fromCharCode(Rem);
// 	Int = Math.floor(Int / 256);
//     }
    
//     return s;
// }

// // Encode a list of bytes into an Erlang bignum. 
// BertClass.prototype.bytes_to_bignum = function(S, Count) {
//     var isNegative = (S.charCodeAt(0) == 1);
//     S = S.substring(1);
//     var Num = 0;
//     for (var i=Count - 1; i>=0; i--) {
// 	var n = S.charCodeAt(i);
// 	if (Num == 0) Num = n;
// 	else Num = Num * 256 + n;
//     }
//     if (isNegative) return Num * -1;
//     return Num;
// }

// // Convert an array of bytes into a string.
// BertClass.prototype.bytes_to_string = function(Arr) {
//     var s = "";
//     for (var i=0; i<Arr.length; i++) {
// 	s += String.fromCharCode(Arr[i]);
//     }
//     return s;
// }

// // - TESTING -

// // Pretty Print a byte-string in Erlang binary form.
// BertClass.prototype.pp_bytes = function(Bin) {
//     s = "";
//     for (var i=0; i<Bin.length; i++) {
// 	if (s != "") s += ",";
// 	s += "" + Bin.charCodeAt(i);
//     }
//     return "<<" + s + ">>";
// }

// // Pretty Print a JS object in Erlang term form.
// BertClass.prototype.pp_term = function(Obj) {
//     return Obj.toString();
// }

// // Show off the different type of encodings we
// // can handle.
// BertClass.prototype.test_encode = function() {
//     alert(Bert.pp_bytes(Bert.encode(Bert.atom("hello"))));
//     alert(Bert.pp_bytes(Bert.encode(Bert.binary("hello"))));
//     alert(Bert.pp_bytes(Bert.encode(true)));
//     alert(Bert.pp_bytes(Bert.encode(42)));
//     alert(Bert.pp_bytes(Bert.encode(5000)));
//     alert(Bert.pp_bytes(Bert.encode(-5000)));
//     alert(Bert.pp_bytes(Bert.encode(987654321)));
//     alert(Bert.pp_bytes(Bert.encode(-987654321)));
//     alert(Bert.pp_bytes(Bert.encode(3.14159)));
//     alert(Bert.pp_bytes(Bert.encode(-3.14159)));
//     alert(Bert.pp_bytes(Bert.encode([1, 2, 3])));
//     alert(Bert.pp_bytes(Bert.encode({a:1, b:2, c:3})));
//     alert(Bert.pp_bytes(Bert.encode(Bert.tuple("Hello", 1))));
//     alert(Bert.pp_bytes(Bert.encode([])));
//     alert(Bert.pp_bytes(Bert.encode({
// 	a : Bert.tuple(1, 2, 3),
// 	b : [4, 5, 6]
//     })));
    
// }

// BertClass.prototype.test_decode = function() {
//     // Try decoding this: [{atom, myAtom},{binary, <<"My Binary">>},{bool, true}, {string, "Hello there"}],
//     TestTerm1 = Bert.bytes_to_string([131,108,0,0,0,4,104,2,100,0,4,97,116,111,109,100,0,6,109,121,65,116,111,109,104,2,100,0,6,98,105,110,97,114,121,109,0,0,0,9,77,121,32,66,105,110,97,114,121,104,2,100,0,4,98,111,111,108,100,0,4,116,114,117,101,104,2,100,0,6,115,116,114,105,110,103,107,0,11,72,101,108,108,111,32,116,104,101,114,101,106]);
//     alert(Bert.pp_term(Bert.decode(TestTerm1)));
    
//     // Try decoding this: [{small_integer, 42},{integer1, 5000},{integer2, -5000},{big_int1, 987654321},{big_int2, -987654321}],
//     TestTerm2 = Bert.bytes_to_string([131,108,0,0,0,5,104,2,100,0,13,115,109,97,108,108,95,105,110,116,101,103,101,114,97,42,104,2,100,0,8,105,110,116,101,103,101,114,49,98,0,0,19,136,104,2,100,0,8,105,110,116,101,103,101,114,50,98,255,255,236,120,104,2,100,0,8,98,105,103,95,105,110,116,49,110,4,0,177,104,222,58,104,2,100,0,8,98,105,103,95,105,110,116,50,110,4,1,177,104,222,58,106]);
//     alert(Bert.pp_term(Bert.decode(TestTerm2)));
    
//     // Try decoding this: -3.14159
//     TestTerm3 = Bert.bytes_to_string([131,99,45,51,46,49,52,49,53,56,57,57,57,57,57,57,57,57,57,57,56,56,50,54,50,101,43,48,48,0,0,0,0]);
//     alert(Bert.pp_term(Bert.decode(TestTerm3)));
    
//     // Try decoding this: [] (empty list)
//     TestTerm4 = Bert.bytes_to_string([131,106]);
//     alert(Bert.pp_term(Bert.decode(TestTerm4)));
// }

// var Bert = new BertClass();

// // Bert.test_encode();
// // Bert.test_decode();