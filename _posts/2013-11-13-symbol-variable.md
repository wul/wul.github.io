---
layout: post
title: Symbol, Variable, Name, Value, what exactly are they in Lisp?
description: 
categories: lisp
author: <Wu Li> li.koun@gmail.com
---

Symbol, Variable, Name, Value, what exactly are they in Lisp?

Let’s start with some simple expressions we saw many times.

    x                     ;the symbol X
    ()                    ;the empty list
    (1 2 3)               ;a list of three numbers
    ("foo" "bar")         ;a list of two strings
    (x y z)               ;a list of three symbols
    (x 1 "foo")           ;a list of a symbol, a number, and a string 
    (+ (* 2 3) 4)         ;a list of a symbol, a list, and a number
	
At first, when we talk about a name, that is always a string, like “foo”, “bar”. They are enclosed with " in both sides.

Value in Lisp is a object.

Now we are back to "Symbol". Symbol is a data type in Lisp. It is typical a word or a phrase, like APPLE, ABS. Actually, it can be composed by any sequence of letters, digits, and permissible characters.

Every symbol has a name, its name is the just the literal presentation of itself. For example, symbol x has the name “X”.

Symbol may or may not has an value. If a symbol has a value, we call the symbol "variable".  That means symbol has a reference to the value (object). For example

     (defparameter x  1)
	 (symbol-value 'x)  ==> 1
	 
This creates a symbol X, and binds the value 1 (object 1) to the symbol with name "X". X now is a variable. A symbol can also have no value. For example, we just type 'y in SBCL interpreter, that will create a symbol Y without value assigned.

     'y                 ==> Y
	 (symbol-value 'y)
	 debugger invoked on a UNBOUND-VARIABLE: The variable Y is unbound.

     Type HELP for debugger help, or (SB-EXT:EXIT) to exit from SBCL.

     restarts (invokable by number or by possibly-abbreviated name):
       0: [ABORT] Exit debugger, returning to top level.

    (SYMBOL-VALUE Y)
    0] 0
     

In additional of the variable, function definition actually assigns a function object to a symbol

     (defun func () (print “hi”))
	 
This binds symbol FUNC with a function object. So, a symbol can either be a identifier for object (we call it a variable, use *symbol-value* get the value) or a identifier to function (we call it function, use *symbol-function* to get the function object)

Let’s take a look the lisp syntax again to review these definition.
`(setf x 1)` defines a varaible, that creates a symbol and an object then bind them together. The variable name is the symbol name.  
`(defun fun() (print 1))` defines a function, that creates a symbol and a function object and bind them together. The function name is the symbol name  
And, furthermore, if we do
`(setf  myvar ‘hello)`, Lisp will create two symbols, MYVAR and HELLO, set value of MYVAR to literal HELLO (set to a symbol object, not “HELLO” string). But Lisp does not assign a value to symbol HELLO, symbol HELLO just has a name attribute.

A full list of symbol attributes are:

*  Name    - a string represents symbol name
*  Package - indicates which package the symbol belongs to
*  Property list - …
*  Value - Used to bind a object.
*  Function - Used to bind a function object
