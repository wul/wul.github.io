---
layout: post
title: Python Decorator
description: Detailed Description of Python Decorator, its types and how it works
categories: python
tags: python, decorator
---

装饰（Decoration）
======
Decoration 是一种向函数或者类添加代码或者进行管理的方式。在Python中，我们是通过使用装饰器来实现这种管理或者代码注入的。

装饰器（Decorator）
======
装饰器 (Decorator) 本身也是callable对象（函数或类）,其处理callable对象并返回callable对象（要理解好下文的概念，一定要理解callable对象，有关callable 的概念请参考 Python手册 ）。 按照被装饰对象分类可以分为

   * Function Decorator 对函数进行装饰，提供函数的管理  
   * Class Decorator 用来对类进行装饰，对类及其类的实例进行管理

无论对类还是对函数进行装饰，其实质都是在函数或者类的定义之后添加了一些代码。至于这些添加的代码，则可以完成各种任务。比如：

用来代理函数调用：通过安装wrapper函数，使得对函数的调用先经过添加的代码，执行一些处理，然后才将执行传递给原先被装饰的函数。
用来接口代理：对类进行代理，使得类实例的创建接口被代理。可以先经过添加的代码执行一些处理，然后调用原先的类创建函数，使得最后被创建的实例能够被管理。
装饰的原理和过程如下：

在函数定义(def)或者类定义(class)之后，装饰器先对原先的函数或者类添加一些处理。
装饰器将处wrapper函数或者原函数或类或者类重新赋值回原先的函数或者类名。
当调用者使用这些callable的函数和类的时候，它实际上调用的是被装饰过的，与先前函数或者类有相同名字的callable而已。至于这些装饰过的函数具体做些什么，则完全看装饰器的目的如何。
为什么需要装饰器呢？因为在实际变成中，我们可能需要处理以下场景：

对函数进行日志，跟踪，调试，timing，锁处理等。
对类及其属性进行跟踪，验证等。
对这些的处理可以手工来做，但是使用装饰器有两个好处：

显式语法：使得处理的意图明确，比如@classmethod,@property,@staticmethod等。
一次定义，到处使用：因为wrapping的方式都是一样的，因此装饰器可以定义后用在任何使用这些代码的地方，不用到处拷贝代码。

使用装饰器
======

在定义自己的装饰器之前，先看一些使用装饰器的例子，来理解装饰器怎么用，它揭示了些什么秘密。

*   函数装饰器:
    函数装饰器作用在函数上，包括普通函数和类的成员函数。用在函数定义语句def之前。

        #对函数进行装饰
        @decorator
        def func (*args):
            ...


    调用被装饰的函数

        func(1,2,3)

    其本质是对函数增加了代码并对函数名字的重绑定

        def func (*args):
            ...

        func = decorator(func)

    decorator就像是一个高阶函数，对函数进行加工处理，因此调用时候实际上调用 func(1,2,3) 就是


        decorator(func)(1,2,3)

    不单对于普通函数可以应用装饰器，对于类函数亦可以。可以参考@classmethod, @staticmethod, @property

*   类装饰器

    类装饰器作用在类上。用在类定义语句class之前。

        #对类进行装饰
        @decorator
        class Cls:
            ...

    使用被装饰的类

        ins = Cls()

    实际发生的是Cls = decorator(Cls)，Cls实际上变成另外一个改装的类。代码ins=Cls()实际上是ins=decorator(Cls)()，因此看起来生成的实例ins实际上是被装饰类的返回callable对象调用的结果。这个callable可以是一个被装饰后的类，也有可能是一个函数而已，完全看装饰器是如何实现的。但是最终的结果是生成了一个callable对象ins.一个 正常 的装饰器应该允许此ins实例调用其实例方法，属性，除过装饰作用，应该保留被装饰类的特性，让用户用起来能够保持其原先的语义。

实现装饰器
======

装饰器本身就是callable对象，因此可以用以下两种方式来实现装饰器

*   通过函数来实现装饰

    最常用和易于理解的实现方式。通过定义一个函数，其加工callable对象，并返回callable对象。比如要对一个callable对象进行装饰，可以通过函数来先定义一个装饰器如下：

        def decorator1(f):
            #added a lot of code here
            ...
            #finally, you return original f
            return f

        def decorator2(f)
            #added a lot of code here
            ...
            #and you defined a wrapper
            def wrapper_of_f(*args):
                #some code 
                ...
                #execute original callable
                f(*args)

            #finally, you return a wapper of f
            return wrapper_of_f
            ... 


    decorator1仅仅在你定义完毕callable的时候增添了些代码，或者用来管理记录，或者用来调试等，它将用户定义的函数重新赋值给原先的函数名。decorator2则安装了一个代理函数，并将其返回，当你使用调用callable对象的时候，实际上已经不是执行原先的函数，而是这个代理函数了。

*   通过类来实现对象

    一个类通过实现_call_方法可以使其变成一个callable对象。借助于这个特性，可以用类来实现装饰器。
	

        class decorator:
            def __init__(self, f):
                self.f = f
                #add your code here
                ...

            def __call__(self, *args):
                #add your code here
                ...
                #then call original callable
                self.f(args)

    通过用类实现的装饰器与用函数实现的装饰器原理类似。对于最初的一个callable，比如foo。首先，在对其装饰的时候

        @decorator
        def foo(*args):
            ...

    会生成一个decorator类的实例，这个实例存有变量self.f，指向原先的foo。这个decorator的实例会被重新绑定到名字foo上。因此之后调用foo其实是调用decorator的实例，由于这个实例实现了"_call__"方法（实例是个callable对象），使得实例的调用即为"decorator._call__"的调用.

需要注意的是，用类来实现装饰器的时候，如果被装饰的callable是个类成员函数的话，会引起副作用。接着用上面的例子，如果foo是一个类Cls的成员函数，如下定义并使用装饰器:

    class decorator:
        def __init__(self, f):
            self.f = f

        def __call__(self, *args):
            print self
            self.f(args)

    class Cls:
        @decorator
        def foo(self, data):
            print data
那么我们调用的如下代码的时候

    ins = Cls()
    ins.foo("hi")   #call ins.
存成/tmp/decorator.py并执行以上代码，输出为

    <__main__.decorator instance at 0x108793560>
    Traceback (most recent call last):
      File "/tmp/decorator.py", line 16, in <module>
        ins.foo("hi")
      File "/tmp/decorator.py", line 7, in __call__
        self.f(args)
    TypeError: foo() takes exactly 2 arguments (1 given)
ins.foo()调用实际上调用的是decorator的实例，这个实例有"_call__"方法，是个callable对象，因此会最终执行到"decorator._call__"中的self.f,这才是最初定义的Cls的成员函数。但是此时遗憾的是，在装饰器调用它的时候，已经丢失了实例对象，self无法被替换成Cls的实例ins。 如果用函数来实现装饰器的话则没有这个问题，如下:

    def decorator(f):
        def wrapper(*args):
            print "inside wrapper"
            f(*args)

        return wrapper

    class Cls:
        @decorator
        def foo(self, data):
            print data
输出为

    inside wrapper
    hi
实际上调用代理方法wrapper的时候，实例ins作为第一个参数，这些参数会原封不动的继续传递给wrapper中的f（即Cls的foo）

总结
======

装饰器的实际上就是一个callable对象，通过在函数或者类定义之后增加一些代码装饰另外一些callable（类或函数），并且返回经过装饰的callable（类或函数），来达到一些特殊目的。

最后，感谢emacs orgmode，最近发现了这个好东西，在emacs直接写文本，自动转换成html，方便了许多。
