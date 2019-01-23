---
layout: post
title: "Python Relative Imports"
date: 2015-04-22 10:11:17 +0800
comments: true
categories: [python]
---
Python Relative Imports
======

Python包内模块之间当然可以像我们熟知的包导入方式一样，通过*import*或者*from ... import ...*来完成。 但是这里有个细节问题，可能会出现并非开发者所想到的后果。通过举例来说明， 现在要开发一个包（Package）Hello，它的目录结构如下：
    
    Hello\
	      __init__.py
          foo.py
          bar.py  
Hello这个包包含两个文件， foo.py 和 bar.py， 我们说 foo.py 包含一个函数叫做 *func*，bar.py 希望通过引入 foo 这个包来使用这个函数。那么对于 bar.py 来讲，导入并使用的方式可以如下：

    import foo
    foo.func()

这个用法似乎也没啥问题。 但是Python2.x 与 Python3.x 对于_import foo_的解释是不同的。

* Python2.x:会在当前包（"Hello"）所在的路径中搜索叫做 foo 的包名，然后搜索系统路径(sys.path)
* Python3.x:只会搜索系统路径

可以看到，对于一些程序，如果既要运行在 Python2.x，又要运行到 Python3.x，可能会出现问题，考虑到 sys.path 里面的目录，其中一个路径恰好也有一个模块是 foo，那么在 bar.py 引入 foo 模块的时候，Python3.x 会用这个模块而非和 bar 在同一个包得模块 foo，当调用*foo.func()*要么报错（不存在），要么用得不是期望的函数。

那么如何通过显示的手段来指明我们想要引入的包，来避免模块内包互相导入呢出现的上述问题呢？ 这可以使用 Python 的相对导入（Relative Imports）机制。 通过

 *  **from .foo import func**
 *  **from . import foo**

来对包 foo 进行导入。 这种通过一个点来指明包内导入的方式叫做相对导入。

无论是Python2.x或者Python3.x碰见这种语句，都会只在当前包目录中查找想要导入的包，而不会去系统路径中去寻找，因此避免了不当导入。

当然，包得层次结构也不都一定是扁平的，只有一个目录，可能有子目录，子目录下可能还有下级子目录。那么又如何在包内导入其他目录下的模块呢？ 

有人或许已经猜到，既然像 Unix 访问目录一样，用 “.”表示包内同一个目录。那么可以用“..”表示包层次结构中的上一层。仍旧通过举例说明，有一个复杂的目录结构如下：

    Hello \
	      __init__.py
          a\
		    __init__.py
            b\
			  __init__.py
              b1.py
          c\
		     __init__.py
             c1.py
             
Hello包包含子目录“a”和子目录“c”，其中子目录 “a”又包含二级子目录 “b”。 在目录“b”与“c”内分别有两个模块“b1”与“c1”。模块“c1”想使用“b1”中的函数“func”。

模块“c1”中可以用下面这种方式来引入模块“b1”.

* *from ..b.b1 import func*
* *import ..b.b1*

如同目录行为一样，很好理解，不用赘述。



可是，仍旧有人会说，其实也简单，将 Hello 包所在路径加入 sys.path 中，在模块“c1”中用如下语句也可以：

    from Hello.a.b.b1 import func
    #
    #or
	#
	import Hello.a.b.b1

是的，这种方式也可以。但首先，如果目录层次比较复杂，敲的字符会很长，不够经济；再者，如果目录进行挪动，需要改动代码，相对导入能够解决这个问题（相对路径如果保持不变）。

虽然用类似目录结构的方式相对导入包内的模块，但这种语句也只是适合包内导入，位于非包这种组织形式(包含\_\_init\_\_.py)的两个文件，不可以通过这种方式（from . .. import）来互相导入。

总结一下：Python 的相对导入的关键在于

* 它限定了搜索的路径在于包内，而不会盲目的在系统路径中找寻一个相同的名字，避免了误导入。
* 所有的相对导入通过*from ... import ...*这种格式来完成



