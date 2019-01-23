---
layout: post
title: " Python 包管理以及导入包时候一些问题辨析"
date: 2015-04-29 14:16:39 +0800
comments: true
categories: [python]
---

之前写了一点关于Python相对包导入机制的东西。除过概念上要区分传统导入方式，记住包导入时候如何寻找路径之外。还经常会碰见在包管理以及使用上因为随意混合这两种方式所带来的意想不到的后果。



之前说过，对于传统包引用：

    import foo

Python2.x和Python3.x对其的解释是不同的。

* Python2.x: 寻找当前路径（CWD），然后系统路径
* Python3.x: 只查找系统路径


那么，加入我们有一个包"foo"，其内部模块如下:

    foo \
        __init__.py
        main.py
        bar.py
        baz.py

其中, main是一个主程序，也放在这个包中，内容如下：

    import bar

bar.py内容只有一行，就是加载 baz 包:

    import baz

baz内容略过，不重要。那么我们运行如下命令

    python foo/main.py

无论对于Python2.x和Python3.x，结果都正常，很和谐，没有错误。这是因为无论从相对路径中还是系统绝对路径中， 主程序 main.py 所在的目录都在其中，所以 python3.x 和 python2.x 都能正常工作。但是如果 main.py 不在 foo 目录下面，将其内容修改为**import foo.baz**，如其下所示:

    main.py
    foo \
        __init__.py
        bar.py
        baz.py

那么运行python3 main.py会出现异常"ImportError: No module named 'baz' "，这是因为python3.x 只会在系统目录下查找 baz 模块，而系统目录只是一组包含主文件程序文件 main.py 所在目录的列表，里面并不包含baz这个模块。 要解决这个问题，唯有把 foo 所在的目录写入到 sys.path 中去（手工写入或者通过 PYTHONPATH 环境变量）

关于传统的 import mod 语句，重点在于：**在 Python3.x 下，如果工作于包管理模式下，则不会从mod.py 所在目录下查找 mod 模块（除非这个目录被用各种方式加入到 sys.path 中去）**


那么对于相对导入 from . import mod 这种语句，我们知道它是用来在包内各个模块至今互相引用的。它的初衷服务于包管理。我们仍然使用包含main.py的最初例子, 这个时候不管 Python2.x还是Python3.x在运行 main.py 主程序的时候都会抛关于无法相对导入模块的异常。Python 不允许你在使用相对导入这种包管理的时候把包的目录当做程序目录。你必须要吧 main.py 从包内部拿出来，比如放在 foo 所在的同级目录下。为什么？ 不清楚。

重点在于，**如果你使用了相对导入，那么就需要将包内部的目录层次完全当做包来使用，不要混合程序文件在其中**。


如果想使用相对导入模式，你会碰到更大麻烦。比如对于包内的模块 bar.py, baz.py， 我怎么在这些文件中加入 unittest，并把这些 module 文件当做程序文件来进行单元测试呢，这可是最好的放 unittest 的地方啊？ 这是一个好问题。 

答案是可以用传统的包导入方式来解决。 什么？ 又回去了！！！ 我也不想啊，Python 看到这里我也想找个人抽一抽，费劲理解这么多概念，一会碰到这个问题一会碰到那个问题，最后来还是用老方式好。 这里一点都不 Pythonic。

抱怨归抱怨，如何解决让包内的文件同时可以当做程序文件来执行呢。 举个例子，对于 bar.py，我们使用

    from foo import baz

 一方面可以解决相对导入模式下无法当做程序文件使用；另外一方面可以解决不小心在 sys.path 下找到一个同名的 baz 文件而导入错的事情。当然了，代价就是敲的字符多了，当初从 import baz 到相对导入 from . import baz， 现在时 from foo import baz， baz 如果层次越深敲得越多。

似乎使用绝对导入能够使用的范围更为广泛。那么为什么要存在相对导入这种奇葩呢？他们说，能够解决一个层次复杂的 package 重新组织时候需要修改各个包的麻烦。 我相信这是对的，不过我也相信相对导入带来的麻烦更多。


