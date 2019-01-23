---
layout: post
title: Python 装饰器实现的单例模式
subtitle: 
author: 李武
head-img: 
catalog: false
tags:
    - python
---

最近看到关于Python来实现设计模式中的singleton模式，发现有两个例子特别有意思。看代码：

```
class Foo():
    def __call__(self, *args, **argv):
        return self
        
Foo = Foo()
```

上面定义了类Foo， 在上面的代码运行完毕后，如果想创建Foo的实例，这些实例都是指向最早创建的那个实例。 这里有两步特别有意思并且互相呼应。 第一步实现了一个__call__方法， 意味着Foo的实例可以像使用函数一样来进行调用，并且始终返回实例本身。 第二步，Foo = Foo()做了名字的重新绑定， 那就意味着在此代码执行完毕后，已经没有Foo这个类型了， “Foo"这个名字完全地绑定在一个Foo的实例上， 配合第一步声明的方法，此后所有的Foo()调用，本来是用来创建一个实例，现在只是调用那个已经存在的实例（此时实例名字为Foo）的__call__方法返回实例本身。这是一种嫁接的方式，在完成第一次实例的初始化方法后，屏蔽了对类初始化函数的再次调用，从而实现singleton模式。

但是这个实现有缺点，每个singleton类型的类都要做类似这样的一个名字重新绑定。并不是一个通用的方法来把某个类变成singleton。

另外一种实现方式代码如下：

```
def singleton(cls):
    ins = {}
    def wrapper(*args, **kws):
        obj = None
        try:
            obj = ins[cls]
        except KeyError:
            obj = cls(*args, **kws)
            ins[cls] = obj
        return obj
    
    return wrapper
    
@singleton
class Foo(): pass
    
@singleton
class Bar(): pass
    
foo1, foo2 = Foo(), Foo()
id(foo1) == id(foo2)   #True
type(Foo)              #function
```

这是一种通用的singleton实现方式，通过实现一个装饰器，来对任一想成为singleton的类进行装饰。

需要注意的是，无论Foo类还是Bar类，通过装饰后，其名字所代表的已经不在时原先的类，而且，被一个叫做wrapper的函数所代替。 这是因为，装饰器实质上是一个重新绑定定义的过程。其过程等同于

```
Foo = singleton(Foo)
```

singleton这里是一个高级函数，它接受类作为参数名，返回一个函数代替了之前的Foo类的定义。

一般我们用装饰器来装饰函数，被装饰的对象与装饰后的对象是同样类型的，比如函数装饰器返回函数对象，类装饰器返回类对象。这里却使用了函数来代替被装饰的类，这在支持多特性的Python中，很容易做到这一点。 使用Foo()来创建一个类的实例，调用的是init方法，如果使用函数来代替，关键是将函数所接收到的参数以合适的方式传递给init构造方法。这里用到了可变参数的概念，通过\*args和**kws来接受与应用变长的位置参数和变长的关键字参数，实现了鸭子类型。一个函数和一个类的构造行为一样，并且它的返回值也是这个类的实例，那么对于外界调用者来讲，这个函数对象就是这个类型对象。

既然函数能够伪装成类，当然类也可以伪装函数的行为。一般我们不是直接使用类构造方法来模拟函数的行为，而是使用Python的魔法函数__call__来达到这个目的。如下代码所示：

```
def decor(f):
    class Wrapper:
        def __init__(self):
            self.func = f
        def __call__(self, *args, **kws):
            print("Inside class method")
            self.func(*args, **kws)
    
    return Wrapper()
    
@decor
def hello(name):
    print("Hello", name)

hello('Zhang San')
#Inside class method
#Hello Zhang San
```

在这里，hello函数经过装饰，实际上变成了一个Wrapper类的实例，对于hello函数的调用，也会变成对__call__函数的调用。

有机的结合Python各种高级概念，比如这里的magic函数，ducking type, 有的时候你会发现实现一些模式非常便利，这大概就是我喜欢Python的原因了吧，不费劲。

