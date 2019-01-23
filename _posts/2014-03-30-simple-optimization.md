---
layout: post
title: 一个简单优化Python程序效率的小技巧
description: 一个简单的，不用改动代码逻辑的小方法。
categories: python
tags: python performance
---


先看以下两个程序, 先是s1.py

    #s1.py
    n = 0
    while n < 10000000:
        i, j , k = [1,2,3]
        m = i+j+k
        n += 1

再是s2.py

    def main():
	    n = 0
	    while n < 10000000:
	        i, j , k = [1,2,3]
		    m = i+j+k
	        n += i
    main()

这两个程序代码几乎一样，只是s2.py的主要代码都包在函数里面执行，而s1.py的代码直接在global空间执行。代码其实很简单，就是执行了一千万次的1，2，3的相加。

然后分别统计以下各自的执行时间。在我的MacPro上，它们的执行时间如下：

    wuli2-mac:tmp wuli2$ time python s1.py 

    real	0m3.898s
    user	0m3.886s
	sys	0m0.011s
	wuli2-mac:tmp wuli2$ time python s2.py 

	real	0m2.151s
	user	0m2.140s
	sys	0m0.010s
	wuli2-mac:tmp wuli2$ time python s1.py 

	real	0m3.875s
	user	0m3.862s
	sys	0m0.012s
	wuli2-mac:tmp wuli2$ time python s2.py 

	real	0m2.157s
	user	0m2.145s
	sys	0m0.011s
	wuli2-mac:tmp wuli2$ time python s1.py 

	real	0m3.908s
	user	0m3.897s
	sys	0m0.010s
	wuli2-mac:tmp wuli2$ time python s2.py 

	real	0m2.163s
	user	0m2.153s
	sys	0m0.009s

先简单说下time命令的输出。real代表实际程序执行的时间，这个是用户最直观的感受，程序到底从执行到结束执行了多少时间；user代表程序在用户态执行了多少时间，体现的是用户模式下执行指令画了多少CPU时间，至于你的用户代码因为等待或者被调度不算在里面；sys是代码因为系统调用进入内核态，在内核态执行占用了多少时间。 因此 user + sys代表了一个程序实际上占用了多少CPU资源。

在我们这个例子里面，sys占到的比值太小了，而且两个程序相比较差别也不大，因此我们看用户态执行时间的差异如何。3次执行结果取平均值，s1.py平均用了3.889秒，s2.py平均用了2.150秒。

这里的问题很明显，对于几乎一模一样的程序，为什么速度上有几乎81%的差异？

这本质上是Python对待全局变量与局部变量的区别。全局变量存放在字典中，对他们的存取是通过__getattribute__的操作，这样必然需要一些类似hash，搜作等操作，而局部变量一般的实现方式都是放在栈中或者寄存器中，它的存取必然比全局变量快。

因此，程序s1.py和s2.py的速度差异主要体现在一千万次对全局变量和局部变量的存取速度上。

单单从这个小小的优化来讲，似乎应用范围很窄，一般来讲，速度的瓶颈一般处于迭代或者递归之处，而这样的代码又一般封装在函数里面作为算法。当然了，这里也是一个提醒，这样的可能造成瓶颈的代码一定不要放在global空间来执行。

最后，还是那句老话。优化的第一步，就是不要优化。 程序的结构和逻辑比优化更重要，不要因为优化而大段改写代码，不易懂又难维护。


参考
======

http://stackoverflow.com/questions/9132288/why-are-local-variables-accessed-faster-than-global-variables-in-lua
    

