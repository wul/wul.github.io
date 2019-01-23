---
layout: post
title: 作为数据结构的列表（List），数组（Array）简单数组（Simple Array），向量（Vector）与简单向量（Simple Vector）
description: Lisp数据结构的效率比较
categories: lisp
author: <Wu Li> li.koun@gmail.com
---

作为数据结构的列表（List），数组（Array）简单数组（Simple Array），向量（Vector）与简单向量（Simple Vector）


在用Lisp实现算法的时候，发现如果只是为了描述一种算法是如何运作的，无论采取什么样的实际数据结构都没有关系。比如描述各种排序算法，用到的数据结构是数组，各种数组的操作主要就是从下标获取数组中元素的值，那么用Lisp来实现，为了表述清楚，可以选择列表，数组，向量，简单向量都行。唯一不同的是，这些实现在实际运行的时候，因为选择了不同的数据结构，在效率上存在很大差异。比如选择数组比选择列表要效率高很多。那么，为了搞清楚围绕他们的一些操作符的效率如何，有必要了解一下Lisp中这些数据结构具体如何实现。对于这些数据结构的定义（或说明）如下：
*   列表（List）：一个列表中的各个元素通过元素之间的指针来索引。比如一个4元素的列表(a b c d)，它的表示如下
    ![列表](/images/list.png)
*   数组（Array）: Lisp支持多维数组，一个完全的使用MAKE-ARRAY来声明数组的细节可以参考Common Lisp HyperSpec.这里先举两个例子：   
    创建一个3维数组，维度分别维2，3，4并且带有初始值：

        (make-array '(2 3 4) :initial-contents '( ((1 2 3 4) (2 3 4 5) (3 4 5 6))
                                                  ((0 0 0 0) (1 1 1 1) (2 2 2 2))))
												  

    创建一个可变长度的一维数组，初始长度为3：

        ;fill-pointer is an integer associated with the vector, it represents the index and above which no elements are active
        (make-array 3 :adjustable t :fill-pointer 0) 
		
*   简单数组（Simple Array）：不与其他数组共享结构，同时没有fill pointers,也不可调整大小。
*   向量（Vector）：向量极为一维数组，可以用MAKE-ARRAY或者VECTOR函数来创建
*   简单向量（Simple Vector）：与简单数组的定义相同，简单向量不可调整大小，没有fill pointers，也不与其他向量共享结构。

Lisp中的序列指的是列表和向量，针对列表和向量的操作符有：

*   ELT： 比如 `(elt seq 2)`
*   NTH: 比如 (nth  1 lst)
*   AREF(对数组操作)：比如`(aref two-demision-arrary 2 3)`或`(aref arr 2)`.简单数组和简单向量因为本身也是数组和向量，这个针对数组的操作符对其也适用.同时，对于简单向量，还额外有一个操作符来获取某个位置上的元素：
*   SVREF:和AREF类似，不同的是，第一个参数必须是一个简单向量.


至于不同结构排序时候的效率，列表肯定是最慢，因为每个元素的索引本事就是需要迭代获取的。至于向量和简单向量，则并没有多大区别。

以下是适用合并排序的一个结果，随机产生数组，包含1，800，000个数字


	IS simple vector: NIL
	Evaluation took:
	  3.449 seconds of real time
	  3.448972 seconds of total run time (3.355315 user, 0.093657 system)
	  [ Run times consist of 0.254 seconds GC time, and 3.195 seconds non-GC time. ]
	  100.00% CPU
	  8,258,992,983 processor cycles
	  1,469,701,840 bytes consed


--------以上是向量，以下适用简单向量，排序数组个数1,800,000------------------

	IS simple vector: T
	Evaluation took:
      3.430 seconds of real time
	  3.430265 seconds of total run time (3.333013 user, 0.097252 system)
	  [ Run times consist of 0.327 seconds GC time, and 3.104 seconds non-GC time. ]
	  100.00% CPU
	  8,214,305,702 processor cycles
	  1,485,606,720 bytes consed

随机产生数组，包含900，000个数字 


	IS simple vector: NIL
	Evaluation took:
	  1.649 seconds of real time
	  1.648613 seconds of total run time (1.605715 user, 0.042898 system)
	  [ Run times consist of 0.096 seconds GC time, and 1.553 seconds non-GC time. ]
	  100.00% CPU
	  3,947,813,569 processor cycles
	  721,261,328 bytes consed`
  
--------以上是向量，以下适用简单向量，排序数组个数900,000------------------ 


	IS simple vector: T
	Evaluation took:
	  1.671 seconds of real time
	  1.670449 seconds of total run time (1.588011 user, 0.082438 system)
	  [ Run times consist of 0.066 seconds GC time, and 1.605 seconds non-GC time. ]
	  99.94% CPU
	  4,000,369,754 processor cycles
	  734,046,224 bytes consed`


随机产生数组，包含500，000个数字


	IS simple vector: NIL
	Evaluation took:
	  0.932 seconds of real time
	  0.932690 seconds of total run time (0.898945 user, 0.033745 system)
	  [ Run times consist of 0.064 seconds GC time, and 0.869 seconds non-GC time. ]
	  100.11% CPU
	  2,233,946,007 processor cycles
	  396,433,808 bytes consed
  
--------以上是向量，以下适用简单向量，排序数组个数500,000------------------ 


	IS simple vector: T
	Evaluation took:
	  0.850 seconds of real time
	  0.850777 seconds of total run time (0.827034 user, 0.023743 system)
	  [ Run times consist of 0.047 seconds GC time, and 0.804 seconds non-GC time. ]
	  100.12% CPU
	  2,037,277,796 processor cycles
	  396,432,288 bytes consed


数据也证实简单数组／向量和数组／向量之间并没有太多效率上的差异，那么为什么要有简单数组／向量呢？ 看来，这篇文章还得继续写下去。
