---
layout: post
title: Lisp list和vector的效率
description:
categories: lisp
author: <Wu Li> li.koun@gmail.com
---

LISP 在做大序列运算中list 和vector的效率

输入分别是包含3万个元素的列表和向量（一位数组），采用合并排序（Merge Sort），下面是所用时间

    Test merge sort by using list
    Evaluation took:
      14.406 seconds of real time
      14.401719 seconds of total run time (14.394148 user, 0.007571 system)
      99.97% CPU
      34,497,526,560 processor cycles
      25,904,128 bytes consed
  
    Test merge sort by using vector
    Evaluation took:
      0.041 seconds of real time
      0.040773 seconds of total run time (0.040756 user, 0.000017 system)
      100.00% CPU
      97,625,097 processor cycles
      22,743,328 bytes consed

明显采用向量更加有效率，其原因在于在列表中，如果想查找第m个元素，使用函数 NTH需要在列表上从头做迭代，一直找到第m个元素。而数组取下标通过偏移一次就可以找到。 

但是同时也比较了下采用向量的合并排序与系统提供的SORT函数的效率，发现奇特现象，系统提供排序函数效率极高，随着序列的增大，所耗费的时间增长不显著。同时使用列表也似乎不影响其效率。以下是

3万个元素的列表排序。

    Test merge sort by sys sort
    Evaluation took:
      0.007 seconds of real time
      0.007739 seconds of total run time (0.007738 user, 0.000001 system)
      114.29% CPU
      18,523,509 processor cycles
      0 bytes consed

6万个元素列表排序：

    Test merge sort by sys sort
    Evaluation took:
      0.015 seconds of real time
      0.015586 seconds of total run time (0.015582 user, 0.000004 system)
      106.67% CPU
      37,315,730 processor cycles
      0 bytes consed

12万个元素列表排序：

    Test merge sort by sys sort
    Evaluation took:
      0.042 seconds of real time
      0.042162 seconds of total run time (0.042148 user, 0.000014 system)
      100.00% CPU
      100,950,376 processor cycles
      0 bytes consed

24万个元素列表排序：

    Test merge sort by sys sort
    Evaluation took:
      0.095 seconds of real time
      0.095124 seconds of total run time (0.095093 user, 0.000031 system)
      100.00% CPU
      227,771,817 processor cycles
      0 bytes consed
  
系统自带的SORT方法效率很高，24万个元素的排序不到0.1秒就能够完成
