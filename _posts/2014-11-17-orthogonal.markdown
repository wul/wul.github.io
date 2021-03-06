---
layout: post
title: 向量的正交性
date: 2014-11-17 09:24:43 +0800
comments: true
categories: [Linear Algebra, math]
---

正交性 (Orthogonal)
======

在向量空间中，如何评价两个向量之间的角度，或者更加进一步说他们之间的相关程度？ 想想在一个二维空间中，一个通过原点的向量的长度和方向由其坐标（x,y）决定。如果同时存在另外一个类似的向量，他们之间会存在一个夹角。这个夹角要么大于或者小于90度，要么正好90度。

这两个向量之间的夹角符合下面公式:


$$cos(\theta)=\frac{x^{T}y}{||x||*||y||}$$

这个公式可以由**余弦定理**推导得出。在此不推导了。我们关系的是两个向量的相关程度。如果2个向量垂直，我们说他们无关，或说正交。如果两个向量方向相同，则相关度最大。由上面这个公式可以看出，如果$\theta$为90度的时候，即，x，y 两个向量的内积为零。 如果两个向量之间夹角为0，则有

$$ ||x|| * ||y|| = x^{T} y $$ 

即内积等于向量长度之积。（如果向量方向相反，则等于内积的负值）