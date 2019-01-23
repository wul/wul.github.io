---
layout: post
title: "期望值，方差和协方差（3）"
date: 2014-11-05 08:05:09 +0800
comments: true
categories: [Statictisc, Math, Machine Learning]
---

在多随机变量的方差运算中，我们看到了协方差的身影 COV。协方差的定义为：

$$cov(X,Y)=E[(X-\mu)(Y-\nu)] = E[XY]-E[X]E[Y]$$

$\mu$是 X 的期望值 E[X]，$\nu$是 Y 的期望值 E[Y]。协方差是描述两个随机变量之间的。协方差有时候也用符号$\sigma$来表示。这个公式看起来有点眼熟，如果我们让 Y=X，实际上它就变成了方差的定义。这不是偶合，下面详细说明。

协方差表示的是两个随机变量的总体误差，并描述两个随机变量之间的线性相关度。为了方便，我们经常使用规范化（normalized）的协方差。

$$corr(X,Y)=\frac{cov(X,Y)}{\sqrt{Var(X)Var(Y)}} $$

由柯西不等式可以知道这个值介于-1与1之间。 下图第一排反映的是2个随机变量（$X\_i,Y\_i$）构成的点集的相关性关系。如果两个随机变量正相关，即Y和 X 之间符合Y=aX+b，则它们的构成的点集为一条直线（corr=1）；如果2个随机变量相反，但符合 Y=-aX+b，此时他们负相关相反（corr=-1）；同时，还有一个特点，如果2个随机变量独立，或者说它们是不相关的，此时，则构成的点集是一团（corr=0）。

![aaaa](https://upload.wikimedia.org/wikipedia/commons/thumb/0/02/Correlation_examples.png/800px-Correlation_examples.png)
<center>图片取自wikipedia</center>

由方差公式 Var(X+Y)=Var(X)+Var(Y)+Cov(X,Y)知道，如果 X，Y 为独立变量，则 Cov(X,Y)=0。但不能翻过来说。亦，如果 Cov(X,Y)=0， 则 X,Y 互为独立变量是错误的。比如在[-1,1]之间 $Cov(X,X^2)$为0（根据 Cov 定义，代$入 X，X^2积分可得），但不能说 X 和 X^2$ 是独立的。

另一方面，如果两个随机变量非常相关，比如 Y=X，那么相关度公式

$$corr(X,X)=\frac{Var(X)}{\sqrt{Var(X)^2}}=1$$

从这个特例可直觉上印证随机变量的相关度。



在线性代数中，对于一个矩阵 A，其由 N 个列向量组成，每个列向量可以看做是一个随机变量的不同值构成，那么它的协方差矩阵为

$$\Sigma=E[(X-E[X])^{T}(X-E[X])]$$

协方差矩阵在这里描述的是任意两个向量之间的线性相关度（一般将 E(x)归一化成0）。在机器学习中得降维处理中有所应用。其目的在于寻找一组基使得排除一些不重要的维度成为可能。
