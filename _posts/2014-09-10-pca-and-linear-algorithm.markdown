---
layout: post
title: PCA的线性代数的背景
date: 2014-09-10 09:36:23 +0800
comments: true
categories: [Machine Learning]
---

前段时间虽然学了机器学习中关于 PCA 的相关算法，也知道如何利用 PCA 对数据进行降维处理；现有的语言如Python，Octave 都也有很多现成的函数可供利用，非常便捷。但是总是觉得学得囫囵，俗话说：授人以鱼不如授人以渔。还是很期望了解背后的数学原理。

于是钻研了一下，一路上是云漫漫兮白日寒，天荆地棘行路难。虽说学过高数，念过线性代数，看相关资料还是十分痛苦。实话说本来学得时候就不敢说都懂，现在又忘记的差不多了，这么多年不用，确实够呛。好在网上有一篇对 PCA 数学原理解析得非常好的文章，我估计让我总结也不如这个好，暂且借用一下，放个链接到此。
http://blog.codinglabs.org/articles/pca-tutorial.html
即使有如此好文章，看到后来的矩阵对角化还是感觉要复习的知识太多。对比 PCA 算法的几个步骤和这些算法库给的一些函数，很多情况不明白为什么要这样做，也不明白这是函数的返回值确切数学含义是什么，只是知道如何用而已。

所以，对照上面的这个文章，结合算法，我也来分析这些算法步骤的意义：

PCA 算法
样本数量为m, feature 个数为n

##计算协方差矩阵

$$
\Sigma=\frac{1}{m}\Sigma_{i=1}^{m}(x^{(i)})(x^{(i)})^{T}
$$
   为什么要算协方差矩阵呢？我们来看看这个算出来的矩阵（我们称这个矩阵为A）包含了些什么东西在里面：

$$
\begin{bmatrix}
\Sigma\_{i=1}^{m}x\_1^{(i)}x\_1^{(i)} &  \Sigma\_{i=1}^{m}x\_1^{(i)}x\_2^{(i)} &  ... & \Sigma\_{i=1}^{m}x\_1^{(i)}x\_n^{(i)} \cr
\Sigma\_{i=1}^{m}x\_2^{(i)}x\_1^{(i)} &  \Sigma\_{i=1}^{m}x\_2^{(i)}x\_2^{(i)} &  ... & \Sigma\_{i=1}^{m}x\_2^{(i)}x\_n^{(i)} \cr
... & ... & ... & ... \cr
\Sigma\_{i=1}^{m}x\_n^{(i)}x\_1^{(i)} &  \Sigma\_{i=1}^{m}x\_n^{(i)}x\_2^{(i)} &  ... & \Sigma\_{i=1}^{m}x\_n^{(i)}x\_n^{(i)} 
\end{bmatrix}=
\begin{bmatrix}
x\_1\*x\_1^{T} &  x\_1\*x\_2^{T} &  ... & x\_1\*x\_n^{T} \cr
x\_2\*x\_1^{T} &  x\_2\*x\_2^{T} &  ... & x\_2\*x\_n^{T} \cr
... & ... & ... & ... \cr
x\_n\*x\_1^{T} &  x\_n\*x\_2^{T} &  ... & x\_n\*x\_n^{T}
\end{bmatrix}
$$
这个矩阵是什么特点呢？ 这个协方差矩阵是一个对称矩阵。什么？费劲画了这么多知道是对称矩阵又如何？先不用急，先考虑一下 PCA 的目的是什么。PCA 主要目的是降维处理数据，方便各种算法，能够快速处理。在一个样本中，并不是所有的 feature 都同等重要，甚至有时候，有的 feature 根本就没有用（比如 feature $x\_2 = 2 \* x\_1$）, 这个时候，实际上 feature x2的引入并没有实际意义。那么 PCA 正是试图去掉这样的一些 feature，并同时最小化信息丢失。

说得好听？怎么才能“最小化信息的丢失”？这里用到线性变换，全部都是线性代数上的知识。首先，将每个样本看做是一个 n 维向量。想想一下二维空间中得一个向量，经过坐标变换可以得到另外一组坐标；n 维向量也可以通过线性变换得到另外一个向量。如果在变换过程中能够依次找到一个一个的基，在第一个基上，所有样本在其上的投影最长（最能区分不同的样本），第二个基，在垂直于第一个基（内积为0）的所有基的基础上，寻找哪个投影次长的基，第三个基在垂直于前两个基的基础上寻找，以此类推。当我们能够找到 n 个基，在他们上面的投影（l1,l2,l3,...,ln）即与原始的（x1,x2,x3,...,xn）相互对应。但是我们的目的是降维，比如从 n 维降低到 k（ 1<=k<n），我们找到的 n 各基，是按照重要程度以此下来的，因此或许找到第 K 的基，在基1,2,...,K上的线性变换或许就可以满足精度需要。

那么这与协方差矩阵有啥关系呢？观察这个协方差矩阵，假设此时的x1,x2,...,xn已经是按照我们找到的这一组基变换后的值。那么对角线上，$x\_i\*x\_i^{T}$ (1=<i <=n)  就是方差的一个变异形式，描述了在这个基上面的分散程度，我们希望其能够最大化，这也正是我们刚才找到的那一组基的目的。因此，这两个目标是一致的。

观察上三角区与下三角区，$x\_i\*x\_j$ (1=<i != j <=n) 是不同 feature 之间的协方差。在降维的时候，我们希望选择的各个基之间是线性无关的，也就是互相正交的。因此x1,x2,...,xn这些在各个基上的投影也应该满足$X\_i*X\_j=0$,或者说让这个值尽量的趋近于零。
	于是，我们的目标实际上就是对A进行对角化。

##矩阵对角化
对角化处理在Python中对应的函数有:numpy.linalg.svd，它用来生成特征向量与特征值。
	$$
	U,S,V=np.linalg.svd(A)
	$$
	
   矩阵对角化在线性代数上是求得特征向量（U）(确切的说，应该是特征向量对应的标准正交基)和特征值（$\lambda$）。数学公式略过，只看定理和结论。
   特征向量与特征值满足如下公式
   A\*U=U\*$\Lambda$, $\Lambda$是特征值$\lambda$构成的对角矩阵。矩阵 A，这里是我们要进行对角化的矩阵，也是我们最初定义的那个$X\*X^{T}$，它最终对角化变成了$\Lambda$，U 是用来进行变化的一组基。S 就是$\lambda\_i(1<=1<=n)$

##降维处理
使用对角化处理得到的U，就可以进行降维处理了，我们的目标既要降维，有不能损失太多的信息，将 S 以降序进行排列。假设我们要从 N 维降到 K 维，我们希望
$$
\frac{\Sigma\_{i=1}^{k}S\_{ii}}{\Sigma\_{i=1}^{n}S\_{ii}} >= 0.99
$$

我们可以依次去K=1,2,3,...,N来使上面的公式得到满足。假设得到这个 K。我们此刻即可对原始的样本 X 进行降维处理
$$
Z=U[0:K,:]\*X
$$

此刻 Z 就是我们经过降维化的样本了。
