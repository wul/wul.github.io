---
layout: post
title: Latex support on github blogs
date: 2014-08-29 09:08:24 +0800
comments: true
categories: stuff
---


$$
f(x)=\frac{1}{\sqrt{2\pi}\sigma}e^{\frac{(x-\mu)^{2}}{2\sigma^{2}}}
$$



Thanks for MathJax ，现在可以在Github blog 上面增加 Latex 支持了。步骤很简单：

1. 将以下代码拷贝到布局页面中去，和其他 load 各种 javascript 的代码放在一起。
        
        <script type="text/javascript" src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"></script>  

2. 然后就可以开始书写 Latex 代码了，前后用双美元符号($$)括起来即可.


有一点不便利，不知道是我哪里用错了，书写 Latex 代码的时候，所有的 superscribe都必须用大括号括起来，否则就不行。
