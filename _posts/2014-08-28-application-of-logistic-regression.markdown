---
layout: post
title: 逻辑回归的一个日常应用
date: 2014-08-28 18:22:40 +0800
comments: true
categories: ML
author: <Wu Li> li.koun@gmail.com
---
学了机器学习，一直没找到好的练习。经常练手的诸如垃圾邮件过滤，似乎已经有很多实现了，况且我也不是很感兴趣。翻着手机查看交通违章记录的时候，突然想到在网站上如果想查看违章记录，需要输入一堆东西，还要图片验证校验码，比较麻烦。 刚好，这些过程，比如自动获取网页，POST 信息，用程序实现都很方便，让人感兴趣的时校验码输入，刚好能够练习一下机器学习学到的东西。

查询违章记录的时针对上海市的交通信息网，里面有供司机查询电子眼拍到或现场违章记录。

用 Python 简单实现了Logistic Regression，配合一些手工得到的样本，图片识别很成功。一方面归功于网站的校验码设置也比较简单，另一方面，也体现 ML 的实用性。

反正是练习，顺便用 SVM 和 Decision Tree 同时来识别校验码，效果都很好，唯一不好的时 kNN, 经常将数字8和0混淆，也可能是样本少得缘故吧。
   
源代码在github:    https://github.com/wul/tvish

   