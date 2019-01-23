---
categories: lisp
layout: post
title: lisp字符串处理的一些方法
author: <Wu Li> li.koun@gmail.com
---

lisp字符串处理的一些方法

Lisp的字符串处理函数是相对贫乏。一些常用的操作，如split, replace, join没有提供。相对于其他的高级语言，比如Python，不能不说有些差距。

这里对照Python提供的字符串相关方法和函数，来比较下Lisp是否有相应的实现。第一列是Python的字符串方法或者函数，第二列是Lisp对应的函数，如果为空，则代表没有相应的处理函数。

首先，比较常用的字符串操作，与Python相比较，如下图:

<table border=1>
	<tr>
		<th>Python</th>
		<th>Lisp</th>
		<th>Description</th>
	</tr>
	<tr>
		<td>s.capitalize</td>
		<td>STRING-CAPITALIZE</td>
		<td>(string-capitalize "this IS A BOOK”) ->  "This Is A Book” "this IS A BOOK".capitalize() ->  'This is a book’ Lisp中的CAPITALIZE函数与Python字符串的Capitalize的效果是不一样的，它实际上的效果与Python字符串的title方法一样
		</td>
	</tr>
	<tr>
		<td>s.center</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.count	</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.encode	</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.decode			</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.endswith			</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.expandtabs			</td>
		<td></td>
		<td></td>
	</tr>
	<tr>
		<td>s.find			</td>
		<td>POSITION FIND SEARCH </td>
		<td>Lisp有三个函数来查找特定的子串，其中position和find查找单个字符，search查找子字符串。position和find的不同在于，find返回的是匹配的字符串，而不是下标索引值
		</td>
	</tr>
	<tr><td>s.format</td><td>FORMAT</td><td></td></tr>
	<tr><td>s.index	</td><td>POSITION FIND SEARCH		</td><td></td></tr>
	<tr><td>s.isalnum</td><td></td><td></td></tr>
	<tr><td>s.isalpha</td><td></td><td></td></tr>
	<tr><td>s.isdigit</td><td></td><td></td></tr>
	<tr><td>s.islower</td><td></td><td></td></tr>
	<tr><td>s.isspace</td><td></td><td></td></tr>
	<tr><td>s.istitle</td><td></td><td></td></tr>
	<tr><td>s.isupper</td><td></td><td></td></tr>
	<tr><td>s.join</td><td></td><td></td></tr>
	<tr><td>s.ljust	</td><td></td><td></td></tr>
	<tr><td>s.lower		</td><td>	STRING-DOWNCASE</td><td></td></tr>
	<tr><td>s.lstrip		</td><td>STRING-LEFT-TRIM</td><td></td></tr>
	<tr><td>s.partition</td></td><td></td></tr>
	<tr><td>s.replace</td><td></td><td>LISP has REPLACE function, but they have different definition</td></tr>
	<tr><td>s.rfind</td><td></td><td></td></tr>
	<tr><td>s.rjust	</td><td></td><td></td></tr>
	<tr><td>s.rpartition</td><td></td><td></td></tr>
	<tr><td>s.rsplit</td><td></td><td></td></tr>
	<tr><td>s.rstrip		</td><td>STRING-RIGHT-TRIM</td><td></td></tr>
	<tr><td>s.split</td><td></td><td></td></tr>
	<tr><td>s.splitlines</td><td></td><td></td></tr>
	<tr><td>s.startswith</td><td></td><td></td></tr>
	<tr><td>s.strip	</td><td>STRING-TRIM</td><td></td></tr>
	<tr><td>s.swapcase</td><td></td><td></td></tr>
	<tr><td>s.title</td>STRING-CAPITALIZE</td><td></td></tr>
	<tr><td>s.translate</td><td></td><td></td></tr>
	<tr><td>s.upper		</td><td>	STRING-UPCASE </td><td></td></tr>
	<tr><td>s.zfill </td><td></td><td></td></tr>
	<tr><td>sorted(s)</td><td>SORT</td><td></td></tr>
	<tr><td>reversed(s)	</td><td>REVERSE</td><td></td></tr>
</table>

再比较字符串与数字转换的一些操作:
<table border=1>
	<tr><th>Python</th><th>Lisp</th><th>说明</th></tr>
	<tr><td>int</td><td>PARSE-INTEGER</td><td>字符串转换成数字</td></tr>
	<tr><td>str</td><td>WRITE-TO-STRING</td><td>数字穿换成字符串</td></tr>
	<tr><td>ord</td><td>CHAR-CODE</td><td>字符的数字值</td></tr>
	<tr><td>chr</td><td>CODE-CHAR</td><td>数字值转换成字符</td></tr>
</table>

第二列所列举的Lisp的字符串处理函数中，很多都是缺失的。而这些缺失的函数里面，以下的几个关键操作比较让人难以容忍。它们是replacing, splitting，join操作

split根据给定的分割字符串来分割目标字符串，我们可以定义为
`(defun split (delimiters str) body*)`  
因为lisp已经有了一个叫做replace的函数，用以替换给定位置上的字符串，同时有另外一个函数substitute，用来替换字符串中的字符。这两个函数都不能够完成寻找并替换匹配的子字符串。因此定义另外一个函数在完成子字符串的替换。  
`(defun replace-all (from to target) body*)`  
还有一个是join，接受一个分割的字符串，把给定列表中的字符串用此分割字符串拼接起来  
`(defun join (str lst) body*)`


可以自己实现这些函数，也可以直接使用其他的库提供的接口。这样的包包括cl-ppcre，提供与Perl兼容的正则表达式处理接口。可以使用它的splitting和replacing相关的函数，这些函数不单可以作用在简单的字符串上面，还可以匹配正则表达式，功能很强大.


cl-ppcre提供的相关函数如下：

    split regex target-string &key start end limit with-registers-p omit-unmatched-p sharedp => list
    (split “ab” “eeeabc”) -> (“eee” “c”)

    regex-replace-all regex target-string replacement &key start end preserve-case simple-calls element-type => string, match
    (regex-replace-all "ab" "eeeabc" "df”)

这两个例子是简单的匹配，没有用到复杂的正则表达式。详细可见cl-ppcre文档:http://weitz.de/cl-ppcre

cl-utilties也提供另外一个版本的split，相对功能简单点。只能够从sequence里面根据指定的delimiter来分割序列，这个delimiter不是一个集合，只是单独的一个object。

至于自己实现这3个函数，其实也不难，代码如下：

	(defun split (delimiters s)
	  (flet ((delimiterp (c) 
				   (position c delimiters)))
		(let ((slices nil) (substr nil))
		  (loop for c across s do
			 (if (not (delimiterp c))
			  (setf substr (concatenate 'string substr (string c)))
			   (when substr
			  (setf slices (append slices (list substr)))
			  (setf substr nil))))
		  ;append last substring
		  (if substr  (setf slices (append slices (list substr))))
		  slices)))


	(defun join (str lst)
		(reduce  #'(lambda (sum ele) (concatenate 'string (string sum) str  (string ele))) lst))

	(defun replace-all (from to target)
	  (let ((sum nil) (begin 0) (end t))
		(loop while end do
		   (setf end (search from target :start2 begin))
		   (print end)
		   (when end
			 (setf sum (concatenate 'string sum (subseq target begin end) to))
			 (setf begin (+ end (length from)))))
		sum)) 
