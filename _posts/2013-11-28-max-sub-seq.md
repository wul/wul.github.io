---
layout: post
title: 最大子序列问题
description: 给定一个实数构成的序列，如何寻找一个子序列，使其相加之和最大。
categories: lisp
author: <Wu Li> li.koun@gmail.com
---


问题：给定一个实数序列，寻找一个子序列，其相加之和为最大。
======
方案：Divide and Conquer
======

这里我有两个实现版本，从不同的思路来做Divide， 同时实现上一个采用递归，一个采用循环

*    方法一：如果将序列一分为二，那么中点要么位于这个子序列的右边，要么位于子序列左边，要么刚好在这个子序列上。因此有思路，找到中点左边最大子序列，找到中点右边最大子序列，找到跨越中点的最大子序列，选择其和最大地子序列，即为真正最大的子序列。其中，左右两个序列的子序列的查找是递归的
	
*    方法二：这个最大的子序列要么从位置0开始，要么从1开始，。。。或者从N-1开始。 因此无非是比较以不同下标开始的子序列中的最大序列。


代码一：

	(defun find-max-crossing-subarray (lst low mid high)
	  (let* ((key (nth mid lst)) 
			 (count key) 
			 (sum-left key) 
			 (sum-right key) 
			 (left mid)
			 (right mid))

		(if (/= low mid)
			(loop for i from (- mid 1) downto low do
				  (incf count (nth i lst))
				  (when (> count sum-left)
					(setf left i)
					(incf sum-left (nth i lst)))))


		(setf count key)
		(if (/= mid high) 
			(loop for j from (+ mid 1) to high do
				  (incf count (nth j lst))
				  (when (> count sum-right)
					(setf right j)
					(incf sum-right (nth j lst)))))

											;duplicated count of A[mid]
		(list left right (- (+ sum-left sum-right) key))))


	(defun find-maximum-subarray (lst &optional low high)
	  (if (eq low  nil) (setf low 0))
	  (if (eq high nil) (setf high (- (length lst) 1)))

	  (if (= low high) (return-from find-maximum-subarray (list low low (nth low lst))))


	  (let* ((mid (floor (+ low high) 2))
			 (seq nil)
			 (left-seq  (find-maximum-subarray lst low mid))
			 (right-seq (find-maximum-subarray lst (1+ mid) high))
			 (mid-seq   (find-max-crossing-subarray lst low mid high))
			 (v-l (third left-seq))
			 (v-m (third mid-seq))
			 (v-r (third right-seq)))

		(if (> v-l v-r)
			(if (> v-l v-m)
				(setf seq left-seq)
			  (setf seq mid-seq))
		  (if (> v-r v-m)
			  (setf seq right-seq)
			(setf seq mid-seq)))
		seq))




代码二：


	(defun find-maximum-subarray-2 (lst)

	  (let (sum old-sum count seq index low high)
		(loop for i from 0 below (length lst) do
			  (setf count 0)
			  (loop for j from i below (length lst) do
					(incf count (nth j lst))
					(when (or (not sum) (> count sum))
					  (setf index j)
					  (setf sum count)))


			  (when (or (not old-sum) (> sum old-sum))
				(setf low i)
				(setf high index)
				(setf old-sum sum)))
		(list low high old-sum)))

代码二的实现显著地比代码一地实现短小。
