---
layout: post
title: Emacs Package Management
description: How emacs manages its packages
categories: stuff
tags: stuff
---


Emacs has a powerful pacakge management system "pacakge.el". Normally you need a few lines of modification of .emacs and restart the Emacs, the packages you wanted will be auto-loaded for you. Of course, like other convinent package management systems such as "apt-get", "yum", "quickload", you need internet access.

Make Emacs Manage Packages
======

*   Configure .emacs file

    In .emacs file, put below lisp forms

        (require 'package)
        (package-initialize)
        (add-to-list 'package-archives
              '("marmalade" . "http://marmalade-repo.org/packages/"))

    These direct emacs to do auto package initialization when startup. The package-archives varaible saves all package repository list. There are some repository maintained by different orgs. Google them for yourself.

*   Restart Emacs

    After restarting Emacs, type 'M-x' and input list-packages, Emacs will open a new buff that displays all available pckages. The package source is what you just configured before.

    In this buffer, you can browse the packages use emacs search shortcut to find wanted packages, you also can use shotcuts to mark/unamrk the package you install/uninstall. Getting the key and command list by typing M-x and input describe mode or get the description here. For impatients, just use 'Up'/'Down' key to navigate to the package and click 'Enter' key to install them.

*   Auto Load Packages When Emacs Starting Up

    Added below Lisp forms into your .emacs file

        (defvar  package-list
           '(djvu
             oauth2)
           "List of package need to be installed")
        #Check if the package is there, if not, downloading/installing/loading
        (dolist (pack package-list)
            (when (not (package-installed-p pack))
              (package-refresh-contents) 
              (package-install pack)))
			  
    Functions package-installed-p, pacakge-refresh-contents and pacakge-install are is offered by package.el. At first, call package-installed-p to check if one package is missed, if so, call package-refresh-contents to get the latest packages info and make them ready for downloading, finally, call package-install to installed missing package. All packages in package-list variable will be saved into ~/.emacs.d directory and loaded automatically.

*   Note

    Some packages in these repository is too old and cannot work properly. For instance, package 'slime' is not working for now. You have to download slime manually and manage them through the instuction of slime docs.
