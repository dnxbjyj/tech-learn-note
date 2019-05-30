;;; watch-other-window.el --- Scroll other window and keep current window's position.
;;;;---- 插件文件标题，一般是xxx.el，后面跟着插件的一句话描述

;; Filename: watch-other-window.el
;;;;---- 插件文件名

;; Description: Watch other window and keep focus on current window.
;;;;---- 插件详细描述

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;;;---- 插件作者，姓名及邮箱

;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;;;;---- 插件维护者，姓名及邮箱

;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;;;;---- 版权声明

;; Created: 2018-09-12 15:44:24
;;;;---- 创建时间

;; Version: 0.1
;;;;---- 插件版本

;; Last-Updated: 2018-09-12 15:44:24
;;;;---- 最近一次更新时间

;;           By: Andy Stewart
;;;;---- 更新人

;; URL: http://www.emacswiki.org/emacs/download/watch-other-window.el
;;;;---- 插件存放URL地址

;; Keywords:
;;;;---- 插件关键词

;; Compatibility: GNU Emacs 27.0.50
;;;;---- 兼容emacs版本

;;
;; Features that might be required by this library:
;;;;---- 插件依赖列表

;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;;;---- GNU版权声明

;;; Commentary:
;;;;---- 注释

;;
;; Scroll other window and keep current window's position.
;;;;---- 功能描述

;;
;; Emacs' `scroll-other-window' is very nice on most situation.
;; But when you view same buffer with two window,
;; you will lost position of current window
;; when you do `scroll-other-window' with same buffer.
;;
;; So `watch-other-window' will fix this problem.
;;;;---- 功能详细描述
;;

;;; Installation:
;;
;; Put watch-other-window.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'watch-other-window)
;;
;; Binding key with below commands:
;;
;; `watch-other-window-up'
;; `watch-other-window-down'
;; `watch-other-window-up-line'
;; `watch-other-window-down-line'
;;;;---- 安装及使用方法
;;

;;; Customize:
;;;;---- 自定义
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET watch-other-window RET
;;

;;; Change log:
;;
;; 2018/09/12
;;      * First released.
;;;;---- 更新日志
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;;;---- 将要做的
;;
;;
;;

;;; Require
;;;;---- 依赖


;;; Code:
;;;;---- 插件代码

(defun watch-other-window-up ()
  (interactive)
  (watch-other-window-internal "up"))
;;;;---- 这里做的比较好，因为要在另一个窗口向上、向下移动一行和一屏，一共4种操作，但是操作的本质是相同的，所以封装了一个核心函数watch-other-window-internal，这个函数接收2个参数，其中第一个参数表示移动的方向，为必选参数；第二个参数表示移动的行数，为非必须参数，如果不传默认表示一屏

(defun watch-other-window-down ()
  (interactive)
  (watch-other-window-internal "down"))

(defun watch-other-window-up-line ()
  (interactive)
  (watch-other-window-internal "up" 1))

(defun watch-other-window-down-line ()
  (interactive)
  (watch-other-window-internal "down" 1))

(defun watch-other-window-internal (direction &optional line)
  ;;;;---- &optional后面的参数是可选参数 【12.15 define function】
  (save-excursion
  ;;;;---- save-excursion是一个special form，用于保存当前的各种上下文状态 【9.2.7 special form】【29.3 Excursions】
    ;; Switch to other window.
    (other-window 1)
    ;;;;---- 切换到另一个window 
    ;; Do scroll operation.
    (ignore-errors
    ;;;;---- 这是一个macro，用于忽略可能发生的异常，如果发生异常，直接返回nil
      (if (string-equal direction "up")
      ;;;;---- 判断两个字符串的字面值是否相等，而忽略属性等，这里判断字符串是否等于"up"
          (if line
          ;;;;---- 如果指定了行数，那么就往上滚动line行，否则往上滚动一屏
              (scroll-up line)
            (scroll-up))
        (if line
        ;;;;---- 否则向下滚动
            (scroll-down line)
          (scroll-down))))
    ;; Switch back to current window.
    (other-window -1)
    ;;;;---- 返回到现在的window
    ))

(provide 'watch-other-window)
;;;;---- 提供当前模块

;;; watch-other-window.el ends here
