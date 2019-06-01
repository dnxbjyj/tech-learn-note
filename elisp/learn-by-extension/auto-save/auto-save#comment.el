;;; auto-save.el --- Auto save files when idle

;; Filename: auto-save.el
;; Description: Auto save files when idle
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2013 ~ 2014, Andy Stewart, all rights reserved.
;; Created: 2013-12-31 00:32:00
;; Version: 0.4
;; Last-Updated: 2018-10-05 08:00:39
;;           By: Andy Stewart
;; URL:
;; Keywords: autosave
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
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

;;; Commentary:
;;
;; Auto save file when emacs idle
;;

;;; Installation:
;;
;; Put auto-save.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-save)
;; (auto-save-enable)
;;
;; Set `auto-save-silent' with non-nil if want emacs save files slient:
;; (setq auto-save-silent t)
;;
;; No need more.

;;; Change log:
;;
;; 2018/10/05
;;      * Update font lock before save file.
;;
;; 2018/08/14
;;      *Fixed typo, change `auto-save-slient' to `auto-save-silent'.
;;
;; 2018/07/06
;;      * Add new option `auto-save-delete-trailing-whitespace'.
;;
;; 2014/01/04
;;      * Add new function `auto-save-enable' to enable auto-save in user config file.
;;      * Add options: `auto-save-idle' and `auto-save-silent'.
;;
;; 2008/10/20
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
;;;;---- 不需要依赖任何其他包

;;; Code:

(defgroup auto-save nil
  "Auto save file when emacs idle."
  :group 'auto-save)
;;;;---- defgroup是一个macro，用于定义一个group，本分组的变量都可以由用户进行自定义

(defcustom auto-save-idle 3
  "The idle seconds to auto save file."
  :type 'integer
  :group 'auto-save)
;;;;---- defcustom是一个macro，用于定义一个变量，该变量可以由用户进行自定义，这里的变量auto-save-idle表示自动保存触发的时延为3秒

(defcustom auto-save-silent nil
  "Nothing to dirty minibuffer if this option is non-nil."
  :type 'boolean
  :group 'auto-save)
;;;;----是否安静地自动保存

(defcustom auto-save-delete-trailing-whitespace nil
  "Delete trailing whitespace when save if this option is non-nil.
Note, this option is non-nil, will delete all training whitespace execpet current line,
avoid delete current indent space when you programming."
  :type 'boolean
  :group 'auto-save)
;;;;---- 自动删除空白字符

;; Emacs' default auto-save is stupid to generate #foo# files!
(setq auto-save-default nil)
;;;;----不要使用默认的自动保存功能，太难用了

(defun auto-save-buffers ()
  ;;;;----定义自动保存函数
  (interactive)
  ;;;----这是一个command
  (let ((autosave-buffer-list))
    ;;;;----定义一个局部变量，用于存储待保存的buffer列表
    (ignore-errors
      ;;;;----这是一个macro，用于吃掉错误
      (save-excursion
        ;;;;----这是一个special form，用于保存执行动作前的上下文
        (dolist (buf (buffer-list))
        ;;;;---- dolist, buffer-list，遍历当前的每一个buffer
          (set-buffer buf)
          ;;;;----设置当前的buffer为buf
          (if (and (buffer-file-name) (buffer-modified-p))
	 ;;;;----如果当前的buffer关联了文件，并且内容有修改
              (progn
	   ;;;;---- progn是一个special form，用于执行一系列的命令，并把最后一个命令的结果作为返回值
                (push (buffer-name) autosave-buffer-list)
	   ;;;;---- 把当前的buffer名称push进待自动保存的buffer列表
                (if auto-save-silent
	       ;;;;---- 如果要安静地保存，则不输出任何信息
                    (with-temp-message ""
                      (basic-save-buffer))
	         ;;;;----保存当前buffer到文件
                  (basic-save-buffer))
                )))
        ;; Tell user when auto save files.
        (unless auto-save-silent
          ;;;;----如果auto-save-silent值为nil，即不要求安静地保存，则执行下面的语句，用于提示保存的文件信息
          (cond
           ;; It's stupid tell user if nothing to save.
           ((= (length autosave-buffer-list) 1)
            ;;;;----待保存buffer列表的长度大于等于1才 保存
            (message "# Saved %s" (car autosave-buffer-list)))
	   ;;;;----car用于取列表的第一个元素
           ((> (length autosave-buffer-list) 1)
            (message "# Saved %d files: %s"
                     (length autosave-buffer-list)
                     (mapconcat 'identity autosave-buffer-list ", ")))))
	        ;;;;----用逗号连接buffer名称列表的每一个元素
        ))))

(defun auto-save-delete-trailing-whitespace-except-current-line ()
  (interactive)
  (when auto-save-delete-trailing-whitespace
    (let ((begin (line-beginning-position))
          (end (line-end-position)))
      (save-excursion
        (when (< (point-min) begin)
          (save-restriction
            (narrow-to-region (point-min) (1- begin))
            (delete-trailing-whitespace)))
        (when (> (point-max) end)
          (save-restriction
            (narrow-to-region (1+ end) (point-max))
            (delete-trailing-whitespace)))))))

(defun auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'auto-save-buffers)
  (add-hook 'before-save-hook 'auto-save-delete-trailing-whitespace-except-current-line)
  (add-hook 'before-save-hook 'font-lock-flush)
  )

(provide 'auto-save)

;;; auto-save.el ends here
