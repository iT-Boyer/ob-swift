;;; ob-swift.el --- org-babel functions for swift evaluation

;; Copyright (C) 2015 Feng Zhou

;; Author: Feng Zhou <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-swift
;; Keywords: org babel swift
;; Version: 0.0.1
;; Created: 4th Dec 2015
;; Package-Requires: ((org "8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; org-babel functions for swift evaluation
;;

;;; Code:
(require 'ob)

(defgroup ob-swift nil
  "Org Mode blocks for Swift."
  :prefix "ob-swift-"
  :group 'org)

(defvar swift-mode:repl-executable nil)

(defcustom ob-swift-executable swift-mode:repl-executable
  "Swift REPL executable for ob-swift."
  :type 'string
  :safe #'stringp
  :group 'ob-swift)

(defcustom ob-swift-default-session "*swift*"
  "Specify ob-swift session name."
  :type 'string
  :safe #'stringp
  :group 'ob-swift)

(defvar ob-swift-process-output "")

(defvar ob-swift-eoe "ob-swift-eoe")

;; 该函数的作用是根据代码块的参数列表中的 `:session` 参数值，支持在 REPL 中或者直接执行 Swift 代码块中的代码。
;; 该函数接受两个参数，`body` 表示代码块中的代码字符串，`params` 包含了代码块的参数列表和其他信息
(defun org-babel-execute:swift (body params)
  ;; 获取参数列表中 `:session` 参数的值，即 REPL 会话名称
  (let ((session (cdr (assoc :session params))))
    ;; 如果 `:session` 参数的值为 "none"，表示不使用 REPL，直接调用 `ob-swift--eval` 函数执行代码
    (if (string= "none" session)
        (ob-swift--eval body)
      ;; 否则使用 `ob-swift--eval-in-repl` 函数在 REPL 中执行代码
      (ob-swift--eval-in-repl session body))))

(defun ob-swift--eval (body)
  (with-temp-buffer
    (insert body)
    (shell-command-on-region (point-min) (point-max) "swift -" nil 't)
    (buffer-string)))

(defun ob-swift--initiate-session (session)
  (message "初始化 session: %s" session)
  (unless (fboundp 'run-swift)
    (error "`run-swift' not defined, load swift-mode.el"))
  ;; 使用`(save-window-excursion ...)`保存当前窗口环境，并在新的窗口环境中执行以下操作：
  (save-window-excursion
    ;; 定义变量`name`作为会话的名称，默认值为`ob-swift-default-session`
    (let ((name (or session ob-swift-default-session)))
      (message "进程名称： %s" name)
      (unless (and (get-buffer-process name)
                   (process-live-p (get-buffer-process name)))
        ;; (setq swift-mode:repl-buffer name)
        ;; (message "开启新的进程：%s" swift-mode:repl-buffer)
        ;; 调用函数`run-swift`创建并执行一个 Swift REPL 进程
        ;; (swift-mode:run-repl swift-mode:repl-executable t t)
        ;; (call-interactively 'run-swift)
        (with-current-buffer (get-buffer-create name)
        (message "开启新的进程：%s" name)
          (erase-buffer)
          (setq-default coding-system-for-write 'utf-8-unix)
          (setq buffer-file-coding-system 'utf-8-unix)
          (start-process name (current-buffer) "swift" "repl")
          (swift-repl-mode)
          ;; (run-swift swift-mode:repl-executable nil nil)
          ))
      (message "初始化 session 结果 %s" (get-buffer-process name))
      (get-buffer name))))

(defun ob-swift--eval-in-repl (session body)
  (let ((full-body (org-babel-expand-body:generic body params))
        (session (ob-swift--initiate-session session))
        (pt (lambda ()
              (marker-position
               (process-mark (get-buffer-process session))))))
    (message "------ %s" session)
    (org-babel-comint-in-buffer session
      (let ((start (funcall pt)))
        (with-temp-buffer
          (insert full-body)
          (comint-send-region session (point-min) (point-max))
          (comint-send-string session "\n"))
        (while (equal start (funcall pt)) (sleep-for 0.1))
        (save-excursion
          (buffer-substring
           (save-excursion
             (goto-char start)
             (next-line)
             (move-beginning-of-line nil)
             (point-marker))
           (save-excursion
             (goto-char (funcall pt))
             (previous-line)
             (move-end-of-line nil)
             (point-marker))))))))

(provide 'ob-swift)
;;; ob-swift.el ends here

(with-current-buffer (get-buffer-create name)
  (erase-buffer)
  (start-process name (current-buffer) "swift" "repl")
  (swift-repl-mode)
  ;; (run-swift swift-mode:repl-executable nil nil)
  )
在emacs 中 使用上述代码新建 buffer 的每行的结尾都出现字符,怎么解决这个问题？
Welcome to Apple Swift version 5.8 (swiftlang-5.8.0.124.2 clang-1403.0.22.11.100).
Welcome to Apple Swift version 5.8 (swiftlang-5.8.0.124.2 clang-1403.0.22.11.100).
Type :help for assistance.
1> a: Int = 1



这个问题通常是因为在 buffer 中使用了不正确的换行符导致的。可以尝试在代码中添加以下语句来设置换行符类型：

```
(setq buffer-file-coding-system 'utf-8-unix)
```

这将设置使用 Unix 风格的换行符。您也可以根据需要使用其他换行符类型。

设置为 utf-8-unix 之后，每行结尾的字符仍然存在


如果设置换行符类型后每行结尾的字符仍然存在，可以尝试将以下代码添加到你的 Emacs 配置文件（如 ~/.emacs.d/init.el）中：

```
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default coding-system-for-write 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
```

这会增强 Emacs 在处理文件时的一致性，并将缓冲区的默认文件编码设置为 Unix 风格的 UTF-8 编码，从而避免每行结尾的字符问题。

在使用 `get-buffer-create` 函数时，
如果指定的 `name` 已经存在于 Emacs 中，则函数会返回对应的 buffer，否则会创建一个新的 buffer。
在你的代码中，多余的字符是因为 `name` 变量的值不是一个合法的 buffer 名称，导致 `get-buffer-create` 创建了一个带有多余字符的 buffer 名称。
可以尝试修改 `name` 变量的值，确保它是一个合法的 buffer 名称，即只包含字母、数字、下划线和连接线等字符。
例如，可以使用 `(concat "swift-repl-" (number-to-string (random)))` 来生成一个随机的 buffer 名称。
这个问题是因为 `(message "开启新的进程：%s" name)` 中的 `%s` 并没有被成功替换为 `name`。可以尝试使用 `(format "开启新的进程：%s" name)` 替代这行代码来解决这个问题。
