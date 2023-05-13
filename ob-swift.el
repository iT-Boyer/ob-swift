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
;;
(defcustom ob-swift-default-session "*Swift REPL [swift repl]*"
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
    ;; 从buffer 列表中查找
    (setq session (get-match-buffer-name session))
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
;; 在通过字符串匹配 buffer 名称，返回包含 swift repl 字符串的 buffer 名称。
(defun get-match-buffer-name (str)
  "Return the buffer whose name contains 'swift repl'."
  (catch 'buffer-found (dolist (buf (buffer-list))
                         (when (string-match (concat "\\[swift repl[^\\]*" str) (buffer-name buf))
                           (throw 'buffer-found (buffer-name buf)))))
    )

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
        ;; 调用函数`run-swift`创建并执行一个 Swift REPL 进程
        ;; (swift-mode:run-repl swift-mode:repl-executable t t)
        (call-interactively 'run-swift)
        ;; (let ((process (with-current-buffer (get-buffer-create name)
        ;;                  (message "开启新的进程：%s" name)
        ;;                  (erase-buffer)
        ;;                  (swift-repl-mode)
        ;;                  (setq-default coding-system-for-write 'utf-8-unix)
        ;;                  (setq buffer-file-coding-system 'utf-8-unix)
        ;;                  (start-process name name "swift" "repl")))))
        )
      (get-buffer name))))

(defun ob-swift--eval-in-repl (session body)
  (let ((full-body (org-babel-expand-body:generic body params))
        (session (ob-swift--initiate-session session))
        (pt (lambda ()
              (marker-position
               (process-mark (get-buffer-process session)))))
        (raw-output)
        results)
    (setq raw-output
          (org-babel-comint-in-buffer session
            (let ((start (funcall pt)))
              (with-temp-buffer
                (insert full-body)
                (comint-send-region session (point-min) (point-max))
                (comint-send-string session "\n//----")
                (comint-send-string session "\n"))
              (while (equal start (funcall pt)) (sleep-for 0.1))
              (save-excursion
                (buffer-substring
                 (save-excursion
                   (goto-char start)
                   (next-line)
                   (move-beginning-of-line nil)
                   ;; (re-search-forward "^[0-9]+>[\t]*" nil 'move)
                   (point-marker))
                 (save-excursion
                   (goto-char (funcall pt))
                   (previous-line)
                   (move-end-of-line nil)
                   ;; (re-search-backward "^[0-9]+>[\t]*" nil 'move)
                   (point-marker)))))
            ))
    (message "原始数据------++++: %s" raw-output)
    (car (last (split-string raw-output "//----\n")))
    ))


(defun ob-swift--eval-in-repl2 (session body)
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
          (comint-send-string session "\n//----")
          (comint-send-string session "\n"))
        (while (equal start (funcall pt)) (sleep-for 0.1))
        (save-excursion
          (buffer-substring
           (save-excursion
             (goto-char start)
             (next-line)
             (move-beginning-of-line nil)
             ;; (re-search-forward "^[0-9]+>[\t]*" nil 'move)
             (point-marker))
           (save-excursion
             (goto-char (funcall pt))
             (previous-line)
             (move-end-of-line nil)
             ;; (re-search-backward "^[0-9]+>[\t]*" nil 'move)
             (point-marker))))))))

(provide 'ob-swift)
;;; ob-swift.el ends here
