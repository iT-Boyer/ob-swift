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

(defun org-babel-execute:swift (body params)
  (let ((session (cdr (assoc :session params))))
    (if (string= "none" session)
        (ob-swift--eval body)
      (ob-swift--eval-in-repl session body))))

(defun ob-swift--eval (body)
  (with-temp-buffer
    (insert body)
    (shell-command-on-region (point-min) (point-max) "swift -" nil 't)
    (buffer-string)))

(defun ob-swift--initiate-session (session)
  (message "初始化 session %s" session)
  (unless (fboundp 'run-swift)
    (error "`run-swift' not defined, load swift-mode.el"))
  (save-window-excursion
    (let ((name (or session ob-swift-default-session)))
      (message "进程 name %s" name)
      (unless (and (get-buffer-process name)
                   (process-live-p (get-buffer-process name)))
        (message "初始化 session 执行 run-swift")
        (call-interactively 'run-swift))
      (message "初始化 session 结果 %s"  name)
      (get-buffer name))))

(defun ob-swift--eval-in-repl (session body)
  (let ((full-body (org-babel-expand-body:generic body params))
        (session (ob-swift--initiate-session session))
        (pt (lambda ()
              (message "获取session 进程 %s" (get-buffer-process session))
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
