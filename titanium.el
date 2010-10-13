;;;titanium.el ---  Titanium Minor Mode
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2010 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.0.1
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'titanium)
;; (global-titanium t)
;;
;; If you use default key map, Put the following expression into your ~/.emacs.
;;
;; (titanium-set-default-keymap)

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `titanium'
;;    Titanium minor mode.
;;  `titanium-switch-to-function'
;;    Switch to function.
;;  `titanium-switch-to-file-history'
;;    Switch to file history.
;;  `titanium-open-dir'
;;    Open directory.
;;  `titanium-open-resources-dir'
;;    Open JavaScript directory.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `titanium-root-path-search-limit'
;;    Search limit
;;    default = 5

;;; Change Log
;; 0.0.1: Initial commit

;;; TODO
;;

;;; Code:

;;require
(require 'cl)
(require 'anything)
(require 'historyf)
(require 'easy-mmode)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'titanium-complete
                                '(length titanium-initial-input)))

(defgroup titanium nil
  "Titanium minor mode"
  :group 'convenience
  :prefix "titanium-")

(defcustom titanium-root-path-search-limit 5
  "Search limit"
  :type 'integer
  :group 'titanium)

;;(global-set-key "\C-c\C-v" 'titanium)

(define-minor-mode titanium
  "Titanium minor mode."
  :lighter " Titanium"
  :group 'titanium
  (if titanium
      (progn
        (setq minor-mode-map-alist
              (cons (cons 'titanium titanium-key-map)
                    minor-mode-map-alist))
        (run-hooks 'titanium-hook))
    nil))

(if (fboundp 'define-global-minor-mode)
    (define-global-minor-mode global-titanium
      titanium titanium-maybe
      :group 'titanium))

(defun titanium-maybe ()
  "What buffer `titanium' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (titanium-set-root-path))
      (titanium 1)
    nil))

;; key-map
(defvar titanium-key-map
  (make-sparse-keymap)
  "Keymap for Titanium.")

(defvar titanium-root-path nil
  "Titanium app directory path.")

(defvar titanium-file-history nil
  "Switch file history.")

(defvar titanium-hook nil
  "Hook")

(defun titanium-set-default-keymap ()
  "Set default key-map"
  (setq titanium-key-map
        (let ((map (make-sparse-keymap)))
          (define-key map "\C-cf" 'titanium-switch-to-function)
          (define-key map "\C-cr" 'titanium-open-resources-dir)
          map)))

(defun titanium-get-current-line ()
  "Get current line."
  (thing-at-point 'line))

(defun titanium-set-root-path ()
  "Set app path."
  (titanium-is-root-path))

(defun titanium-is-root-path ()
  "Check app directory name and set regExp."
  (setq titanium-root-path (titanium-find-root-path))
  (if (not titanium-root-path)
      nil
    t))

(defun titanium-find-root-path ()
  "Find app directory"
  (let ((current-dir default-directory))
    (loop with count = 0
          until (file-exists-p (concat current-dir "tiapp.xml"))
          ;; Return nil if outside the value of
          if (= count titanium-root-path-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun titanium-switch-to-function ()
  "Switch to function."
  (interactive)
  (require 'imenu nil t)
  (anything 'anything-c-source-imenu))

(defun titanium-switch-to-file-history ()
  "Switch to file history."
  (interactive)
  (historyf-back '(titanium)))

(defun titanium-open-dir (dir &optional recursive)
  "Open directory."
  (interactive)
  (if (titanium-set-root-path)
      (anything-other-buffer
       (titanium-create-open-dir-anything-sources dir recursive) nil)
    (message "Can't set app path.")))

(defun titanium-create-open-dir-anything-sources (dir &optional recursive)
  "Careate 'Open dir' anything-sources"
  (let (sources)
    (unless (listp dir)
      (setq dir (list dir)))
    (if (titanium-set-root-path)
        (progn
          (loop for d in dir do
                (unless (not (file-directory-p (concat titanium-root-path d)))
                  (push
                   `((name . ,(concat "Open directory: " d))
                     (candidates . ,(titanium-directory-files d recursive))
                     (display-to-real . (lambda (candidate)
                                          (concat ,titanium-root-path ,d candidate)))
                     (type . file))
                   sources)))
          (reverse sources))
      (message "Can't set app path."))))

(defun titanium-directory-files (dir &optional recursive)
  "Get directory files recuresively."
  (let
      ((file-list nil))
    (if (not recursive)
        (directory-files (concat titanium-root-path dir))
      (loop for x in (titanium-get-recuresive-path-list (concat titanium-root-path dir))
            do (progn
                 (string-match (concat titanium-root-path dir "\\(.+\\)") x)
                 (push (match-string 1 x) file-list)))
      file-list)))

(defun titanium-get-recuresive-path-list (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (loop for x
          in file-list
          do (if (file-directory-p x)
                 (setq path-list
                       (append
                        (titanium-get-recuresive-path-list
                         (remove-if
                          (lambda(y) (string-match "\\.$\\|\\.svn" y)) (directory-files x t)))
                        path-list))
               (setq path-list (push x path-list))))
    path-list))

(defun titanium-open-resources-dir ()
  "Open JavaScript directory."
  (interactive)
  (titanium-open-dir "Resources/" t))

;; mode provide
(provide 'titanium)

;;; end
;;; titanium.el ends here