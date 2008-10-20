;;; gravatar.el --- gravatar fetch/store functions

;; Copyright (C) 2008  Iwata

;; Author: Iwata <iratqq@gmail.com>
;; Keywords: faces, tools, extensions, mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;
;; Usage:
;; (require 'gnus-gravatar)
;;

;;
;; detail of gravatar API is following,
;;  http://en.gravatar.com/site/implement/url
;;

(require 'gnus-art)
(require 'gnus-picon)
(require 'gravatar)

(defalias 'gnus-gravatar-insert-glyph 'gnus-picon-insert-glyph)
(defalias 'gnus-gravatar-create-glyph 'gnus-picon-create-glyph)

(defvar gnus-treat-gravatar-icon
  (if (and (gnus-image-type-available-p 'png)
	   (gnus-image-type-available-p 'jpeg))
      'head nil))

(add-to-list 'gnus-treatment-function-alist
              '(gnus-treat-gravatar-icon gnus-treat-gravatar-icon))

(defcustom gnus-gravatar-style
  (if (boundp 'gnus-picon-style) gnus-picon-style 'inline)
  "*How should gravatar icons be displayed.
If `inline', the textual representation is replaced.  If `right', picons are
added right to the textual representation."
  :type '(choice (const inline)
                 (const right))
  :group 'gnus-gravatar)

(defun gnus-gravatar-split-address (address)
  (car (mail-header-parse-address address)))

(defun gnus-gravatar-transform-field (header category)
  (gnus-with-article-headers
    (let ((field (mail-fetch-field header))
          file image len)
      (when (and field
		 (setq field (gnus-gravatar-split-address (downcase field)))
		 (setq file
		       (let ((size (gravatar-make-query-size gravatar-icon-size)))
			 (gravatar-retrieve
			  (gravatar-make-store-filename field size)
			  (gravatar-make-url field size))
			 (gravatar-make-store-filename field size)))
		 (setq image (cons (gnus-gravatar-create-glyph file) header))
		 (gnus-article-goto-header header))
	(case gnus-gravatar-style
          (right
           (setq len (car (image-size (car image))))
           (when (and len (> len 0))
             (goto-char (point-at-eol))
             (insert (propertize
                      " " 'display
                      (cons 'space
                            (list :align-to (- (window-width) 1 len))))))
           (goto-char (point-at-eol)))
          (inline nil))
	(gnus-gravatar-insert-glyph image category)))))

(defun gnus-treat-gravatar-icon ()
  "Display gravatar icons in the from header.
If icons are already displayed, remove them."
  (interactive)
  (let ((wash-gravatar-p buffer-read-only))
    (gnus-with-article-headers
      (if (and wash-gravatar-p (memq 'gravatar-icon gnus-article-wash-types))
          (gnus-delete-images 'gravatar-icon)
        (gnus-gravatar-transform-field "from" 'gravatar-icon)))))

(provide 'gnus-gravatar)

;;; gnus-gravatar.el ends here
