;;; indent-info-mode.el --- show indentation information in status bar -*- coding: utf-8 -*-

;; Copyright (C) 2017 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; Maintainer: Terje Larsen <terlar@gmail.com>
;; Created: 2017-05-28
;; Version: 0.1
;; Keywords: indentation, indent, tabs, spaces, tab-width, mode line

;; This file is NOT part of GNU Emacs.

;; indent-info-mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; indent-info-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `indent-info-mode' is a small Emacs minor mode that provides information
;; about currently configured indentation mode as well as tab-width in the
;; status bar.

;;; Code:

(defgroup indent-info-mode nil
  "Display indent information in mode line."
  :group 'modeline)

(defcustom indent-info-prefix " "
  "Text to display before the indentation info in the mode line."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-suffix " "
  "Text to display after the indentation info in the mode line."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-tab-text "Tab Size: "
  "The text to use for tab indentation."
  :group 'indent-info-mode)

(defcustom indent-info-space-text "Spaces: "
  "The text to use for space indentation."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-use-symbols nil
  "Indicates whether to use symbols for the `indent-tabs-mode' and `tab-width' number or not."
  :group 'indent-info-mode)

(defcustom indent-info-tab-symbol "⇥"
  "The symbol to use for tab indentation when `indent-info-use-symbols' is active."
  :group 'indent-info-mode)

(defcustom indent-info-space-symbol "·"
  "The symbol to use for space indentation when `indent-info-use-symbols' is active."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-number-symbols
  '((0 . "⓪")
    (2 . "②")
    (4 . "④")
    (8 . "⑧"))
  "Alist mapping `tab-width' numbers to the value used in the mode line.
Each element is a list of the form (KEY . VALUE)."
  :group 'indent-info-mode)

(defun indent-info-mode-line ()
  "The modeline with menu and content."
  (let* ((map
          '(keymap
            (mode-line keymap
                       (mouse-1 . toggle-indent-mode-setting)
                       (mouse-3 . toggle-tab-width-setting))))
         (help "Indentation\n\ mouse-1: Toggle tabs/spaces\n\ mouse-3: Toggle tab-width"))
    (concat indent-info-prefix
            (propertize (indent-info-mode-line-text)
                        'help-echo help
                        'local-map map)
            indent-info-suffix)))

(defun indent-info-mode-line-text ()
  "The indentation information text."
  (if indent-info-use-symbols
      (concat
       (cdr (assoc tab-width indent-info-number-symbols))
       (if (eq indent-tabs-mode t)
           indent-info-tab-symbol
         indent-info-space-symbol))
    (concat
     (if (eq indent-tabs-mode t)
         indent-info-tab-text
       indent-info-space-text)
     (int-to-string tab-width))))

(setq mode-line-position (assq-delete-all 'indent-info-mode mode-line-position))
(setq mode-line-position
      (append
       mode-line-position
       '((indent-info-mode (:eval (indent-info-mode-line))))))

 ;;;###autoload
(define-minor-mode indent-info-mode
  "Toggle indent-info mode
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, information about the currently configured `indent-tabs-mode' and
`tab-width' is displayed in the mode line."
  :lighter nil :global nil)

;;;###autoload
(define-globalized-minor-mode global-indent-info-mode indent-info-mode turn-on-indent-info-mode
  :require 'indent-info-mode
  :group 'indent-info-mode)

(defun turn-on-indent-info-mode ()
  "Turn on `indent-info-mode'."
  (indent-info-mode 1))

 ;;;###autoload
(defun toggle-tab-width-setting ()
  "Cycle 'tab-width' between values 2, 4, and 8."
  (interactive)
  (setq tab-width
        (cond ((eq tab-width 8) 2)
              ((eq tab-width 2) 4)
              (t 8)))
  (redraw-display)
  (message "Set tab-width to %d." tab-width))

 ;;;###autoload
(defun toggle-indent-mode-setting ()
  "Toggle indentation modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode
        (if (eq indent-tabs-mode t) nil t))
  (message "Set indentation mode to %s."
           (if (eq indent-tabs-mode t) "tabs" "spaces")))

(provide 'indent-info-mode)

;;; indent-info-mode.el ends here
