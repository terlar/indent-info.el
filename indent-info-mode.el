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
  "Display indentation information in mode line."
  :group 'modeline)

(defcustom indent-info-prefix " "
  "Text to display before the indentation info in the mode line."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-suffix " "
  "Text to display after the indentation info in the mode line."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-tab-format "Tab Size: %s"
  "Tab indentation format."
  :group 'indent-info-mode)

(defcustom indent-info-space-format "Spaces: %s"
  "Space indentation format."
  :type 'string
  :group 'indent-info-mode)

(defcustom indent-info-use-symbols nil
  "Indicates whether to use symbols for the `tab-width' number or not."
  :group 'indent-info-mode)

(defcustom indent-info-number-symbols
  '((1  . "➀")
    (2  . "②")
    (3  . "➂")
    (4  . "④")
    (5  . "➄")
    (6  . "➅")
    (7  . "➆")
    (8  . "⑧")
    (9  . "➈")
    (10 . "➉"))
  "Alist mapping `tab-width' numbers to the value used in the mode line.
Each element is a list of the form (KEY . VALUE)."
  :group 'indent-info-mode)

(defcustom indent-tab-width-min 2
  "Min `tab-width' for `tab-width' cycling."
  :group 'indent-info-mode)

(defcustom indent-tab-width-max 8
  "Max `tab-width' for `tab-width' cycling."
  :group 'indent-info-mode)

(defcustom indent-tab-width-step 2
  "Step to use for `tab-width' cycling."
  :group 'indent-info-mode)

(defun indent-info-mode-line ()
  "The modeline with menu and content."
  (let* ((map
          '(keymap
            (mode-line keymap
                       (mouse-1 . toggle-indent-mode-setting)
                       (mouse-4 . cycle-tab-width-increase)
                       (mouse-5 . cycle-tab-width-decrease))))
         (help "Indentation\n\ mouse-1: Toggle tabs/spaces\n\ mouse-4: Increase tab-width\n\ mouse-5: Decrease tab-width"))
    (concat indent-info-prefix
            (propertize (indent-info-mode-line-text)
                        'help-echo help
                        'mouse-face 'mode-line-highlight
                        'local-map map)
            indent-info-suffix)))

(defun indent-info-mode-line-text ()
  "The indentation information text."
  (let ((format (if (eq indent-tabs-mode t)
                    indent-info-tab-format
                  indent-info-space-format)))
    (if indent-info-use-symbols
        (format format (cdr (assoc tab-width indent-info-number-symbols)))
      (format format (int-to-string tab-width)))))

(setq mode-line-position (assq-delete-all 'indent-info-mode mode-line-position))
(setq mode-line-position
      (nconc
       '((indent-info-mode (:eval (indent-info-mode-line))))
       mode-line-position))

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
(defun cycle-tab-width-increase ()
  "Cycle `tab-width' increasing with `indent-tab-width-step'.
When reaching `indent-tab-width-max' it won't do anything."
  (interactive)
  (let ((width (+ tab-width indent-tab-width-step)))
    (when (<= width indent-tab-width-max)
      (setq tab-width width)
      (message "Set tab-width to %d." width)
      (redraw-frame))))

(defun cycle-tab-width-decrease ()
  "Cycle `tab-width' decreasing with `indent-tab-width-step'.
When reaching `indent-tab-width-min' it won't do anything."
  (interactive)
  (let ((width (- tab-width indent-tab-width-step)))
    (when (>= width indent-tab-width-min)
      (setq tab-width width)
      (message "Set tab-width to %d." width)
      (redraw-frame))))

 ;;;###autoload
(defun toggle-indent-mode-setting ()
  "Toggle indentation modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode
        (if (eq indent-tabs-mode t) nil t))
  (let ((mode (if (eq indent-tabs-mode t) "tabs" "spaces")))
    (message "Set indentation mode to %s." mode)
    (redraw-frame)))

(provide 'indent-info-mode)

;;; indent-info-mode.el ends here
