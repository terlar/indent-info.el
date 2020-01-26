;;; indent-info.el --- Show indentation information in status bar -*- lexical-binding: t -*-

;; Copyright (C) 2018 Terje Larsen
;; All rights reserved.

;; Author: Terje Larsen <terlar@gmail.com>
;; URL: https://github.com/terlar/indent-info.el
;; Keywords: convenience, tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; indent-info is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; indent-info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `indent-info' is a small Emacs minor mode that provides information
;; about currently configured indentation mode as well as tab-width in the
;; status bar.

;;; Code:

(eval-when-compile
  (defvar evil-shift-width))

(autoload 'tabify "tabify" nil t)
(autoload 'untabify "tabify" nil t)

(defgroup indent-info nil
  "Display indentation information in mode line."
  :group 'modeline)

(defcustom indent-info-insert-target 'mode-line-position
  "Target list for insertion of the indentation information."
  :type '(choice
          (const :tag "Lighter" nil)
          (variable :tag "Target variable"))
  :group 'indent-info)

(defcustom indent-info-insert-position 'before
  "Position for insertion of indentation information.
Choices are `before', `after'."
  :type '(choice (const :tag "Before insert target" before)
                 (const :tag "After insert target" after))
  :group 'indent-info)

(defcustom indent-info-prefix " "
  "Text to display before the indentation info in the mode line."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-suffix " "
  "Text to display after the indentation info in the mode line."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-tab-format "Tab Size: %s"
  "Tab indentation format."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-space-format "Spaces: %s"
  "Space indentation format."
  :type 'string
  :group 'indent-info)

(defcustom indent-info-use-symbols nil
  "Indicates whether to use symbols for the `tab-width' number or not."
  :type '(choice (boolean :tag "Symbols"))
  :group 'indent-info)

(defcustom indent-info-tab-width-min 2
  "Min `tab-width' for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-tab-width-max 8
  "Max `tab-width' for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-tab-width-step 2
  "Step to use for `tab-width' cycling."
  :type 'integer
  :group 'indent-info)

(defcustom indent-info-number-symbol-alist
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
  "List of `tab-width' number mappings.
Each element is a list of the form (NUMBER . SYMBOL)."
  :type '(alist :key-type (integer :tag "Number")
                :value-type (string :tag "Symbol"))
  :group 'indent-info)

(defcustom indent-info-sync-to-editorconfig nil
  "Sync configuration to editorconfig."
  :type 'boolean
  :group 'indent-info)

(defcustom indent-info-sync-from-editorconfig nil
  "Sync configuration from editorconfig."
  :type 'boolean
  :group 'indent-info)

(defvar indent-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-~") 'indent-info-toggle-indent-mode)
    (define-key map (kbd "C-M->") 'indent-info-cycle-tab-width-increase)
    (define-key map (kbd "C-M-<") 'indent-info-cycle-tab-width-decrease)
    map)
  "The keymap for when indentation information mode is active.")

(easy-menu-define indent-info-menu indent-info-mode-map "Indent Info"
  '("Indent Info"
    [ "Convert Indentation to Spaces" indent-info-convert-to-spaces t ]
    [ "Convert Indentation to Tabs"   indent-info-convert-to-tabs t ]))

(defvar indent-info--mode-line-format '(:eval (indent-info--mode-line-format)))

(put 'indent-info--mode-line-format 'risky-local-variable t)

(defun indent-info--mode-line-format ()
  "The mode line with menu and content."
  `(,indent-info-prefix
    (:propertize (:eval (indent-info--mode-line-text))
                 mouse-face mode-line-highlight
                 help-echo
                 ,(concat "mouse-1: Display minor mode menu\n"
                          "mouse-3: Toggle tabs/spaces\n"
                          "mouse-4: Increase tab-width\n"
                          "mouse-5: Decrease tab-width")
                 keymap
                 ,(let ((map (make-sparse-keymap)))
                    (define-key map [mode-line down-mouse-1]
                      indent-info-menu)
                    (define-key map [mode-line mouse-3]
                      (lambda () (interactive)
                        (indent-info-toggle-indent-mode)))
                    (define-key map [mode-line mouse-4]
                      (lambda () (interactive)
                        (indent-info-cycle-tab-width-increase)))
                    (define-key map [mode-line mouse-5]
                      (lambda () (interactive)
                        (indent-info-cycle-tab-width-decrease)))
                    map))
    ,indent-info-suffix))

(defun indent-info--mode-line-text ()
  "The indentation information text."
  (let ((fmt (if indent-tabs-mode
                 indent-info-tab-format
               indent-info-space-format)))
    (if indent-info-use-symbols
        (format fmt (cdr (assoc tab-width indent-info-number-symbol-alist)))
      (format fmt (int-to-string tab-width)))))

(defun indent-info-mode-enable ()
  "Enable indentation information in the current buffer."
  (unless (minibufferp)
    (indent-info-mode 1)))

(defun indent-info--sync-to-editorconfig ()
  "Sync `tab-width' and `indent-tabs-mode' settings to editorconfig."
  (when (and indent-info-sync-to-editorconfig (fboundp 'editorconfig-set-indentation))
    (let ((style (if indent-tabs-mode "tab" "space")))
      (editorconfig-set-indentation style (int-to-string tab-width)))))

(defun indent-info--sync-from-editorconfig (props)
  "Sync `tab-width' and `indent-tabs-mode' settings from editorconfig.
These settings arrive as a hash within PROPS."
  (let ((use-tabs (pcase (gethash 'indent_style props)
                    ("tab" t)
                    ("space" nil)
                    (_ indent-tabs-mode)))
        (width-str (or (gethash 'indent_size props)
                       (gethash 'tab_width props)
                       (number-to-string tab-width))))
    (setq indent-tabs-mode use-tabs
          tab-width (string-to-number width-str))))

(defun indent-info--set-indentation-width (width)
  "Set `tab-width' and other width related variables to WIDTH."
  (setq tab-width width)
  (when (featurep 'evil)
    (setq-local evil-shift-width width))
  (indent-info--sync-to-editorconfig))

;;;###autoload
(defun indent-info-toggle-indent-mode ()
  "Toggle indentation modes between tabs and spaces."
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (indent-info--sync-to-editorconfig)
  (let ((mode (if indent-tabs-mode "tabs" "spaces")))
    (message "Set indentation mode to %s." mode)))

;;;###autoload
(defun indent-info-cycle-tab-width-increase ()
  "Cycle `tab-width' increasing with `indent-info-tab-width-step'.
When reaching `indent-info-tab-width-max' it won't do anything."
  (interactive)
  (let ((width (+ tab-width indent-info-tab-width-step)))
    (when (<= width indent-info-tab-width-max)
      (indent-info--set-indentation-width width)
      (message "Set tab-width to %d." width))))

;;;###autoload
(defun indent-info-cycle-tab-width-decrease ()
  "Cycle `tab-width' decreasing with `indent-info-tab-width-step'.
When reaching `indent-info-tab-width-min' it won't do anything."
  (interactive)
  (let ((width (- tab-width indent-info-tab-width-step)))
    (when (>= width indent-info-tab-width-min)
      (indent-info--set-indentation-width width)
      (message "Set tab-width to %d." width))))

;;;###autoload
(defun indent-info-convert-to-spaces ()
  "Convert indentation to spaces and switch `indent-tabs-mode' to nil."
  (interactive)
  (let ((tabify-regexp "^\t* [ \t]+"))
    (null tabify-regexp) ;; special variable used by tabify
    (untabify (point-min) (point-max)))
  (setq indent-tabs-mode nil))

;;;###autoload
(defun indent-info-convert-to-tabs ()
  "Convert indentation to tabs and switch `indent-tabs-mode' to t."
  (interactive)
  (let ((tabify-regexp "^\t* [ \t]+"))
    (null tabify-regexp) ;; special variable used by tabify
    (tabify (point-min) (point-max)))
  (setq indent-tabs-mode t))

(defun indent-info--add-to-insert-target ()
  "Add variable `indent-info--mode-line-format' to `indent-info-insert-target'."
  (add-to-list indent-info-insert-target
               '(indent-info-mode indent-info--mode-line-format)
               (eq indent-info-insert-position 'after)))

(defun indent-info--remove-from-insert-target ()
  "Remove variable `indent-info--mode-line-format' from `indent-info-insert-target'."
  (set indent-info-insert-target
       (assq-delete-all 'indent-info-mode (symbol-value indent-info-insert-target))))

;;;###autoload
(define-minor-mode indent-info-mode
  "Toggle indent-info mode
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, information about the currently configured `indent-tabs-mode' and
`tab-width' is displayed in the mode line."
  :group 'indent-info
  :global nil
  :lighter (:eval (unless indent-info-insert-target (indent-info--mode-line-format)))
  :keymap indent-info-mode-map
  (cond
   ;; Turning the mode ON
   (indent-info-mode
    (when indent-info-insert-target
      (indent-info--add-to-insert-target))
    (when indent-info-sync-from-editorconfig
      (add-hook 'editorconfig-after-apply-functions #'indent-info--sync-from-editorconfig)))
   ;; Turning the mode OFF.
   (t
    (when indent-info-insert-target
      (indent-info--remove-from-insert-target))
    (remove-hook 'editorconfig-after-apply-functions #'indent-info--sync-from-editorconfig))))

;;;###autoload
(define-global-minor-mode global-indent-info-mode
  indent-info-mode indent-info-mode-enable
  :group 'indent-info
  :require 'indent-info)

(provide 'indent-info)

;;; indent-info.el ends here
