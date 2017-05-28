================================================================================
indent-info-mode.el
================================================================================

Show indentation information in status bar. This is a small minor mode for the
mode-line which places itself inside the `mode-line-position` area. It displays
the current indentation mode (Tabs or Spaces) as well as the current configured `tab-width`.

You can configure it either to use the full text or a symbol mode just
displaying representations of the numbers and tab/space character.

Appearance
================================================================================

With spaces:

    .. image:: docs/spaces.png

It is clickable, left click toggles indentation mode, right click toggles
between tab widths 2, 4 and 8:

    .. image:: docs/mouse-actions.png

Tabs with symbol:

    .. image:: docs/tabs-symbol.png

Spaces with symbol:

    .. image:: docs/spaces-symbol.png

Usage
================================================================================

Put it in your load path and use it with the following code::

    (require 'indent-info-mode)
    (global-indent-info-mode +1)

Configuration
================================================================================

It is possible to configure the output to achieve something more to your own
liking.

``indent-info-prefix``
    Text to display before the indentation info in the mode line.

``indent-info-suffix``
    Text to display after the indentation info in the mode line.

``indent-info-tab-text``
    The text to use for tab indentation.

``indent-info-space-text``
    The text to use for space indentation.

``indent-info-use-symbols``
    Indicates whether to use symbols for the `indent-tabs-mode' and `tab-width' number or not.

``indent-info-tab-symbol``
    The symbol to use for tab indentation when `indent-info-use-symbols' is active.

``indent-info-space-symbol``
    The symbol to use for space indentation when `indent-info-use-symbols' is active.

``indent-info-number-symbols``
    Alist mapping ``tab-width`` numbers to the value used in the mode line. A
    list of `(KEY . VALUE)` pairs.

Functions
================================================================================

Two functions are also provided that you can choose to bind to some key.


``toggle-tab-width-setting``
    Cycle 'tab-width' between values 2, 4, and 8.

``toggle-indent-mode-setting``
    Toggle indentation modes between tabs and spaces.
