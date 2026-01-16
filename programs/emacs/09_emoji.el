;; Configure emoji display width for terminal Emacs
;; This fixes rendering issues where emojis cause line number spacing problems,
;; duplicate lines, and other display glitches.

;; Set emoji characters to display width 2 (double-width)
;; This covers the main emoji ranges in Unicode
(defun my/setup-emoji-display-width ()
  "Configure display width for emoji characters to prevent rendering glitches."
  (let ((emoji-ranges
         '((#x1F300 . #x1F5FF)  ; Miscellaneous Symbols and Pictographs
           (#x1F600 . #x1F64F)  ; Emoticons
           (#x1F680 . #x1F6FF)  ; Transport and Map Symbols
           (#x1F700 . #x1F77F)  ; Alchemical Symbols
           (#x1F780 . #x1F7FF)  ; Geometric Shapes Extended
           (#x1F800 . #x1F8FF)  ; Supplemental Arrows-C
           (#x1F900 . #x1F9FF)  ; Supplemental Symbols and Pictographs
           (#x1FA00 . #x1FA6F)  ; Chess Symbols
           (#x1FA70 . #x1FAFF)  ; Symbols and Pictographs Extended-A
           (#x2600 . #x26FF)    ; Miscellaneous Symbols
           (#x2700 . #x27BF)    ; Dingbats
           (#xFE00 . #xFE0F)    ; Variation Selectors
           (#x1F1E6 . #x1F1FF)  ; Regional Indicator Symbols (flags)
           )))
    (dolist (range emoji-ranges)
      (set-char-table-range char-width-table range 2))))

;; Apply the configuration
(my/setup-emoji-display-width)

;; Additionally, for terminal mode, ensure we use the correct width calculation
(when (not (display-graphic-p))
  ;; Force redisplay to use the updated width table
  (setq redisplay-dont-pause t))
