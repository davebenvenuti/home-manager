;; Notes
;;
;;  C-h k to describe a keybinding
;;  C-h f to describe a function
;;  C-h v to describe a variable
;;  C-h <tab> for a help menu
;;
;;  C-x 0 to close the current window
;;  C-x 1 to close all windows except the current one
;;  C-x 2 to split the current window horizontally
;;  C-x 3 to split the current window vertically
;;  C-x { and C-x } to vertically grow and shrink windows
;;  C-u is the universal argument, which can be used to pass numeric arguments
;;
;;    eg:
;;
;;    C-u 4 C-x { will vertically grow the window 4 times
;;


(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-c p") 'git-gutter:previous-hunk)
