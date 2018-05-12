;; * Rust Hacking Support -*- lexical-binding: t; -*-
;; Authors:
;;	jgd = J. Greg Davidson

;; ** Dependencies

(defvar *ngender-evil-packages*	'(evil)
	"minimal set of evil packages" )

;; ** Evil packages available as of 29 March 2018

;; evil-anzu :: anzu for evil-mode
;; evil-args :: Motions and text objects for delimited arguments in Evil.
;; evil-colemak-ba... :: Basic Colemak key bindings for evil-mode
;; evil-commentary :: Comment stuff out. A port of vim-commentary.
;; evil-embrace :: Evil integration of embrace.el
;; evil-escape :: Escape from anything with a customizable key sequence
;; evil-escape :: Escape from anything with a customizable key sequence
;; evil-iedit-state :: Evil states to interface iedit mode.
;; evil-indent-tex... :: evil textobjects based on indentation
;; evil-leader :: let there be <leader>
;; evil-leader :: let there be <leader>
;; evil-lisp-state :: An evil state to edit Lisp code
;; evil-magit :: evil-based key bindings for magit
;; evil-mark-replace :: replace the thing in marked area
;; evil-matchit :: Vim matchit ported into Emacs (requires EVIL)
;; evil-matchit :: Vim matchit ported to Evil
;; evil-mc :: Multiple cursors for evil-mode
;; evil-multiedit :: multiple cursors for evil-mode
;; evil-nerd-comme... :: Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
;; evil-nerd-comme... :: Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
;; evil-numbers :: increment/decrement numbers like in vim
;; evil-numbers :: increment/decrement numbers like in vim
;; evil-opener :: opening urls as buffers in evil
;; evil-org :: evil keybindings for org-mode
;; evil-paredit :: Paredit support for evil keybindings
;; evil-quickscope :: Highlight unique characters in words for f,F,t,T navigation
;; evil-replace-wi... :: replace chars of a text object with a char
;; evil-rsi :: Use emacs motion keys in evil, inspired by vim-rsi
;; evil-search-hig... :: Persistent highlights after search
;; evil-smartparens :: Evil support for smartparens
;; evil-snipe :: emulate vim-sneak & vim-seek
;; evil-space :: Repeat motion in Evil. Correct the behaviour of what SPC should do.
;; evil-string-inf... :: snake_case -> CamelCase -> etc. for text objects
;; evil-surround :: emulate surround.vim from Vim
;; evil-swap-keys :: intelligently swap keys on text input with evil
;; evil-test-helpers :: unit test helpers for Evil
;; evil-text-objec... :: Python specific evil text objects
;; evil-textobj-an... :: Textobject for the closest user-defined blocks.
;; evil-tutor :: Vimtutor adapted to Evil and wrapped in a major-mode
;; evil-visual-mar... :: Display evil marks on buffer
;; evil-visual-rep... :: search/replace commands for evil visual state, inc. blocks
;; evil-visualstar :: Starts a * or # search from the visual selection

(apply #'ngender-package *ngender-evil-packages*)

(require 'evil)
(evil-mode)

;; ** Provides

(ngender-provide evil)
