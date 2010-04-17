;;; tehom-2.el --- Emacs lisp extensions for history lists

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <Tehom@localhost>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;This code is to make history lists more generally available.  It is
;;mostly modified from isearch.el

;;tehom-define-history-{edit,motion,update} are macros that define
;;functions that move around in a history list.  See the examples at
;;the end of this file, which mirror isearch.el's history functions.

;;To use the history functions, you must supply several
;;application-specific callbacks:

;;With proper callbacks and local variables, these history functions
;;can cooperate with the form-editing in tehom-1.  See tehom-search.el
;;for an example.

;;;;;;;;;;;;;;;;;;;; The callbacks ;;;;;;;;;;;;;;;;;;;;;;;;

;;get-ring-code: Code that gives your history list.  It can be a
;;function, a defsubst, a variable, or a literal.  EG,
;;tehom-isearch-get-ring.

;;get-sym-code: Code that gives the *symbol* that corresponds to the
;;current *position* in your history list.  It can be a function, a
;;variable, or a literal. EG, tehom-isearch-get-position-sym

;;insert-func: A function taking an object from your history list,
;;that inserts a given object into "the place you edit it in",
;;(whatever that may be in your application) .  Not called if the
;;history list is empty.  EG, tehom-isearch-insert-string

;;set-func: A function taking an object from your history list, that
;;sets "the current thing" (whatever that may be) to that object.  Not
;;called if the history list is empty.  EG, tehom-isearch-set-string

;;update-func: Code to execute if history editing is called at all,
;;regardless of whether the history list is empty.  EG,
;;tehom-isearch-do-update

;;my-ring:  Your history list.  Unlike get-ring-code, it can only be
;;an object.  EG, regexp-search-ring

;;my-ring-max: A number, the maximum number of items allowed in your
;;history list.  EG, regexp-search-ring-max

;;my-compare-func: A function that compares objects of the type that
;;are in your history list.  Used by tehom-define-history-update to
;;determine whether a new object should be stored on the history
;;list.  EG, 'string= or 'equal

;;; Code:


( defun tehom-ring-modify-position  (adjustment my-ring position-sym my-func)
  "

POSITION-SYM must be a symbol.  If calling this function indirectly,
pass it with `, so its value always remains the same symbol."

  (assert (symbolp position-sym))
  (let*
    ((my-length (length my-ring))
      (position (symbol-value position-sym)))
    
    (if (zerop my-length)
      0
      (progn
	(set position-sym
	  (mod (+ (or position 0) adjustment) my-length))
      
	;; A function that takes the new object
	(funcall my-func (nth position my-ring))))))



( defmacro tehom-define-history-editing 
  (base-name doc-substring get-ring-code get-sym-code insert-func )
  "Defines 2 things."

  `( progn
     (defun 
       ,(intern (concat base-name "-advance-edit"))  (n)
       ,(concat "Insert the next element of " doc-substring "." )
       (interactive "p")
       (tehom-ring-modify-position
	 (- n)
	 ,get-ring-code
	 ,get-sym-code
	 ,insert-func))

     (defun 
       ,(intern (concat base-name "-retreat-edit"))  (n)
       ,(concat "Insert the previous element of " doc-substring "." )
       (interactive "p")
       (tehom-ring-modify-position
	 n
	 ,get-ring-code
	 ,get-sym-code
	 ,insert-func))))


( defmacro tehom-define-history-motion 
  (base-name doc-substring get-ring-code get-sym-code set-func update-func )
  "Defines 2 things."

  `( progn
     (defun 
       ,(intern (concat base-name "-advance"))  ()
       ,(concat "Advance to the next " doc-substring " in the ring." )
       (interactive )
       (tehom-ring-modify-position
	 -1
	 ,get-ring-code
	 ,get-sym-code
	 ,set-func)
       ,update-func)

     (defun 
       ,(intern (concat base-name "-retreat"))  ()
       ,(concat "Retreat to the previous " doc-substring " in the ring." )
       (interactive)
       (tehom-ring-modify-position
	 1
	 ,get-ring-code
	 ,get-sym-code
	 ,set-func)
       ,update-func)))


( defmacro tehom-define-history-update 
  (base-name my-ring my-ring-max my-compare-func )
  ""
  
  `( progn
     (defun 
       ,(intern (concat base-name "-update-ring")) 
       (object)
       "Add OBJECT to the beginning of the ring."

       (if 
	 (or 
	   (null ,my-ring)
	   (not (,my-compare-func object (car ,my-ring))))
	 (progn
	   (setq ,my-ring
	     (cons object ,my-ring))
	   (if (> (length ,my-ring) ,my-ring-max)  
	     (setcdr (nthcdr (1- ,my-ring-max) ,my-ring)
	       nil)))))     
     ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;An example from isearch: The commented stuff below mimics the
;;isearch history functionality.

;(tehom-define-history-editing isearch-ring "search history" 
;  (tehom-isearch-get-ring) 
;  (tehom-isearch-get-position-sym)
;  'tehom-isearch-insert-string
;  )

;(tehom-define-history-motion isearch-ring "search string" 
;  (tehom-isearch-get-ring) 
;  (tehom-isearch-get-position-sym)
;  'tehom-isearch-set-string
;  'tehom-isearch-do-update
;  )

;;Not quite the same as isearch, beause isearch splits between 2
;;functions, 1 for regex and one for normal.  This defines only an
;;update function for regexp-search, not vanilla search.
;( tehom-define-ring-update "isearch" regexp-search-ring
;  regexp-search-ring-max string=  )

;( defun tehom-isearch-do-update ()
;  ""
;  (if search-ring-update
;      (progn
;	(isearch-search)
;	(isearch-update))
;    (isearch-edit-string))

;  (isearch-push-state))


;( defun tehom-isearch-insert-string (the-string)
;  ""

;  (erase-buffer)
;  (insert the-string)
;  (goto-char (point-max))) 


;( defun tehom-isearch-set-string (the-string)
;  ""

;  (setq 
;    isearch-string the-string
;    isearch-message (mapconcat 'isearch-text-char-description
;		      isearch-string "")))

;( defsubst tehom-isearch-get-ring ()
;  ""
;  (if isearch-regexp 
;    regexp-search-ring 
;    search-ring))

;( defsubst tehom-isearch-get-position-sym ()
;  ""
;  (if isearch-regexp 
;    'regexp-search-ring-yank-pointer 
;    'search-ring-yank-pointer))

(provide 'tehom-2)

;;; tehom-2.el ends here
