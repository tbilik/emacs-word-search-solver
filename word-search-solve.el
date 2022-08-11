;; require cl-lib because i'm a loser
(require 'cl-lib)

;; main func
(defun word-search-solve () (interactive)
       (let* ((word-search (convert-string-to-2d-array (buffer-string))) ;; generate word search
              (words (cl-loop ;; keep probing user for more words until minibuff input is empty
                      for x = (read-from-minibuffer "What would you like to search? ")
                      until (zerop (length x))
                      collect x))
              (notfound nil))
         ;; try and find words
         (dolist (word words)
           (unless (or (find-word-in-word-search word-search word)
                       (find-word-in-word-search word-search (reverse word)))
             (push word notfound)))
         ;; log results
         (if notfound
             (message "Words not found: %s" notfound)
           (message "All words found!"))
         ;; print the solved puzzle in a temp buffer
         (with-output-to-temp-buffer "Solved Puzzle"
           (set-buffer "Solved Puzzle")
           (nice-print word-search))))

(defun convert-string-to-2d-array (contents)
  "converts string into a vector of strings, all upcase and non-alphabet removed"
  ;; Roger hates lambdas. And regular expressions are wack.
  (cl-map 'vector (lambda (x) (replace-regexp-in-string "\\W" "" (upcase x)))
          (split-string contents "\n"))) ;; split!!

(defun toggle-case-if-same (word-search x y c)
  (when (and (< -1 y (length word-search)) ;; check that x is valid
             (< -1 x (length (aref word-search y)))) ;; check that y is valid
    (let ((word-search-c (aref (aref word-search y) x)))
      (when (equal c (upcase word-search-c)) ;; is equal
        (setf (aref (aref word-search y) x) (downcase c))
        word-search-c)))) ;; make lowercase

;; this will attempt to find a word AT A POINT
(defun find-word-in-table (word-search x y mode word)
  (if word ;; any of word left?
      (let ((c (toggle-case-if-same word-search x y (car word))))
        (when c ;; check that current character is equal
          ;; check next char recursively
          (if (cond ((eq mode 'all) (or (find-word-in-table word-search (1+ x) y 'across (cdr word))
                                        (find-word-in-table word-search x (1+ y) 'down (cdr word))
                                        (find-word-in-table word-search (1+ x) (1+ y) 'diagonal-main (cdr word))
                                        (find-word-in-table word-search (1- x) (1+ y) 'diagonal-op (cdr word))))
                    ((eq mode 'across) (find-word-in-table word-search (1+ x) y 'across (cdr word)))
                    ((eq mode 'down) (find-word-in-table word-search x (1+ y) 'down (cdr word)))
                    ((eq mode 'diagonal-main) (find-word-in-table word-search (1+ x) (1+ y) 'diagonal-main (cdr word)))
                    ((eq mode 'diagonal-op) (find-word-in-table word-search (1- x) (1+ y) 'diagonal-op (cdr word))))
              t
            (progn (setf (aref (aref word-search y) x) c) nil))))
    t))

;; this uses the above function at all points
(defun find-word-in-word-search (word-search word)
  (let ((formatted-word (cl-map 'list 'upcase word)))
    ;; weird loop macro? Nah, it's just special. Thank you cl-loop, very cool.
    (cl-loop
     for y from 0 to (1- (length word-search))
     when (cl-loop
           for x from 0 to (1- (length (aref word-search y)))
           when (find-word-in-table word-search x y 'all formatted-word)
           return t
           finally return nil)
     return t
     finally return nil)))


;; prints things NICELY. Don't know why you would think otherwise.
(defun nice-print (word-search)
  ;; loop macro? Hmmm, let's just do some dotimes
  (dotimes (y (length word-search))
    (dotimes (x (length (aref word-search y)))
      (let ((c (aref (aref word-search y) x)))
        (if (<= 97 c 122)
            (insert (propertize (upcase (char-to-string c)) 'font-lock-face '(:foreground "red")))
          (insert (propertize (upcase (char-to-string c)) 'font-lock-face '(:foreground "white")))))
      (insert " "))
    (insert "\n")))
