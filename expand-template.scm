(import (srfi srfi-1)
        (srfi srfi-26)
        (ice-9 receive)
        (ice-9 rdelim)
        (ice-9 regex)
        (rnrs io ports)
        (web client)
        (json))

;;; Utilities
(define-syntax-rule (values->list body)
  (receive lst body lst))

(define (lines str)
  (string-split str (char-set #\newline)))

(define (unlines lst)
  (string-join lst "\n"))

(define (clamp lower upper x)
  (min upper (max lower x)))

(define* (port-map proc
                   #:optional
                   (input-port (current-input-port))
                   (output-port (current-output-port)))
  (let ((line (read-line input-port 'concat)))
    (unless (eof-object? line)
      (display (proc line) output-port)
      (port-map proc input-port output-port))))

;;; Markdown file parsing
(define (header? str)
  (let ((matches (string-match " {0,3}(#{1,6}) ([^#]+)" str)))
    (and matches
         (values (match:substring matches 2)
                 (- (match:end matches 1) (match:start matches 1))))))

(define (shift-header str offset)
  (if (header? str)
      (let* ((matches (string-match " {0,3}(#{1,6})" str))
             (level (string-length (match:substring matches 1))))
        (string-replace str
                        (make-string (clamp 0 6 (+ level offset)) #\#)
                        (match:start matches 1)
                        (match:end matches 1)))
      str))

(define (get-section text str)
  (define (%get-section text lst state min-level)
    (if (null? lst)
        '()
        (let ((header-info (values->list (header? (car lst)))))
         (case state
           ((header-not-found)
            (if (and (car header-info)
                     (>= (cadr header-info) min-level)
                     (string=? (car header-info) text))
                (%get-section text (cdr lst) 'header-found (cadr header-info))
                (%get-section text (cdr lst) 'header-not-found min-level)))
           ((header-found)
            (if (and (car header-info) (<= (cadr header-info) min-level))
                '()
                (cons (car lst)
                      (%get-section text (cdr lst) 'header-found min-level))))))))

  (unlines (%get-section text (lines str) 'header-not-found 0)))

;;; GitHub-related procedures
(define (render-markdown str)
  (receive (status body)
      (http-post "https://api.github.com/markdown"
                 #:headers '((user-agent . "Celluloid Website Updater"))
                 #:body (call-with-output-string
                          (cute scm->json `(("text" . ,str)) <>)))
    body))

(define (fetch url)
  (receive (status body)
      (http-get url)
    body))

(define (get-readme)
  (fetch "https://raw.githubusercontent.com/celluloid-player/celluloid/master/README.md"))

(define (get-faq)
  (fetch "https://raw.githubusercontent.com/wiki/celluloid-player/celluloid/FAQ.md"))

;;; Template expansion
(define* (expand-template str #:optional (start 0) (min-level 2))
  (define (ensure-min-level str)
    (let* ((str-lines (lines str))
           (first-header (find (cute header? <>) (lines str)))
           (first-header-level (cadr (values->list (header? first-header)))))
       (unlines (map (cute shift-header <> (- min-level first-header-level))
                     str-lines))))

  (let ((matches (string-match "\\{([^\\ }]+)(\\s+[^\\ }]+)?\\}" str start)))
    (if matches
        (let* ((process-section (compose render-markdown ensure-min-level))
               (page-text (match:substring matches 1))
               (header-text (let ((text (match:substring matches 2)))
                              (and text (string-trim text))))
               (src (cond
                      ((string-ci=? page-text "readme") (get-readme))
                      ((string-ci=? page-text "faq") (get-faq))
                      (else (error "ERROR: Unknown page '~A'" page-text))))
               (section (process-section (if header-text
                                             (get-section header-text src)
                                             src)))
               (new-str (string-replace str
                                        section
                                        (match:start matches 0)
                                        (match:end matches 0)))
               (new-start (+ (match:start matches 0) (string-length section))))
          (expand-template new-str new-start min-level))
        str)))

(port-map expand-template)
