#lang racket
;;; Copyright © 2019 Kisaragi Hiu <mail@kisaragi-hiu.com>
;;;
;;; This work is Free.
;;; You can redistribute it and/or modify it under the terms of the Do What The Fuck You Want To Public License, Version 2, as published by Sam Hocevar. A copy of the license is shown below.
;;;
;;;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;                     Version 2, December 2004
;;;
;;;  Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
;;;
;;;  Everyone is permitted to copy and distribute verbatim or modified
;;;  copies of this license document, and changing it is allowed as long
;;;  as the name is changed.
;;;
;;;             DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;;
;;;     0. You just DO WHAT THE FUCK YOU WANT TO.
(require "helper.rkt")

(provide resize-@ resize-@2x resize-resizeto crop trim get-@-size)

; orig, target : path?
; ratio : number?
(define (resize-vips orig target ratio)
  (run-command "vips" "resize"
               (path->string orig)
               (path->string target)
               (number->string (exact->inexact ratio))))

; orig, target : path?
; size : string?
(define (resize-im orig target size)
  (run-command "gm" "convert" "-resize"
               size
               (path->string orig)
               (path->string target)))

;; get-@-size : string -> number
(define (get-@-size string)
  (let* ((it (string-replace string #rx".*@" ""))
         (it (string-replace it #rx"x.*" "")))
    (string->number it)))

(define (resize-@ path)
  (define base-path (path-basename path))
  (cond [(not (regexp-match #px".*@[[:digit:]]+x.*" (path->string base-path))) #f]
        [(not (file-exists? path)) #f]
        [(regexp-match #rx".*@2x.*" (path->string base-path)) (resize-@2x path)]
        [else
          (define target (path-replace path #px"@[[:digit:]]+x" "@2x"))
          (define orig-size (get-@-size (path->string path))) ; parse the n in @_n_x
          (resize-vips path target (/ 2 orig-size))
          (resize-@2x target) ; target is always @2x. resize it to SD here
          (delete-file path) ; we don't want left over @4x's
          ;; return the target path
          target]))

(define (resize-@2x path)
  (define base-path (path-basename path))
  (cond [(not (regexp-match #rx".*@2x.*" (path->string base-path))) #f]
        [(not (file-exists? path)) #f]
        [else
          (define target (path-replace path #rx"@2x" ""))
          (resize-vips path target 0.5)
          ;; return target path
          target]))

(define (resize-resizeto path)
  (define base-path (path-basename path))
  (cond [(not (or (regexp-match #px"resizeto[[:digit:]]+x[[:digit:]]+" (path->string base-path))
                  (regexp-match #px"resizeto[[:digit:]]+%" (path->string base-path))))
         #f]
        [(not (file-exists? path)) #f]
        [else
         (define target (path-replace path #rx"_resizeto.*.png$" ".png"))
         (define size
           (let* ((it (path->string path))
                  (it (string-replace it #rx"^.*resizeto" ""))
                  (it (string-replace it #rx"\\..*" "")))
             it))
         (resize-im path target size)
         (delete-file path)
         ;; return target
         target]))

(define (crop path)
  (define base-path (path-basename path))
  (cond [(not (regexp-match #px"tocrop" (path->string base-path))) #f]
        [(not (file-exists? path)) #f]
        [else
         (define crop-dimention
           (let* ((it (path->string path))
                  (it (regexp-match #px"[[:digit:]]+x[[:digit:]]+\\+[[:digit:]]+\\+[[:digit:]]" it)))
             ;; regexp-match returns a list of matching substrings
             (first it)))
         (define target (path-replace path (string-append "_tocrop" crop-dimention) ""))
         (run-command "convert" "-crop"
                      crop-dimention
                      (path->string path)
                      (path->string target))
         (delete-file path)
         ;; return target
         target]))

(define (trim path)
  (define base-path (path-basename path))
  (cond [(not (regexp-match #px"totrim" (path->string base-path))) #f]
        [(not (file-exists? path)) #f]
        [else
          (define target (path-replace path "totrim" ""))
          (run-command "convert" "-trim" "+repage"
                       (path->string path)
                       (path->string target))
          (delete-file path)
          ;; return target
          target]))
