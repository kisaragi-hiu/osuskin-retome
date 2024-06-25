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

(require json
         racket/future
         "helper.rkt"
         "post-process.rkt")

(define current-project-directory (make-parameter (build-path (current-directory))))
(define current-revision (make-parameter "dev"))
(define current-skinname (make-parameter "Retome"))
(define current-skip-render (make-parameter #f))
(define modules empty)
(define cache-directory (build-path (current-project-directory) ".cache"))

(define (main)
  (parse-arguments)
  (unless (current-skip-render)
    (unless (directory-exists? cache-directory)
      (make-directory cache-directory))
    ;; just clean up cache for now
    (map delete-directory/files
         (directory-list cache-directory #:build? #t))
    ;; this should be run here, after modules has already been set
    (for/async ((it (let* ((it (directory-list (current-project-directory) #:build? #t))
                           (it (filter directory-exists? it)) ; only directories
                           (it (filter (λ (%1) (file-exists? (build-path %1 "render"))) it)) ; if dir/render is a file
                           (it (filter (λ (%1)
                                         (if (member 'execute
                                                     (file-or-directory-permissions
                                                      (build-path %1 "render")))
                                             #t
                                             (begin
                                               (displayln (string-append "warning: "
                                                                         (path->string (build-path %1 "render"))
                                                                         " is present but is not executable"))
                                               #f)))
                                       it))
                           (it (filter default-directories-or-specified-module? it)))
                      it)))
      (render-directory it)))
  (post-process cache-directory)
  (optimize-png-in-dir cache-directory)
  (package cache-directory))

(define (parse-arguments)
  (command-line
   #:program "mkosuskin"
   #:once-each
   [("-p" "--project") dir
                       "Specify project directory"
                       (current-project-directory (build-path dir))]
   [("-n" "--name") name
                    "Specify skin name"
                    (current-skinname name)]
   ["--skip-render" "Skip rendering and just do post processing on cache"
                    (current-skip-render #t)]
   [("-r" "--revision") rev
                        "Specify revision string (default is 'dev')"
                        (current-revision rev)]
   #:multi
   [("-m" "--module") mod
                      "Specify extra modules to render"
                      (set! modules (append modules (list mod)))]))

(define (default-directories-or-specified-module? path)
  (define (parse-mods path)
    (let* ((it (string-split (path->string path) "%"))
           ;; handle a%ja.blend
           (it (map (λ (x) (string-split x ".")) it))
           ;; first element is path up to first "%". drop it
           (it (rest it))
           ;; drop the extension after string-split
           (it (map first it)))
      it))
  (cond
    ; if path doesn't specify module like path%modname, it should be rendered
    [(not (path-contains? (path-basename path) "%")) #t]
    ; if path does, parse the modules and compare with the 'modules' list
    [(share-some-elements? (parse-mods path)
                           modules) #t]
    [else #f]))

; rendered-files is a file with one json list in it

(define/contract (move-file-to-cache file)
  (-> path? void?)
  (rename-file-or-directory file (build-path cache-directory (path-basename file)) #t))
(define/contract (copy-file-to-cache file)
  (-> path? void?)
  (copy-file file (build-path cache-directory (path-basename file)) #t))

(define (file->jsexpr file)
  (string->jsexpr (file->string file)))

(define (render-directory dir)
  (define render (build-path dir "render"))
  (define to-move (build-path dir "to-move"))
  (define to-copy (build-path dir "to-copy"))
  (unless (member 'execute
                  (file-or-directory-permissions render))
    (error 'render-directory (string-append (path->string (build-path dir "render")) " is not executable")))
  (system* render) ; run the render
  (when (file-exists? to-move)
    (map move-file-to-cache
         (map (λ (path) (build-path dir path)) ; complete the paths in to-move
              (file->jsexpr to-move))))
  (when (file-exists? to-copy)
    (map copy-file-to-cache
         (map (λ (path) (build-path dir path))
              (file->jsexpr to-copy)))))

; post-process : path? -> void?
(define (post-process dir)
  (define (files dir)
    (map path->complete-path (sequence->list (in-directory dir file-exists?))))

  (map resize-@ (files dir))
  (map resize-resizeto (files dir))
  (map crop (files dir))
  (map trim (files dir)))

(define (package dir)
  (define outfile (build-path (current-project-directory)
                              "_out"
                              (string-append (current-skinname) " " (current-revision) ".zip")))
  (run-command "7z" "a"
               (path->string outfile)
               (map path->string (directory-list cache-directory #:build? #t)))
  (rename-file-or-directory outfile
                            (path-replace-extension outfile ".osk")
                            ;; overwrite existing file?
                            #t))

(define (optimize-png-in-dir dir)
  (displayln "optimizing png")
  (run-command "pngquant" "--skip-if-larger" "--ext" ".png" "--force"
               (let* ((it (directory-list dir))
                      (it (filter (λ (%1) (path-has-extension? %1 ".png")) it))
                      (it (map (λ (%1) (build-path dir %1)) it))
                      (it (map path->string it)))
                 it)))

(unless (= 0 (vector-length (current-command-line-arguments)))
  (main))
