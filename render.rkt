#lang racket
(require threading)

(define out ".out")
(define current-skin-template (make-parameter "skin.ini.template"))
(define current-project-root (make-parameter "."))

(define (copy-from-assets file [target-dir "."])
  (set! file (~a file))
  (copy-file (build-path (current-project-root) "assets" file)) target-dir)

(define (generate-skin.ini path search replace)
  (set! search (~a search))
  (set! replace (~a replace))
  (with-output-to-file path
    (lambda ()
      (~> (file->string (current-skin-template))
          (string-replace _ search replace)
          (display _)))))

(define (generate-empty path)
  (set! path (~a path))
  (define ext (path-get-extension path))
  (copy-file (~a "assets/empty" ext) path))

(define (render source-file target-dir)
  (define (get-target-path suffix)
    (~a (build-path target-dir (path-replace-extension source-file suffix))))
  (cond [(string-suffix? source-file ".rendernormal.blend")
         ;; Blender "rendernormal" files need to specify their path on their own
         (system
          (~a "blender -b " source-file " -a"))]
        [(string-suffix? source-file ".rendermarker.blend")
         (system
          (~a "blender -b " source-file " -o " target-dir " -P rendermarker.py"))]
        [(string-suffix? source-file ".kra")
         (system
          (~a "krita --export"
              " --export-filename " (get-target-path ".png")
              " " source-file))]
        [(string-suffix? source-file ".mmpz")
         (system
          (~a "lmms render --format wav --output " (get-target-path ".wav")))]
        [(string-suffix? source-file ".svg")
         (system
          (~a "inkscape -z " source-file " -e " (get-target-path ".png")))]
        [(string-suffix? source-file ".kra")]))

;; Get directory in project
(define (output-dir . relative-paths)
  (apply build-path out relative-paths))

(map (lambda (f) (render f (output-dir)))
     (filter
      (lambda (p) (or (not (string=? (~a p) "numbers_dot.rendernormal.blend"))
                      (not (string=? (~a p) "numbers_jkgothic.rendernormal.blend"))))
      (directory-list)))

(map copy-from-assets
     '(0.Credits.txt
       applause.ogg
       failsound.ogg
       pause-loop.ogg
       spinner-top@2x.png))

(render "numbers_dot.rendernormal.blend" (output-dir "customize/numbers/dot"))
(generate-skin.ini "customize/numbers/dot/skin.ini"
                   "◊hit-overlap◊" 128)

(render "numbers_jkgothic.rendernormal.blend" (output-dir "customize/numbers/jkgothic"))
(generate-skin.ini "customize/numbers/jkgothic/skin.ini"
                   "◊hit-overlap◊" 10)

(map generate-empty
     '(count1.png
       count2.png
       count3.png
       hit300-0.png
       hit300g-0.png
       hit300k-0.png
       inputoverlay-background.png
       ranking-graph.png
       scorebar-bg.png
       sliderendcircle.png
       sliderpoint10.png
       sliderpoint30.png
       sliderscorepoint.png
       spinner-approachcircle.png
       spinner-clear.png
       spinner-glow.png
       spinner-middle.png
       nightcore-kick.wav
       nightcore-clap.wav
       nightcore-hat.wav
       nightcore-finish.wav))
