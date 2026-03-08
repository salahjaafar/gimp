; ================================================================
; waifu2x-gimp.scm
; Script GIMP - Super-résolution style Waifu2x
; Auteur    : Salah JAAFAR 
; Copyright : Salah JAAFAR 
; Version   : 1.0
;
; Description :
;   Simule le comportement de waifu2x en combinant :
;   - Agrandissement par interpolation bicubique/sinc
;   - Réduction de bruit adaptatif (gaussian + median)
;   - Amélioration de netteté (unsharp mask multi-passes)
;   - Renforcement des contours (edge enhance)
;   - Correction gamma et niveaux
;   - Modes : Anime, Photo, Manga (N&B), Réduction bruit seul
;   - Facteurs : x1, x1.5, x2, x3, x4
;   - Batch dossier complet
;
; Prérequis : GIMP 2.10+ (aucun plugin externe requis)
; ================================================================


; ================================================================
; UTILITAIRES
; ================================================================

(define (filename-without-ext path)
  (let* (
    (len     (string-length path))
    (dot-pos (let loop ((i (- len 1)))
               (cond ((< i 0) len)
                     ((char=? (string-ref path i) #\.) i)
                     (else (loop (- i 1))))))
  )
    (substring path 0 dot-pos)
  )
)

(define (export-png image drawable path)
  (file-png-save RUN-NONINTERACTIVE image drawable path path 0 9 1 1 1 1 1)
)

(define (export-jpeg image drawable path quality)
  (file-jpeg-save RUN-NONINTERACTIVE image drawable path path
    (/ quality 100.0) 0 0 0 "" 0 1 0 2)
)

; Clamp une valeur entre min et max
(define (clamp val vmin vmax)
  (max vmin (min vmax val))
)


; ================================================================
; ÉTAPE 1 : RÉDUCTION DE BRUIT
; ================================================================

; Réduction bruit légère (photos, bruit faible)
(define (denoise-light drawable)
  (plug-in-gauss RUN-NONINTERACTIVE
    (car (gimp-item-get-image drawable)) drawable
    3 3 0)                          ; rayon 3x3, méthode IIR
)

; Réduction bruit médiane (anime, artefacts JPEG)
(define (denoise-median image drawable radius)
  (plug-in-median-cut RUN-NONINTERACTIVE image drawable radius radius)
)

; Réduction bruit forte (manga scan, bruit élevé)
(define (denoise-strong drawable)
  (let* ((image (car (gimp-item-get-image drawable))))
    ; Passe 1 : gaussian léger
    (plug-in-gauss RUN-NONINTERACTIVE image drawable 2 2 0)
    ; Passe 2 : sélectif (préserve les bords)
    (plug-in-sel-gauss RUN-NONINTERACTIVE image drawable 7 50)
  )
)

; Réduction bruit adaptatif (préserve les bords via enhance)
(define (denoise-adaptive image drawable noise-level)
  (cond
    ((= noise-level 0) #t)   ; aucun bruit : rien à faire
    ((= noise-level 1)       ; bruit faible
     (plug-in-gauss RUN-NONINTERACTIVE image drawable 2 2 0))
    ((= noise-level 2)       ; bruit moyen
     (plug-in-sel-gauss RUN-NONINTERACTIVE image drawable 5 40))
    ((= noise-level 3)       ; bruit fort
     (plug-in-sel-gauss RUN-NONINTERACTIVE image drawable 9 30)
     (plug-in-gauss    RUN-NONINTERACTIVE image drawable 2 2 0))
  )
)


; ================================================================
; ÉTAPE 2 : AGRANDISSEMENT HAUTE QUALITÉ
; ================================================================

; Upscale par interpolation — méthode configurée globalement
; INTERPOLATION-CUBIC = 2  (bicubique — meilleur pour photos)
; INTERPOLATION-NOHALO = 4 (nohalo  — meilleur pour anime/netteté)
; INTERPOLATION-LOHALO = 3 (lohalo  — compromis)
(define (upscale-image image new-width new-height interp-method)
  (gimp-image-scale-full image new-width new-height interp-method)
)

; Calcule les dimensions après scale
(define (calc-new-dims image scale-factor)
  (let* (
    (w (car (gimp-image-width image)))
    (h (car (gimp-image-height image)))
  )
    (list
      (inexact->exact (round (* w scale-factor)))
      (inexact->exact (round (* h scale-factor)))
    )
  )
)


; ================================================================
; ÉTAPE 3 : RENFORCEMENT DES CONTOURS (EDGE ENHANCEMENT)
; ================================================================

; Netteté légère (photos)
(define (sharpen-light image drawable)
  (plug-in-unsharp-mask RUN-NONINTERACTIVE image drawable
    0.5    ; rayon
    0.3    ; amount
    0      ; threshold
  )
)

; Netteté moyenne (anime)
(define (sharpen-medium image drawable)
  (plug-in-unsharp-mask RUN-NONINTERACTIVE image drawable
    0.8    ; rayon
    0.6    ; amount
    1      ; threshold
  )
)

; Netteté forte (manga, lignes nettes)
(define (sharpen-strong image drawable)
  ; Passe 1 : netteté globale
  (plug-in-unsharp-mask RUN-NONINTERACTIVE image drawable
    1.0 0.8 0)
  ; Passe 2 : affinage fin
  (plug-in-unsharp-mask RUN-NONINTERACTIVE image drawable
    0.4 0.4 0)
)

; Enhancement de bords style waifu2x (calque bords + fusion)
(define (edge-enhance-waifu image drawable strength)
  (let* (
    ; Dupliquer le calque pour créer un masque de bords
    (edge-layer (car (gimp-layer-copy drawable FALSE)))
    (opacity    (inexact->exact (round (* strength 100))))
  )
    (gimp-image-insert-layer image edge-layer 0 -1)
    (gimp-image-set-active-layer image edge-layer)

    ; Détecter les bords sur le calque dupliqué
    (plug-in-edge RUN-NONINTERACTIVE image edge-layer 1 0 0)

    ; Convertir en masque de luminosité
    (gimp-desaturate-full edge-layer DESATURATE-LUMINOSITY)
    (gimp-curves-spline edge-layer HISTOGRAM-VALUE 10
      (list->vector '(0 0  64 20  128 180  192 240  255 255)))

    ; Fusionner en mode Screen pour renforcer les contours
    (gimp-layer-set-mode edge-layer LAYER-MODE-SCREEN)
    (gimp-layer-set-opacity edge-layer opacity)
    (gimp-image-flatten image)

    ; Retourner le calque aplati
    (car (gimp-image-get-active-drawable image))
  )
)


; ================================================================
; ÉTAPE 4 : CORRECTION COLORIMÉTRIQUE POST-UPSCALE
; ================================================================

; Correction gamma légère (compense l'assombrissement après upscale)
(define (color-correct-gamma image drawable)
  (gimp-levels drawable HISTOGRAM-VALUE 0 255 1.05 0 255)
)

; Correction saturation (anime — couleurs plus vivantes)
(define (color-correct-saturation drawable amount)
  (gimp-drawable-hue-saturation drawable HUE-RANGE-ALL
    0        ; hue-offset
    0        ; lightness
    amount   ; saturation (+/- 100)
    0        ; overlap
  )
)

; Correction contraste local (clarté, style Lightroom)
(define (color-correct-clarity image drawable)
  (gimp-brightness-contrast drawable 0 10)
  (gimp-curves-spline drawable HISTOGRAM-VALUE 10
    (list->vector '(0 0  64 60  128 130  192 196  255 255)))
)


; ================================================================
; PIPELINES PAR MODE
; ================================================================

; ---- MODE ANIME ----
; Bords nets, aplats de couleurs, réduction artefacts JPEG
(define (pipeline-anime image drawable noise-level scale-factor)
  (let* (
    (dims    (calc-new-dims image scale-factor))
    (new-w   (car dims))
    (new-h   (cadr dims))
  )
    ; 1. Débruitage adaptatif (surtout artefacts JPEG)
    (denoise-adaptive image drawable noise-level)
    (let* ((drw1 (car (gimp-image-get-active-drawable image))))
      ; 2. Upscale nohalo (préserve aplats + bords)
      (upscale-image image new-w new-h INTERPOLATION-NOHALO)
      (let* ((drw2 (car (gimp-image-get-active-drawable image))))
        ; 3. Netteté medium
        (sharpen-medium image drw2)
        ; 4. Renforcement contours (style waifu2x anime)
        (let* ((drw3 (edge-enhance-waifu image drw2 0.25)))
          ; 5. Saturation légèrement boostée
          (color-correct-saturation drw3 8)
          ; 6. Gamma
          (color-correct-gamma image drw3)
        )
      )
    )
  )
)

; ---- MODE PHOTO ----
; Douceur, préservation des gradients, bruit réduit
(define (pipeline-photo image drawable noise-level scale-factor)
  (let* (
    (dims  (calc-new-dims image scale-factor))
    (new-w (car dims))
    (new-h (cadr dims))
  )
    ; 1. Débruitage adaptatif
    (denoise-adaptive image drawable noise-level)
    (let* ((drw1 (car (gimp-image-get-active-drawable image))))
      ; 2. Upscale bicubique (meilleur pour photos)
      (upscale-image image new-w new-h INTERPOLATION-CUBIC)
      (let* ((drw2 (car (gimp-image-get-active-drawable image))))
        ; 3. Netteté légère
        (sharpen-light image drw2)
        ; 4. Clarté locale
        (color-correct-clarity image drw2)
        ; 5. Gamma
        (color-correct-gamma image drw2)
      )
    )
  )
)

; ---- MODE MANGA ----
; N&B, bords ultra-nets, contraste élevé
(define (pipeline-manga image drawable noise-level scale-factor)
  (let* (
    (dims  (calc-new-dims image scale-factor))
    (new-w (car dims))
    (new-h (cadr dims))
  )
    ; 1. Convertir en niveaux de gris si nécessaire
    (when (not (= (car (gimp-image-get-color-profile image)) 0))
      (gimp-desaturate-full drawable DESATURATE-LUMINOSITY))
    ; 2. Débruitage fort (scans)
    (denoise-adaptive image drawable noise-level)
    (let* ((drw1 (car (gimp-image-get-active-drawable image))))
      ; 3. Upscale nohalo
      (upscale-image image new-w new-h INTERPOLATION-NOHALO)
      (let* ((drw2 (car (gimp-image-get-active-drawable image))))
        ; 4. Netteté forte
        (sharpen-strong image drw2)
        ; 5. Renforcement bords très marqué
        (let* ((drw3 (edge-enhance-waifu image drw2 0.4)))
          ; 6. Contraste final
          (gimp-brightness-contrast drw3 5 25)
        )
      )
    )
  )
)

; ---- MODE DÉBRUITAGE SEUL (sans agrandissement) ----
(define (pipeline-denoise-only image drawable noise-level)
  (denoise-adaptive image drawable noise-level)
  (let* ((drw (car (gimp-image-get-active-drawable image))))
    (sharpen-light image drw)
    (color-correct-gamma image drw)
  )
)


; ================================================================
; PLUGIN PRINCIPAL — DIALOGUE INTERACTIF
; ================================================================

(define (script-fu-waifu2x
          image drawable
          mode            ; 0=Anime 1=Photo 2=Manga 3=Débruitage seul
          scale           ; 0=x1 1=x1.5 2=x2 3=x3 4=x4
          noise-level     ; 0=aucun 1=faible 2=moyen 3=fort
          auto-export
          export-format   ; 0=PNG 1=JPEG
          export-quality
          export-suffix)

  (gimp-image-undo-group-start image)

  ; Résoudre le facteur d'échelle
  (let* (
    (scale-factor
      (cond ((= scale 0) 1.0)
            ((= scale 1) 1.5)
            ((= scale 2) 2.0)
            ((= scale 3) 3.0)
            ((= scale 4) 4.0)
            (else 2.0)))
  )

    ; Lancer le pipeline selon le mode
    (cond
      ((= mode 0) (pipeline-anime        image drawable noise-level scale-factor))
      ((= mode 1) (pipeline-photo        image drawable noise-level scale-factor))
      ((= mode 2) (pipeline-manga        image drawable noise-level scale-factor))
      ((= mode 3) (pipeline-denoise-only image drawable noise-level))
    )

    ; Export automatique
    (when (= auto-export TRUE)
      (let* (
        (src  (car (gimp-image-get-filename image)))
        (base (filename-without-ext src))
        (drw  (car (gimp-image-get-active-drawable image)))
        (ext  (if (= export-format 0) ".png" ".jpg"))
        (out  (string-append base export-suffix ext))
      )
        (if (= export-format 0)
            (export-png  image drw out)
            (export-jpeg image drw out export-quality))
        (gimp-message (string-append "Waifu2x terminé → " out))
      )
    )
  )

  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  (gimp-image-clean-all image)
)

(script-fu-register
  "script-fu-waifu2x"
  "Waifu2x Upscaler"
  "Super-résolution style Waifu2x.\nv1.0 — Anime / Photo / Manga / Débruitage, facteurs x1 à x4."
  "Salah JAAFAR"
  "Copyright Salah JAAFAR - CIMS"
  "2024"
  "RGB* GRAY*"

  SF-IMAGE    "Image"        0
  SF-DRAWABLE "Calque actif" 0

  SF-OPTION "Mode" '(
    "Anime  (aplats + bords nets)"
    "Photo  (gradients + douceur)"
    "Manga  (N&B + contraste fort)"
    "Débruitage seul (sans agrandissement)"
  )

  SF-OPTION "Facteur d'agrandissement" '(
    "x1   (débruitage uniquement)"
    "x1.5 (agrandissement modéré)"
    "x2   (standard — recommandé)"
    "x3   (grand agrandissement)"
    "x4   (très grand agrandissement)"
  )

  SF-OPTION "Niveau de bruit" '(
    "0 - Aucun bruit"
    "1 - Bruit faible (légère compression)"
    "2 - Bruit moyen (JPEG standard)"
    "3 - Bruit fort  (scan / basse qualité)"
  )

  SF-TOGGLE     "Exporter automatiquement" FALSE
  SF-OPTION     "Format export"            '("PNG (sans perte)" "JPEG")
  SF-ADJUSTMENT "Qualité JPEG (1-100)"     '(95 1 100 1 5 0 SF-SLIDER)
  SF-STRING     "Suffixe fichier"          "_w2x"
)

(script-fu-menu-register
  "script-fu-waifu2x"
  "<Image>/Filtres/Script-Fu/Waifu2x Upscaler"
)


; ================================================================
; BATCH — FICHIER UNIQUE
;
; Usage :
;   gimp -i -b '(batch-waifu2x "/img.jpg" 0 2 2)' -b '(gimp-quit 0)'
;
; Paramètres :
;   filename    : chemin fichier
;   mode        : 0=Anime 1=Photo 2=Manga 3=Débruitage
;   scale       : 0=x1 1=x1.5 2=x2 3=x3 4=x4
;   noise-level : 0-3
; ================================================================

(define (batch-waifu2x filename mode scale noise-level)
  (let* (
    (image    (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
    (drawable (car (gimp-image-get-active-drawable image)))
    (scale-factor
      (cond ((= scale 0) 1.0) ((= scale 1) 1.5) ((= scale 2) 2.0)
            ((= scale 3) 3.0) ((= scale 4) 4.0) (else 2.0)))
    (outfile  (string-append (filename-without-ext filename) "_w2x.png"))
  )
    (cond
      ((= mode 0) (pipeline-anime        image drawable noise-level scale-factor))
      ((= mode 1) (pipeline-photo        image drawable noise-level scale-factor))
      ((= mode 2) (pipeline-manga        image drawable noise-level scale-factor))
      ((= mode 3) (pipeline-denoise-only image drawable noise-level))
    )
    (let* ((drw (car (gimp-image-get-active-drawable image))))
      (export-png image drw outfile))
    (gimp-image-delete image)
    (string-append "OK : " outfile)
  )
)


; ================================================================
; BATCH — DOSSIER COMPLET
;
; Usage :
;   gimp -i \
;     -b '(batch-waifu2x-folder "/dossier" 0 2 2 95)' \
;     -b '(gimp-quit 0)'
;
; Paramètres :
;   folder-path : dossier source
;   mode        : 0=Anime 1=Photo 2=Manga 3=Débruitage
;   scale       : 0=x1 1=x1.5 2=x2 3=x3 4=x4
;   noise-level : 0-3
;   quality     : qualité JPEG (0 = export PNG)
; ================================================================

(define (batch-waifu2x-folder folder-path mode scale noise-level quality)
  (let* (
    (f1    (cadr (file-glob (string-append folder-path "/*.jpg")  1)))
    (f2    (cadr (file-glob (string-append folder-path "/*.jpeg") 1)))
    (f3    (cadr (file-glob (string-append folder-path "/*.png")  1)))
    (all   (append f1 f2 f3))
    (scale-factor
      (cond ((= scale 0) 1.0) ((= scale 1) 1.5) ((= scale 2) 2.0)
            ((= scale 3) 3.0) ((= scale 4) 4.0) (else 2.0)))
    (count 0)
  )
    (for-each
      (lambda (fname)
        (when (not (string-contains fname "_w2x"))
          (let* (
            (image    (car (gimp-file-load RUN-NONINTERACTIVE fname fname)))
            (drawable (car (gimp-image-get-active-drawable image)))
            (ext      (if (= quality 0) ".png" ".jpg"))
            (outfile  (string-append (filename-without-ext fname) "_w2x" ext))
          )
            (cond
              ((= mode 0) (pipeline-anime        image drawable noise-level scale-factor))
              ((= mode 1) (pipeline-photo        image drawable noise-level scale-factor))
              ((= mode 2) (pipeline-manga        image drawable noise-level scale-factor))
              ((= mode 3) (pipeline-denoise-only image drawable noise-level))
            )
            (let* ((drw (car (gimp-image-get-active-drawable image))))
              (if (= quality 0)
                  (export-png  image drw outfile)
                  (export-jpeg image drw outfile quality)))
            (gimp-image-delete image)
            (set! count (+ count 1))
          )
        )
      )
      all)
    (string-append "Waifu2x batch terminé : "
                   (number->string count)
                   " fichier(s) traité(s).")
  )
)
