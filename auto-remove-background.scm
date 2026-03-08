; ============================================================
; auto-remove-background.scm
; Script GIMP - Suppression automatique de fond
; Auteur : Salah JAAFAR
; Version : 1.1 (bugfix)
; ============================================================

; --- Convertit un pixel brut (vector) en liste couleur RGB ---
(define (pixel->color drawable x y)
  (let* (
    (result  (gimp-drawable-get-pixel drawable x y))
    (nch     (car result))
    (arr     (cadr result))
  )
    (cond
      ((= nch 1) (list (vector-ref arr 0) (vector-ref arr 0) (vector-ref arr 0)))
      ((= nch 2) (list (vector-ref arr 0) (vector-ref arr 0) (vector-ref arr 0)))
      ((= nch 3) (list (vector-ref arr 0) (vector-ref arr 1) (vector-ref arr 2)))
      ((= nch 4) (list (vector-ref arr 0) (vector-ref arr 1) (vector-ref arr 2)))
      (else      (list 255 255 255))
    )
  )
)

; --- Méthode 1 : Par couleur du coin supérieur gauche ---
(define (auto-remove-bg-by-color image drawable threshold)
  (let* (
    (color (pixel->color drawable 0 0))
  )
    (gimp-layer-add-alpha drawable)
    (gimp-by-color-select
      drawable
      color
      threshold
      CHANNEL-OP-REPLACE
      TRUE    ; antialias
      FALSE   ; feather
      0       ; feather-radius
      TRUE    ; sample-merged
    )
    (gimp-edit-clear drawable)
    (gimp-selection-none image)
  )
)

; --- Méthode 2 : Fuzzy select sur les 4 coins ---
(define (auto-remove-bg-fuzzy image drawable threshold)
  (let* (
    (width  (car (gimp-image-width image)))
    (height (car (gimp-image-height image)))
  )
    (gimp-layer-add-alpha drawable)
    (gimp-fuzzy-select drawable 0           0            threshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable (- width 1) 0            threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable 0           (- height 1) threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable (- width 1) (- height 1) threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (gimp-selection-grow    image 1)
    (gimp-selection-feather image 0.5)
    (gimp-edit-clear drawable)
    (gimp-selection-none image)
  )
)

; --- Méthode 3 : Avancée avec feather paramétrable ---
(define (auto-remove-bg-advanced image drawable threshold feather-px)
  (let* (
    (width  (car (gimp-image-width image)))
    (height (car (gimp-image-height image)))
  )
    (gimp-layer-add-alpha drawable)
    (gimp-fuzzy-select drawable 0           0            threshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable (- width 1) 0            threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable 0           (- height 1) threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (gimp-fuzzy-select drawable (- width 1) (- height 1) threshold CHANNEL-OP-ADD     TRUE FALSE 0 TRUE)
    (when (> feather-px 0)
      (gimp-selection-feather image feather-px)
    )
    (gimp-edit-clear drawable)
    (gimp-selection-none image)
  )
)

; ============================================================
; PLUGIN PRINCIPAL
; ============================================================

(define (script-fu-auto-remove-bg image drawable method threshold feather)
  (gimp-image-undo-group-start image)
  (cond
    ((= method 0) (auto-remove-bg-by-color image drawable threshold))
    ((= method 1) (auto-remove-bg-fuzzy    image drawable threshold))
    ((= method 2) (auto-remove-bg-advanced image drawable threshold feather))
  )
  (gimp-image-undo-group-end image)
  (gimp-displays-flush)
  (gimp-image-clean-all image)
)

(script-fu-register
  "script-fu-auto-remove-bg"
  "Auto Remove Background"
  "Supprime le fond d'une image (3 méthodes). v1.1"
  "Salah JAAFAR"
  "Salah JAAFAR - CIMS"
  "2024"
  "RGB* GRAY*"
  SF-IMAGE      "Image"      0
  SF-DRAWABLE   "Calque"     0
  SF-OPTION     "Méthode"    '("Par couleur (coin [0,0])"
                               "Fuzzy 4 coins"
                               "Avancé + feather")
  SF-ADJUSTMENT "Tolérance"  '(30 0 255 1 10 0 SF-SLIDER)
  SF-ADJUSTMENT "Feather px" '(1  0  20  1  1 0 SF-SLIDER)
)

(script-fu-menu-register
  "script-fu-auto-remove-bg"
  "<Image>/Filtres/Script-Fu/Auto Remove Background"
)

; ============================================================
; MODE BATCH (ligne de commande)
; Usage :
;   gimp -i -b '(batch-remove-bg "/chemin/img.jpg" 30)' -b '(gimp-quit 0)'
; ============================================================

(define (batch-remove-bg filename threshold)
  (let* (
    (image    (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
    (drawable (car (gimp-image-get-active-drawable image)))
    (outfile  (string-append
                (substring filename 0 (- (string-length filename) 4))
                "_nobg.png"))
  )
    (auto-remove-bg-advanced image drawable threshold 1)
    (file-png-save RUN-NONINTERACTIVE image drawable outfile outfile 0 9 1 1 1 1 1)
    (gimp-image-delete image)
    (string-append "Sauvegardé : " outfile)
  )
)
