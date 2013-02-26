(define (script-fu-ks-rr-podpis img text pos-x pos-y font-size font-color font)
    (let (
            (layer-opis (car (gimp-text-fontname img -1 pos-x pos-y text 0 TRUE font-size PIXELS font)))
         )
         (gimp-text-layer-set-color layer-opis font-color)
    )
)

(define (script-fu-ks-rr-zrodlo img text pos-x pos-y font-size font-color font source-prefix)
    (let
        (
            (source-text (if (= source-prefix TRUE) (begin (string-append "źródło: " text)) (begin text)))
        )

        (let
            (
                (layer-zrodlo (car (gimp-text-fontname img -1 0 0 source-text 0 TRUE font-size PIXELS font)))
            )
            (gimp-text-layer-set-color layer-zrodlo font-color)
            (gimp-layer-set-offsets layer-zrodlo (- pos-x (car (gimp-drawable-width layer-zrodlo))) pos-y )
        )
    )
)

(define (script-fu-ks-rr-tlo img newW newH frame-color)
    (let 
         (
            (layer-bg (car (gimp-layer-new img newW newH (if (= (car (gimp-image-base-type img)) 0) RGB-IMAGE GRAY-IMAGE) "tło" 100 NORMAL-MODE )))
         )
         ; wstawienie warstwy "tło" na spodzie
         (gimp-image-insert-layer img layer-bg 0 10)

         ; ustawienie koloru pierwszoplanowego
         (gimp-context-set-foreground frame-color)

         ; wypełnienie warstwy "tło" kolorem
         (gimp-edit-fill layer-bg FOREGROUND-FILL)
    )
)

(define (script-fu-ks-rr-obramowanie img newW newH frame-size frame2-size frame-size-bottom frame2-color)
    (let (
            (layer-frame (car (gimp-layer-new img newW newH (if (= (car (gimp-image-base-type img)) 0) RGBA-IMAGE GRAYA-IMAGE) "ramka" 100 NORMAL-MODE )))
         )
         (gimp-image-insert-layer img layer-frame 0 0)
         (gimp-image-select-rectangle img CHANNEL-OP-REPLACE frame-size frame-size (- newW (* frame-size 2)) (- newH frame-size frame-size-bottom))
         (gimp-image-select-rectangle img CHANNEL-OP-SUBTRACT (+ frame-size frame2-size) (+ frame-size frame2-size) (- newW (* frame-size 2) (* frame2-size 2)) (- newH frame-size frame-size-bottom (* frame2-size 2)))

         ; ustawienie koloru pierwszoplanowego
         (gimp-context-set-foreground frame2-color)

         ; wypełnienie zaznaczonego obszaru kolorem pierwszoplanowym
         (gimp-edit-fill layer-frame FOREGROUND-FILL)

         ; usunięcie zaznaczenia
         (gimp-selection-none img)
    )
)

(define (script-fu-ks-rr img drawable frame-size frame-size-bottom use-frame-color podpis podpis-font podpis-font-size use-podpis-font-color source-prefix source source-font source-font-size use-source-font-color frame2-size use-frame2-color colors-set)

    (gimp-image-undo-group-start img)

    (let (
          (width (car (gimp-image-width img)))
          (height (car (gimp-image-height img)))
          (newWidth (+ (car (gimp-image-width img)) (* frame-size 2) (* frame2-size 2)))
          (newHeight (+ (car (gimp-image-height img)) frame-size frame-size-bottom (* frame2-size 2)))

          ; Kolor ramki
          (frame-color (cond ((= colors-set 0) '(0 0 0))
                             ((= colors-set 1) '(255 255 255))
                             (else use-frame-color)
                        )
          )

          ; Kolor obramowania
          (frame2-color (cond ((= colors-set 0) '(255 255 255))
                              ((= colors-set 1) '(0 0 0))
                              (else use-frame2-color)
                        )
          )

          ; Kolor czcionki podpisu zdjecia
          (podpis-font-color (cond ((= colors-set 0) '(255 255 255))
                                   ((= colors-set 1) '(0 0 0))
                                   (else use-podpis-font-color)
                             )
          )

          ; Kolor czcionki zrodla
          (source-font-color (cond ((= colors-set 0) '(164 164 164))
                                   ((= colors-set 1) '(76 76 76))
                                   (else use-source-font-color)
                             )
          )
         )

        (gimp-image-resize img newWidth newHeight (+ frame-size frame2-size) (+ frame-size frame2-size))
        (gimp-item-set-name drawable "default")

        ; nowa warstwa "tło" wypełniona kolorem ramki
        (script-fu-ks-rr-tlo img newWidth newHeight frame-color)

        ; obramowanie zdjecia
        (if (> frame2-size 0)
            (script-fu-ks-rr-obramowanie img newWidth newHeight frame-size frame2-size frame-size-bottom frame2-color)
        )

        ; podpis pod zdjęciem
        (if (> (string-length podpis) 0)
            (script-fu-ks-rr-podpis img podpis (- frame-size 1) (- newHeight frame-size podpis-font-size) podpis-font-size podpis-font-color podpis-font)
        )

        ; podanie źródła zdjęcia
        (if (> (string-length source) 0)
            (script-fu-ks-rr-zrodlo img source (+ frame-size (* frame2-size 2) width) (- (+ frame-size (* frame2-size 2) height) 1) source-font-size source-font-color source-font source-prefix)
        )
    )

    (gimp-image-undo-group-end img)
    (gimp-displays-flush)
)

(script-fu-register "script-fu-ks-rr"
    "<Image>/Skrypty/Ramka do zdjęć"
    "Dodaje ramkę z opisem do zdjęć.\n\nWersja 1.3"
    "K.S."                                          ; author
    "All rights reserved."                          ; copyright information
    "August 2012"                                   ; date
    "*"                                             ; image types
    SF-IMAGE       "Image" 0
    SF-DRAWABLE    "Image" 0
    SF-ADJUSTMENT  "Szerokość ramki"                '(10 0 50 1 5 0 0)
    SF-ADJUSTMENT  "Ramka na dole"                  '(40 0 200 1 5 0 0)
    SF-COLOR       "Kolor ramki"                    '(255 255 255)

    SF-STRING      "Podpis" ""
    SF-FONT        "Czcionka podpisu"               "URW Chancery L Medium Italic"
    SF-ADJUSTMENT  "Rozmiar czcionki podpisu"       '(22 1 200 1 5 0 1)
    SF-COLOR       "Kolor tekstu podpisu"           '(0 0 0)

    SF-TOGGLE      "Dodaj przedrostek \"źródło: \"" FALSE
    SF-STRING      "Źródło"                         ""
    SF-FONT        "Czcionka źródła"                "PF Tempesta Seven Extended"
    SF-ADJUSTMENT  "Rozmiar czcionki źródła"        '(8 1 200 1 5 0 1)
    SF-COLOR       "Kolor tekstu źródła"            '(76 76 76)

    SF-ADJUSTMENT  "Rozmiar obwódki"                '(1 0 50 1 5 0 1)
    SF-COLOR       "Kolor obwódki"                  '(0 0 0)
    
    SF-OPTION      "Kolorystyka ramki"              '("Czarna" "Biała" "Kolory ustawione powyżej")
)
