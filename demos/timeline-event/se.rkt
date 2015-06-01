#lang racket

(define input (list '(71 82 "left-down") '(71 84 "dragging") '(71 85 "dragging") '(71 87 "dragging") 
                    '(71 89 "dragging") '(72 92 "dragging") '(73 95 "dragging") '(74 99 "dragging") 
                    '(76 102 "dragging") '(78 106 "dragging") '(82 112 "dragging") '(86 117 "dragging") 
                    '(90 122 "dragging") '(94 128 "dragging") '(99 134 "dragging") '(104 139 "dragging") 
                    '(108 146 "dragging") '(113 153 "dragging") '(118 160 "dragging") '(123 167 "dragging") 
                    '(128 174 "dragging") '(134 181 "dragging") '(139 187 "dragging") '(143 192 "dragging") 
                    '(147 197 "dragging") '(151 202 "dragging") '(156 206 "dragging") '(157 211 "dragging") 
                    '(160 216 "dragging") '(163 220 "dragging") '(165 225 "dragging") '(167 230 "dragging") 
                    '(169 235 "dragging") '(171 239 "dragging") '(172 242 "dragging") '(174 245 "dragging") 
                    '(175 246 "dragging") '(176 247 "dragging") '(177 248 "dragging") '(177 249 "dragging") 
                    '(178 250 "dragging") '(179 251 "dragging") '(179 252 "dragging") '(180 253 "dragging") 
                    '(180 254 "dragging") '(181 256 "dragging") '(181 257 "dragging") '(182 258 "dragging") 
                    '(183 259 "dragging") '(183 260 "dragging") '(184 260 "dragging") '(184 260 "left-up") 
                    '(117 132 "left-down") '(117 134 "dragging") '(116 136 "dragging") '(116 138 "dragging") 
                    '(116 141 "dragging") '(116 145 "dragging") '(116 150 "dragging") '(118 158 "dragging") 
                    '(119 165 "dragging") '(121 174 "dragging") '(123 183 "dragging") '(125 193 "dragging") 
                    '(128 202 "dragging") '(131 211 "dragging") '(135 220 "dragging") '(137 228 "dragging") 
                    '(142 235 "dragging") '(146 242 "dragging") '(150 248 "dragging") '(155 254 "dragging") 
                    '(160 259 "dragging") '(164 265 "dragging") '(168 270 "dragging") '(172 274 "dragging") 
                    '(177 279 "dragging") '(180 283 "dragging") '(183 286 "dragging") '(185 290 "dragging") 
                    '(187 293 "dragging") '(191 296 "dragging") '(192 298 "dragging") '(193 300 "dragging") 
                    '(195 302 "dragging") '(196 304 "dragging") '(197 305 "dragging") '(198 306 "dragging") 
                    '(199 307 "dragging") '(200 308 "dragging") '(202 310 "dragging") '(204 312 "dragging") 
                    '(205 313 "dragging") '(206 314 "dragging") '(207 315 "dragging") '(209 317 "dragging") 
                    '(211 320 "dragging") '(213 321 "dragging") '(214 322 "dragging") '(216 324 "dragging")
                    '(218 325 "dragging") '(218 325 "left-up")))

(define dragged #f)
(define color "green")
(define dark-color "SeaGreen")
(define rect-x 0)
(define rect-y 0)
(define rect-width 200)

(define (set-dragged b)
  (if b 
      (set! color dark-color)
      (set! color color))
  (set! dragged b))
  
(define (process input)
  (for ([e input])
    (define x (first e))
    (define y (second e))
    (define event (third e))
    (cond
      [(equal? event "left-up")
       (when dragged
         (set-dragged #f))]
      
      [(equal? event "left-down")
       (when (< x rect-width)
         (set-dragged #t))]
      
      [(equal? event "dragging")
       (when dragged 
         (set! rect-x x)
         (set! rect-y y))])))

(process input)