#lang racket
(require racket/vector)
(require racket/string)
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

;  Monographs --------------------------------------------------------------------------------------
(define (v-group) #((あ a) (い i) (う u) (え e) (お o)))
(define (k-group) #((か ka) (き ki) (く ku) (け ke) (こ ko)))
(define (s-group) #((さ sa) (し shi) (す su) (せ se) (そ so)))
(define (t-group) #((た ta) (ち chi) (つ tsu) (て te) (と to)))
(define (n-group) #((な na) (に ni) (ぬ nu) (ね ne) (の no )))
(define (h-group) #((は ha) (ひ hi) (ふ fu) (へ he) (ほ ho)))
(define (m-group) #((ま ma) (み mi) (む mu) (め me) (も mo)))
(define (y-group) #((や ya) (ゆ yu) (よ yo)))
(define (r-group) #((ら ra) (り ri) (る ru) (れ re) (ろ ro )))
(define (w-group) #((わ wa) (を wo)))
(define (nn-group) #((ん n)))
; Mono Slides ---------------------------------------------------------------------------------------
(define (sk-group) #((きゃ kya) (きゅ kyu) (きょ kyo)))
(define (ss-group) #((しゃ sha) (しゅ shu) (しょ sho)))
(define (st-group) #((ちゃ cha) (ちゅ chu) (ちょ cho)))
(define (sn-group) #((にゃ nya) (にゅ nyu) (にょ nyo)))
(define (sh-group) #((ひゃ hya) (ひゅ hyu) (ひょ hyo)))
(define (sm-group) #((みゃ mya) (みゅ myu) (みょ myo)))
(define (sr-group) #((りゃ rya) (りゅ ryu) (りょ ryo)))

; Dia Slides ---------------------------------------------------------------------------------------
(define (sg-group) #((ぎゃ gya) (ぎゅ gyu) (ぎょ gyo)))
(define (sz-group) #((じゃ ja) (じゅ ju) (じょ jo)))
(define (sb-group) #((びゃ bya) (びゅ byu) (びょ byo)))
(define (sp-group) #((ぴゃ pya) (ぴゅ pyu) (ぴょ pyo)))

; Diacritics ---------------------------------------------------------------------------------------
(define (g-group) #((が ga) (ぎ gi ) (ぐ gu) (げ ge) (ご go)))
(define (z-group) #((ざ za) (じ ji) (ず zu) (ぜ ze) (ぞ zo)))
(define (d-group) #((だ da) (ぢ di) (づ du) (で de) (ど do)))
(define (b-group) #((ば ba) (び bi) (ぶ bu) (べ be) (ぼ bo)))
(define (p-group) #((ぱ pa) (ぴ pi) (ぷ pu) (ぺ pe) (ぽ po)))

(define monographs '(v k s t n h m y r s w nn)) 
(define diacritics '(g z d b p))
(define mono-slides '(sk ss st sn sh sm sr))
(define dia-slides '(sg sz sb sp))
(define all (append monographs diacritics mono-slides dia-slides))

; Takes a list of symbols, appends said list of symbols with "-group" and then evaluates them with
;vector-append as their operator
(define (learning-set args)
  (eval `(vector-append
          ,@(for/list ([i args])
              `(,(string->symbol
                 (string-append
                  (symbol->string i)
                  "-group")))))
        ns))

;Takes a list of pairs that contain the kana and the romanji sound representations
; and returns a function that returns a random element of the parameter set
(define (random-kana set)
  (let* ([vec (learning-set set)]
         [len (vector-length vec)])
    (λ () (vector-ref vec (random len)))))

;Randomly chooses the type of problem the program will ask the user
(define (kana-or-sound kana)
  (define choice `#(,car ,cadr))
  ((vector-ref choice (random 2))
   kana))

; Program IO ---------------------------------------------------------------------------------------

; The parameter k used in preset, custom and choose-set is a continuation used to capture
; an earlier moment in time on the stack so that a user may return back to home menu at anytime
; I am deeply sorry for any lack of clarity from my use of continuations this is my first
; time using them like this.
; Getting users choice for learning group ----------------------------------------------------------

(define (preset k)
  (display (format "1. Monographs | 2. Diacritics | 3. Mono-slides | 4. Dia-Slides | 5. All~%?: "))
  (define in (read))
  (when (eq? in '-h)
    (k k))
  (if (and (number? in) (positive? in) (< in 6))
      (vector-ref (vector monographs diacritics mono-slides dia-slides all) (- in 1))
      (begin (displayln "Invalid option try again!")
             (preset k))))   

(define (custom k)
  (displayln "Choose your groups, options appended with an s are slides of that group")
   (display
    (format 
     (for/fold ([v ""])
               ([i (in-range 1 29)]
                [vec all])
       (let* ([sym (symbol->string vec)]
              [str (string-append sym (if (= (string-length sym) 1) " " ""))]) 
       (string-append v (format "~a | " str
                                )
                      (format (if (zero? (modulo i 5))
                                  "~%"
                                  "")))))))
  (display (format "~%?: "))
  (read-line)
  (define in (map string->symbol (string-split (read-line) " ")))
  (when (eq? '-h (car in))
    (k k))
  (if (andmap (lambda (x) (member x all)) in)
      in
      (begin (displayln "Invalid option try again!")
             (custom))))
  
 

(define (choose-set k)
  (display (format "1. Preset | 2. Custom~%?: "))
  (define in (read))
  (cond [(= in 1) (preset k)]
        [(= in 2) (custom k)]
        [else (displayln "Invalid option, try again!")
              (choose-set k)]))

; IO for main game loop ----------------------------------------------------------------------------

(define (show-kana kana home game)
  (define ans (kana))
  (display (format "~a?: " (car ans)))
  ;Loop continuation for when a user gets something wrong
  (let ([loop (call/cc (lambda (k) k))])
    (define in (read))
    (when (eq? in '-h)
      (home home))
    (if (eq? in (cadr ans))
        (begin (display (format "Correct!~%"))
               (game game))
        (begin (display (format "Wrong! Answer is: ~a try again!~%"(cadr ans)))
               (loop loop)))))

(define (show-pro kana home game)
  (define ans  (kana))
  (define ques (shuffle (list ans (kana) (kana) (kana))))
  (display (format "~v | " (cadr ans)))
  (display (apply (lambda (a b c d)
                    (format "1. ~a | 2. ~a | 3. ~a | 4. ~a~%" a b c d)) (map car ques)))
  (let ([loop (call/cc (lambda (k) k))])
    (display "?: ")
    (define in (read))
    (when (eq? in '-h)
      (home home))
    (if (and (number? in) (positive? in) (<= 4))
        (if (eq? (car (list-ref ques (- in 1))) (car ans))
            (begin (displayln "Correct!")
                   (game game))
            (begin (display (format "Wrong! Answer is: ~a try again!~%" (car ans)))
                   (loop loop)))
        (begin (displayln "Invalid option! try again!")
               (loop loop)))))
    
  


(define (main)
  (displayln "Hiragana Study Tool")
  ;Home point.
  (let ([home (call/cc (lambda (k) k))])
    (define kana (random-kana (choose-set home)))
    (let ([game (call/cc (lambda (k) k))])
      ((vector-ref `#(,show-kana ,show-pro) (random 2))
       kana home game))))

(main)
    
  