(ns csp.core
  (:gen-class))

(defn stop
  "Stop never engages in any action"
  [x]
  'bleep)

(defn coin->stop
  [x]
  (if (= x 'coin)
    stop
    'bleep))

(defmacro prefix
  "the operation (c->P) 'c then P'"
  [c P]
  `(fn [x#]
     (if (= x# ~c)
       ~P
       '~'bleep)))

(defmacro choice2
  "The binary choice (c->P|d->Q) 'c then P choose d then Q'"
  [c P d Q]
  `(fn [x#]
     (if (= x# ~c)
       ~P
       (if (= x# ~d)
         ~Q
         '~'bleep))))

(def vending
  "mu X . coin -> choc -> X"
  (prefix 'coin (prefix 'choc vending)))

(defn ct
  [n]
  (if (= n 0)
    (choice2 'around (ct 0) 'up (ct 1))
    (choice2 'up (ct (+ n 1)) 'down (ct (- n 1)))))

(defn menu
  "List all the symbols of A which can occur as the first event of P"
  [A P]
  (if-not (seq A)
    ()
    (if (= (P (first A)) 'bleep)
      (menu (rest A) P)
      (cons (first A) (menu (rest A) P)))))

(defn interact
  "Interact with process P using symbols from k and alphabet A"
  [A P k]
  (cons (menu A P)
        (if (= (first k) 'end)
          ()
          (if (= (P (first k)) 'bleep)
            (cons 'bleep (interact A P (rest k)))
            (interact A (P (first k)) (rest k))))))

(defn example
  [x]
  (assert (= 'beep
             ( ( ( ( (vending 'coin) 'choc) 'coin) 'choc) 'other)))

  (assert (= 'bleep
             ( ( ( (ct 0) 'around) 'up) 'around)))

  (assert (= '(coin)
             (menu '(coin badger) coin->stop))))
