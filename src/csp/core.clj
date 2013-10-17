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

(defn example
  [x]
  (assert (= 'beep
             ( ( ( ( (vending 'coin) 'choc) 'coin) 'choc) 'other)))

  (assert (= 'bleep
             ( ( ( (ct 0) 'around) 'up) 'around))))
