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

(defn prefix
  "the operation (c->P) 'c then P'"
  [c P]
  (fn [x]
    (if (= x c)
      P
      'bleep)))

(defn choice2
  "The binary choice (c->P|d->Q) 'c then P choose d then Q'"
  [c P d Q]
  (fn [x]
    (if (= x c)
      P
      (if (= x d)
        Q
        'bleep))))

(def vending
  "mu X . coin -> choc -> X"
  (prefix 'coin (prefix 'choc #'vending)))

(defn example
  [x]
  (assert (= 'beep
             ( ( ( ( (vending 'coin) 'choc) 'coin) 'choc) 'other))))
