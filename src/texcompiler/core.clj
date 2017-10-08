(ns texcompiler.core
  (:gen-class))

(def dfa
  {:s (fn [c]
        (cond (= \\ c) :s-cs
              (= \{ c) :s-group
              (= \} c) :e-group
              :else :text))
   :s-cs (fn [c]
           (cond (is-cs-char c) :s-cs :else :text))
   :s-group (fn [c] :s)
   :e-group (fn [c] :s)
   :text (fn [c]
           (cond (= \\ c) :s-cs
                 (=\{ c) :s-group
                 (= \} c) :e-group
                 :else :text))})
(defn is-cs-char
  [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))))

(defn next-char
  [[state buf] c]
  (let [new-state ((state dfa) c)]
    (if (= new-state state)
      [new-state (conj buf c)]
      [new-state [c]])))

(defn next-token
  [[state buf tokens] x]
  (let [[new-state new-buf] (next-char [state buf] x)]
    [new-state new-buf
     (if (= new-state state) tokens (conj tokens [state buf]))]))

(defn tokenizer
  "tokenize tex string based on DFA
  "
  [s]
  (reduce next-token [:s [] []] s))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
