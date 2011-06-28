(ns brainfudje.core)

(defn find-char [input curr char func]
  (cond
     (>= curr (count input)) (count input)
     (< curr 0) 0
     (= (nth input curr) char) (+ curr 1)
     :else (find-char input (func curr) char func)))

(defn bf-interp [input instr d-array ptr]
  (cond
   (<= (count input) instr)
     (list d-array ptr)
   (= (nth input instr) \>)
     (cond
        (= ptr (- (count d-array) 1))
          (bf-interp input (+ instr 1) d-array ptr)
        :else
          (bf-interp input (+ instr 1) d-array (+ ptr 1)))
   (= (nth input instr) \<)
     (cond
        (= ptr 0) (bf-interp input (+ instr 1) d-array ptr)
        :else (bf-interp input (+ instr 1) d-array (- ptr 1)))
   (= (nth input instr) \+) 
     (bf-interp input (+ instr 1)
              (assoc d-array ptr (mod (+ (nth d-array ptr) 1) 256)) ptr)
   (= (nth input instr) \-)
     (bf-interp input (+ instr 1)
              (assoc d-array ptr (mod (- (nth d-array ptr) 1) 256)) ptr)
   (= (nth input instr) \,)
     (let [in (read-line)]
        (flush)
        (bf-interp input (+ instr 1)
           (assoc d-array ptr (mod (+
                                    (nth d-array ptr)
                                    (int (first in))) 256)) ptr))
   (= (nth input instr) \[)
     (if
        (= 0 (nth d-array ptr)) (bf-interp input (find-char input instr \] inc) d-array ptr)
        (bf-interp input (+ instr 1) d-array ptr))
   (= (nth input instr) \])
     (if
        (not (= 0 (nth d-array ptr))) (bf-interp input (find-char input instr \[ dec) d-array ptr)
        (bf-interp input (+ instr 1) d-array ptr))
   (= (nth input instr)  \.)
     (do
       (println (char (nth d-array ptr)))
       (flush)
       (bf-interp input (+ instr 1) d-array ptr))
   :else (bf-interp input (+ instr 1) d-array ptr)))

(defn bf-repl [d-array ptr]
  (do
    (print "brainfudje> ")
    (flush)
    (let [input (read-line)
          out-tup (bf-interp input 0 d-array ptr)]
        (bf-repl (first out-tup) (second out-tup)))))

(bf-repl (vec (take 30000 (repeat 0))) 0)