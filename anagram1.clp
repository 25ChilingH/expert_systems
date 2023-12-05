/*
** September 19, 2022
** Chiling Han
**
** A basic program to produce anagrams of four letters for ATCS: Expert Systems.
**
**
** To run the program, run Jess in terminal and then type (batch "./anagram1.clp") and (run).
*/

(clear)
(reset)

;;;;;;;;;;;;

/*
** Defines a template of a letter with a character and a position of a word.
** 
** slot c is the character in the word
** slot p is the position of the character in the word
*/
(deftemplate Letter (slot c) (slot p))

(assert (Letter (c A) (p 1)))
(assert (Letter (c T) (p 2)))
(assert (Letter (c C) (p 3)))
(assert (Letter (c A) (p 4)))

(defrule rule-1
   "Enumerate strings of unique letters with the 4 letters defined above: A T C A"
   (Letter (c ?c1) (p ?p1)) (Letter (c ?c2) (p ?p2 &~?p1))
   (Letter (c ?c3) (p ?p3 &~?p1 &~?p2)) (Letter (c ?c4) (p ?p4 &~?p1 &~?p2 &~?p3))
   =>
   (printout t ?c1 ?c2 ?c3 ?c4 " ")
)