/*
** October 5, 2022
** Chiling Han
**
** A user-interfaced program to guess the animal that the user has thought of in twenty questions
** for ATCS:Expert Systems.
**
** incrementQ    - increments the number of questions asked and halts the rule engine if it exceeds the max questions
** checkQuestion - calls incrementQ and returns TRUE if parameter starts with a "y," FALSE otherwise
** win           - prints out "I win" or "I lose" based on the boolean parameter and halts the rule engine
** guess         - asks user if the animal was the correct guess
** 
** To run the program, run Jess in terminal and then type (batch "./animal-forward.clp").
*/

(clear)
(reset)

;;;;;;;;;;;;

(batch "./utilities_v4.clp")

(defglobal ?*MAX_QUESTIONS* = 20)  ; constant for the maximum number of questions to ask the user about the chosen animal
                                   ; used to check when the number of questions have exceeded twenty
(defglobal ?*NO_OF_QUESTIONS* = 0) ; current number of questions that system has asked user about
                                   ; used to check when the number of questions have exceeded twenty

(defrule promptUser
   "This rule has no LHS so it will always fire. It will begin the program of twenty questions to guess the user's animal."
   (declare (salience 1))
   =>
   (printline (str-cat "Think of any animal, and I will try to guess it via " ?*MAX_QUESTIONS* " questions. You can input y for yes and n for no."))
   (printline "--------------------------------------------------------")
)

(defrule askMammal
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is a mammal and assert
    the user's answer as a fact."
   =>
   (assert (mammal (checkQuestion (askQuestion "Is your animal a mammal"))))
)

(defrule askLand
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal lives on land and assert
    the user's answer as a fact."
   =>
   (assert (land (checkQuestion (askQuestion "Does your animal live on land"))))
)

(defrule askSwim
   "This rule will ask if the user's chosen animal can swim given that it doesn't live on land and assert
    the user's answer as a fact."
   (land ?a &FALSE)
   =>
   (assert (canSwim (checkQuestion (askQuestion "Can your animal swim"))))
)

(defrule askInvertebrate
   "This rule will ask if the user's chosen animal is an invertebrate given that it's not a mammal and assert
    the user's answer as a fact."
   (mammal ?a &FALSE)
   =>
   (assert (invertebrate (checkQuestion (askQuestion "Is your animal an invertebrate"))))
)

(defrule askForest
   "This rule will ask if the user's chosen animal can live in a forest given that the animal lives on land and assert
    the user's answer as a fact."
   (land ?a &TRUE)
   =>
   (assert (liveInForest (checkQuestion (askQuestion "Can your animal live in the forest"))))
)

(defrule askApexPredator
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is an apex predator and assert
    the user's answer as a fact."
   =>
   (assert (apexPredator (checkQuestion (askQuestion "Is your animal an apex predator"))))
)

(defrule askSize
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is bigger than a human and assert
    the user's answer as a fact."
   =>
   (assert (biggerThanHuman (checkQuestion (askQuestion "Is your animal bigger than a human"))))
)

(defrule askWhale
   "This rule will ask if the user's chosen animal is a type of whale given that the animal can swim, is a mammal, and is
    bigger than a human. Then, it asserts the user's answer as a fact."
   (canSwim ?a &TRUE)
   (mammal ?b &TRUE)
   (biggerThanHuman ?c &TRUE)
   =>
   (assert (typeOfWhale (checkQuestion (askQuestion "Is your animal a type of whale"))))
)

(defrule askInsect
   "This rule will ask if the user's chosen animal is an insect given that the animal lives on land, is an invertebrate,
    and is smaller than a human. Then, it asserts the user's answer as a fact."
   (land ?b &TRUE)
   (invertebrate ?c &TRUE)
   (biggerThanHuman ?d &FALSE)
   =>
   (assert (insect (checkQuestion (askQuestion "Is your animal an insect"))))

)

(defrule askArachnid
   "This rule will ask if the user's chosen animal is an arachnid given that the animal is an insect and asserts the
    user's answer as a fact."
   (insect ?a &TRUE)
   =>
   (assert (arachnid (checkQuestion (askQuestion "Is your animal an arachnid"))))
)

(defrule askPest
   "This rule will ask if the user's chosen animal is a pest given that the animal is an insect and asserts the
    user's answer as a fact."
   (insect ?a &TRUE)
   =>
   (assert (pest (checkQuestion (askQuestion "Is your animal a pest"))))
)

(defrule askFly
   "This rule will ask if the user's chosen animal can fly given that the animal lives on land and asserts the
    user's answer as a fact."
   (land ?a &TRUE)
   =>
   (assert (canFly (checkQuestion (askQuestion "Can your animal fly"))))
)

(defrule askNocturnal
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is a nocturnal and asserts
    the user's answer as a fact."
   =>
   (assert (nocturnal (checkQuestion (askQuestion "Is your animal nocturnal"))))
)

(defrule askBanana 
   "This rule will ask if the user's chosen animal eats bananas given that it lives in forests and asserts
    the user's answer as a fact."
   (liveInForest ?a &TRUE)
   =>
   (assert (eatBanana (checkQuestion (askQuestion "Does your animal eat bananas"))))

)

(defrule askDistinctSound
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal makes a distinct sound and asserts
    the user's answer as a fact."
   =>
   (assert (distinctSound (checkQuestion (askQuestion "Does your animal make a distinct sound"))))
)

(defrule askIntelligent
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is more intelligent than other
    animals and asserts the user's answer as a fact."
   =>
   (assert (intelligent (checkQuestion (askQuestion "Is your animal more intelligent than other animals"))))
)

(defrule askMulticolored
   "This rule has no LHS so it will always fire. It will ask if the user's chosen animal is multicolored and
    asserts the user's answer as a fact."
   =>
   (assert (multicolored (checkQuestion (askQuestion "Is your animal multicolored"))))
)

(defrule askFur
   "This rule will ask if the user's chosen animal has fur given that it lives on land, is a mammal, and can't fly. It then
    asserts the user's answer as a fact."
   (land ?a &TRUE)
   (mammal ?b &TRUE)
   (canFly ?c &FALSE)
   =>
   (assert (fur (checkQuestion (askQuestion "Does your animal have fur"))))
)

(defrule askRelatedToDogs
   "This rule will ask if the user's chosen animal is related to dogs given that it has fur. It then asserts
    the user's answer as a fact."
   (fur ?a &TRUE)
   =>
   (assert (relatedToDogs (checkQuestion (askQuestion "Is your animal related to dogs"))))
)

(defrule askRelatedToCats
   "This rule will ask if the user's chosen animal is related to cats given that it has fur and
    is not related to dogs. It then asserts the user's answer as a fact."
   (fur ?a &TRUE)
   (relatedToDogs ?b &FALSE)
   =>
   (assert (relatedToCats (checkQuestion (askQuestion "Is your animal related to cats"))))
)

(defrule askInLionKing
   "This rule will ask if the user's chosen animal is a protagonist in Lion King given that it is related to cats,
    not multicolored, makes a distinct sound, is an apex predator, is bigger than a human, and doesn't in a forest. It then
    asserts the user's answer as a fact."
   (relatedToCats TRUE)
   (multicolored ?b &FALSE)
   (distinctSound ?c &TRUE)
   (apexPredator ?d &TRUE)
   (biggerThanHuman ?e &TRUE)
   (liveInForest ?f &FALSE)
   =>
   (assert (mainInLionKing (checkQuestion (askQuestion "Is your animal a protagonist in Lion King"))))
) ; defrule askInLionKing

(defrule dolphin
   "This rule will guess that the user's chosen animal is a dolphin given that it is a type of whale and is not an
    apex predator."
   (typeOfWhale ?a &TRUE)
   (apexPredator ?b &FALSE)
   =>
   (guess "dolphin")
)

(defrule butterfly
   "This rule will guess that the user's chosen animal is a butterfly given that it is not a pest, not an arachnid, not nocturnal,
    and can fly."
   (pest ?a &FALSE)
   (arachnid ?b &FALSE)
   (nocturnal ?c &FALSE)
   (canFly ?d &TRUE)
   =>
   (guess "butterfly")
)

(defrule monkey
   "This rule will guess that the user's chosen animal is a monkey given that it is not an apex predator, eats bananas,
    makes a distinct sound, is more intelligent than other animals, is not multicolored, and is not bigger than a human."
   (apexPredator ?a &FALSE)
   (eatBanana ?b &TRUE)
   (distinctSound ?c &TRUE)
   (intelligent ?d &TRUE)
   (multicolored ?e &FALSE)
   (biggerThanHuman ?f &FALSE)
   =>
   (guess "monkey")
) ; defrule monkey

(defrule lion
   "This rule will guess that the user's chosen animal is a lion given that it is a protagonist in the Lion King."
   (mainInLionKing ?a &TRUE)
   =>
   (guess "lion")
)

(defrule fail
   "This rule has no LHS so it will always fire. It will end the program and declare that the system has lost the game."
   (declare (salience -1))
   =>
   (win FALSE)
)

/*
** This function increments the number of questions asked by the system by one. If the number of questions
** have exceeded the maximum questions of twenty, the system recognizes it has lost.
** 
*/
(deffunction incrementQ ()
   (++ ?*NO_OF_QUESTIONS*)

   (if (> ?*NO_OF_QUESTIONS* ?*MAX_QUESTIONS*) then
      (win FALSE)
   )

   (return)
) ; deffunction incrementQ

/*
** This function returns TRUE if the first character is a Y or y, FALSE otherwise. It also increments
** the number of questions asked by calling incrementQ.
** 
** Function assumes ?char is a string
*/
(deffunction checkQuestion (?char)
   (incrementQ)
   
   (bind ?res FALSE)
   (if (= (lowcase (sub-string 1 1 ?char)) "y") then
      (bind ?res TRUE)
   )

   (return ?res)
) ; deffunction checkQuestion

/*
** This function prints to the user whether or not the system has guessed the animal based on ?bool.
** Then, it exits the program.
** 
** Function assumes ?bool is a boolean, TRUE or FALSE
*/
(deffunction win (?bool)
   (if ?bool then
      (printline "I win!")
     else
      (printline "The game has ended. I lose.")
   )

   (halt)
   (return)
) ; deffunction win

/*
** This function asks the user if the system's guess was the correct animal and then calls win
** to communicate whether or not the system has won. At last, it exits the program.
** 
** Function assumes ?animal is a string that is the name of an animal.
*/
(deffunction guess (?animal)
   (bind ?correct (checkQuestion (askQuestion (str-cat "Is your animal a(n) " ?animal))))
   (win ?correct)

   (return)
) ; deffunction guess