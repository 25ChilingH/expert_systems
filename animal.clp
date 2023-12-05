/*
** October 22, 2022
** Chiling Han
**
** A simple expert system to guess the animal that the user has thought of in twenty questions
** for ATCS:Expert Systems. My expert for this project was Kashish Priyam. This system can only
** guess thirty five different animals: horse, cow, elephant, panda, kangaroo, walrus, tiger,
** lion, polar bear, cat, weasel, monkey, human, deer, pig, fox, sheep, koala, beaver, dolphin,
** sea otter, fish, jellyfish, sea star, stingray, shark, octopus, chicken, butterfly, snake, 
** bee, eagle, owl, flamingo, alligator
** 
** Facts, which represent animal traits, are backward chained so that whenever the fact is
** needed for pattern matching, its corresponding rule will fire. This rule's RHS asks the user
** if the animal has the given trait. Animals are represented by forward-chained rules.
** Once the LHS of an animal rule is satisfied, the system asks the user if the predicted animal
** is correct and halts the rule engine.
**
** incrementQ    - increments the number of questions asked and halts the rule engine if it exceeds the max questions
** firstToken    - returns lowercased first character of user's response to a question
** validate      - returns "y", "n", or "u" according to user's input and keeps asking the given question till valid input
** checkQuestion - calls incrementQ and validate; returns the character validate returns
** win           - prints out "I win" or "I lose" based on the boolean parameter and halts the rule engine
** guess         - asks user if the animal was the correct guess
** 
** To run the program, run Jess in terminal and then type (batch "./animal.clp").
*/

(clear)
(reset)

;;;;;;;;;;;;

(batch "./utilities_v4.clp")

(defglobal ?*MAX_QUESTIONS* = 20)  ; constant for the maximum number of questions to ask the user about the chosen animal
                                   ; used to check when the number of questions have exceeded twenty
(defglobal ?*NO_OF_QUESTIONS* = 0) ; current number of questions that system has asked user about
                                   ; used to check when the number of questions have exceeded twenty

(do-backward-chaining mammal)
(do-backward-chaining land)
(do-backward-chaining biggerThanHuman)
(do-backward-chaining carnivore)
(do-backward-chaining swim)
(do-backward-chaining fourLegs)
(do-backward-chaining forest)
(do-backward-chaining fly)
(do-backward-chaining marsupial)
(do-backward-chaining mane)
(do-backward-chaining sting)
(do-backward-chaining tusks)
(do-backward-chaining nocturnal)
(do-backward-chaining domesticated)
(do-backward-chaining beak)
(do-backward-chaining wool)
(do-backward-chaining longTail)
(do-backward-chaining antler)
(do-backward-chaining fin)

/********************************
**
** Rule to capture the program losing the Animal game.
** This rule will go onto the agenda immediately, but
** because the salience level is so low, it will
** just sit there until there are no rules left to
** fire. Only then will it be examined, at which point
** if we have not found the animal, we can't match the 
** user's inputted characteristics to an animal in our program.
**
**/
(defrule fail "Can't find an animal that matches user's answers to questions."
   (declare (salience -1))
   =>
   (win FALSE)
)

/********************************
**
** Startup rule that will fire first because of the high salience
**
**/
(defrule promptUser "Rule that begins the twenty questions game to guess the user's animal."
   (declare (salience 1))
   =>
   (printout t crlf
               "Think of any animal, and I will try to guess it via " ?*MAX_QUESTIONS* " questions." crlf
               "You can input Y for yes, U for unknown and N for no." crlf
               "--------------------------------------------------------" crlf crlf)
)

/********************************
**
** Backward Chaining Rules
** Only ask for more information on the attributes if
** the animal has not been guessed.
**
*/
(defrule isMammal
   (need-mammal ?)
   =>
   (bind ?check (checkQuestion "Is your animal a mammal"))
   (assert (mammal ?check))
) ; defrule isMammal

(defrule livesOnLand
   (need-land ?)
   =>
   (bind ?check (checkQuestion "Can your animal live on land (flying creatures and sea ice inhabitants count)"))
   (assert (land ?check))
) ; defrule livesOnLand

(defrule liveInForest
   (need-forest ?)
   =>
   (bind ?check (checkQuestion "Is one of your animal's natural habitats a forest"))
   (assert (forest ?check))
) ; defrule liveInForest

(defrule canFly
   (need-fly ?)
   =>
   (bind ?check (checkQuestion "Can your animal fly"))
   (assert (fly ?check))
) ; defrule canFly

(defrule canSwim
   (need-swim ?)
   =>
   (bind ?check (checkQuestion "Can your animal swim"))
   (assert (swim ?check))
) ; defrule canSwim

(defrule isBiggerThanHuman
   (need-biggerThanHuman ?)
   =>
   (bind ?check (checkQuestion "Is your animal bigger than a human (just the animal's body)"))
   (assert (biggerThanHuman ?check))
) ; defrule isBiggerThanHuman

(defrule isMarsupial
   (need-marsupial ?)
   =>
   (bind ?check (checkQuestion "Is your animal a marsupial"))
   (assert (marsupial ?check))
) ; defrule isMarsupial

(defrule isCarnivore
   (need-carnivore ?)
   =>
   (bind ?check (checkQuestion "Is your animal a carnivore (only feed on flesh)"))
   (assert (carnivore ?check))
) ; defrule isCarnivore

(defrule hasMane
   (need-mane ?)
   =>
   (bind ?check (checkQuestion "Does your animal have a mane"))
   (assert (mane ?check))
) ; defrule hasMane

(defrule canSting
   (need-sting ?)
   =>
   (bind ?check (checkQuestion "Does your animal sting"))
   (assert (sting ?check))
) ; defrule canSting

(defrule hasTusks
   (need-tusks ?)
   =>
   (bind ?check (checkQuestion "Does your animal have tusks"))
   (assert (tusks ?check))
) ; defrule hasTusks

(defrule hasFourLegs
   (need-fourLegs ?)
   =>
   (bind ?check (checkQuestion "Does your animal have four legs"))
   (assert (fourLegs ?check))
) ; defrule hasFourLegs

(defrule isNocturnal
   (need-nocturnal ?)
   =>
   (bind ?check (checkQuestion "Is your animal nocturnal"))
   (assert (nocturnal ?check))
) ; defrule isNocturnal

(defrule hasLongTail
   (need-longTail ?)
   =>
   (bind ?check (checkQuestion "Does your animal have a long tail (on average longer than 10 in)"))
   (assert (longTail ?check))
) ; defrule hasLongTail

(defrule hasWool
   (need-wool ?)
   =>
   (bind ?check (checkQuestion "Does your animal produce wool"))
   (assert (wool ?check))
) ; defrule hasWool

(defrule hasBeak
   (need-beak ?)
   =>
   (bind ?check (checkQuestion "Does your animal have a beak"))
   (assert (beak ?check))
) ; defrule hasBeak

(defrule hasAntler
   (need-antler ?)
   =>
   (bind ?check (checkQuestion "Does your animal have antlers"))
   (assert (antler ?check))
) ; defrule hasAntler

(defrule hasFin
   (need-fin ?)
   =>
   (bind ?check (checkQuestion "Does your animal have fins"))
   (assert (fin ?check))
) ; defrule hasFin

(defrule isDomesticated
   (need-domesticated ?)
   =>
   (bind ?check (checkQuestion "Can your animal be domesticated"))
   (assert (domesticated ?check))
) ; defrule isDomesticated

/********************************
**
** Forward Chaining Rules
** Checks if the user's animal is the animal
** guessed by the program if the dynamically asserted 
** facts matches the pattern and will short circuit so that
** no unnecessary questions are asked. Handles unknowns by
** pattern matching with not y when checking if a trait doesn't exist
** and not n when checking if a trait does exist in the animal.
**
*/

(defrule dolphin "grouped in aquatic mammals"
   (mammal ?a &~n)
   (land ?b &~y)
   (biggerThanHuman ?c &~n) ; differentiates from sea otters
   =>
   (guess "dolphin")
) ; defrule dolphin

(defrule seaOtter "grouped in aquatic mammals"
   (mammal ?a &~n)
   (land ?b &~y)
   (biggerThanHuman ?c &~y) ; differentiates from dolphins
   =>
   (guess "sea otter")
) ; defrule seaOtter

(defrule seaStar "grouped in small, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~y)
   (swim ?d &~y) ; differentiates from jellyfish
   =>
   (guess "sea star")
) ; defrule seaStar

(defrule jellyfish "grouped in small, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~y)
   (swim ?d &~n)  ; differentiates from sea star
   (sting ?e &~n) ; differentiates from fish
   =>
   (guess "jellyfish")
) ; defrule jellyfish

(defrule fish "grouped in small, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~y)
   (swim ?d &~n)  ; differentiates form sea star
   (sting ?e &~y) ; differentiates from jellyfish
   =>
   (guess "fish")
) ; defrule fish

(defrule stingray "grouped in large, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~n)
   (fin ?d &~n)   ; differentiates from octopus
   (sting ?e &~n) ; differentiates from shark
   =>
   (guess "stingray")
) ; defrule stingray

(defrule shark "grouped in large, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~n)
   (fin ?d &~n)   ; differentiates from octopus
   (sting ?e &~y) ; differentiates from stingray
   =>
   (guess "shark")
) ; defrule shark

(defrule octopus "grouped in large, aquatic non-mammals"
   (mammal ?a &~y)
   (land ?b &~y)
   (biggerThanHuman ?c &~n)
   (fin ?d &~y) ; differentiates from shark, stingray
   =>
   (guess "octopus")
) ; defrule octopus

(defrule flamingo "grouped in large, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y) ; differentiates from alligator
   =>
   (guess "flamingo")
) ; defrule flamingo

(defrule alligator "grouped in large, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~n) ; differentiates from flamingo
   =>
   (guess "alligator")
) ; defrule alligator

(defrule snake "grouped in small, carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~n)
   (fly ?e &~y) ; differentiates from eagles, owls 
   =>
   (guess "snake")
) ; defrule snake

(defrule eagle "grouped in small, carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~n)
   (fly ?e &~n)       ; differentiates from snakes
   (nocturnal ?f &~y) ; differentiates from owls
   =>
   (guess "eagle")
) ; defrule eagle

(defrule owl "grouped in small, carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~n)
   (fly ?e &~n)       ; differentiates from snakes
   (nocturnal ?f &~n) ; differentiates from eagles
   =>
   (guess "owl")
) ; defrule owl

(defrule chicken "grouped in small, non-carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (sting ?e &~y) ; differentiates from bees
   (beak ?f &~n)  ; differentiates from butterflies
   =>
   (guess "chicken")
) ; defrule chicken

(defrule butterfly "grouped in small, non-carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (sting ?e &~y) ; differentiates from bees
   (beak ?f &~y)  ; differentiates from chickens
   =>
   (guess "butterfly")
) ; defrule butterfly

(defrule bee "grouped in small, non-carnivorous, land non-mammals"
   (mammal ?a &~y)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (sting ?e &~n) ; differentiates from chicken, butterfly
   =>
   (guess "bee")
) ; defrule bee

(defrule human "grouped in small, non-carnivorous, non-four-legged land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?f &~y)
   (longTail ?g &~y) ; differentiates from monkeys
   =>
   (guess "human")
) ; defrule human

(defrule monkey "grouped in small, non-carnivorous, non-four-legged land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?f &~y)
   (longTail ?g &~n) ; differentiates from humans
   =>
   (guess "monkey")
) ; defrule monkey

(defrule beaver "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~y) ; differentiates from koalas, sheep, foxes, deers, pig
   =>
   (guess "beaver")
) ; defrule beaver

(defrule koala "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~n)    ; differentiates from beavers
   (marsupial ?g &~n) ; differentiates from sheep, pigs, deers, foxes
   =>
   (guess "koala")
) ; defrule koala

(defrule sheep "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~n)    ; differentiates from beavers
   (marsupial ?g &~y) ; differentiates from koalas
   (wool ?h &~n)      ; differentiates from pigs, deers, foxes
   =>
   (guess "sheep")
) ; defrule sheep

(defrule fox "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~n)    ; differentiates from beavers
   (marsupial ?g &~y) ; differentiates from koalas
   (wool ?h &~y)      ; differentiates from sheep
   (longTail ?i &~n)  ; differentiates from pigs, deers
   =>
   (guess "fox")
) ; defrule fox

(defrule pig "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~n)    ; differentiates from beavers
   (marsupial ?g &~y) ; differentiates from koalas
   (wool ?h &~y)      ; differentiates from sheep
   (longTail ?i &~y)  ; differentiates from foxes
   (antler ?j &~y)    ; differentiates from deers
   =>
   (guess "pig")
) ; defrule pig

(defrule deer "grouped in small, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)
   (forest ?f &~n)    ; differentiates from beavers
   (marsupial ?g &~y) ; differentiates from koalas
   (wool ?h &~y)      ; differentiates from sheep
   (longTail ?i &~y)  ; differentiates from foxes
   (antler ?j &~n)    ; differentiates from pigs
   =>
   (guess "deer")
) ; defrule deer

(defrule cat "grouped in small, carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~n)
   (domesticated ?e &~n)
   =>
   (guess "cat")
) ; defrule cat

(defrule weasel "grouped in small, carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~y)
   (carnivore ?d &~n)
   (domesticated ?e &~y)
   =>
   (guess "weasel")
) ; defrule weasel

(defrule walrus "grouped in large, non-carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~y) ; differentiates from kangaroos, pandas, elephants, horses, cows
   =>
   (guess "walrus")
) ; defrule walrus

(defrule horse "grouped in large, non-carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~n) ; differentiates from walruses
   (forest ?f &~y)   ; differentiates from kangaroos, pandas, elephants
   (mane ?g &~n)     ; differentiates from cow
   =>
   (guess "horse")
) ; defrule horse

(defrule cow "grouped in large, non-carnivorous, four-legged, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~n) ; differentiates from walruses
   (forest ?f &~y)   ; differentiates from kangaroos, pandas, elephants 
   (mane ?g &~y)     ; differentiates from horse
   =>
   (guess "cow")
) ; defrule cow

(defrule kangaroo "grouped in large, non-carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)  ; differentiates from walruses
   (forest ?f &~n)    ; differentiates from horses, cows
   (marsupial ?g &~n) ; differentiates from pandas, elephants
   =>
   (guess "kangaroo")
) ; defrule kangaroo

(defrule panda "grouped in large, non-carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)  ; differentiates from walruses
   (forest ?f &~n)    ; differentiates from horses, cows
   (marsupial ?g &~y) ; differentiates from kangaroos
   (tusks ?h &~y)     ; differentiates from elephant
   =>
   (guess "panda")
) ; defrule panda

(defrule elephant "grouped in large, non-carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~y)
   (fourLegs ?e &~n)  ; differentiates from walruses
   (forest ?f &~n)    ; differentiates from horses, cows
   (marsupial ?g &~y) ; differentiates from kangaroos
   (tusks ?h &~n)     ; differentiates from pandas
   =>
   (guess "elephant")
) ; defrule elephant

(defrule lion "grouped in large, carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~n)
   (forest ?e &~y) ; differentiates from tigers
   (mane ?f &~n)   ; differentiates from polar bears
   =>
   (guess "lion")
) ; defrule lion

(defrule polarBear "grouped in large, carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~n)
   (forest ?e &~y) ; differentiates from tigers
   (mane ?f &~y)   ; differentiates from lions
   =>
   (guess "polar bear")
) ; defrule polarBear

(defrule tiger "grouped in large, carnivorous, land mammals"
   (mammal ?a &~n)
   (land ?b &~n)
   (biggerThanHuman ?c &~n)
   (carnivore ?d &~n)
   (forest ?e &~n) ; differentiates from lions, polar bears
   =>
   (guess "tiger")
) ; defrule tiger

/********************************
**
** Functions
**
*/

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
** This function returns the lowercased first character in the user's response when the user is asked the given question.
** 
** Function assumes ?q is a string that is the question to ask the user along with the number of questions asked so far
*/
(deffunction firstToken (?question)
   (return (lowcase (sub-string 1 1 (askQuestion ?question))))
)

/*
** This function returns "y" for yes, "n" for no, and "u" for unknown. It will continuously ask the user to re-input their answer
** if their answer does not match the above characters: "y", "n", or "u".
** 
** Function assumes ?q is a string that is the question to ask the user along with the number of questions asked so far
*/
(deffunction validate (?q)
   (bind ?char (firstToken ?q))
   (while (not (or (= ?char "y") (= ?char "n") (= ?char "u"))) do
      (printline "Please enter a Y for yes, U for unknown, or N for no")
      (bind ?char (firstToken ?q))
   )

   (return ?char)
) ; deffunction validate

/*
** This function returns a symbol y if the first character is a Y or y, n if the first character is a N or n,
** and u if the first character is a U or u. It will validate the user's answers by calling validate.
** It also increments the number of questions asked by calling incrementQ.
** 
** Function assumes ?prompt is a string that is the question to ask the user
*/
(deffunction checkQuestion (?prompt)
   (incrementQ)
   
   (bind ?char (validate (str-cat ?*NO_OF_QUESTIONS* ". " ?prompt)))

   (return (sym-cat ?char))
) ; deffunction checkQuestion

/*
** This function prints to the user whether or not the system has guessed the animal based on ?bool.
** Then, it exits the program.
** 
** Function assumes ?bool is a boolean, TRUE or FALSE
*/
(deffunction win (?bool)
   (if ?bool then
      (printline (str-cat "I won in " ?*NO_OF_QUESTIONS* " questions!"))
     else
      (printline "I don't know the animal. I lose.")
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
   (bind ?correct (firstToken (str-cat "Is your animal a(n) " ?animal)))
   (if (= ?correct "y") then
      (win TRUE)
    else
      (if (= ?correct "n") then
         (win FALSE)
      )
   )

   (return)
) ; deffunction guess