# Glados EPITECH Project #

## Qu'est-ce que Glados ? ##

Le projet Glados est un projet de programmation fonctionnelle en Haskell. Le but de ce projet est d'implémenter un langage de programmation de notre propre conception, en Haskell.

## Il est divisé en deux parties: ##

Une partie LISP qui consiste à implémenter un interpreteur pour un sous-ensemble du langage LISP.
Et une partie Glados où on doit implémenter notre propre langage de programmation.

# Première partie du projet #

- [x] Parser
- [x] CI/CD
- [x] Tests unitaires
- [x] Un début d'AST (Abstract Syntax Tree)
- [x] Un début d'évaluateur

## Overview Parser ##

Le parser du projet Glados est un outil conçu pour analyser et transformer le code source écrit dans un langage de programmation spécifique en une représentation structurée. Cette représentation peut être utilisée pour une variété de tâches, telles que l'interprétation, la compilation ou l'analyse statique du code.

## USAGE ##

./glados < <file>

Remplacez <file> par le chemin du fichier contenant le code source que vous souhaitez parser.

## Exemple de fichier d'entrée ##

(define x 42)
(* 42 x)

## Sortie du Parser ##

[CptList [CptSymbols "define", CptSymbols "x", CptInt 42], CptList [CptSymbols "*", CptInt 42, CptSymbols "x"]]

## Overview CI/CD ##

La CI/CD dans ce projet est composé de plusieurs parties, tout d'abord une partie build qui compile le projet et qui vérifie que la compilation s'est bien déroulée. Ensuite une partie test qui vérifie que les tests unitaires se sont bien déroulés. Après une partie mirror qui permet de mettre à jour le dépôt github d'EPITECH. Et enfin une partie release qui permet de créer une release sur le github mirror.

## Overview Tests unitaires première partie ##

Les tests unitaires dans ce projet sont composés de plusieurs parties:
Pour lancer les tests unitaires il faut utiliser la commande `make tests_run`

On a des tests sur:

- la tokenisation
- conversion des tokens au format CPT
- les opérations d'évaluation
- l'identification des variables

## Comment fonctionne le projet première partie ##

On fait cette commmande:

./glados < \<file>

## ---------------------------------------- ###

## Objectif deuxième partie ##

Le but étant de transformer notre lisp en nouveau langage et de créer une VM permettant de compiler le parsing de ce nouveau langage en liste d'instrcution dans un fichier et d'exectuer son résultat

## Réparition des tâches deuxième partie du projet ##

- [x] Parser Nouveau langage
- [x] CI/CD
- [x] Tests unitaires
- [x] Tests fonctionnels
- [x] Compiler Vm
- [x] Executer Vm

## USAGE ##

./glados < \<file>

Remplacez \<file> par le chemin du fichier contenant le code source dont vous souhaitez obtenir le résultat.

## Exemple de fichier d'entrée ##

define x 42;
\+ x 32;

## Résultat de fichier d'entrée ##

• -> Parsing du nouveau langage:
[AstDefine (Left "x") (AstInteger 42),AstCall [AstSymbol "+",AstSymbol "x",AstInteger 32]]

• -> Compilation du nouveau langage:
Define
PushEnv x
Push VInt 42
PushEnv x
Push VInt 32
Push VOp Add
Call

• -> Execution du nouveau langage:
74

## Overview Parser ##

Parse le nouveau langage en enlevant des [] et () du lisp et en ajoutant des ;

## Overview VM ##
Gère les opérations sous formes de builtin.
La VM récupère le parsing du nouveau langage et les transforme en liste d'instruction unique et stock cette liste en le compilant dans un fichier.
La VM peut également récupérer ce fichier compiler et executer cette liste d'instruction pour y obtenir un résultat de calcul.

## Overview Tests unitaires deuxième partie ##

Les tests unitaires dans ce projet sont composés de plusieurs parties:
Pour lancer les tests unitaires il faut utiliser la commande `make tests_run`

On a des tests sur:

- la tokenisation
- conversion des tokens au format CPT
- les opérations d'évaluation
- l'identification des variables
- les operations de la vm
- le parsing

## Overview CI/CD ##

Identique à la première partie


## Comment fonctionne le projet deuxième partie ##

On fait cette commmande:

./glados < \<file>
