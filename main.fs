//
// F# program to input a string and print out information
// about the # of vowels and digraphs in that string.
//
// Name: Daniyal Khokhar
// UIC NetID: 667875169
// 
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

//
// explode:
//
// Given a string s, explodes the string into a list of characters.
// Example: explode "apple" => ['a';'p';'p';'l';'e']
//
let explode (S:string) = 
  List.ofArray (S.ToCharArray())

//
// implode
//
// The opposite of explode --- given a list of characters, returns
// the list as a string. Example: implode ['t';'h';'e'] => "the"
//
let implode (L:char list) = 
  new string(List.toArray L)


let rec length L =
  match L with
  | [] -> 0
  | head::tail -> 1 + length tail

let rec num1Vowel L x = 
  match L with 
  | [] -> 0
  | head::tail when head = x -> 1 + num1Vowel tail x
  | head::tail -> num1Vowel tail x
  

let rec numVowels L = 
  match L with 
    | [] -> 0
    | head::tail when head = 'a' -> 1 + numVowels tail
    | head::tail when head = 'e' -> 1 + numVowels tail
    | head::tail when head = 'i' -> 1 + numVowels tail
    | head::tail when head = 'o' -> 1 + numVowels tail
    | head::tail when head = 'u' -> 1 + numVowels tail
    | head::tail -> numVowels tail

// let rec numDiagraphs L = 
//   match L with 
//     | [] -> 0
//     | headA::headB::tail when headA = 'a' && headB = 'i' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'c' && headB = 'h' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'e' && headB = 'a' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'i' && headB = 'e' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'o' && headB = 'u' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'a' && headB = 'i' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'p' && headB = 'h' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 's' && headB = 'h' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 't' && headB = 'h' -> 1 + numDiagraphs tail
//     | headA::headB::tail when headA = 'w' && headB = 'h' -> 1 + numDiagraphs tail
//     | head::tail -> numDiagraphs tail

let rec diagraphsIndiv L letter1 letter2= 
  match L with 
    | [] -> 0
    | headA::headB::tail when headA = letter1 && headB = letter2 -> 1 + diagraphsIndiv tail letter1 letter2
    | head::tail -> diagraphsIndiv tail letter1 letter2

[<EntryPoint>]
let main argv =
  printfn "Starting"
  printfn ""

  //
  // input string, output length and # of vowels:
  //
  
  
  printf("input> ")
  let input = System.Console.ReadLine()

  let L = explode input
  printfn "exploded: %A" L

  let len = length L
  printfn "length: %A" len

  let num = numVowels L
  printfn "vowels: %A" num

  //
  // TODO: print count of each vowel:
  //
  //let x = ['a'; 'e'; 'i'; 'o'; 'u';]
  
  //let num1V = num1Vowel L 'a'
  //printfn "vowels for A: A%" (num1Vowel L 'a')
  printfn "'a': %A" (num1Vowel L 'a')
  printfn "'e': %A" (num1Vowel L 'e')
  printfn "'i': %A" (num1Vowel L 'i')
  printfn "'o': %A" (num1Vowel L 'o')
  printfn "'u': %A" (num1Vowel L 'u')
  //
  // TODO: print number of digraphs, count of each:
  //
  //printfn "digraphs: %A" (numDiagraphs L)

  printfn "digraphs: %A" ((diagraphsIndiv L 'a' 'i')+(diagraphsIndiv L 'c' 'h')+(diagraphsIndiv L 'e' 'a')+(diagraphsIndiv L 'i' 'e')+(diagraphsIndiv L 'o' 'u')+(diagraphsIndiv L 'p' 'h')+(diagraphsIndiv L 's' 'h')+(diagraphsIndiv L 't' 'h')+(diagraphsIndiv L 'w' 'h'))
  
  //Second part
  
  printfn "'a','i': %A" (diagraphsIndiv L 'a' 'i')
  printfn "'c','h': %A" (diagraphsIndiv L 'c' 'h')
  printfn "'e','a': %A" (diagraphsIndiv L 'e' 'a')
  printfn "'i','e': %A" (diagraphsIndiv L 'i' 'e')
  
  printfn "'o','u': %A" (diagraphsIndiv L 'o' 'u')
  printfn "'p','h': %A" (diagraphsIndiv L 'p' 'h')
  printfn "'s','h': %A" (diagraphsIndiv L 's' 'h')
  printfn "'t','h': %A" (diagraphsIndiv L 't' 'h')
  printfn "'w','h': %A" (diagraphsIndiv L 'w' 'h')


  
  // done: implode list, print, and return
  //
  let S = implode L
  printfn "imploded: %A" S

  printfn ""
  printfn "Done"
  0  // return 0 => success, much like C++
