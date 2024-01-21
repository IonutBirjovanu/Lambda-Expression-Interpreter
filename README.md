# Lambda-Expression-Interpreter
This project is a Haskell interpreter for lambda expressions. Lambda expressions are a powerful way of representing functions in a concise form. The interpreter supports the evaluation of lambda expressions, parsing expressions, and handling macros within a computational context.

## Table of Contents
### 1.Lambda Expressions
### 2.Running the REPL
### 3.Testing

## 1.Lambda Expressions
Lambda expressions are represented using the following data type:
<sub>data Expr = Variable String
          | Function String Expr
          | Application Expr Expr</sub>

## 2.Running the REPL
To interact with the interpreter, run the following command:
<sub>runhaskell main.hs</sub>
In the REPL we can write our own lambda functions and macros and evaluate them:
### Expressions:
<sub>(\x.\y.\z.x (y z))</sub> **or** <sub>λx.λy.λz.x y z</sub>
### Macros:
<sub>true = \x.\y.x</sub> **or** <sub>false = λx.λy.y</sub>
To use the macros, use them with <sub>$</sub> in your expressions: <sub>$and $true $false</sub>

## 3.Testing
Run the provided test script to check the correctness of the implementation:
<sub>runhaskell test.hs</sub>
