# Lambda-Expression-Interpreter
This project is a Haskell interpreter for lambda expressions. Lambda expressions are a powerful way of representing functions in a concise form. The interpreter supports the evaluation of lambda expressions, parsing expressions, and handling macros within a computational context.

## 1.Lambda Expressions
Lambda expressions are represented using the following data type:

```data Expr = Variable String```

```          | Function String Expr```

```          | Application Expr Expr```

## 2.Running the REPL
To interact with the interpreter, run the following command:
<sub>runhaskell main.hs</sub>
In the REPL we can write our own lambda functions and macros and evaluate them:
### Expressions:
```(\x.\y.\z.x (y z))``` **or** ```λx.λy.λz.x y z```
### Macros:
```true = \x.\y.x``` **or** ```false = λx.λy.y```
To use the macros, use them with ```$``` in your expressions: ```$and $true $false```

## 3.Testing
Run the provided test script to check the correctness of the implementation:
```runhaskell test.hs```
