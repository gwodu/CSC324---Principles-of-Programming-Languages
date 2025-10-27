# Type Check Function Requirements

## Overview

The `typeCheck` function implements static type checking for the λ-calculus with various features including:
- Booleans and Naturals
- Conditionals
- Arithmetic operations
- Functions (lambda abstraction and application)
- Records
- Products
- Sums (tagged unions)

## Function Signature

```haskell
typeCheck :: TcContext -> Term -> TcResult
```

- **Input**: A type checking context (list of types) and a term to check
- **Output**: Either `TcOk Type` if the term is well-typed, or `TcError String` if there's a type error

## Context Management

- `TcContext` is a list of types representing the environment: `type TcContext = [Type]`
- The context is indexed by DeBruijn indices (variables reference positions in the context)
- Use `lookupCtx :: Int -> TcContext -> Maybe Type` to look up variable types

## Typing Rules

### 1. Booleans (TmTrue, TmFalse)
```haskell
typeCheck ctx TmTrue  = TcOk TyBool
typeCheck ctx TmFalse = TcOk TyBool
```
- Always return `TyBool` regardless of context

### 2. Naturals (TmZero, TmSucc, TmPred, TmIsZero)
```haskell
typeCheck ctx TmZero = TcOk TyNat
typeCheck ctx (TmSucc t) = 
  case typeCheck ctx t of
    TcOk TyNat -> TcOk TyNat
    _ -> TcError "succ expects Nat"
    
typeCheck ctx (TmPred t) = 
  case typeCheck ctx t of
    TcOk TyNat -> TcOk TyNat
    _ -> TcError "pred expects Nat"

typeCheck ctx (TmIsZero t) = 
  case typeCheck ctx t of
    TcOk TyNat -> TcOk TyBool
    _ -> TcError "iszero expects Nat"
```
- `TmZero` always has type `TyNat`
- `TmSucc` and `TmPred` expect a `Nat` argument and return `Nat`
- `TmIsZero` expects a `Nat` argument and returns `Bool`

### 3. Conditionals (TmIf)
```haskell
typeCheck ctx (TmIf cond thenBranch elseBranch) =
  case typeCheck ctx cond of
    TcOk TyBool ->
      case typeCheck ctx thenBranch of
        TcOk t1 ->
          case typeCheck ctx elseBranch of
            TcOk t2 | t1 == t2 -> TcOk t1
                     | otherwise -> TcError "branches must have same type"
            _ -> TcError "else branch must be well-typed"
        _ -> TcError "then branch must be well-typed"
    _ -> TcError "condition must be Bool"
```
- Condition must type check to `TyBool`
- Both branches must be well-typed and have the same type
- Return the common type of the branches

### 4. Variables (TmVar)
```haskell
typeCheck ctx (TmVar k) =
  case lookupCtx k ctx of
    Just ty -> TcOk ty
    Nothing -> TcError ("variable " ++ show k ++ " not in context")
```
- Look up the variable in the context using its DeBruijn index
- If not found, return an error

### 5. Lambda Abstraction (TmAnnAbs)
```haskell
typeCheck ctx (TmAnnAbs paramType body) =
  case typeCheck (paramType : ctx) body of
    TcOk bodyType -> TcOk (TyArrow paramType bodyType)
    err -> err
```
- Add the parameter type to the context (shift context)
- Check the body in the extended context
- Return an arrow type from parameter to body type

### 6. Application (TmApp)
```haskell
typeCheck ctx (TmApp func arg) =
  case typeCheck ctx func of
    TcOk (TyArrow argType resultType) ->
      case typeCheck ctx arg of
        TcOk argType' | argType == argType' -> TcOk resultType
        TcOk _ -> TcError "argument type mismatch"
        err -> err
    TcOk _ -> TcError "expected function type"
    err -> err
```
- Function must be an arrow type
- Argument type must match function's domain type
- Return the function's codomain type

### 7. Records (TmRecord)
```haskell
typeCheck ctx (TmRecord fields) =
  let checkField (label, term) = 
        case typeCheck ctx term of
          TcOk ty -> Just (label, ty)
          _ -> Nothing
  in case mapM checkField fields of
       Just fieldTypes -> TcOk (TyRecord fieldTypes)
       Nothing -> TcError "all fields must be well-typed"
```
- All field terms must be well-typed
- Return a record type with the same labels and their types
- Empty records are allowed (`TyRecord []`)

### 8. Record Projection (TmProj)
```haskell
typeCheck ctx (TmProj record label) =
  case typeCheck ctx record of
    TcOk (TyRecord fields) ->
      case lookup label fields of
        Just ty -> TcOk ty
        Nothing -> TcError ("label " ++ label ++ " not found")
    _ -> TcError "projection expects record type"
```
- Record must have type `TyRecord`
- Label must exist in the record
- Return the type of the projected field

### 9. Pairs (TmPair, TmFst, TmSnd)
```haskell
typeCheck ctx (TmPair t1 t2) =
  case (typeCheck ctx t1, typeCheck ctx t2) of
    (TcOk ty1, TcOk ty2) -> TcOk (TyProd ty1 ty2)
    (TcError msg, _) -> TcError msg
    (_, TcError msg) -> TcError msg

typeCheck ctx (TmFst pair) =
  case typeCheck ctx pair of
    TcOk (TyProd ty1 _) -> TcOk ty1
    TcOk _ -> TcError "fst expects product type"
    err -> err

typeCheck ctx (TmSnd pair) =
  case typeCheck ctx pair of
    TcOk (TyProd _ ty2) -> TcOk ty2
    TcOk _ -> TcError "snd expects product type"
    err -> err
```
- Pairs create product types
- Projections require product types and return the appropriate component

### 10. Sums (TmInl, TmInr, TmMatch)
```haskell
typeCheck ctx (TmInl term sumType) =
  case sumType of
    TySum leftType _ ->
      case typeCheck ctx term of
        TcOk termType | termType == leftType -> TcOk sumType
        _ -> TcError "inl argument type mismatch"
    _ -> TcError "inl expects sum type"

typeCheck ctx (TmInr term sumType) =
  case sumType of
    TySum _ rightType ->
      case typeCheck ctx term of
        TcOk termType | termType == rightType -> TcOk sumType
        _ -> TcError "inr argument type mismatch"
    _ -> TcError "inr expects sum type"

typeCheck ctx (TmMatch sumTerm leftBranch rightBranch) =
  case typeCheck ctx sumTerm of
    TcOk (TySum leftType rightType) ->
      case typeCheck (leftType : ctx) leftBranch of
        TcOk t1 ->
          case typeCheck (rightType : ctx) rightBranch of
            TcOk t2 | t1 == t2 -> TcOk t1
                     | otherwise -> TcError "match branches must have same type"
            err -> err
        err -> err
    _ -> TcError "match expects sum type"
```
- `TmInl` and `TmInr` inject into sum types
- Argument type must match the corresponding side of the sum type
- `TmMatch` deconstructs sums
- Both branches are checked with the appropriate type in context
- Both branches must have the same type

## Error Messages

Error messages should be descriptive but the exact text may vary. The function should return `TcError` for:
- Type mismatches
- Missing variables in context
- Applying operations to wrong types
- Projecting missing labels
- Not matching expected types

## Additional Notes

1. **Empty contexts**: Some operations (like checking lambda body, match branches) extend the context, while others (like variable lookup) may fail with empty contexts

2. **Context extension**: When checking abstractions and match cases, the new type is added to the **front** of the context (at index 0)

3. **DeBruijn indices**: The context is indexed from left to right with the leftmost type being index 0

4. **All-or-nothing**: If any subterm fails to type check, return that error immediately

5. **Type equality**: Use `(==)` to check type equality (available because `Type` derives `Eq`)

## Testing

The implementation must pass all tests in `TypeCheckTests.hs`, including:
- Basic type tests (TmTrue, TmFalse, TmZero)
- Arithmetic operations
- Conditionals
- Variables and contexts
- Lambda abstractions
- Function applications
- Records
- Pairs
- Sums

## Implementation Tips

1. Pattern match on the `Term` constructor
2. Use `case` expressions to handle `TcResult`
3. Use guards or equality checks to validate types
4. Build up arrow types and record types as you traverse
5. Extend context with `:` for new bindings
6. Use `lookupCtx` for safe variable access

