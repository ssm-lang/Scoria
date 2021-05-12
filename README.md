# Embedded DSL for the Sparse Synchronous Model
---

This repository contains an embedding of the Sparse Synchronous Model. There is a lot of hacking and experimenting happening currently so some things might look wierd!

### Installing
---
Clone the repository
```
stack build
stack repl
```

### EDSL API
---
The intention is that users of the language will be guided into not doing malicious things, such as constructing illegal terms or programs. Users will get access to the primitives to write programs by importing the module `Frontend` in their files.


The API exposed by the frontend module at the moment is the following:

There are three types that represents the kind of values that we can construct. They are references, expressions and literals. The polymorphic type `a` indicates what type the expression has. E.g `Exp Int` represents an integer expression, and `Exp Word8` represents an expression of an unsigned 8bit integer.
```Haskell
newtype Ref a = ...
newtype Exp a = ...
newtype Lit a = ...
```
To be able to write literals such as `2` and having them be promoted to e.g something of type `Exp Int`, we use a type class.
```Haskell
class FromLiteral a where
instance FromLiteral Int where
instance FromLiteral Int64 where
instance FromLiteral Word64 where
instance FromLiteral Word8 where
```
With this instance (which is not exported, users are not intended to use it directly), we can implement a `Num` instance for expressions. If we now write e.g `2 + 5 :: Exp Word8` it will be turned into an expression in our language. Note that since for arithmetic operators we use Haskell's `Num` instance, we can not add boolean expressions together etc.
```Haskell
instance (Num a, FromLiteral a, SSMType a) => Num (Exp a) where
```
When a program is either interpreted or compiled we need to be able to apply it. Let's say that we have written the program `myfib` which has the type `Exp Int -> Ref Int -> SSM ()`, and we want to interpret this. We need to apply that function to two values before we get an `SSM ()` that we can interpret. For this we have created an `inputref` value which essentially says that this is an input reference to the main entrypoint of the program. You would now apply it like this: `myfib 13 inputref :: SSM ()`.
```Haskell
inputref :: forall a. SSMType a => Ref a
```
We can assign expressions to variables and expressions to references, and to be able to expose a single function which does both we use a type class. This is one of the points where the frontend can be further improved. The type does specify that the assignment operation must be well typed, which is nice, but e.g the expression that is receiving the new value must of course be a variable, but that is not expressed in the type. As we discussed with Koen, when we have DSL's we have two phases of typechecking. First we have Haskells phase, and then that of the DSL. Making sure that we are assigning a value to a variable is something that we very easily can do in the second phase, but doing it in the first phase would require us to add some additional information in the type of `Exp`. I am of the feeling that this would clutter the types unecessarily and make them look very scary, and that it might be better to leave it to phase two. We can implement both ways and see what we think though! This change would only affect the frontend, so none of the other compiler phases would need to be altered.
```Haskell
class Assignable a b where
instance Assignable (Exp a) (Exp a) where
instance Assignable (Ref a) (Exp a) where
```
Two operators for comparing expressions. I am not sure we can use the regular `Eq` or `Ord` instances here. E.g `(==) :: a -> a -> Bool` would in our case be `(==) :: Exp a -> Exp a -> Bool`, meaning that to implement this function we would need to go from our expression language to a normal `Bool` in the Haskell language. We can not do this until we are interpreting an expression, unless the expression only contains literals (which we don't know at this point).
```Haskell
(<.)  :: SSMType a => Exp a -> Exp a -> Exp Bool
(==.) :: SSMType a => Exp a -> Exp a -> Exp Bool
```
We can negate numeric expressions. Here we can (but we don't right now) use an additional constraint to specify that only signed integer types can be negated. This is very trivial to add.
```Haskell
neg        :: (Num a, SSMType a) => Exp a -> Exp a
```
Usually by writing something like `2` Haskell can infer what type the `a` in `Exp a` can be, but if for some reason not enough type information exists to be able to make the inference, these function create the expression explicitly instead.
```Haskell
int        :: Int -> Exp Int
int64      :: Int64 -> Exp Int64
uint64     :: Word64 -> Exp Word64
word8      :: Word8 -> Exp Word8
```
Boolean literals.
```Haskell
true'      :: Exp Bool
false'     :: Exp Bool
```
Now, when we are writing our programs we need to be able to collect the sequence of statements that make up a program. We use a custom monad to do this, `SSM a`. The frontend is designed such that a user will not have to care anything about this monad, only use its monad instance, `return` & `(>>=)`. Internally this monad maintains a list of statements collected so far, a counter used to generate fresh names and so on.

To create variables you use the `var` combinator. It needs an expression that represents the references initial value. We can get access to references only in two ways; either we get them as a parameter to our function or we create them with `var`. I think of `var` references as _local references_. They are deallocated then a procedures terminates, while the ones given as arguments are not deallocated. _All_ references in a program is created either with `var`, or are given to the program in some main entrypoint with `inputref`.
```Haskell
var        :: Exp a -> SSM (Ref a)
deref      :: Ref a -> SSM (Exp a)
```
We can `wait` for _any_ reference in a list of references to receive a write. This is a place where we need to think of something clever. Right now there is an `a` in the type of `wait`, meaning that it must unify with _one_ type during typechecking. This function can not wait of references of different types! The underlying AST representation of wait does not have this limitation, so it's only in the frontend. You can use `HList`s but then there would need to be some wrapping and unwrapping, which will clutter the code. I need to discuss this with Koen to figure out a nice way of doing this.
```Haskell
wait       :: [Ref a] -> SSM ()
```
You can schedule a future write to a variable with `after`.
```Haskell
after      :: Exp Word64 -> Ref a -> Exp a -> SSM ()
```
You can fork processes with `fork`. A very nice thing here is that the things we fork are just applied functions! No need to use some fancy machinery, just normal Haskell function application.
```Haskell
fork       :: [SSM ()] -> SSM ()
```
To see if a variable has been written to in the current instant we can use `changed`. Right now this is a statement, and can not be used directly as an expression. I can't remember why I designed it like this, but I don't believe changing it to be an expression is difficult, if we want to do that instead.
```Haskell
changed    :: Ref a -> SSM (Exp Bool)
```
Conditional executions. The `if'` _might_ have an else. It might look neater to just make two functions here instead, like `ifThen` and `ifThenElse`.
```
if'        :: Exp Bool -> SSM () -> Maybe (SSM ()) -> SSM ()
while'     :: Exp Bool -> SSM () -> SSM ()
```
Since this is a EDSL we can very easily add derived operators! Since `wait` takes a list of references and waits for a write to any of them, we might also want an operator that waits for _all_ of the references.
```Haskell
waitSingle :: Ref a -> SSM ()
waitSingle = box "waitSingle" ["r"] $ \r -> wait [r]

waitAll :: [Ref a] -> SSM ()
waitAll refs = fork $ map waitSingle refs
```

### Writing Programs
---

To write a program a user needs to create a Haskell module and import the frontend.
```Haskell
module Fib where
import Frontend
```

In Stephens paper there is a very interesting version of computing fibonacci numbers. First we need to create the `mywait` function that takes a single reference and waits for it.
```Haskell
mywait :: Ref int -> SSM ()
mywait = box "mywait" ["r"] $ \r -> do
  wait [r]
```
We use a function `box :: String -> [String] -> (a -> SSM ()) -> (a -> SSM ())` to to tell the `SSM` monad that this is a new, separate procedure that other procedures can fork. The first argument is the name of the procedure, and the second argument is a list of the argument names. Right now we need to do this manually, but if we have written something like this instead
```Haskell
mywait r = wait [r]
```
all the information is essentially already there to transform it to the version that uses `box`. It should definitely be possible to write a simple compiler plugin that does this. We'll see what we do! For now we need to write it ourselves :)

Then we need to add the `mysum` procedure, which takes three references. It will wait for the first two references to be written to and then write their sum to the third reference. We use our `waitAll` combinator to wait for all the references.
```Haskell
mysum :: Ref Int -> Ref Int -> Ref Int -> SSM ()
mysum = box "mysum" ["r1", "r2", "r"] $ \r1 r2 r -> do
    waitAll [r1,r2]
    v1 <- deref r1
    v2 <- deref r2
    after 1 r (v1 + v2)
```
Lastly we have the `myfib` procedure itself. We use `var` to create two local references which we can then share with forked processes. Some nice things is that we can use ordinary integer literals and have them automatically be promoted to the proper `Exp a` variant. Forking processes is simply normal Haskell function application, so there's no need to use some special operator to apply arguments to procedures.
```Haskell
myfib :: Exp Int -> Ref Int -> SSM ()
myfib = box "myfib" ["n", "r"] $ \n r -> do
    r1 <- var 0 
    r2 <- var 0
    if' (n <. 2)
            (after 1 r 1)
            (Just (fork [ myfib (n - 1) r1
                        , myfib (n - 2) r2
                        , mysum r1 r2 r
                        ]
                  )
            )
```

Now, if we want to run e.g `myfib 13 inputref` we have something of type `SSM ()`. The interpreter has type `Program -> Output`, and the compiler has type `Bool -> Just Int -> Program -> String`. To turn a `SSM ()` into a `Program` we can use `transpile :: SSM () -> Program`. It takes the program captured by the frontend and turns it into a slightly flatter version where the statements are a bit simpler.

If we want to prettyprint the program above we can use `prettyProgram :: Program -> String`.
```
> putStrLn $ prettyProgram $ transpile $ myfib 13 inputref
entrypoint:
  myfib(13, r)

myfib(int n, *int r) {
  int v0 = var 0
  int v1 = var 0
  if((n < 2)) {
    after 1 then r = 1
  } else {
    fork [myfib((n - 1), v0), myfib((n - 2), v1), mysum(v0, v1, r)]
  }
}

mysum(*int r1, *int r2, *int r) {
  fork [waitSingle(r1), waitSingle(r2)]
  int v0 = *r1
  int v1 = *r2
  after 1 then r = (v0 + v1)
}

waitSingle(*int r) {
  wait [r]
}
```
This looks nice and is very readable, but it's a shame that the variable names we chose in `myfib`, namely `r1` and `r2`, are not in the generated code. Fortunately for us, a colleague of Koen, Joel & I (Agustin Mista) has written a plugin that will _capture_ names created by monadic `do` expressions. The plugin runs before typechecking and transforms expressions such as
```Haskell
do a <- ma
   b <- mb
   mc
   return (a + b)
```
into
```Haskell
do a <- ma `annotateM` (Just "a", {- some additional location information -})
   b <- mb `annotateM` (Just "b", {- some additional location information -})
   mc
   return (a + b)
```
We can enable this plugin in our file by importing a file and enabling a GHC option. Agustin says he will make the plugin also insert the import statement in the future, so you'll only have to enable the GHC option in the future. Our file now begins like this
```Haskell
{-# OPTIONS_GHC -fplugin BinderAnn.Monadic #-}
module Fib where

import BinderAnn.Monadic
import Frontend
```
and the prettyprinter will now output this code
```
> putStrLn $ prettyProgram $ transpile $ myfib 13 inputref
entrypoint:
  myfib(13, r)

myfib(int n, *int r) {
  int r1 = var 0
  int r2 = var 0
  if((n < 2)) {
    after 1 then r = 1
  } else {
    fork [myfib((n - 1), r1), myfib((n - 2), r2), mysum(r1, r2, r)]
  }
}

mysum(*int r1, *int r2, *int r) {
  fork [waitSingle(r1), waitSingle(r2)]
  int v1 = *r1
  int v2 = *r2
  after 1 then r = (v1 + v2)
}

waitSingle(*int r) {
  wait [r]
}
```

### Interpreting Programs
---
The interpreter has type `interpret :: Program -> Output`. The output is a trace of the actions it's taken. At the end it will print the result of any input references, such as the `inputref` in `myfib 13 inputref`. Running the interpreter and printing the output nicely is done like this.
```Haskell
> putStrLn $ unlines $ map show $ interpret $ transpile $ myfib 5 inputref
Fork ["myfib","myfib","mysum"]
Fork ["myfib","myfib","mysum"]
Fork ["myfib","myfib","mysum"]
Fork ["myfib","myfib","mysum"]
Fork ["waitSingle","waitSingle"]
Fork ["waitSingle","waitSingle"]
Fork ["myfib","myfib","mysum"]
Fork ["waitSingle","waitSingle"]
Fork ["waitSingle","waitSingle"]
Fork ["myfib","myfib","mysum"]
Fork ["myfib","myfib","mysum"]
Fork ["waitSingle","waitSingle"]
Fork ["waitSingle","waitSingle"]
Fork ["waitSingle","waitSingle"]
Instant 0 8
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Event 1 (Lit TInt (LInt 1))
Instant 1 3
Event 2 (Lit TInt (LInt 2))
Event 2 (Lit TInt (LInt 2))
Event 2 (Lit TInt (LInt 2))
Instant 2 2
Event 3 (Lit TInt (LInt 3))
Event 3 (Lit TInt (LInt 3))
Instant 3 1
Event 4 (Lit TInt (LInt 5))
Instant 4 1
Event 5 (Lit TInt (LInt 8))
Instant 5 0
Result "r" (Lit TInt (LInt 8))
```
The `Instant i1 i2` constructor means that instant `i2` just finished executing and that there are `i2` events in the eventqueue.

### Compiling Programs
---
The compiler has type `compile_ :: Bool -> Maybe Int -> Program -> String`. The first argument is a bool that says if you want it to also generate a `main` function i C that can be used to run the program. If the compiled program is compiled with the `-DDEBUG` flag it will also produce a trace like the interpreter. The second argument specifies if you want to limit the execution time of the program to some number of trace items, or if you want to let it run indefinitely. Compiling the `myfib` program is done like this
```Haskell
writeFile "fib.c" $ compile_ True (Just 2500) $ transpile $ myfib 13 inputref
```
and then in `fib.c` we'll find this code
```c
#include "peng-platform.h"
#include "peng.h"
#include <stdio.h>


#ifdef DEBUG
#include <stdint.h>
uint64_t limit = 2500;
#endif
typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t n;
    sv_int_t* r;
    sv_int_t r1;
    sv_int_t r2;
} act_myfib_t;

typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t* r1;
    sv_int_t* r2;
    sv_int_t* r;
    sv_int_t v1;
    sv_int_t v2;
} act_mysum_t;

typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int_t* r;
    trigger_t trig1;
} act_waitSingle_t;

act_myfib_t* enter_myfib(act_t* caller, uint32_t priority, uint8_t depth, int n, sv_int_t* r);
void step_myfib(act_t* gen_act);

act_mysum_t* enter_mysum(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r1, sv_int_t* r2, sv_int_t* r);
void step_mysum(act_t* gen_act);

act_waitSingle_t* enter_waitSingle(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r);
void step_waitSingle(act_t* gen_act);


act_myfib_t* enter_myfib(act_t* caller, uint32_t priority, uint8_t depth, int n, sv_int_t* r) {
    act_myfib_t* act = (act_myfib_t*) enter(sizeof(act_myfib_t), step_myfib, caller, priority, depth);
    initialize_int(&act->n);
    assign_int(&act->n, act->priority, n);
    act->r = r;
    return act;
}

act_mysum_t* enter_mysum(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r1, sv_int_t* r2, sv_int_t* r) {
    act_mysum_t* act = (act_mysum_t*) enter(sizeof(act_mysum_t), step_mysum, caller, priority, depth);
    act->r1 = r1;
    act->r2 = r2;
    act->r = r;
    return act;
}

act_waitSingle_t* enter_waitSingle(act_t* caller, uint32_t priority, uint8_t depth, sv_int_t* r) {
    act_waitSingle_t* act = (act_waitSingle_t*) enter(sizeof(act_waitSingle_t), step_waitSingle, caller, priority, depth);
    act->r = r;
    act->trig1.act = (act_t *) act;
    return act;
}

void step_myfib(act_t* gen_act) {
    act_myfib_t* act = (act_myfib_t*) gen_act;
    switch(act->pc) {
        case 0:
            initialize_int(&act->r1);
            assign_int(&act->r1, act->priority, 0);
            initialize_int(&act->r2);
            assign_int(&act->r2, act->priority, 0);
            if (!((act->n.value < 2))) goto L0;
            later_int(act->r, now + 1UL, 1);
            goto L1;
            
            L0:
            DEBUG_PRINT("fork myfib myfib mysum\n");
            {
                uint8_t new_depth = act->depth - 2;
                if((int8_t) new_depth < 0) {
                    DEBUG_PRINT("crash\n");
                    exit(1);
                }
                uint32_t pinc = 1 << new_depth;
                uint32_t new_priority = act->priority;
                fork_routine((act_t *) enter_myfib((act_t *) act, new_priority, new_depth, (act->n.value - 1), &act->r1));
                new_priority += pinc;
                fork_routine((act_t *) enter_myfib((act_t *) act, new_priority, new_depth, (act->n.value - 2), &act->r2));
                new_priority += pinc;
                fork_routine((act_t *) enter_mysum((act_t *) act, new_priority, new_depth, &act->r1, &act->r2, act->r));
            }
            act->pc = 1;
            return;
        case 1:
            
            L1:
        case 2:
        leave((act_t *) act, sizeof(act_myfib_t));
    }
}

void step_mysum(act_t* gen_act) {
    act_mysum_t* act = (act_mysum_t*) gen_act;
    switch(act->pc) {
        case 0:
            DEBUG_PRINT("fork waitSingle waitSingle\n");
            {
                uint8_t new_depth = act->depth - 1;
                if((int8_t) new_depth < 0) {
                    DEBUG_PRINT("crash\n");
                    exit(1);
                }
                uint32_t pinc = 1 << new_depth;
                uint32_t new_priority = act->priority;
                fork_routine((act_t *) enter_waitSingle((act_t *) act, new_priority, new_depth, act->r1));
                new_priority += pinc;
                fork_routine((act_t *) enter_waitSingle((act_t *) act, new_priority, new_depth, act->r2));
            }
            act->pc = 1;
            return;
        case 1:
            assign_int(&act->v1, act->priority, act->r1->value);
            assign_int(&act->v2, act->priority, act->r2->value);
            later_int(act->r, now + 1UL, (act->v1.value + act->v2.value));
        case 2:
        leave((act_t *) act, sizeof(act_mysum_t));
    }
}

void step_waitSingle(act_t* gen_act) {
    act_waitSingle_t* act = (act_waitSingle_t*) gen_act;
    switch(act->pc) {
        case 0:
            sensitize((sv_t *)act->r, &act->trig1);
            act->pc = 1;
            return;
        case 1:
            desensitize(&act->trig1);
        case 2:
        leave((act_t *) act, sizeof(act_waitSingle_t));
    }
}


void top_return(act_t* act) {
    return;
}

void main() {
    
    act_t top = { .step = top_return };
    sv_int_t r;
    initialize_int(&r);
    r.value = 0;
    fork_routine((act_t *) enter_myfib(&top, PRIORITY_AT_ROOT, DEPTH_AT_ROOT, 13, &r));
    tick();
    DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    while(1) {
        now = next_event_time();
        if(now == NO_EVENT_SCHEDULED)
            break;
        tick();
        DEBUG_PRINT("now %lu eventqueuesize %d\n", now, event_queue_len);
    }
    printf("result r int %d\n", r.value);
}
```
The `DEBUG_PRINT` method is in `peng.h` and looks like this
```c
#ifdef DEBUG
extern uint64_t debug_count;
extern uint64_t limit;
#define DEBUG_PRINT(...) {     \
    if(debug_count >= limit) { \
      exit(1);                 \
    }                          \
    debug_count++;             \
    printf(__VA_ARGS__);       \
}
#else
#define DEBUG_PRINT(x) while(0) {}
#endif
```

### Testing Programs
---
So this is something that I am hacking on right now, so these things change quite fast. However, I've written a function `testSingle :: Program -> Maybe Int -> IO Report` that takes a program, a trace length (if any) and returns an `IO Report`. The `Report` type exists in `Evaluation.hs`. It will report on some different types of errors (compilation errors, execution errors, parse errors etc). `testSingle` will take the program and run both the generated c code and the interpreter and compare the traces. If they are equal `Good` is returned, and otherwise some different variants of errors are returned. If I run this
```Haskell
do r <- testSingle (transpile (myfib 5 inputref)) (Just 10000)
   print r 
   return ()
```
it will print `Good`, which means the test was successful. Now, this only means that the interpreter and the code generator does the same thing. We still pray that they do the right thing ;)
In `Spec.hs` there's a property that generates random programs and runs this test for all of them. You can run the tests by issuing `stack test`.
