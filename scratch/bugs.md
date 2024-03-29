## Bugs I found with the generator

1. It generated this program

```
Procedure fun1
Argument var1
If False
Then
Else
Result ()

Procedure main
Fork [
       fun1((1 + -1))
     ]
Result ()
```

Which of course makes no sense. However, the C code inflooped where it was supposed to be able to
handle it anyway. The problem was the code generator. When a process is forked it is placed in the
ready queue and then the current process will increment its program counter and return, yielding to
the new forked process. However, forking only works like this if you fork more than one process. If
you are forking only one process the new process will be immediately called by the current process. The
call will not return before the child is run, so there is no chance for the parent to increment its
program counter. The fix was to make sure that if only one process is forked, the program counter
is incremented before the call, while if more than one process is forked the program counter should
be incremented after they are forked.

2. Function prototypes were for some programs generated in the wrong place, namely after their first reference point. The fix was to collect all structs and function prototypes during code generation and make sure that they are the first thing that appears in the C file.

3. I was pattern-matching only on the scenario where BinderAnn had captured variable names for us. I had to add the more general pattern and make sure to generate fresh names if BinderAnn hasn't run, which is the case with the generator.

4. I had nonexhaustive-patterns in the part of the interpreter that evaluated expressions. I Had to finish the function definition.

5. The interpreter would infinitely loop if a program does not terminate. In order to be able to cease execution if needed I rewrote it to give output lazily instead. It still infinitely loops but I don't have to wait for the computation to finish before I get the output.

6. When I implemented the interpreter I did not consider a variable 'written' when it was created, so if I create a reference r and then check @r, the result would be false. It appears that the RTS considers variables to be written to in the same instant where they are created, so I changed the interpreter to act accordingly.

## Bugs I found after changing the AST to be more easily manipulated

7. I had forgotten to include a return statement in the generated enter function, resulting in the scheduler throwing an assertion error. Caught by a generated program.

8. I had wrongly specified the output format of the generated c code program, so the result parser did not work properly.

9. Bug in the code generator where returns where not inserter properly at all places. Whenever the program should block it needs to return, pending future rescheduling into the readyqueue. This is the program the produced the bug:

```Haskell
Program {main = "fun2", args = [Right ("ref1",Ref TInt),Left (-1 < 1),Right ("ref3",Ref TBool)],

funs = fromList [("fun1",

Procedure {name = "fun1", arguments = [], body = [ If (3 == -1)
                                                    [Fork [("fun1",[]),("fun1",[])]]
                                                    [Fork [("fun1",[])]]
                                                 , If ((1 - 1) < (- -1))
                                                    [If (0 < 0) [] []]
                                                    [If False 
                                                      []
                                                      []
                                                    ]
                                                 ]}),("fun2",
Procedure {name = "fun2", arguments = [("ref1",Ref TInt),("var2",TBool),("ref3",Ref TBool)],
      body = [ If ((1 * 3) < (-2 - 0))
                 [NewRef (Fresh "v0") (Ref TBool) ((-3 == -1) == (0 < 2))]
                 [Wait [("ref1",Ref TInt)]]
             , After ((-3 * -3) + 1) ("ref3",Ref TBool) (-1 < 1)
             , NewRef (Fresh "v1") (Ref TInt) ((- 3) * (-2 + 2))
             ]})]}
```

In fun2 the second branch in the if-statement is chosen, but rather than blocking there the codegenerator just kept on evaluating everything after the if. Clearly an issue.

10. Found bug in the interpreter where I had forgotten to make the interpreter output the final result of the input variables once the program had finished executiing. The traces between the two different runs did not share the same suffix.

11. Found a bug in the interpreter where it did not print the outputreferences in the end properly. It only worked if the process the program started evaluating first was also the last one to be evaluated before the program terminated. If the last statement executed by any process in the program is a blocking one, and the entrypoint was not in the readyqueue, it would not print the state of the inputreferences properly.

12. Found a bug in the codegenerator where the state of the inputreferences to the program was printed incorrectly. Integers were printed with the formatter %u while it should have been %d.

13. It managed to shrink a true monster of a program down to this very short one that produced a bug:

```Haskell
Program:
  entrypoint: fun17
  arguments: [Right ("ref9",Ref TBool)]

fun17(("ref9",Ref TBool))
  NewRef (Fresh "v5") (Ref TBool) ((1 < -9) == (1 == 1))
  After ((((- 1) * (1 + 1)) * ((- 1) * (1 + 1))) + 1) ("ref9",Ref TBool) (1 < 1)
  After (((1 * 1) * (1 * 1)) + 1) ("v5",Ref TBool) (1 < 1)
fun3()

After rewriting the expressions to a more readable format we get:

Program:
  entrypoint: fun17
  arguments: [Right ("ref9",Ref TBool)]

fun17(("ref9",Ref TBool))
  NewRef (Fresh "v5") (Ref TBool) false
  After 5 ("ref9",Ref TBool) false
  After 2 ("v5",Ref TBool) false
fun3()
```

It is easy to see that it should first perform the update at time 2, and then the update at time 5. The
interpreter did it in the other order, meaning there's a bug in the interpreter.

The bug was in the nextEventTime function, which is supposed to return the time of the next event scheduled
to happen.

This is the old version:
```Haskell
-- | Inspects the eventqueue and returns the next event time.
nextEventTime :: Interp s Int
nextEventTime = do
    evs <- gets events
    return $ foldl max 0 (map at evs)
```

Clearly it does the exact opposite, which is proof that there is more than an insignificant amount of
neanderthal DNA in me. The fix is to get the minimum instead.

```Haskell
return $ foldl min maxBound (map at evs)
```

14. Found a bug in the runtime system where an activation record could be deallocated with dangling pointers. If you register an event on a local variable and then terminate that process, the event will remain in the event queue while the reference to the variable to update is the same one. The fix is to track down all local variables in the event queue before deallocating the activation record, and removing them if there exists any scheduled update. Since we are using a priority queue this incurs a linear time search through the event queue. Perhaps another data structure is more appropriate.

15. I found a bug in the interpreter when I ran the following program `e2 inputref`:

```Haskell
e1 :: Ref Int64 -> SSM ()
e1 = box "e1" ["ref2"] $ \ref2 -> do
  ref2 <~ (int64 5)
  wait [ref2]
  ref2 <~ (int64 10)

e2 :: Ref Int64 -> SSM ()
e2 = box "e2" ["ref1"] $ \ref1 -> do
  fork [ e1 ref1, e1 ref1 ]
```

If we start in e2 we will fork two e1, and then they will both block, at which point the program
should terminate as there are no scheduled events and no processes in the ready queue. The interpreter
proceeded past the wait statement instead.

In one place in the report it said that when a variable is written we should only wake up those processes
that are waiting for a update, that have a greater priority than the process that is performing the
update. Here is the patch

```Haskell
writeVar :: Var s -> SSMExp -> Interp s ()
writeVar ref e = do
    (variable,waits, _) <- lift' $ readSTRef ref
    lift' $ writeSTRef variable e -- actually update the variable value

    -- which waiting processes should be woken up? Only those whose priority
    -- is strictly greater than the one who updated the variable
    p <- gets process
    let (towait, keep) = partition (\p' -> priority p < priority p') waits

    -- wake up and desensitize the processes
    mapM_ desensitize towait

    -- update the variable to be written to in this instant and give it knowledge of
    -- which processes are still waiting on it
    lift' $ writeSTRef ref (variable, keep, True)
```
16. QuickCheck generated a program like this one

```Haskell
eventorder :: Ref Int -> Ref Int -> SSM ()
eventorder = box "eventorder" ["r1","r2"] $ \r1 r2 -> do
  after 2 r1 5
  after 2 r2 10
```

The event r1 = 5 should happen first (in the instant) and then r2 = 10. It should not make a difference
I think, but we still want them to do the same thing. The fix was to ensure that events are
inserted in the right order in the interpreter.

```Haskell
schedule_event :: Event s -> Interp s ()
schedule_event e = do
    evs <- gets events
    if any ((==) e) evs
        then let evs' = delete e evs
             in modify $ \st -> st { events = insert e evs' }
        else    modify $ \st -> st { events = insert e evs  }
  where
      insert :: Event s -> [Event s] -> [Event s]
      insert e []       = [e]
      insert e1 (e2:es) =
          if at e1 < at e2
              then e1 : e2 : es
              else e2 : insert e1 es
```

Right now the two queues are in the interprer just lists that we make sure to insert things into
in an ordered fashion, but there should really be a better datastructure for this. A tree or something.

17. It generated a program that was quite large, even after shrinking. The interpreter crashed.

```Haskell
entrypoint:
  fun5(ref17, ref20, ref23, ref26, ref27, (42 + 21), ref30, ref31, ref33, ref38, ref41, ref43)

fun4(*bool ref11, *bool ref18, *uint64 ref20) {
  *int v2 = var ((37 - 1) * (1 - 200))
  bool v3 = *ref18
  fork [fun4(ref11, ref18, ref20)]
  wait [ref18]
  if((1 < 1)) {
  } else {
  }
}

fun5(*int ref17, *uint64 ref20, *int64 ref23, *bool ref26, *bool ref27, int var28, *bool ref30, *int ref31, *int ref33, *bool ref38, *int ref41, *uint64 ref43) {
  *int v0 = var var28
  after 1228 then ref38 = ((182 + 1) < (1 - 147))
  bool v1 = @ref26
  *bool v2 = var ((153 * 1) < (1))
  fork [fun6(ref31, ((1 * 1) + (191 * 168)), ref41, ref33, ref31, ref30, v0, ref43, ref20, ref17, ref20, v0, ref38, v2, ref26, ref23, ref27, (85 + 1))]
  if(((True == True) == (1 == 1))) {
  } else {
    bool v3 = *ref27
  }
  int64 v5 = *ref23
  uint64 v6 = *ref20
}

fun6(*int ref6, int var9, *int ref11, *int ref22, *int ref24, *bool ref27, *int ref28, *uint64 ref29, *uint64 ref31, *int ref34, *uint64 ref37, *int ref38, *bool ref39, *bool ref40, *bool ref42, *int64 ref45, *bool ref46, int var47) {
  int v0 = *ref28
  *bool v1 = var ((1 == 1) == (1 == 1))
  ref39 = ((1 + 1) == (1 - 1))
  bool v2 = @ref34
  ref42 = True
  after 2132 then ref38 = ((84 + 57) - (1 + var9))
  fork [fun4(ref40, ref39, ref31), fun6(ref24, ((v0 + 1) * (1 - 27)), ref34, ref6, ref11, ref27, ref22, ref37, ref29, ref38, ref37, ref11, ref40, ref39, ref42, ref45, ref40, (1 + 84))]
}
```

The problem was that the interpreter crashed when it evaluated (v0 + 1) * (1 - 27) in fun6. So, we see that
v0 is the same as the value of ref28. When we forked fun6, ref28 was applied to the value v0 in 
fun5. In fun5 the value v0 was created by creating a new reference with the initial value of var28.
When we evaluated that expression in fun6, we got an error saying that var28 was not bound in the current
process. Super wierd!

What should happen is that whenever we create a reference and put some initial value inside it, we must
evaluate that value! We are doing this in every place where we write to references, but I had forgotten
to do this when we created a new reference. If the initial value to a reference was in fact some variable
that we've gotten from another process etc, some issue arose where the name var28 was not resolved to its
actual value.

18. Apparently Haskells `Int` type is not actually 32 bit, despite it saying so all over the internet. Running `maxBound :: Int` quickly reveals that they are in fact 64 bits. There was some issue where the code generator would overflow its values while the interpreter would not. The fix is to just switch all `Int` points to using actual `Int32` instead.

19. When a process terminates any outstanding events on the local references must be deallocated. I did this in the interpreter but I had forgotten to update the counter that keeps track of how many events
there are in the event queue.

```Haskell
leave :: Interp s ()
leave = do
    ...
    -- need to dequeue event on local references before we leave
    let lrefs = Map.elems $ localrefs p
    todeq <- flip filterM lrefs $ \r -> do
        (_,_,_,mt,_) <- lift' $ readSTRef r
        return $ isJust mt
    modify $ \st -> st { events = events st \\ todeq }
    ...
```

became

```Haskell
leave :: Interp s ()
leave = do
    ...
    -- need to dequeue event on local references before we leave
    let lrefs = Map.elems $ localrefs p
    todeq <- flip filterM lrefs $ \r -> do
        (_,_,_,mt,_) <- lift' $ readSTRef r
        return $ isJust mt
    modify $ \st -> st { events    = events st \\ todeq
                       , numevents = numevents st - length todeq
                       }
    ...
```

20. I had a bug in the interpreter related to the max length of the continuation queue. The generated C code uses a dummy parent process to fork the initial entrypoint of the program, and thus the length of the continuation queue is 1 when the program starts initially. The c program reported an error when the queue length was overrun, but the interpreter still thought it had room for 1 more process. The solution was to start the continuation count at 1 when interpretation begins.

21. Bug in the code generator. When a variable is initialized the event time is set to ULONG_MAX. The code generator initialized a variable only when it was first used, but if this was in e.g an else branch that was never executed, the variable would have remained uninitialized. When the call to dequeue_event was made before leaving the RTS inspects the event time to figure out if it was in the event queue (and should thus be removed). If it is not in the event queue the event time value is
ULONG_MAX. However, without initializing the variable this value is 0, tricking the RTS into deallocating an event that is not there.

So, initially the generated code looked like this. Notice that the variable v3 is not initialized in the
step function and that it is instead initialized when we are interpreting the `NewRef` constructor,
which is a result of the `var` combinator.

```c
typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int64_t* ref6;
    sv_int_t v3;
} act_fun8_t;

act_fun8_t* enter_fun8(act_t* caller, uint32_t priority, uint8_t depth, sv_int64_t* ref6);
void step_fun8(act_t* gen_act);


act_fun8_t* enter_fun8(act_t* caller, uint32_t priority, uint8_t depth, sv_int64_t* ref6) {
    act_fun8_t* act = (act_fun8_t*) enter(sizeof(act_fun8_t), step_fun8, caller, priority, depth);
    act->ref6 = ref6;
    return act;
}

void step_fun8(act_t* gen_act) {
    act_fun8_t* act = (act_fun8_t*) gen_act;
    switch(act->pc) {
        case 0:
            if (!((true == true))) goto L0;
            if (!((207 == 181))) goto L2;
            goto L3;
            
            L2:
            
            L3:
            goto L1;
            
            L0:
            SCHEDULE(&later_int64, act->ref6, now + 4393UL, (-1L));
            initialize_int(&act->v3);
            assign_int(&act->v3, act->priority, ((1 - 99) - (1 - 42)));
            
            L1:
            SCHEDULE(&later_int64, act->ref6, now + 3753UL, ((29237L + (-35431L)) * (6301L - (-9233L))));
        case 1:
        dequeue_event((sv_t *)&act->v3);
        leave((act_t *) act, sizeof(act_fun8_t));
    }
}
```

The fix is to initialize all local references in the enter function rather than the first time that the
variable is used. This was we ensure that whenever they are referenced in the step function they will
always have been initialized properly.

```c
typedef struct {
    ACTIVATION_RECORD_FIELDS;
    sv_int64_t* ref6;
    sv_int_t v3;
} act_fun8_t;

act_fun8_t* enter_fun8(act_t* caller, uint32_t priority, uint8_t depth, sv_int64_t* ref6);
void step_fun8(act_t* gen_act);


act_fun8_t* enter_fun8(act_t* caller, uint32_t priority, uint8_t depth, sv_int64_t* ref6) {
    act_fun8_t* act = (act_fun8_t*) enter(sizeof(act_fun8_t), step_fun8, caller, priority, depth);
    act->ref6 = ref6;
    initialize_int(&act->v3);
    return act;
}

void step_fun8(act_t* gen_act) {
    act_fun8_t* act = (act_fun8_t*) gen_act;
    switch(act->pc) {
        case 0:
            if (!((true == true))) goto L0;
            if (!((207 == 181))) goto L2;
            goto L3;
            
            L2:
            
            L3:
            goto L1;
            
            L0:
            SCHEDULE(&later_int64, act->ref6, now + 4393UL, (-1L));
            assign_int(&act->v3, act->priority, ((1 - 99) - (1 - 42)));
            
            L1:
            SCHEDULE(&later_int64, act->ref6, now + 3753UL, ((29237L + (-35431L)) * (6301L - (-9233L))));
        case 1:
        dequeue_event((sv_t *)&act->v3);
        leave((act_t *) act, sizeof(act_fun8_t));
    }
}
```

22. There was a bug in the interpreter. I initially used a `Bool` in each variable to keep track on if they had been written to in the current instant. The problem with this approach is that it then becomes important to make sure to 'reset' this Bool before each instant begins. There were some issues where the input references to a program were not reset properly. The easiest fix was to change from a Bool to a `Word64`, where the `Word64` signifies which instant the variable was most recenty written to in. Then there is no need to reset the variables as the @-operator is now just implemented as a comparison between the stored time and now.

23. Now we are stepping into strange territory. Some things in C have no guarantees at all, and are just referred to as undefined behavior. In C, I believe signed overflows are undefined behavior, so the compiler can choose to do anything. So far the goal of testing has been to make sure that our interpreter and generated C-code behaves exactly the same.

Quickcheck generated this program:
```
entrypoint:
  fun2(ref2, (107 + 58), (138 * 205))

fun2(*bool ref2, int var4, int var7) {
  *bool v1 = var True
  var4 = ((166 + var4) * (var7 * var4))
  ref2 = ((var4 + var4) < (var7 - var7))
  wait [v1]
}
```
It is small and straightforward, but a subtlety is that the left operand to the `<` operand overflows. More precisely, the value _should_ overflow but when in conjunction with the `<` operator, the C compiler decides to completely remove the addition from the generated asembler code (on my machine). The result of this program (final state of `ref2`) when compiled to C is 0, while the Haskell interpreter reports it as 1.

Look at this small C-program:

```c
#include <stdio.h>

void main() {
    int a = 1545058350;
    int b = 28290;
    printf("%d\n", (1545958350 + 1545058350) < (28290 - 28290));
    printf("%d\n", (a + a) < (b - b));
}
```
On my machine the output is
```
1
0
```
On Joel's machine, the output is
```
1
1
```
What seems to happen is that it just inspects the sign bit on the left operand or something, and notices that an overflow occurred. From my generated assembler Joel deduced that it optimized away the addition alltogether. It is undefined behaviour, so there's no issue really, but we are comparing the results of the C code with the interpreter in Haskell, and the interpreter in Haskell behaves as I would expect, in that it performs the addition (which overflows) before it performs the comparison, so that the result will be `True`.

There is a hack around this to ensure that they at least do the same thing.
```c
#include <stdio.h>

void main() {
    int a = 1545058350;
    int b = 28290;
    printf("%d\n", (1545958350 + 1545058350) < (28290 - 28290));
#ifdef DEBUG
    int lop = a + a;
    int rop = b - b;
    printf("%d\n", lop < rop);
#else
    printf("%d\n", (a + a) < (b - b));
#endif DEBUG
}
```
If I compile this with the `DEBUG` flag now (which should probably named `TESTING` or something, as that is what I am using it for), it would perform the addition (which overflows) and then do the comparison. This alignes with what is happening in the Haskell interpreter. If we compile it without debug we would get the 'normal' undefined behavior.

One alternative would be to include special cases for this operator (and any other we discovery to have this behavior) in the interpreter, but since mine and Joel's machine behaved differently here it seems like the above solution is probably the best. At least it behaves the same on both of our machines, but we are entering undefined land after all, so maybe this whole issue is pointless?

I have fixed this with a macro like this:

```c
#ifdef DEBUG

int add(int a, int b) { return a + b; }

#define ADD(a,b) add(a,b)

#else

#define ADD(a,b) (a + b)

#endif
```

and changed all occurences of `a + b` to `ADD(a,b)`.

24. This program produces a bug. First it will register an event at time 5 and apply that, but then three events will be registered until second instant is over. Two of these events are for the same time (10), while one is earlier. When we insert things into the ready queue we will insert it in the end and then sift it up. When we insert the new event if will end up at the back and then be replaced with the earliest event. This will however break the order between the two events scheduled for time 10. It should not matter, however, in which order the events of an instant is applied. All that it does is to update the variable values and potentially schedule sensitized processes. Since the processes are inserted in a ready queue, the order should not matter w.r.t enqueue/dequeue operations. I fixed this by making sure that when I compare two traces I don't care about the order the events _at the same instant_ is applied, just that the same events are applied. Order is irrelevant.

```Haskell
program = bfun3 inputref inputref inputref

bfun1 :: Ref Int64 -> Exp Int64 -> SSM ()
bfun1 = box "bfun1" ["ref2","var5"] $ \ref2 var5 -> do
  after 5 ref2 $ (var5 * 1)
  wait [ref2]

bfun3 :: Ref Int64 -> Ref Int64 -> Ref Int64 -> SSM ()
bfun3 = box "bfun3" ["ref3","ref4","ref5"] $ \ref3 ref4 ref5 -> do
  fork [ bfun1 ref4 1]
  fork [ bfun3 ref3 ref3 ref5
       , bfun4 ref5 ref5
       , bfun1 ref4 0
       ]

bfun4 :: Ref Int64 -> Ref Int64 -> SSM ()
bfun4 = box "bfun4" ["ref1","ref4"] $ \ref1 ref4 -> do
  after 1 ref4 1
  wait [ ref1 ]
  fork [ bfun4 ref1 ref4 ]
```