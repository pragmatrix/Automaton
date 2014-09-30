module Automaton

(*
    A port of 
    https://github.com/evancz/automaton/blob/master/Automaton.elm
*)


let inline private uncurry f = fun (a, b) -> f a b

(*
    This library is for structuring reactive code. The key concepts come
    directly from [Arrowized FRP][afrp]. It is not yet clear how
    valuable this is, so it is a great domain for experimentation and iteration
    to see if we can make it a really useful tool.

    This library aims to be a simple and minimal API that will help you get
    started with Arrowized FRP (AFRP), which can be very hard to understand
    from just the academic papers. From there, let us know on [the mailing
    list](https://groups.google.com/forum/#!forum/elm-discuss) if you wrote
    a larger program with it or have ideas of how to extend the API.

    [afrp]: http://haskell.cs.yale.edu/wp-content/uploads/2011/02/workshop-02.pdf
*)

type Automaton<'i, 'o> = 
    Step of ('i -> Automaton<'i, 'o> * 'o)


(* 
    Step an automaton forward once with a given input.

    Say we start with the `count` automaton, which begins with the counter at zero.
    When we run `step 42 count` we get back a new automaton with the counter at
    1 and the value 1. The original `count` automaton is unchanged, so we need to
    use the new automaton to use the latest state.
*)

let step i (Step f) = f i

(* 
    Compose two automatons into a pipeline. For example, lets say we have a way
    to create a ship out of wood and a way to gather wood and build a ship.

        gatherWood : Automaton Trees Wood
        buildShip  : Automaton Wood  Ship

        createShip : Automaton Trees Ship
        createShip = gatherWood >>> buildShip
*)

let rec (>>>) f g =
    Step <| fun a ->
        let (f', b) = step a f
        let (g', c) = step b g
        in
            (f' >>> g', c)

(* 
    Compose two automatons into a pipeline. For example, lets say we have a way
    to create a ship out of wood and a way to gather wood and build a ship.

        gatherWood : Automaton Trees Wood
        buildShip  : Automaton Wood  Ship

        createShip : Automaton Trees Ship
        createShip = buildShip <<< gatherWood
*)

let rec (<<<) f g =
    Step <| fun a ->
        let (f', b) = step a f
        let (g', c) = step b g
        in
            (g' <<< f', c)

(*
    Take a single input and branch it out into two different results.

        buildShip  : Automaton Wood Ship
        buildHouse : Automaton Wood House

        build : Automaton Wood (Ship,House)
        build = branch buildShip buildHouse
*)

let rec branch f g =
    Step <| fun a ->
        let (f', b) = step a f
        let (g', c) = step a g
        in
            (branch f' g', (b, c))

(* 
    Combine two independent automatons. The new automaton takes a pair of
    inputs and produces a pair of outputs. In this case we convert two separate
    values into two separate piles of wood:

    tsunami : Automaton Ship  Wood
    tornado : Automaton House Wood

    disaster : Automaton (Ship,House) (Wood,Wood)
    disaster = pair tsunami tornado
*)

let rec pair f g = 
    Step <| fun (a, b) ->
        let (f', c) = step a f
        let (g', d) = step b g
        in
            (pair f' g', (c, d))

(*
    Create an automaton that takes in a tuple and returns a tuple, but only
    transform the *first* thing in the tuple.

        build       : Automaton Wood (Ship,House)
        upgradeShip : Automaton Ship Yacht

        buildNicer : Automaton Wood (Yacht,House)
        buildNicer = build >>> first upgradeShip

    It may be helpful to know about the following equivalence:

        first upgradeShip == pair upgradeShip (pure id)
*)

let rec first auto = 
    Step <| fun (i, ex) ->
        let (f, o) = step i auto
        in
            (first f, (o, ex))

(*
    Create an automaton that takes in a tuple and returns a tuple, but only
    transform the *first* thing in the tuple.

        build        : Automaton Wood (Ship,House)
        upgradeHouse : Automaton House Palace

        buildNicer : Automaton Wood (Ship,Palace)
        buildNicer = build >>> second upgradeHouse

    It may be helpful to know about the following equivalence:

        second upgradeHouse == pair (pure id) upgradeHouse
*)

let rec second auto = 
    Step <| fun (ex, i) ->
        let (f, o) = step i auto
        in 
            (second f, (ex, o))

(*
    Create an automaton with no memory. It just applies the given function to
    every input.
*)

let rec pure f = Step (fun x -> (pure f, f x))

(*
    Create an automaton that takes a branched input and merges it into a single
    output.

        disaster : Automaton (Ship,House) (Wood,Wood)
        pileWood : Wood -> Wood -> Wood

        disasterRelief : Automaton (Ship,House) Wood
        disasterRelief = disaster >>> merge pileWood

    It may be helpful to notice that merge is just a variation of `pure`:

        merge plieWood == pure (\(logs,sticks) -> pileWood logs sticks)

*)

let merge f = pure (uncurry f)

(*
    Turn an automaton into a loop, feeding some of its output back into itself!
    This is how you make a stateful automaton the hard way.

        data Feelings = Happy | Sad

        stepPerson : (Action, Feelings) -> (Reaction, Feelings)

        person : Automaton Action Reaction
        person = loop Happy (pure stepPerson)

    This example is equivalent to using `hiddenState` to create a `person`, but the
    benefit of loop is that you can add state to *any* automaton. We used
    `(pure stepPerson)` in our example, but something more complex such as
    `(branch f g >>> merge h)` would work just as well with `loop`.
*)

let rec loop state auto =
    Step <| fun input ->
        let (auto', (output,state')) = step (input,state) auto
        in 
            (loop state' auto', output)


(*
    Combine a list of automatons into a single automaton that produces a
    list.
*)

let rec combine autos =
    Step <| fun a ->
        let (autos', bs) = List.unzip (List.map (step a) autos)
        in 
            (combine autos', bs)

(*
    Create an automaton with state. Requires an initial state and a step
    function to step the state forward. For example, an automaton that counted
    how many steps it has taken would look like this:

        count : Automaton a Int
        count = state 0 (\_ c -> c+1)

    It is a stateful automaton. The initial state is zero, and the step function
    increments the state on every step.
*)

let rec state s f =
    Step <| fun x ->
        let s' = f x s
        in
            (state s' f, s')


(*
    Create an automaton with hidden state. Requires an initial state and a
    step function to step the state forward and produce an output.

        data Feelings = Happy | Sad

        stepPerson : Action -> Feelings -> (Reaction, Feelings)

        person : Automaton Action Reaction
        person = hiddenState Happy stepPerson

    Notice that a `person` has feelings, but like [the
    Behaviorists](http://en.wikipedia.org/wiki/Behaviorism), we do not need to
    worry about that as an outside observer.
*)

let rec hiddenState s f =
    Step <| fun x ->
        let (s',out) = f x s
        in
            (hiddenState out f, s')

(*
    Count the number of steps taken. 
*)

let count<'a> : Automaton<'a, int> = state 0 (fun _ c -> c + 1)

type private Queue<'t> = 't list * 't list

let private empty : Queue<'t> = ([],[])

let private enqueue x (en,de) = (x::en, de)

let rec private dequeue q =
    match q with
    | ([],[]) -> None
    | (en,[]) -> dequeue ([], List.rev en)
    | (en,hd::tl) -> Some (hd, (en,tl))

(*
    Computes the running average of the last `n` inputs. 
*)

let average k =
    let step n (ns, len, sum) =

        let stepFull n (ns,len,sum) =
            match dequeue ns with
            | None -> (0., (ns,len,sum))
            | Some (m,ns') ->
                let sum' = sum + n - m
                in  
                    ((sum' / float len), (enqueue n ns', len, sum'))

        if len = k
        then stepFull n (ns,len,sum)
        else ((sum+n) / (len+1 |> float), (enqueue n ns, len+1, sum+n))
    in
        hiddenState (empty,0,0.) step
