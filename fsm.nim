import tables, strutils, options

type
  StateActions = tuple[entryAction: Option[Callback], exitAction: Option[Callback]]

  Callback = proc(): void
  StateEvent[S,E] = tuple[state: S, event: E]
  Transition[S] = tuple[nextState: S, action: Option[Callback]]

  Machine[S,E] =  ref object of RootObj
    initialState: S
    currentState: Option[S]
    stateActions: array[S, StateActions]
    transitions: TableRef[StateEvent[S,E], Transition[S]]
    transitionsAny: TableRef[S, Transition[S]]
    defaultTransition: Option[Transition[S]]

  TransitionNotFoundException = object of Exception

proc setCurrentState[S,E](m: Machine[S,E], nextState: S) =
  if m.currentState.isSome:
    if m.stateActions[m.currentState.get].exitAction.isSome:
      get(m.stateActions[m.currentState.get].exitAction)()
  m.currentState = some(nextState)
  if m.stateActions[m.currentState.get].entryAction.isSome:
    get(m.stateActions[m.currentState.get].entryAction)()

proc reset*[S,E](m: Machine[S,E]) =
  m.setCurrentState m.initialState

proc setInitialState*[S,E](m: Machine[S,E], state: S) =
  m.initialState = state

proc addStateActions*[S,E](m: Machine[S,E], state: S, entryAction: Callback = nil, exitAction: Callback = nil) =
  let 
    entry = if entryAction == nil: none(Callback) else: some(entryAction)
    exit = if exitAction == nil: none(Callback) else: some(exitAction)

  m.stateActions[state] = (entry, exit)

proc newMachine*[S,E](initialState: S): Machine[S,E] =
  result = new(Machine[S,E])
  result.transitions = newTable[StateEvent[S,E], Transition[S]]()
  result.transitionsAny = newTable[S, Transition[S]]()
  result.setInitialState(initialState)

proc addTransitionAny*[S,E](m: Machine[S,E], state: S, nextState: S) =
  m.transitionsAny[state] = (nextState, none(Callback))

proc addTransitionAny*[S,E](m: Machine[S,E], state, nextState: S, action: Callback) =
  m.transitionsAny[state] = (nextState, some(action))

proc addTransition*[S,E](m: Machine[S,E], state: S, event: E, nextState: S) =
  m.transitions[(state, event)] = (nextState, none(Callback))

proc addTransition*[S,E](m: Machine[S,E], state: S, event: E, nextState: S, action: Callback) =
  m.transitions[(state, event)] = (nextState, some(action))

proc setDefaultTransition*[S,E](m: Machine[S,E], state: S) =
  m.defaultTransition = some((state, none(Callback)))

proc setDefaultTransition*[S,E](m: Machine[S,E], state: S, action: Callback) =
  m.defaultTransition = some((state, some(action)))

proc getTransition*[S,E](m: Machine[S,E], event: E, state: S): Transition[S] =
  let map = (state, event)
  if m.transitions.hasKey(map):
    result = m.transitions[map]
  elif m.transitionsAny.hasKey(state):
    result = m.transitionsAny[state]
  elif m.defaultTransition.isSome:
    result = m.defaultTransition.get
  else: raise newException(TransitionNotFoundException, "Transition is not defined: Event($#) State($#)" % [$event, $state])

proc getCurrentState*[S,E](m: Machine[S,E]): auto =
  m.currentState.get

proc process*[S,E](m: Machine[S,E], event: E) =
  let transition = m.getTransition(event, m.currentState.get)
  if transition[1].isSome:
    get(transition[1])()
  m.setCurrentState transition[0]
  #echo event, " ", m.currentState.get


when isMainModule:

  type
    StateName = enum
      SOLID
      LIQUID
      GAS
      PLASMA

    Event = enum
      MELT
      EVAPORATE
      SUBLIMATE
      IONIZE

  var m = newMachine[StateName, Event](LIQUID)

  proc cb() =
    echo "i'm evaporating"

  var condition: bool

  proc enterLiquid() =
    echo "entering liquid state"

  proc exitLiquid() =
    echo "exiting liquid state"

  proc enterGas() =
    if condition:
      echo "entering gas state and ionizing immediately"
      m.process(IONIZE)
    else:
      echo "entering gas state"

  proc exitGas() =
    echo "exiting gas state"

  proc enterPlasma() =
    echo "entering plasma state"


  m.addTransition(SOLID, MELT, LIQUID)
  m.addTransition(LIQUID, EVAPORATE, GAS, cb)
  m.addTransition(SOLID, SUBLIMATE, GAS)
  m.addTransition(GAS, IONIZE, PLASMA)
  m.addTransition(SOLID, MELT, LIQUID)

  m.addStateActions(LIQUID, entryAction=enterLiquid, exitAction=exitLiquid)
  m.addStateActions(PLASMA, enterPlasma)
  m.addStateActions(GAS, enterGas, exitGas)

  # to "start" the fsm
  # this is necessary
  m.reset

  assert m.getCurrentState() == LIQUID
  condition = false
  m.process(EVAPORATE)
  assert m.getCurrentState() == GAS
  m.process(IONIZE)
  assert m.getCurrentState() == PLASMA
  echo "\nreseting\n"

  m.reset

  assert m.getCurrentState() == LIQUID
  condition = true
  m.process(EVAPORATE)
  assert m.getCurrentState() == PLASMA, $m.getCurrentState()


