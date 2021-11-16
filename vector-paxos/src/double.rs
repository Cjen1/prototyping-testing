use sstd::collections::HashSet;

use stateright::actor::{Actor, ActorModel, DuplicatingNetwork, Id}

type proposerId = Id;
type Value = char;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct BallotNumber {
    proposerid : Id,
    dependencies : HashSet<Ballot>
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum FinitePaxosMsg {
    P1a  {balNum: BallotNumber},
    P1b  {balNum: BallotNumber, maxVBalNum: BallotNumber, maxVBalVal: Value},
    Nack {balNum: BallotNumber},
    P2a  {balNum: BallotNumber, val: Value},
    P2b  {balNum: BallotNumber},
}

enum ProposerSM {
    P1 {responses: Vec<(Id, BallotNumber, Value)>},
    P2 {Value},
    Commit {Value},
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct ProposerState {
    balNum : BallotNumber,
    state : ProposerSM,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
struct AcceptorState {
    maxBalNum  : BallotNumber,
    maxVBalNum : BallotNumber,
    maxVBalVal : Value,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum NodeState {
    Proposer {state : ProposerState},
    Acceptor {state : AcceptorState},
}



#[derive(Debug, Clone, Copy, Default, Hash)]
pub struct State {
    i: u8,
    t1: u8,
    t2: u8,
    pc1: u8,
    pc2: u8,
}

impl Model for State {
    type State = State;
    type Action = Action;

    fn init_states(&self) -> std::vec::Vec<<Self as stateright::Model>::State> {
        vec![State {
            i: 0,
            t1: 0,
            t2: 0,
            pc1: 1,
            pc2: 1,
        }]
    }

    fn actions(&self, _state: &Self::State, actions: &mut Vec<Self::Action>) {
        actions.append(&mut vec![Action::Read, Action::Write]);
    }

    fn next_state(&self, last_state: &Self::State, action: Self::Action) -> Option<Self::State> {
        match action {
            Action::Read if last_state.pc1 == 1 => Some(State {
                pc1: 2,
                t1: last_state.i,
                ..*last_state
            }),
            Action::Write if last_state.pc1 == 2 => Some(State {
                pc1: 3,
                t1: last_state.t1,
                i: last_state.t1 + 1,
                ..*last_state
            }),
            Action::Read if last_state.pc2 == 1 => Some(State {
                pc2: 2,
                t2: last_state.i,
                ..*last_state
            }),
            Action::Write if last_state.pc2 == 2 => Some(State {
                pc2: 3,
                t2: last_state.t2,
                i: last_state.t2 + 1,
                ..*last_state
            }),
            _ => None,
        }
    }

    fn properties(&self) -> Vec<Property<Self>> {
        vec![Property::<Self>::eventually("fin", |_, state| {
            state.pc1 == 3 && state.pc2 == 3 && state.i == 2
        })]
    }
}
