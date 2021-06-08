use stateright::*;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum PathType {
    Hill,
    Slide,
    Death,
    Blank,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum Location {
    START,
    NWO,
    NO,
    NNEO,
    ENEO,
    EO,
    SEO,
    SO,
    SWO,
    WO,
    NWI,
    NI,
    NNEI,
    ENEI,
    EI,
    SEI,
    SI,
    GOAL,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
enum IsUsed {
    Unused,
    Used,
}

type Cards = Vec<IsUsed>;
fn init_cards() -> Cards {
    vec![IsUsed::Unused; 6]
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct Puzzle {
    alive: bool,
    loc: Location,
    rem_cards: Cards,
    rem_rolls: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum PAct {
    Roll,
    Putt(usize),
}

fn handle_path(state: Puzzle, pt: PathType, length: usize, dst: Location) -> Option<Puzzle> {
    if state.rem_rolls >= length {
        Some(Puzzle {
            alive: state.alive,
            loc: dst,
            rem_cards: state.rem_cards,
            rem_rolls: state.rem_rolls - length,
        })
    } else {
        match pt {
            PathType::Hill => Some(Puzzle {
                alive: state.alive,
                loc: state.loc,
                rem_cards: state.rem_cards,
                rem_rolls: 0,
            }),
            PathType::Slide => Some(Puzzle {
                alive: state.alive,
                loc: dst,
                rem_cards: state.rem_cards,
                rem_rolls: 0,
            }),
            PathType::Death => Some(Puzzle {
                alive: false,
                loc: state.loc,
                rem_cards: state.rem_cards,
                rem_rolls: 0,
            }),
            PathType::Blank => None,
        }
    }
}

impl Model for Puzzle {
    type State = Puzzle;
    type Action = PAct;

    fn init_states(&self) -> Vec<Self::State> {
        vec![Puzzle {
            alive: true,
            loc: Location::START,
            rem_rolls: 0,
            rem_cards: init_cards(),
        }]
    }

    fn actions(&self, last_state: &Self::State, actions: &mut Vec<Self::Action>) {
        if last_state.alive {
            if last_state.rem_rolls != 0 {
                actions.push(PAct::Roll)
            } else {
                for (idx, card) in last_state.rem_cards.iter().enumerate() {
                    match card {
                        IsUsed::Used => (),
                        IsUsed::Unused => actions.push(PAct::Putt(idx + 4)),
                    }
                }
            }
        }
    }

    fn next_state(&self, last_state: &Self::State, action: Self::Action) -> Option<Self::State> {
        let last_state = last_state.clone();
        match action {
            PAct::Putt(strength) => {
                let cards = last_state
                    .rem_cards
                    .iter()
                    .enumerate()
                    .map(|(idxp, cardp)| {
                        if strength - 4 == idxp {
                            IsUsed::Used
                        } else {
                            *cardp
                        }
                    })
                    .collect();
                Some(Puzzle {
                    alive: last_state.alive,
                    loc: last_state.loc,
                    rem_cards: cards,
                    rem_rolls: strength,
                })
            }
            PAct::Roll => {
                use Location::*;
                use PathType::*;
                match last_state.loc {
                    START => handle_path(last_state, Hill, 4, NWO),
                    NWO => handle_path(last_state, Slide, 4, NO),
                    NO => handle_path(last_state, Slide, 3, NNEO),
                    NNEO => handle_path(last_state, Death, 3, ENEO),
                    ENEO => handle_path(last_state, Death, 2, EO),
                    EO => handle_path(last_state, Slide, 4, SEO),
                    SEO => handle_path(last_state, Hill, 4, SO),
                    SO => handle_path(last_state, Slide, 3, SWO),
                    SWO => handle_path(last_state, Hill, 3, WO),
                    WO => handle_path(last_state, Hill, 3, NWI),
                    NWI => handle_path(last_state, Slide, 3, NI),
                    NI => handle_path(last_state, Blank, 1, NNEI),
                    NNEI => handle_path(last_state, Death, 2, ENEI),
                    ENEI => handle_path(last_state, Blank, 1, EI),
                    EI => handle_path(last_state, Slide, 2, SEI),
                    SEI => handle_path(last_state, Hill, 2, SI),
                    SI => handle_path(last_state, Blank, 4, GOAL),
                    GOAL => handle_path(last_state, Death, 2, GOAL),
                }
            }
        }
    }

    fn properties(&self) -> Vec<Property<Self>> {
        vec![Property::<Self>::sometimes("solved", |_, state| {
            state.loc == Location::GOAL
        })]
    }
}

fn main() {
    Puzzle {
        alive: true,
        loc: Location::START,
        rem_rolls: 0,
        rem_cards: init_cards(),
    }
    .checker()
    .serve("localhost:3000");
}
