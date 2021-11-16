use clap::Parser;
use stateright::*;

mod double;
mod double_lock;
mod general;
mod general_lock;
mod single;

#[derive(Debug, Parser)]
enum Variant {
    Single,
    Double,
    DoubleLock,
    General {
        #[clap(long, default_value = "3")]
        num_threads: usize,
    },
    GeneralLock {
        #[clap(long, default_value = "3")]
        num_threads: usize,
    },
}

#[derive(Debug, Parser)]
struct Options {
    /// Address to serve stateright ui on.
    #[clap(long, default_value = "localhost:3000")]
    address: String,

    #[clap(subcommand)]
    variant: Variant,
}

fn main() {
    let options = Options::parse();

    println!("Serving on http://{}", options.address);

    match options.variant {
        Variant::Single => {
            single::State::default().checker().serve(options.address);
        }
        Variant::Double => {
            double::State::default().checker().serve(options.address);
        }
        Variant::DoubleLock => {
            double_lock::State::default()
                .checker()
                .serve(options.address);
        }
        Variant::General { num_threads } => {
            general::State::new(num_threads)
                .checker()
                .serve(options.address);
        }
        Variant::GeneralLock { num_threads } => {
            general_lock::State::new(num_threads)
                .checker()
                .serve(options.address);
        }
    }
}
