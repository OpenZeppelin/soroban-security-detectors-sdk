#![no_std]
use soroban_sdk::{contract, contractimpl, log, symbol_short, Env, Symbol};

#[contract]
pub struct ContractContract;

fn get_condition(env: &Env, key: &Symbol) -> bool {
    let storage = env.storage();
    storage.temporary().has(key)
}

#[contractimpl]
impl ContractContract {
    pub fn way_aaa(env: Env, to: Symbol) -> Vec<Symbol> {
        let storage = env.storage();
        let key = symbol_short!("key");
        let condition = get_condition(&env, &key);
        if condition {
            let value: Vec<Symbol> = storage.temporary().get(&key).unwrap();
            return value;
        }
        vec![]
    }
}
