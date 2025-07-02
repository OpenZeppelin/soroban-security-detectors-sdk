use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct InnerBlock;

#[contractimpl]
impl InnerBlock {
    pub fn some(env: Env, k: Symbol) {
        if true {
            let max = env.storage().persistent().max_ttl();
            env.storage().persistent().extend_ttl(&k, 0, max);
        }
    }
}
