use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct DifferentExt;

#[contractimpl]
impl DifferentExt {
    pub fn different_ext(env: Env, symbol: Symbol) {
        let maxval = env.storage().persistent().max_ttl();
        env.storage()
            .persistent()
            .extend_ttl(&symbol, maxval, maxval);
    }
}
