use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct DoubleAlias;

#[contractimpl]
impl DoubleAlias {
    pub fn double_alias(env: Env, key: Symbol) {
        let a = env.storage().persistent().max_ttl();
        let b = a;
        let c = b;
        env.storage().persistent().extend_ttl(&key, 4, c);
    }
}
