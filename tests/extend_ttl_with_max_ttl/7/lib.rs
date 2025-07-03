use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct BlockScope;

#[contractimpl]
impl BlockScope {
    pub fn block_scope(env: Env, key: Symbol) {
        let key_clone = key.clone();
        {
            let mt = env.storage().persistent().max_ttl();
            env.storage().persistent().extend_ttl(&key_clone, 1, mt);
        }
    }
}
