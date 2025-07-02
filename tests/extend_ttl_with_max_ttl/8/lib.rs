use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct FqPath;

#[contractimpl]
impl FqPath {
    pub fn fq_path(env: Env, key: Symbol) {
        let mt = env.storage().persistent().max_ttl();
        env.storage().persistent().extend_ttl(&key, 2, mt);
    }
}
