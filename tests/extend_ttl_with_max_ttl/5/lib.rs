use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct NestedAlias;

#[contractimpl]
impl NestedAlias {
    pub fn nested_alias(env: Env, key: Symbol, days: u32) {
        let m1 = env.storage().persistent().max_ttl();
        let m2 = m1;
        let extension = days.min(m2);
        env.storage().persistent().extend_ttl(&key, extension, m2);
    }
}
