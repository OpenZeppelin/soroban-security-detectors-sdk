use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct AliasStorage;

fn get_max_ttl(env: &Env) -> u32 {
    env.storage().persistent().max_ttl()
}

#[contractimpl]
impl AliasStorage {
    pub fn alias_storage(env: Env, key: Symbol, days: u32) {
        let max_ttl = get_max_ttl(&env);
        let extension = days.min(max_ttl);
        let st = env.storage().persistent();
        st.extend_ttl(&key, extension, max_ttl);
    }
}
