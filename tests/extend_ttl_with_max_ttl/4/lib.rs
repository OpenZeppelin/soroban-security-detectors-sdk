use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct AliasStorage;

#[contractimpl]
impl AliasStorage {
    pub fn alias_storage(env: Env, key: Symbol, days: u32) {
        let st = env.storage().persistent();
        let max_ttl = st.max_ttl();
        let extension = days.min(max_ttl);
        st.extend_ttl(&key, extension, max_ttl);
    }
}
