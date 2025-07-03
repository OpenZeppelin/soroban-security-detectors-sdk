use soroban_sdk::{contract, contractimpl, Env, Symbol};
#[contract]
pub struct AliasTwo;

#[contractimpl]
impl AliasTwo {
    pub fn alias_two(env: Env, key: Symbol) {
        let st = env.storage().persistent();
        let max_ttl_storage = st.max_ttl();
        st.extend_ttl(&key, 3, max_ttl_storage);
    }
}
