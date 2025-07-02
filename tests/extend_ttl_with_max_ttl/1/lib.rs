use soroban_sdk::{contract, contractimpl, Address, Env, Symbol, Vec};
#[contract]
pub struct TTLExamples;

#[contractimpl]
impl TTLExamples {
    pub fn basic_extend_data(env: Env, key: Symbol, days: u32) {
        let ledgers = days * 17280; // ~17280 ledgers per day
        let max_ttl = env.storage().persistent().max_ttl();
        let extension = ledgers.min(max_ttl);

        env.storage()
            .persistent()
            .extend_ttl(&key, extension, max_ttl);
    }
}
