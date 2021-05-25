use crate::parser::StateMachine;

mod parser;

pub type Result<T> = std::result::Result<T, failure::Error>;

fn to_plant_uml(state_machine: &StateMachine) -> String {
    let mut result = String::new();

    result.push_str("@startuml");
    result.push_str("@enduml");
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    #[test]
    fn exports_to_plantuml() -> Result<()> {
        let file_path_str = "assets/fsml/simple-state-machine.fsml";
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let file_content =
            String::from_utf8(byte_vec.clone()).expect("should be able to read the file");
        let byte_slice: &[u8] = &byte_vec;

        let fsm : StateMachine = state_machine().parse(byte_slice).expect("could not parse");

        let plant = to_plant_uml(&fsm);
        Ok(())
    }

}