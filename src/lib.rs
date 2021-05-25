use crate::parser::{StateMachine, StateId};

mod parser;

pub type Result<T> = std::result::Result<T, failure::Error>;

fn to_plant_uml(state_machine: &StateMachine) -> String {

    fn escaped_state_id(state_id: &StateId) -> String {
        // TODO: not right but here so we can fix later. Will want to escape for plantuml rules
        format!("{}", state_id)
    }

    let mut lines: Vec<String> = vec![];

    lines.push("@startuml".into());
    lines.push("".into());

    // output all the states so we know what we've got;
    for state in state_machine.states.iter()    {
        lines.push(format!("state {}", escaped_state_id(&state.id)));
    }

    // entry line;
    lines.push("".into());
    let first_state = state_machine.states
        .iter()
        .find(|s| s.is_starting_state)
        .expect("no starting state");

    lines.push(format!("[*] -> {}", first_state.id));

    // all the other transitions;
    for accept_state in state_machine.accept_states.iter() {
        lines.push(format!("{} -> {}", escaped_state_id(&accept_state.source()), escaped_state_id(&accept_state.target())));

    }

    lines.push("".into());

    lines.push("@enduml".into());

    lines.join("\n").into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::*;

    fn load_file_content(file_path_str: &str) -> String {
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).expect("could not read file");
        let file_content =
            String::from_utf8(byte_vec.clone()).expect("should be able to read the file");
        file_content
    }

    fn save_file_content(file_path_str: &str, content: &str) {
        std::fs::write(file_path_str, content).unwrap();
    }

    #[test]
    fn exports_to_plantuml() -> Result<()> {
        let input_file_content = load_file_content("assets/fsml/pod-lifecycle.fsml");
        let output_file_content = load_file_content("assets/plantuml/pod-lifecycle.puml");

        let fsm : StateMachine = state_machine().parse(input_file_content.as_bytes()).expect("could not parse");

        let plant = to_plant_uml(&fsm);

        // for debugging state machines
        //save_file_content("assets/plantuml/pod-lifecycle.puml", &plant);
        assert_eq!(plant, output_file_content);
        Ok(())
    }

}