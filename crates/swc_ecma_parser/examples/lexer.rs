use std::vec;

use swc_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecma_parser::{lexer::Lexer, Capturing, EsSyntax, Parser, StringInput, Syntax};

fn main() {
    let cm: Lrc<SourceMap> = Default::default();
    // let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false,
    // Some(cm.clone()));

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");

    let fm = cm.new_source_file(
        FileName::Custom("test.js".into()).into(),
        "'\\\nabc'".into(),
    );

    let lexer = Lexer::new(
        Syntax::Es(EsSyntax {
            jsx: true,
            ..Default::default()
        }),
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut tokens = vec![];

    for token in lexer {
        tokens.push(token)
    }

    println!("tokens :{:?}", tokens);

    // let capturing = Capturing::new(lexer);

    // let mut parser = Parser::new_from(capturing);

    // for e in parser.take_errors() {
    //     e.into_diagnostic(&handler).emit();
    // }

    // let _module = parser
    //     .parse_module()
    //     .map_err(|e| e.into_diagnostic(&handler).emit())
    //     .expect("Failed to parse module.");

    // println!("Tokens: {:?}", parser.input().take());
}
