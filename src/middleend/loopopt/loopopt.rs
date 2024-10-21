// pub fn pass(ir: Vec<IRNode>) -> Vec<IRNode> {
//     let mut res = Vec::new();
//     let mut in_func = false;
//     let mut func_inner = Vec::new();
//     for node in ir.iter() {
//         match node {
//             IRNode::FuncBegin(_, _, _) => {
//                 res.push(node.clone());
//                 in_func = true;
//             }
//             IRNode::FuncEnd => {
//                 let mut df = DataFlow::from(func_inner.clone());
//                 df.main();
//                 res.extend(df.get_ir());
//                 func_inner.clear();
//                 in_func = false;
//                 res.push(node.clone());
//             }
//             _ => {
//                 if in_func {
//                     func_inner.push(node.clone());
//                 } else {
//                     res.push(node.clone());
//                 }
//             }
//         }
//     }
//     res
// }
