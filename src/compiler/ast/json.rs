use crate::compiler::ast::ExprHandle;
use json::object;
use json::JsonValue;

use crate::compiler::identifier::VariableIdentifier;
use crate::compiler::identifier::{Identifier, StructId};
use crate::compiler::types::Type;

use super::expression::*;
use super::statement::*;
use super::visitor::ExprVisitor;
use super::visitor::ProgramVisitor;
use super::visitor::StmtVisitor;
use super::AST;

impl From<Identifier> for JsonValue {
  fn from(value: Identifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<VariableIdentifier> for JsonValue {
  fn from(value: VariableIdentifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<StructId> for JsonValue {
  fn from(value: StructId) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<&Type> for JsonValue {
  fn from(value: &Type) -> Self {
    JsonValue::String(format!("{:?}", value))
  }
}

fn type_list_to_json(types: &[Type]) -> JsonValue {
  types
    .iter()
    .map(|ty| JsonValue::String(format!("{:?}", ty)))
    .collect::<Vec<_>>()
    .into()
}

fn expr_list_to_json(expressions: &[ExprHandle], ast: &AST) -> JsonValue {
  let mut printer = ASTJSONPrinter {};
  JsonValue::Array(
    expressions
      .iter()
      .map(|&e| printer.visit_expr(ast, e))
      .collect::<Vec<_>>(),
  )
}

pub struct ASTJSONPrinter {}

impl ExprVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_literal_string(&mut self, ast: &AST, literal_string: &expr::LiteralString) -> JsonValue {
    object! {
      "LiteralString": {
        "value": literal_string.handle.get(ast).to_string()
      }
    }
  }

  fn visit_literal_number(&mut self, _ast: &AST, literal_number: &expr::LiteralNumber) -> JsonValue {
    object! {
      "LiteralNumber": {
        "value": literal_number.value.to_string()
      }
    }
  }

  fn visit_literal_bool(&mut self, _ast: &AST, literal_bool: &expr::LiteralBool) -> JsonValue {
    object! {
      "LiteralBool": {
        "value": literal_bool.value
      }
    }
  }

  fn visit_id(&mut self, _ast: &AST, id: &expr::Id) -> JsonValue {
    object! {
      "Id": {
        "id": id.id,
        "id_type": format!("{:?}", id.id_type)
      }
    }
  }

  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> JsonValue {
    object! {
      "Paren": {
        "expr": self.visit_expr(ast, paren.inner)
      }
    }
  }

  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> JsonValue {
    object! {
      "Assignment": {
        "id": assignment.id,
        "value": self.visit_expr(ast, assignment.value),
        "type_": &assignment.type_
      }
    }
  }

  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> JsonValue {
    object! {
      "Binary": {
        "operator": format!("{}", binary.operator),
        "left": self.visit_expr(ast, binary.left),
        "right": self.visit_expr(ast, binary.right),
        "expr_type": &binary.expr_type
      }
    }
  }

  fn visit_logical(&mut self, ast: &AST, logical: &expr::Logical) -> JsonValue {
    object! {
      "Logical": {
        "operator": format!("{}", logical.operator),
        "left": self.visit_expr(ast, logical.left),
        "right": self.visit_expr(ast, logical.right),
        "expr_type": &logical.expr_type
      }
    }
  }

  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> JsonValue {
    object! {
      "Unary": {
        "operator": format!("{}", unary.operator),
        "right": self.visit_expr(ast, unary.right),
        "expr_type": &unary.expr_type,
      }
    }
  }

  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> JsonValue {
    let body = lambda
      .body
      .iter()
      .map(|&stmt| self.visit_stmt(ast, stmt))
      .collect::<Vec<_>>();
    object! {
      "Lambda": {
        "captures": lambda.captures.as_slice(),
        "return_type": &lambda.return_type,
        "parameter_types": type_list_to_json(&lambda.parameter_types),
        "body": body
      }
    }
  }

  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> JsonValue {
    let arguments = expr_list_to_json(&fn_call.arguments, ast);
    object! {
      "FnCall": {
        "function": self.visit_expr(ast, fn_call.func),
        "arguments": arguments,
        "expr_type": &fn_call.expr_type,
      }
    }
  }

  fn visit_member_get(&mut self, ast: &AST, member_get: &expr::MemberGet) -> JsonValue {
    object! {
      "MemberGet": {
        "lhs": self.visit_expr(ast, member_get.lhs),
        "member_index": format!("{:?}", member_get.member_index)
      }
    }
  }

  fn visit_member_set(&mut self, ast: &AST, member_set: &expr::MemberSet) -> JsonValue {
    object! {
      "MemberSet": {
        member: self.visit_member_get(ast, &member_set.member),
        value: self.visit_expr(ast, member_set.value),

      }
    }
  }

  fn visit_dot_call(&mut self, ast: &AST, dot_call: &expr::DotCall) -> JsonValue {
    let arguments = expr_list_to_json(&dot_call.arguments, ast);
    object! {
      "DotCall": {
        "lhs": self.visit_expr(ast, dot_call.lhs),
        "function": dot_call.function,
        "arguments": arguments
      }
    }
  }

  fn visit_constructor(&mut self, ast: &AST, constructor: &expr::Construct) -> JsonValue {
    let arguments = expr_list_to_json(&constructor.arguments, ast);
    object! {
      "Constructor": {
        "struct_id": constructor.struct_id,
        "arguments": arguments
      }
    }
  }
}

impl StmtVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &stmt::VarDecl) -> JsonValue {
    object! {
      "VarDecl": {
        "id": var_decl.identifier,
        "type": format!("{:?}", var_decl.var_type),
        "init expr": self.visit_expr(ast, var_decl.init_expr)
      }
    }
  }

  fn visit_stmt_expr(&mut self, ast: &AST, expr: &stmt::StmtExpr) -> JsonValue {
    object! {
      "StmtExpr": {
        "expr": self.visit_expr(ast, expr.expr),
        "expr_type": &expr.expr_type
      }
    }
  }

  fn visit_block(&mut self, ast: &AST, block: &stmt::Block) -> JsonValue {
    let stmts = block
      .statements
      .iter()
      .map(|&s| self.visit_stmt(ast, s))
      .collect::<Vec<_>>();
    object! {
      "Block": {
        "locals": format!("{}", block.locals),
        "statements": stmts
      }
    }
  }

  fn visit_if_branch(&mut self, ast: &AST, if_branch: &stmt::IfBranch) -> JsonValue {
    let else_branch = if let Some(stmt) = if_branch.else_branch {
      self.visit_stmt(ast, stmt)
    } else {
      JsonValue::Null
    };
    object! {
      "IfBranch": {
        "condition": self.visit_expr(ast, if_branch.condition),
        "true branch": self.visit_stmt(ast, if_branch.true_branch),
        "else branch": else_branch
      }
    }
  }

  fn visit_while(&mut self, ast: &AST, while_stmt: &stmt::While) -> JsonValue {
    object! {
      "While": {
        "condition": self.visit_expr(ast, while_stmt.condition),
        "body": self.visit_stmt(ast, while_stmt.loop_body)
      }
    }
  }

  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &stmt::FunctionDefinition,
  ) -> JsonValue {
    let body = function_definition
      .body
      .iter()
      .map(|&stmt| self.visit_stmt(ast, stmt))
      .collect::<Vec<_>>();
    object! {
      "FunctionDefinition": {
        "id": function_definition.id,
        "captures": function_definition.captures.as_slice(),
        "parameter types": type_list_to_json(&function_definition.parameter_types),
        "return type": &function_definition.return_type,
        "body": body
      }
    }
  }

  fn visit_function_declaration(
    &mut self,
    _ast: &AST,
    function_declaration: &stmt::FunctionDeclaration,
  ) -> JsonValue {
    object! {
      "FunctionDeclaration": {
        "id": function_declaration.id,
        "parameter types": type_list_to_json(&function_declaration.parameter_types),
        "return type": &function_declaration.return_type,
      }
    }
  }

  fn visit_extern_function(
    &mut self,
    _ast: &AST,
    extern_function: &stmt::ExternFunction,
  ) -> JsonValue {
    object! {
      "ExternalFunction": {
        "identifier": format!("{:?}", extern_function.identifier),
        "parameter_types": type_list_to_json(&extern_function.parameter_types),
        "return_type": &extern_function.return_type,
      }
    }
  }

  fn visit_break(&mut self, _ast: &AST, _break_node: &stmt::Break) -> JsonValue {
    JsonValue::String("Break".to_string())
  }

  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> JsonValue {
    let ret_expr = if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr)
    } else {
      JsonValue::Null
    };
    object! {
      "Return": {
        "expr": ret_expr
      }
    }
  }

  fn visit_struct(&mut self, _ast: &AST, struct_stmt: &stmt::Struct) -> JsonValue {
    object! {
      "Struct": {
        "id": format!("{:?}", struct_stmt.id),
      }
    }
  }

  fn visit_import(&mut self, _ast: &AST, import: &stmt::Import) -> JsonValue {
    object! {
      "Import": {
        "module id": format!("{}", import.module_id.0)
      }
    }
  }
}

impl ASTJSONPrinter {
  pub fn print_to_string(ast: &AST) -> String {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    json::stringify_pretty(program, 1)
  }

  pub fn to_json(ast: &AST) -> JsonValue {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    JsonValue::Array(program)
  }
}
